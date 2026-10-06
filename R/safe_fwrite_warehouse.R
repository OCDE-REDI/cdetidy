#' Legacy warehouse CSV export
#'
#' @description
#' `safe_fwrite_warehouse()` preserves the retired warehouse-specific export
#' interface for compatibility with older scripts. It is deprecated and will
#' be removed in a future version of cdetidy.
#'
#' New code should use [build_warehouse_path()] to construct the destination
#' and [safe_fwrite()] to validate and write the file.
#'
#' @details
#' This function retains deprecated behavior, including inferring the table
#' type from `user_note`, accepting metadata through `log_metadata`, and
#' constructing paths within the former Warehouse Ready Files directory.
#'
#' No new code should depend on this interface.
#'
#' The legacy function:
#'
#' - Constructs names using
#'   `<table_name>_<dim_description>_<fact|dim>.csv`.
#' - Coerces selected identifier columns to character.
#' - Creates missing output directories.
#' - Replaces matching legacy export-log entries.
#'
#' @param data A data frame to be exported.
#' @param path Optional. A full file path to write the CSV file. If `NULL`, a default path is constructed using metadata fields.
#' @param char_cols A character vector of column names to force as character before export. Defaults to key ID fields (`cds`, `county_code`, etc.).
#' @param compress Logical. If `TRUE`, writes the file as a gzipped `.csv.gz`. Default is `FALSE`.
#' @param n_check Number of rows to preview (with `fread()`) after writing. Default is 6.
#' @param log_metadata Optional. A named list containing key metadata: `data_year`, `data_source`, `data_description`, `user_note`.
#' @param data_year Data year (numeric or character) — required if `log_metadata` is not supplied.
#' @param data_source A string describing the data source (e.g., `"CDE"`, `"Dashboard"`).
#' @param data_description A short description of the dataset (e.g., `"Absenteeism summary by subgroup"`).
#' @param user_note A comment describing the export context (must include `"fact"` or `"dim"` to infer table type).
#' @param table_name Base name for the output table.
#' @param dim_description Optional. A short, clean label for the dimension table (used in the file name).
#' @param log_path Path to the export log CSV file. Default is `"export_log.csv"` within the warehouse structure.
#' @param canonical_table_id Optional. A unique identifier for the export record. Defaults to `table_name`.
#' @param dimension_type Optional. Type of dimension table (`"universal"`, `"annualized"`, or `"other"`).
#' @param overwrite Logical. If `FALSE`, stop when the output file already
#'   exists. Default is `FALSE`.
#' @param write_log Logical. If `TRUE`, create or update the export metadata
#'   log at `log_path`. Default is `FALSE`.
#'
#' @return Invisibly returns a one-row data frame containing the export log
#'   entry.
#'
#' @seealso [safe_fwrite()], [build_warehouse_path()]
#'
#' @export

safe_fwrite_warehouse <- function(data, path = NULL,
                                  char_cols = c("cds", "county_code", "district_code", "school_code"),
                                  compress = FALSE,
                                  n_check = 6,
                                  log_metadata = NULL,
                                  data_year = NULL,
                                  data_source = NULL,
                                  data_description = NA,
                                  user_note = NA,
                                  table_name = NULL,
                                  dim_description = NULL,
                                  log_path = "export_log.csv",
                                  canonical_table_id = NULL,
                                  dimension_type = NULL,
                                  overwrite = FALSE,
                                  write_log = FALSE) {

  .Deprecated(
    new = "safe_fwrite",
    package = "cdetidy",
    msg = paste0(
      "`safe_fwrite_warehouse()` is deprecated and will be removed ",
      "in a future version of cdetidy. Use `build_warehouse_path()` ",
      "to construct the destination and `safe_fwrite()` to validate ",
      "and write the file."
    )
  )

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (length(n_check) != 1L || is.na(n_check) || n_check < 0L) {
    stop("`n_check` must be one nonnegative number.", call. = FALSE)
  }
  n_check <- as.integer(n_check)

  # Allow shorthand metadata if log_metadata not provided
  if (is.null(log_metadata)) {
    if (is.null(data_year) || is.null(data_source)) {
      stop("❌ You must supply either `log_metadata` or both `data_year` and `data_source`.")
    }
    log_metadata <- list(
      data_year = data_year,
      data_source = data_source,
      data_description = data_description,
      user_note = user_note
    )
  }

  if (!is.null(log_metadata$data_year)) {
    data_year <- log_metadata$data_year
  }
  if (!is.null(log_metadata$data_source)) {
    data_source <- log_metadata$data_source
  }
  if (!is.null(log_metadata$data_description)) {
    data_description <- log_metadata$data_description
  }
  if (!is.null(log_metadata$user_note)) {
    user_note <- log_metadata$user_note
  }

  if (is.null(path)) {
    required_values <- list(
      data_year = data_year,
      data_source = data_source,
      table_name = table_name)

    missing_args <- names(required_values)[
      vapply(
        required_values,
        function(x) is.null(x) || length(x) == 0L || all(is.na(x)),
        logical(1))]

    if (length(missing_args) > 0L) {
      stop(
        "❌ To auto-generate the path, you must supply: ",
        paste(missing_args, collapse = ", "),
        ".",
        call. = FALSE)
    }
  }

  # Validate that data_description is provided
  if (isTRUE(write_log) &&
      (is.null(data_description) ||
       is.na(data_description) ||
       data_description == "")) {
    stop("❌ Please provide a brief description of what's in this data file using `data_description`.")
  }

  # Validate required metadata fields
  required_fields <- c("data_year", "data_source")
  missing_fields <- setdiff(required_fields, names(log_metadata))
  if (length(missing_fields) > 0) {
    stop("❌ Missing required `log_metadata` fields: ",
         paste(missing_fields, collapse = ", "))
  }

  # Validate and standardize data_source values
  allowed_sources <- c("Assessment", "CDE", "Dashboard")
  input_source <- tolower(data_source)
  matched_idx <- match(input_source, tolower(allowed_sources))

  if (is.na(matched_idx)) {
    stop("❌ `data_source` must be one of: ", paste(allowed_sources, collapse = ", "))
  }

  # Store canonical label for logging (title-case)
  log_metadata$data_source <- allowed_sources[matched_idx]

  # Store lowercase version separately if you need it for folder names
  folder_data_source <- tolower(allowed_sources[matched_idx])

  # Validate user_note contains "fact" or "dim"
  if (is.null(user_note) ||
      is.na(user_note) ||
      !grepl("\\bfact\\b|\\bdim\\b", user_note, ignore.case = TRUE)) {
    stop("❌ Your `user_note` must clearly include either 'fact' or 'dim' in the note.")
  }

  # Determine table suffix
  table_type <- tolower(
    stringr::str_extract(user_note, "\\b(fact|dim)\\b"))
  if (is.na(table_type)) {
    stop("❌ Your `user_note` must clearly include either 'fact' or 'dim'.")
  }

  # Clean dim_description for file name (if provided)
  if (!is.null(dim_description)) {
    dim_description <- janitor::make_clean_names(dim_description)
  }

  # Build enhanced table name
  final_table_name <- paste0(
    table_name,
    if (!is.null(dim_description)) paste0("_", dim_description),
    "_", table_type
  )

  # Auto-generate saving path, T drive specific
  if (is.null(path)) {
    warehouse_dir <- "T:/Data Warehouse/Warehouse Ready Files"
    path <- file.path(warehouse_dir,
                      as.character(data_year),
                      "Data Files",
                      folder_data_source,
                      paste0(final_table_name, ".csv"))

    # Make sure the folder exists
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  # Apply compression if requested
  if (compress && !grepl("\\.gz$", path)) {
    path <- paste0(path, ".gz")
  }

  if (file.exists(path) && !isTRUE(overwrite)) {
    stop(
      "Output file already exists: ",
      path,
      ". Use `overwrite = TRUE` to replace it.",
      call. = FALSE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  # Define required character columns based on table type.
  default_char_cols <- if (table_type == "dim") {
    c("cds", "county_code", "district_code", "school_code")
  } else if (table_type == "fact") {
    c("cds")
  } else {
    stop("❌ Could not determine table type from `user_note`. Please include 'fact' or 'dim'.")
  }

  # Filter to only existing columns in the data
  existing_char_cols <- intersect(
    unique(c(char_cols, default_char_cols)),
    names(data))

  # Coerce character columns
  if (length(existing_char_cols) > 0L) {
    data <- dplyr::mutate(
      data,
      dplyr::across(
        dplyr::all_of(existing_char_cols),
        as.character))
  }

  # Write the file
  data.table::fwrite(data, path)
  message("✅ File written: ", path)

  # Preview first n rows with character column types enforced
  if (n_check > 0L) {
    preview <- if (length(existing_char_cols) > 0L) {
      data.table::fread(
        path,
        nrows = n_check,
        colClasses = list(character = existing_char_cols))
    } else {
      data.table::fread(path, nrows = n_check)
    }
    print(preview)
  }

  # File info
  file_info <- file.info(path)
  if (is.na(file_info$size)) {
    stop("File does not exist or is unreadable: ", path)
  }

  canonical_table_id <- if (is.null(canonical_table_id)) table_name else canonical_table_id
  dimension_type     <- if (is.null(dimension_type)) NA_character_ else dimension_type

  valid_dimension_types <- c("universal", "annualized", "other")
  if (!is.null(dimension_type) && !is.na(dimension_type)) {
    if (!tolower(dimension_type) %in% valid_dimension_types) {
      stop("❌ `dimension_type` must be one of: ",
           paste(valid_dimension_types, collapse = ", "))
    }
    dimension_type <- tolower(dimension_type)
  }

  # Construct and write log entry
  log_entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    file_name = basename(path),
    file_path = normalizePath(path),
    file_size_MB = round(file_info$size / 1e6, 2),
    canonical_table_id = canonical_table_id,
    dimension_type = dimension_type,
    n_rows = nrow(data),
    n_cols = ncol(data),
    data_year = data_year,
    data_source = log_metadata$data_source,
    table_type = table_type,
    data_description = data_description,
    dim_description = if (is.null(dim_description)) NA else dim_description,
    user_note = user_note,
    user = Sys.info()[["user"]],
    stringsAsFactors = FALSE
  )

  if (isTRUE(write_log)) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)

    if (file.exists(log_path)) {
      existing_log <- read.csv(log_path, stringsAsFactors = FALSE)
      match_idx <- which(
        existing_log$canonical_table_id == canonical_table_id)

      if (length(match_idx) > 0L) {
        existing_log[match_idx[1], ] <- log_entry
        write.csv(existing_log, file = log_path, row.names = FALSE)
        message("🔁 Existing log entry overwritten for: ", log_entry$file_name)
      } else {
        write.table(
          log_entry,
          file = log_path,
          append = TRUE,
          sep = ",",
          row.names = FALSE,
          col.names = FALSE)
        message("📝 Log entry appended to: ", log_path)
      }
    } else {
      write.table(
        log_entry,
        file = log_path,
        append = FALSE,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE)
      message("📄 New log created: ", log_path)
    }
  }

  invisible(log_entry)
}
