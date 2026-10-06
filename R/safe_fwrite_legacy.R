#' Legacy safe CSV export
#'
#' @description
#' `safe_fwrite_legacy()` preserves the retired `safe_fwrite()` interface
#' for compatibility with older scripts. It is deprecated and will be
#' removed in a future version of cdetidy.
#'
#' New code should use [safe_fwrite()] with an explicit output path. Use
#' [build_warehouse_path()] when a warehouse destination must be constructed.
#'
#' @param data A data frame to export.
#' @param path Optional output file path. When `NULL`, the function constructs
#'   a warehouse path from `data_source`, `data_type`, and `table_name`.
#' @param char_cols Character vector containing columns that must be written as
#'   character. Defaults to common organization identifier columns.
#' @param compress Logical. When `TRUE`, appends `.gz` to the output path and
#'   writes a compressed CSV file.
#' @param n_check Nonnegative integer specifying the number of written rows to
#'   read and print as a preview.
#' @param log_metadata Optional named list containing legacy export metadata.
#'   Recognized elements are `data_year`, `data_source`, `data_description`,
#'   and `user_note`.
#' @param data_year Optional year associated with the exported data.
#' @param data_source Data source. Accepted values are `"Assessment"`, `"CDE"`,
#'   and `"Dashboard"`.
#' @param data_type Legacy warehouse category used to construct the output path
#'   when `path` is `NULL`.
#' @param data_description Human-readable description of the exported data.
#'   Required when `write_log = TRUE`.
#' @param user_note Legacy export note. The note must contain either `"fact"`
#'   or `"dim"` so the function can infer the table type.
#' @param table_name Base name of the exported table.
#' @param dim_description Optional description appended to the filename for a
#'   dimension table.
#' @param log_path Path to the legacy export-log CSV file.
#' @param canonical_table_id Optional stable identifier for the exported table.
#'   Defaults to `table_name`.
#' @param dimension_type Optional dimension classification. Accepted values are
#'   `"universal"`, `"annualized"`, and `"other"`.
#' @param overwrite Logical. When `FALSE`, the function stops if the output
#'   file already exists.
#' @param write_log Logical. When `TRUE`, the function creates or updates the
#'   legacy export log at `log_path`.
#'
#' @return Invisibly returns a one-row data frame containing the legacy export
#'   metadata.
#'
#' @details
#' This function retains deprecated behavior, including inferring the table
#' type from `user_note`, accepting metadata through `log_metadata`, and
#' optionally constructing warehouse paths from an internal category catalog.
#' These behaviors are not supported by the current [safe_fwrite()] interface.
#'
#' @seealso [safe_fwrite()], [build_warehouse_path()]
#'
#' @export

safe_fwrite_legacy <- function(
    data, path = NULL,
    char_cols = c("cds","county_code","district_code","school_code"),
    compress = FALSE,
    n_check = 6,
    log_metadata = NULL,
    data_year = NULL,
    data_source = NULL,
    data_type = NULL,         # validated below
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
        "`safe_fwrite_legacy()` uses the retired export interface and ",
        "will be removed in a future version of cdetidy. ",
        "Use `safe_fwrite()` with an explicit output path, `table_type`, ",
        "`data_source`, and `data_category`. Use ",
        "`build_warehouse_path()` when a warehouse destination must be ",
        "constructed."
      )
    )
  
  norm_token <- function(x) gsub("[^a-z0-9]+", "", tolower(trimws(as.character(x))))
  
  title_underscore <- function(x) gsub("\\s+", "_", tools::toTitleCase(gsub("_", " ", x)))
  
  # --- catalogs & resolver ---
  catalog <- list(
    Assessment = list(
      values = c("SBAC","CAST","ELPAC", "dim"),
      syns = list(
        SBAC  = c("sbac","smarter","smarterbalanced"),
        CAST  = c("cast","science"),
        ELPAC = c("elpac","englishlanguageproficiency","elpa"),
        dim = c("dim", "dimension"))),
    CDE = list(
      values = c("Absenteeism","Enrollment","Discipline","EL","Grad_Dropout","Post_Secondary", 
                 "Staff", "Alternative_Ed", "Special_Education", "dim"),
      syns = list(
        Absenteeism    = c("absenteeism","chronic","chronicabsenteeism"),
        Enrollment     = c("enrollment","enrol"),
        Discipline     = c("discipline","suspension","suspensions"),
        EL             = c("el","englishlearner","ell","englishlearners"),
        Grad_Dropout   = c("graddropout","graduation","grad","dropout","cohort"),
        Post_Secondary = c("postsecondary","post_secondary","collegecareer","cci","collegeandcareer"),
        Staff = c("staff", "certificated", "classified", "tamo", "hire"),
        Alternative_Ed = c("alted", "alt", "juvenile", "community schools", "juvenile court"),
        Special_Education = c("special_ed", "sped", "special_education"),
        dim = c("dim", "dimension"))),
    Dashboard = list(
      values = c("Achievement", "Engagement", "Climate", "Broad_Course", "Info_Only", "dim"),
      syns = list(
        Achievement = c("ela", "math", "elpi", "academics"),
        Engagement = c("grad", "grad_rate", "gr", "absenteeism", "ca", "chronic"),
        Climate = c("suspension","sus", "susp"),
        Broad_Course = c("cci", "college_and_career", "college", "career"),
        Info_Only = c("science", "sci", "growth_rate", "growth"),
        dim = c("dim", "dimension"))))
  
  resolve_type <- function(ds_label, dt_input) {
    catg <- catalog[[ds_label]]
    if (is.null(catg)) stop("❌ Unsupported data_source catalog: ", ds_label)
    tok <- norm_token(dt_input)
    direct <- match(tok, norm_token(catg$values))
    if (!is.na(direct)) return(catg$values[direct])
    for (v in names(catg$syns)) if (tok %in% norm_token(catg$syns[[v]])) return(v)
    stop("❌ For data_source=", ds_label,
         " the `data_type` must be one of: ",
         paste(catg$values, collapse=", "),
         " (case-insensitive; synonyms accepted).")
  }
  
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  
  if (length(n_check) != 1L || is.na(n_check) || n_check < 0L) {
    stop("`n_check` must be one nonnegative number.", call. = FALSE)
  }
  n_check <- as.integer(n_check)
  
  # --- build/validate metadata ---
  if (is.null(log_metadata)) {
    if (is.null(data_source)) stop("❌ Provide `data_source` (or a `log_metadata` list).")
    log_metadata <- list(
      data_year       = data_year,        # optional, for audit only
      data_source     = data_source,
      data_description= data_description,
      user_note       = user_note)
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
  
  if (isTRUE(write_log) &&
      (is.null(data_description) ||
       is.na(data_description) ||
       data_description == ""))
    stop("❌ Please provide `data_description`.")
  
  ds_label <- (function(x){
    tok <- norm_token(x)
    out <- c(assessment="Assessment", cde="CDE", dashboard="Dashboard")[tok]
    if (is.na(out)) stop("❌ `data_source` must be one of: Assessment, CDE, Dashboard.")
    out
  })(data_source)
  
  if (is.null(table_name)) stop("❌ Please supply `table_name`.")
  if (is.null(user_note) || !grepl("\\b(fact|dim)\\b", user_note, ignore.case = TRUE))
    stop("❌ `user_note` must include 'fact' or 'dim'.")
  table_type <- tolower(stringr::str_extract(user_note, "\\b(fact|dim)\\b"))
  if (is.na(table_type)) stop("❌ Could not parse table type from `user_note`.")
  if (!is.null(dim_description)) dim_description <- janitor::make_clean_names(dim_description)
  
  final_table_name <- paste0(
    table_name,
    if (!is.null(dim_description)) paste0("_", dim_description),
    "_", table_type)
  
  # --- auto path: T:/Data Warehouse/{DataSource}/{DataType}/ ---
  if (is.null(path)) {
    if (is.null(data_type)) stop("❌ Please provide `data_type` when `path` is NULL.")
    type_label  <- resolve_type(ds_label, data_type)
    type_folder <- title_underscore(type_label)
    base_dir    <- "T:/Data Warehouse"
    path <- file.path(base_dir, ds_label, type_folder, paste0(final_table_name, ".csv"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }
  if (compress && !grepl("\\.gz$", path)) path <- paste0(path, ".gz")
  
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop(
      "Output file already exists: ",
      path,
      ". Use `overwrite = TRUE` to replace it.",
      call. = FALSE)
  }
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  # --- enforce character on code columns ---
  default_char_cols <- if (table_type == "dim")
    c("cds","county_code","district_code","school_code") else c("cds")
  target_char_cols <- intersect(unique(c(char_cols, default_char_cols)), names(data))
  if (length(target_char_cols))
    data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(target_char_cols), as.character))
  
  # --- write + quick preview ---
  data.table::fwrite(data, path)
  message("✅ File written: ", path)
  if (n_check > 0L) {
    preview <- if (length(target_char_cols) > 0L) {
      data.table::fread(
        path,
        nrows = n_check,
        colClasses = list(character = target_char_cols))
    } else {
      data.table::fread(path, nrows = n_check)
    }
    print(preview)
  }
  
  # --- logging ---
  fi <- file.info(path)
  if (is.na(fi$size)) stop("File does not exist or is unreadable: ", path)
  
  if (is.null(canonical_table_id)) {
    canonical_table_id <- table_name
  }
  if (is.null(dimension_type)) {
    dimension_type <- NA_character_
  }
  valid_dimension_types <- c("universal","annualized","other")
  if (!is.na(dimension_type) && !tolower(dimension_type) %in% valid_dimension_types)
    stop("❌ `dimension_type` must be one of: ", paste(valid_dimension_types, collapse = ", "))
  dimension_type <- if (is.na(dimension_type)) NA_character_ else tolower(dimension_type)
  
  log_entry <- data.frame(
    timestamp        = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    file_name        = basename(path),
    file_path        = normalizePath(path),
    file_size_MB     = round(fi$size / 1e6, 2),
    canonical_table_id = canonical_table_id,
    dimension_type   = dimension_type,
    n_rows           = nrow(data),
    n_cols           = ncol(data),
    data_year        = if (is.null(data_year)) NA else data_year,
    data_source      = ds_label,
    table_type       = table_type,
    data_description = data_description,
    dim_description  = if (is.null(dim_description)) NA else dim_description,
    user_note        = user_note,
    user             = Sys.info()[["user"]],
    stringsAsFactors = FALSE)
  
  if (isTRUE(write_log)) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    
    if (file.exists(log_path)) {
      existing_log <- read.csv(log_path, stringsAsFactors = FALSE)
      match_idx <- which(
        existing_log$canonical_table_id == canonical_table_id)
      
      if (length(match_idx)) {
        existing_log[match_idx[1], ] <- log_entry
        write.csv(existing_log, log_path, row.names = FALSE)
        message("🔁 Existing log entry overwritten for: ", log_entry$file_name)
      } else {
        write.table(
          log_entry,
          log_path,
          append = TRUE,
          sep = ",",
          row.names = FALSE,
          col.names = FALSE)
        message("📝 Log entry appended to: ", log_entry$file_name)
      }
    } else {
      write.table(
        log_entry,
        log_path,
        append = FALSE,
        sep = ",",
        row.names = FALSE,
        col.names = TRUE)
      message("📄 New log created: ", log_path)
    }
  }
  
  invisible(log_entry)
}
