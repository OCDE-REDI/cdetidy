#' Safely write a data frame to CSV
#'
#' Writes a nonempty data frame to an explicit CSV destination, protects
#' existing files from accidental replacement, preserves identifier columns
#' as character, verifies the staged output, and optionally updates an export
#' metadata log.
#'
#' File-location rules are intentionally handled outside this function.
#' Use a separate warehouse-path helper to construct standardized Assessment,
#' Dashboard, or CDE destinations.
#'
#' @param data A nonempty data frame to export.
#' @param path Explicit full output path ending in `.csv` or `.csv.gz`.
#' @param table_name Nonempty warehouse table name used in export metadata.
#' @param table_type Either `"fact"` or `"dimension"`.
#' @param data_year Four-digit reporting year represented by the data.
#' @param data_source One of `"Assessment"`, `"Dashboard"`, or `"CDE"`.
#' @param data_category Nonempty category within the source, such as `"CAST"`,
#'   `"Enrollment"`, or `"Science"`.
#' @param data_description Nonempty description of the exported data.
#' @param user_note Optional note about the export.
#' @param canonical_table_id Stable identifier for the table across reporting
#'   years. Defaults to `table_name`.
#' @param dimension_type For dimension tables, one of `"universal"`,
#'   `"annualized"`, or `"other"`. Must be `NULL` for fact tables.
#' @param char_cols Candidate identifier columns to convert to character when
#'   present in `data`.
#' @param n_check Number of rows to read back and display after writing.
#' @param overwrite If `FALSE`, stop when `path` already exists.
#' @param write_log If `TRUE`, update the metadata log at `log_path`.
#' @param log_path Explicit `.csv` path for the export log. Required when
#'   `write_log = TRUE`.
#'
#' @return Invisibly returns a one-row data frame describing the export.
#'
#' @details
#' Data are first written to a temporary file in the destination directory.
#' The staged file must be nonempty and have the expected column names before
#' it replaces the final destination.
#'
#' When overwriting, the existing file is temporarily renamed and restored if
#' the staged file cannot be moved into place.
#'
#' @export

safe_fwrite <- function(
    data,
    path,
    table_name,
    table_type,
    data_year,
    data_source,
    data_category,
    data_description,
    user_note = NA_character_,
    canonical_table_id = table_name,
    dimension_type = NULL,
    char_cols = c(
      "cds",
      "county_code",
      "district_code",
      "school_code"
    ),
    n_check = 6L,
    overwrite = FALSE,
    write_log = FALSE,
    log_path = NULL) {

  # ---------------------------------------
  # Internal validation helpers
  # ---------------------------------------

  is_scalar_string <- function(value,
                               allow_na = FALSE) {
    if (length(value) != 1L ||
        !is.character(value)) {
      return(FALSE)
    }

    if (is.na(value)) {
      return(isTRUE(allow_na))
    }

    nzchar(
      trimws(value)
    )
  }

  validate_logical <- function(value,
                               argument) {
    if (length(value) != 1L ||
        is.na(value) ||
        !is.logical(value)) {
      stop(
        paste0(
          "`",
          argument,
          "` must be TRUE or FALSE."
        ),
        call. = FALSE
      )
    }
  }

  normalize_choice <- function(value,
                               choices,
                               argument) {
    if (!is_scalar_string(value)) {
      stop(
        paste0(
          "`",
          argument,
          "` must be one nonempty character value."
        ),
        call. = FALSE
      )
    }

    matched_position <- match(
      tolower(
        trimws(value)
      ),
      tolower(
        choices
      )
    )

    if (is.na(
      matched_position
    )) {
      stop(
        paste0(
          "`",
          argument,
          "` must be one of: ",
          paste(
            choices,
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }

    choices[[matched_position]]
  }

  replace_file <- function(staged_path,
                           final_path,
                           allow_overwrite) {
    backup_path <- NULL

    if (file.exists(final_path)) {
      if (!isTRUE(
        allow_overwrite
      )) {
        stop(
          paste0(
            "Output file already exists: ",
            final_path,
            ". Set `overwrite = TRUE` to replace it."
          ),
          call. = FALSE
        )
      }

      backup_path <- tempfile(
        pattern = paste0(
          ".",
          basename(final_path),
          "_backup_"
        ),
        tmpdir = dirname(
          final_path
        )
      )

      backup_created <- file.rename(
        final_path,
        backup_path
      )

      if (!isTRUE(
        backup_created
      )) {
        stop(
          paste0(
            "Could not create a recoverable backup of the existing file: ",
            final_path
          ),
          call. = FALSE
        )
      }
    }

    replacement_succeeded <- file.rename(
      staged_path,
      final_path
    )

    if (!isTRUE(
      replacement_succeeded
    )) {
      if (!is.null(backup_path) &&
          file.exists(backup_path)) {
        file.rename(
          backup_path,
          final_path
        )
      }

      stop(
        paste0(
          "Could not move the staged file to its final destination: ",
          final_path
        ),
        call. = FALSE
      )
    }

    if (!is.null(backup_path) &&
        file.exists(backup_path)) {
      unlink(
        backup_path
      )
    }

    invisible(
      final_path
    )
  }

  read_export_file <- function(file_path,
                               nrows,
                               character_columns = character()) {
    compressed_file <- grepl(
      "\\.csv\\.gz$",
      file_path,
      ignore.case = TRUE
    )

    if (!compressed_file) {
      fread_arguments <- list(
        file = file_path,
        nrows = nrows,
        check.names = FALSE,
        showProgress = FALSE
      )

      if (length(
        character_columns
      ) > 0L) {
        fread_arguments$colClasses <- list(
          character = character_columns
        )
      }

      return(
        do.call(
          data.table::fread,
          fread_arguments
        )
      )
    }

    connection <- gzfile(
      file_path,
      open = "rt"
    )

    on.exit(
      close(
        connection
      ),
      add = TRUE
    )

    col_classes <- if (length(
      character_columns
    ) > 0L) {
      stats::setNames(
        rep(
          "character",
          length(
            character_columns
          )
        ),
        character_columns
      )
    } else {
      NA
    }

    utils::read.csv(
      connection,
      nrows = nrows,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      colClasses = col_classes
    )
  }

  # ---------------------------------------
  # Validate data
  # ---------------------------------------

  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0L) {
    stop(
      "`data` must contain at least one row.",
      call. = FALSE
    )
  }

  if (ncol(data) == 0L) {
    stop(
      "`data` must contain at least one column.",
      call. = FALSE
    )
  }

  if (anyDuplicated(
    names(data)
  ) > 0L) {
    duplicate_columns <- unique(
      names(data)[
        duplicated(
          names(data)
        )
      ]
    )

    stop(
      paste0(
        "`data` contains duplicate column name(s): ",
        paste(
          duplicate_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Validate path and overwrite settings
  # ---------------------------------------

  if (!is_scalar_string(path)) {
    stop(
      "`path` must be one explicit, nonempty file path.",
      call. = FALSE
    )
  }

  path <- trimws(
    path
  )

  if (!grepl(
    "\\.csv(?:\\.gz)?$",
    path,
    ignore.case = TRUE
  )) {
    stop(
      "`path` must end in `.csv` or `.csv.gz`.",
      call. = FALSE
    )
  }

  validate_logical(
    overwrite,
    "overwrite"
  )

  validate_logical(
    write_log,
    "write_log"
  )

  if (file.exists(path) &&
      !isTRUE(overwrite)) {
    stop(
      paste0(
        "Output file already exists: ",
        path,
        ". Set `overwrite = TRUE` to replace it."
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Validate table metadata
  # ---------------------------------------

  if (!is_scalar_string(
    table_name
  )) {
    stop(
      "`table_name` must be one nonempty character value.",
      call. = FALSE
    )
  }

  table_name <- trimws(
    table_name
  )

  if (!grepl(
    "^[A-Za-z0-9_]+$",
    table_name
  )) {
    stop(
      paste0(
        "`table_name` may contain only letters, numbers, ",
        "and underscores."
      ),
      call. = FALSE
    )
  }

  table_type <- normalize_choice(
    table_type,
    c(
      "fact",
      "dimension"
    ),
    "table_type"
  )

  if (length(data_year) != 1L ||
      is.na(data_year) ||
      !is.numeric(data_year) ||
      data_year != as.integer(data_year) ||
      data_year < 1900L ||
      data_year > 9999L) {
    stop(
      "`data_year` must be one four-digit reporting year.",
      call. = FALSE
    )
  }

  data_year <- as.integer(
    data_year
  )

  data_source <- normalize_choice(
    data_source,
    c(
      "Assessment",
      "Dashboard",
      "CDE"
    ),
    "data_source"
  )

  if (!is_scalar_string(
    data_category
  )) {
    stop(
      "`data_category` must be one nonempty character value.",
      call. = FALSE
    )
  }

  data_category <- trimws(
    data_category
  )

  if (!is_scalar_string(
    data_description
  )) {
    stop(
      "`data_description` must be one nonempty character value.",
      call. = FALSE
    )
  }

  data_description <- trimws(
    data_description
  )

  if (is.null(user_note) ||
      (
        length(user_note) == 1L &&
        is.character(user_note) &&
        is.na(user_note)
      )) {
    user_note <- NA_character_
  } else if (!is_scalar_string(
    user_note
  )) {
    stop(
      paste0(
        "`user_note` must be one nonempty character value, ",
        "`NA_character_`, or `NULL`."
      ),
      call. = FALSE
    )
  } else {
    user_note <- trimws(
      user_note
    )
  }

  if (is.null(canonical_table_id)) {
    canonical_table_id <- table_name
  }

  if (!is_scalar_string(
    canonical_table_id
  )) {
    stop(
      "`canonical_table_id` must be one nonempty character value.",
      call. = FALSE
    )
  }

  canonical_table_id <- trimws(
    canonical_table_id
  )

  valid_dimension_types <- c(
    "universal",
    "annualized",
    "other"
  )

  if (identical(
    table_type,
    "dimension"
  )) {
    if (is.null(dimension_type) ||
        length(dimension_type) != 1L ||
        !is.character(dimension_type) ||
        is.na(dimension_type) ||
        !nzchar(trimws(dimension_type))) {
      stop(
        paste0(
          "`dimension_type` must be one of: ",
          paste(
            valid_dimension_types,
            collapse = ", "
          ),
          "."
        ),
        call. = FALSE
      )
    }

    dimension_type <- normalize_choice(
      dimension_type,
      valid_dimension_types,
      "dimension_type"
    )
  } else {
    if (!is.null(dimension_type) &&
        !(
          length(dimension_type) == 1L &&
          is.character(dimension_type) &&
          is.na(dimension_type)
        )) {
      stop(
        paste0(
          "`dimension_type` must be `NULL` for fact tables."
        ),
        call. = FALSE
      )
    }

    dimension_type <- NA_character_
  }

  # ---------------------------------------
  # Validate character-column handling
  # ---------------------------------------

  if (is.null(char_cols)) {
    char_cols <- character()
  }

  if (!is.character(char_cols) ||
      anyNA(char_cols) ||
      any(!nzchar(
        trimws(char_cols)
      ))) {
    stop(
      paste0(
        "`char_cols` must be a character vector of nonempty ",
        "column names or `NULL`."
      ),
      call. = FALSE
    )
  }

  char_cols <- trimws(
    char_cols
  )

  if (anyDuplicated(
    char_cols
  ) > 0L) {
    stop(
      "`char_cols` must not contain duplicate column names.",
      call. = FALSE
    )
  }

  target_char_cols <- intersect(
    char_cols,
    names(data)
  )

  # ---------------------------------------
  # Validate preview and logging settings
  # ---------------------------------------

  if (length(n_check) != 1L ||
      is.na(n_check) ||
      !is.numeric(n_check) ||
      n_check != as.integer(n_check) ||
      n_check < 0L) {
    stop(
      "`n_check` must be one nonnegative whole number.",
      call. = FALSE
    )
  }

  n_check <- as.integer(
    n_check
  )

  if (isTRUE(
    write_log
  )) {
    if (!is_scalar_string(
      log_path
    )) {
      stop(
        paste0(
          "`log_path` must be an explicit, nonempty file path ",
          "when `write_log = TRUE`."
        ),
        call. = FALSE
      )
    }

    log_path <- trimws(
      log_path
    )

    if (!grepl(
      "\\.csv$",
      log_path,
      ignore.case = TRUE
    )) {
      stop(
        "`log_path` must end in `.csv`.",
        call. = FALSE
      )
    }

    if (identical(
      normalizePath(
        path,
        mustWork = FALSE
      ),
      normalizePath(
        log_path,
        mustWork = FALSE
      )
    )) {
      stop(
        "`path` and `log_path` must be different files.",
        call. = FALSE
      )
    }
  }

  expected_log_columns <- c(
    "timestamp",
    "file_name",
    "file_path",
    "file_size_mb",
    "canonical_table_id",
    "dimension_type",
    "n_rows",
    "n_cols",
    "data_year",
    "data_source",
    "data_category",
    "table_name",
    "table_type",
    "data_description",
    "user_note",
    "overwritten",
    "user"
  )

  existing_log <- NULL

  if (isTRUE(write_log) &&
      file.exists(log_path)) {
    existing_log <- data.table::fread(
      log_path,
      sep = ",",
      showProgress = FALSE
    )

    missing_log_columns <- setdiff(
      expected_log_columns,
      names(existing_log)
    )

    unexpected_log_columns <- setdiff(
      names(existing_log),
      expected_log_columns
    )

    if (length(missing_log_columns) > 0L ||
        length(unexpected_log_columns) > 0L) {
      stop(
        paste0(
          "The existing export log has an incompatible schema.",
          "\nMissing: ",
          if (length(missing_log_columns) == 0L) {
            "none"
          } else {
            paste(
              missing_log_columns,
              collapse = ", "
            )
          },
          "\nUnexpected: ",
          if (length(unexpected_log_columns) == 0L) {
            "none"
          } else {
            paste(
              unexpected_log_columns,
              collapse = ", "
            )
          }
        ),
        call. = FALSE
      )
    }

    data.table::setcolorder(
      existing_log,
      expected_log_columns
    )
  }

  # ---------------------------------------
  # Prepare export data
  # ---------------------------------------

  export_data <- data.table::copy(
    data.table::as.data.table(
      data
    )
  )

  for (column in target_char_cols) {
    data.table::set(
      export_data,
      j = column,
      value = as.character(
        export_data[[column]]
      )
    )
  }

  # ---------------------------------------
  # Create destination and staged path
  # ---------------------------------------

  destination_directory <- dirname(
    path
  )

  if (!dir.exists(
    destination_directory
  )) {
    directory_created <- dir.create(
      destination_directory,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!isTRUE(
      directory_created
    ) &&
    !dir.exists(
      destination_directory
    )) {
      stop(
        paste0(
          "Could not create destination directory: ",
          destination_directory
        ),
        call. = FALSE
      )
    }
  }

  staged_extension <- if (grepl(
    "\\.csv\\.gz$",
    path,
    ignore.case = TRUE
  )) {
    ".csv.gz"
  } else {
    ".csv"
  }

  staged_path <- tempfile(
    pattern = paste0(
      ".",
      basename(path),
      "_staged_"
    ),
    tmpdir = destination_directory,
    fileext = staged_extension
  )

  on.exit(
    {
      if (file.exists(
        staged_path
      )) {
        unlink(
          staged_path
        )
      }
    },
    add = TRUE
  )

  # ---------------------------------------
  # Write and validate staged file
  # ---------------------------------------

  data.table::fwrite(
    export_data,
    staged_path
  )

  staged_info <- file.info(
    staged_path
  )

  if (!file.exists(staged_path) ||
      is.na(staged_info$size) ||
      staged_info$size <= 0) {
    stop(
      "The staged export file is missing or empty.",
      call. = FALSE
    )
  }

  staged_header <- read_export_file(
    file_path = staged_path,
    nrows = 0L
  )

  if (!identical(
    names(staged_header),
    names(export_data)
  )) {
    stop(
      paste0(
        "The staged export header does not match the source data.",
        "\nExpected: ",
        paste(
          names(export_data),
          collapse = ", "
        ),
        "\nFound: ",
        paste(
          names(staged_header),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Commit staged file
  # ---------------------------------------

  file_existed <- file.exists(
    path
  )

  replace_file(
    staged_path = staged_path,
    final_path = path,
    allow_overwrite = overwrite
  )

  final_info <- file.info(
    path
  )

  if (!file.exists(path) ||
      is.na(final_info$size) ||
      final_info$size <= 0) {
    stop(
      paste0(
        "The final export file is missing or empty: ",
        path
      ),
      call. = FALSE
    )
  }

  message(
    "File written: ",
    path
  )

  # ---------------------------------------
  # Read and display preview
  # ---------------------------------------

  if (n_check > 0L) {
    preview <- read_export_file(
      file_path = path,
      nrows = n_check,
      character_columns = target_char_cols
    )

    print(
      preview
    )
  }

  # ---------------------------------------
  # Construct export record
  # ---------------------------------------

  log_entry <- data.frame(
    timestamp = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    file_name = basename(
      path
    ),
    file_path = normalizePath(
      path,
      mustWork = TRUE
    ),
    file_size_mb = round(
      final_info$size / 1e6,
      2
    ),
    canonical_table_id = canonical_table_id,
    dimension_type = dimension_type,
    n_rows = nrow(
      export_data
    ),
    n_cols = ncol(
      export_data
    ),
    data_year = data_year,
    data_source = data_source,
    data_category = data_category,
    table_name = table_name,
    table_type = table_type,
    data_description = data_description,
    user_note = user_note,
    overwritten = file_existed,
    user = unname(
      Sys.info()[["user"]]
    ),
    stringsAsFactors = FALSE
  )

  # ---------------------------------------
  # Optionally update export log
  # ---------------------------------------

  if (isTRUE(
    write_log
  )) {
    log_directory <- dirname(
      log_path
    )

    if (!dir.exists(
      log_directory
    )) {
      log_directory_created <- dir.create(
        log_directory,
        recursive = TRUE,
        showWarnings = FALSE
      )

      if (!isTRUE(
        log_directory_created
      ) &&
      !dir.exists(
        log_directory
      )) {
        stop(
          paste0(
            "Could not create export-log directory: ",
            log_directory
          ),
          call. = FALSE
        )
      }
    }

    if (!is.null(existing_log)) {
      existing_log <- existing_log[
        !(
          canonical_table_id ==
            log_entry$canonical_table_id &
            data_year ==
            log_entry$data_year
        )
      ]

      updated_log <- data.table::rbindlist(
        list(
          existing_log,
          data.table::as.data.table(
            log_entry
          )
        ),
        use.names = TRUE,
        fill = FALSE
      )
    } else {
      updated_log <- data.table::as.data.table(
        log_entry
      )
    }

    staged_log_path <- tempfile(
      pattern = paste0(
        ".",
        basename(log_path),
        "_staged_"
      ),
      tmpdir = log_directory,
      fileext = ".csv"
    )

    on.exit(
      {
        if (file.exists(
          staged_log_path
        )) {
          unlink(
            staged_log_path
          )
        }
      },
      add = TRUE
    )

    data.table::fwrite(
      updated_log,
      staged_log_path
    )

    replace_file(
      staged_path = staged_log_path,
      final_path = log_path,
      allow_overwrite = TRUE
    )

    message(
      "Export log updated: ",
      log_path
    )
  }

  invisible(
    log_entry
  )
}
