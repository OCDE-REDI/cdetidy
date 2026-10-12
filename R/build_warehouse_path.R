#' Construct a warehouse output path
#'
#' @description
#' Constructs a standardized output path within the Tableau-ready warehouse
#' structure. The function only constructs and returns a path; it does not
#' create directories or write files.
#'
#' Folder routing is controlled independently from export metadata. Values such
#' as an assessment name or data category should be recorded by
#' [safe_fwrite()] and do not automatically become folders.
#'
#' @param data_source Warehouse data source. Accepted values are
#'   `"Assessment"`, `"Dashboard"`, and `"CDE"`. Matching is case-insensitive.
#' @param table_name Base table name without a `_fact` or `_dim` suffix.
#'   The name may contain only letters, numbers, and underscores.
#' @param table_type Table classification. Accepted values are `"fact"` and
#'   `"dimension"`. Matching is case-insensitive.
#' @param warehouse_root Root directory containing the warehouse data-source
#'   folders. Defaults to
#'   `"T:/Data Warehouse/Data & Scripts/Tableau Ready Files"`.
#' @param subdirectories Optional character vector containing one or more
#'   folders to place beneath the data-source folder. For example,
#'   `"Dimensions"` or
#'   `c("Dimensions", "Annualized Dimensions")`. Use `NULL` or
#'   `character()` to write directly within the data-source folder.
#' @param compressed Logical. When `TRUE`, the returned filename ends in
#'   `.csv.gz`; otherwise, it ends in `.csv`.
#'
#' @return A character string containing the constructed output path.
#'
#' @details
#' The function appends `_fact` to fact-table names and `_dim` to
#' dimension-table names. `table_name` must not already contain either suffix.
#'
#' Each value supplied through `subdirectories` must be one folder name rather
#' than a path. Path separators, parent-directory references, drive
#' specifications, and Windows-invalid filename characters are rejected.
#'
#' No directories or files are created.
#'
#' @examples
#' build_warehouse_path(
#'   data_source = "Assessment",
#'   table_name = "cast_core",
#'   table_type = "fact"
#' )
#'
#' build_warehouse_path(
#'   data_source = "Assessment",
#'   table_name = "assessment_test",
#'   table_type = "dimension",
#'   subdirectories = "Dimensions"
#' )
#'
#' build_warehouse_path(
#'   data_source = "Assessment",
#'   table_name = "cast_25_entities",
#'   table_type = "dimension",
#'   subdirectories = c(
#'     "Dimensions",
#'     "Annualized Dimensions"
#'   )
#' )
#'
#' @export

build_warehouse_path <- function(
    data_source,
    table_name,
    table_type,
    warehouse_root =
      "T:/Data Warehouse/Data & Scripts/Tableau Ready Files",
    subdirectories = character(),
    compressed = FALSE) {

  # ---------------------------------------
  # Internal validation helpers
  # ---------------------------------------

  is_scalar_string <- function(value) {
    length(value) == 1L &&
      is.character(value) &&
      !is.na(value) &&
      nzchar(
        trimws(
          value
        )
      )
  }

  normalize_choice <- function(value,
                               choices,
                               argument) {
    if (!is_scalar_string(
      value
    )) {
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
        trimws(
          value
        )
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

  # ---------------------------------------
  # Validate source and table metadata
  # ---------------------------------------

  data_source <- normalize_choice(
    data_source,
    c(
      "Assessment",
      "Dashboard",
      "CDE"
    ),
    "data_source"
  )

  table_type <- normalize_choice(
    table_type,
    c(
      "fact",
      "dimension"
    ),
    "table_type"
  )

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

  if (grepl(
    "_(?:fact|dim)$",
    table_name,
    ignore.case = TRUE
  )) {
    stop(
      paste0(
        "`table_name` must not already end in `_fact` or `_dim`."
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Validate root and subdirectories
  # ---------------------------------------

  if (!is_scalar_string(
    warehouse_root
  )) {
    stop(
      "`warehouse_root` must be one nonempty directory path.",
      call. = FALSE
    )
  }

  warehouse_root <- trimws(
    warehouse_root
  )

  if (is.null(
    subdirectories
  )) {
    subdirectories <- character()
  }

  if (!is.character(
    subdirectories
  ) ||
  anyNA(
    subdirectories
  ) ||
  any(
    !nzchar(
      trimws(
        subdirectories
      )
    )
  )) {
    stop(
      paste0(
        "`subdirectories` must be a character vector of nonempty ",
        "folder names, `character()`, or `NULL`."
      ),
      call. = FALSE
    )
  }

  subdirectories <- trimws(
    subdirectories
  )

  invalid_subdirectory <- vapply(
    subdirectories,
    function(subdirectory) {
      identical(
        subdirectory,
        "."
      ) ||
        identical(
          subdirectory,
          ".."
        ) ||
        grepl(
          "[/\\\\]",
          subdirectory
        ) ||
        grepl(
          "[<>:\"|?*]",
          subdirectory
        )
    },
    logical(1)
  )

  if (any(
    invalid_subdirectory
  )) {
    stop(
      paste0(
        "Each `subdirectories` value must be one folder name, ",
        "not a path."
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(
    subdirectories
  ) > 0L) {
    stop(
      "`subdirectories` must not contain duplicate folder names.",
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Validate compression and build path
  # ---------------------------------------

  if (length(compressed) != 1L ||
      is.na(compressed) ||
      !is.logical(compressed)) {
    stop(
      "`compressed` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  table_suffix <- if (identical(
    table_type,
    "fact"
  )) {
    "fact"
  } else {
    "dim"
  }

  file_extension <- if (isTRUE(
    compressed
  )) {
    ".csv.gz"
  } else {
    ".csv"
  }

  file_name <- paste0(
    table_name,
    "_",
    table_suffix,
    file_extension
  )

  path_components <- c(
    warehouse_root,
    data_source,
    subdirectories,
    file_name
  )

  do.call(
    file.path,
    as.list(
      path_components
    )
  )
}
