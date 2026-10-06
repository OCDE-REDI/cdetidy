#' Build a standardized warehouse output path
#'
#' Constructs a warehouse CSV path from an explicit warehouse root, data
#' source, category, table name, and table type. The function does not create
#' directories or write files.
#'
#' Folder categories are supplied explicitly rather than maintained in a
#' hard-coded catalog. This allows Assessment, Dashboard, and CDE categories
#' to evolve without changing this function.
#'
#' @param data_source One of `"Assessment"`, `"Dashboard"`, or `"CDE"`.
#' @param data_category Nonempty folder name within `data_source`, such as
#'   `"CAST"`, `"Enrollment"`, or `"Science"`.
#' @param table_name Base warehouse table name using only letters, numbers,
#'   and underscores.
#' @param table_type Either `"fact"` or `"dimension"`.
#' @param warehouse_root Root warehouse directory. Defaults to
#'   `"T:/Data Warehouse"`.
#' @param compressed If `TRUE`, return a path ending in `.csv.gz`; otherwise,
#'   return a path ending in `.csv`.
#'
#' @return A single character value containing the constructed file path.
#'
#' @details
#' Fact filenames receive the suffix `_fact`. Dimension filenames receive the
#' suffix `_dim`.
#'
#' The function only constructs and validates the path. Directory creation,
#' overwrite protection, writing, verification, and logging are handled by
#' `safe_fwrite()`.
#'
#' @examples
#' build_warehouse_path(
#'   data_source = "Assessment",
#'   data_category = "CAST",
#'   table_name = "cast_25_core",
#'   table_type = "fact"
#' )
#'
#' build_warehouse_path(
#'   data_source = "CDE",
#'   data_category = "Enrollment",
#'   table_name = "enrollment_25_schools",
#'   table_type = "dimension"
#' )
#'
#' @export

build_warehouse_path <- function(
    data_source,
    data_category,
    table_name,
    table_type,
    warehouse_root = "T:/Data Warehouse",
    compressed = FALSE) {
  
  # ---------------------------------------
  # Internal validation helpers
  # ---------------------------------------
  
  is_scalar_string <- function(value) {
    length(value) == 1L &&
      is.character(value) &&
      !is.na(value) &&
      nzchar(
        trimws(value)
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
  
  validate_folder_component <- function(value,
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
    
    value <- trimws(
      value
    )
    
    if (value %in% c(
      ".",
      ".."
    ) ||
    grepl(
      "[/\\\\:]",
      value
    )) {
      stop(
        paste0(
          "`",
          argument,
          "` must be one folder name, not a path."
        ),
        call. = FALSE
      )
    }
    
    value
  }
  
  # ---------------------------------------
  # Validate arguments
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
  
  data_category <- validate_folder_component(
    data_category,
    "data_category"
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
        "`table_name` must not already end in `_fact` or `_dim`; ",
        "the suffix is added from `table_type`."
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
  
  if (length(compressed) != 1L ||
      is.na(compressed) ||
      !is.logical(compressed)) {
    stop(
      "`compressed` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  
  # ---------------------------------------
  # Construct filename and path
  # ---------------------------------------
  
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
  
  file.path(
    warehouse_root,
    data_source,
    data_category,
    file_name
  )
}
