#' Apply dashboard student-group classifications
#'
#' Maps one or more dashboard classification columns to standardized labels,
#' numeric codes, and broader classification groups.
#'
#' For each source column, four output columns are created:
#' \itemize{
#'   \item \code{<prefix>_label}
#'   \item \code{<prefix>_num}
#'   \item \code{<prefix>_group_num}
#'   \item \code{<prefix>_group}
#' }
#'
#' Missing source values remain missing in all corresponding output columns.
#' Any nonmissing source value not included in the classification map causes
#' the function to stop.
#'
#' @param df A data frame containing the source classification columns.
#' @param var_names Character vector naming the source columns.
#' @param output_names Character vector containing the output prefixes. Must
#'   have the same length as \code{var_names}.
#'
#' @return The original data frame with four classification columns added for
#'   each source column.
#'
#' @export
dashboard_files_group_labeling <- function(df,
                                           var_names,
                                           output_names) {
  if (!is.data.frame(df)) {
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }

  if (
    !is.character(var_names) ||
    length(var_names) == 0L ||
    anyNA(var_names) ||
    any(var_names == "") ||
    anyDuplicated(var_names)
  ) {
    stop(
      "`var_names` must be a nonempty character vector ",
      "containing unique column names.",
      call. = FALSE
    )
  }

  if (
    !is.character(output_names) ||
    length(output_names) == 0L ||
    anyNA(output_names) ||
    any(output_names == "") ||
    anyDuplicated(output_names)
  ) {
    stop(
      "`output_names` must be a nonempty character vector ",
      "containing unique output prefixes.",
      call. = FALSE
    )
  }

  if (length(var_names) != length(output_names)) {
    stop(
      "`var_names` and `output_names` must have the same length.",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(
    var_names,
    names(df)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "Missing source column(s): ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  classification_map <- data.frame(
    source_value = c(
      "AA",
      "AI",
      "AS",
      "FI",
      "HI",
      "PI",
      "MR",
      "WH",
      "SED",
      "SWD",
      "FOS",
      "HOM",
      "RFP",
      "LTEL",
      "SBA",
      "CAA",
      "CAST",
      "EL",
      "ELO",
      "EO",
      "ALL"
    ),
    label = c(
      "Black/African American",
      "American Indian or Alaska Native",
      "Asian",
      "Filipino",
      "Hispanic",
      "Pacific Islander",
      "Multiple Races/Two or More",
      "White",
      "Socioeconomically Disadvantaged",
      "Students with Disabilities",
      "Foster Youth",
      "Homeless Youth",
      paste0(
        "Recently Reclassified Fluent-English ",
        "Proficient Only"
      ),
      "Long-Term English Learner",
      "Students Who Took SBAC",
      "Students Who Took CAA",
      "Students Who Took CAST",
      "English Learner",
      "English Learners Only",
      "English Only",
      "All Students"
    ),
    num = c(
      1L,
      2L,
      3L,
      4L,
      5L,
      6L,
      7L,
      8L,
      9L,
      10L,
      11L,
      12L,
      13L,
      14L,
      15L,
      16L,
      17L,
      18L,
      19L,
      20L,
      21L
    ),
    group_num = c(
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      2L,
      2L,
      2L,
      2L,
      4L,
      2L,
      3L,
      3L,
      3L,
      4L,
      4L,
      4L,
      5L
    ),
    group = c(
      rep(
        "Race",
        8L
      ),
      rep(
        "Student Subgroup",
        4L
      ),
      "English Language Acquisition Status",
      "Student Subgroup",
      rep(
        "Test Taken",
        3L
      ),
      rep(
        "English Language Acquisition Status",
        3L
      ),
      "All Students"
    ),
    stringsAsFactors = FALSE
  )

  if (anyDuplicated(classification_map$source_value)) {
    duplicated_values <- unique(
      classification_map$source_value[
        duplicated(
          classification_map$source_value
        )
      ]
    )

    stop(
      "Duplicate source values found in the classification map: ",
      paste(
        duplicated_values,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  output_columns <- unlist(
    lapply(
      output_names,
      function(prefix) {
        paste0(
          prefix,
          c(
            "_label",
            "_num",
            "_group_num",
            "_group"
          )
        )
      }
    ),
    use.names = FALSE
  )

  existing_output_columns <- intersect(
    output_columns,
    names(df)
  )

  if (length(existing_output_columns) > 0L) {
    stop(
      "Classification output column(s) already exist: ",
      paste(
        existing_output_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  for (i in seq_along(var_names)) {
    source_column <- var_names[[i]]
    output_prefix <- output_names[[i]]

    source_values <- as.character(
      df[[source_column]]
    )

    match_index <- match(
      source_values,
      classification_map$source_value
    )

    unmapped_values <- sort(
      unique(
        source_values[
          !is.na(source_values) &
            is.na(match_index)
        ]
      )
    )

    if (length(unmapped_values) > 0L) {
      displayed_values <- ifelse(
        unmapped_values == "",
        "<blank>",
        unmapped_values
      )

      stop(
        "Unmapped value(s) found in `",
        source_column,
        "`: ",
        paste(
          displayed_values,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    df[[paste0(
      output_prefix,
      "_label"
    )]] <- classification_map$label[
      match_index
    ]

    df[[paste0(
      output_prefix,
      "_num"
    )]] <- classification_map$num[
      match_index
    ]

    df[[paste0(
      output_prefix,
      "_group_num"
    )]] <- classification_map$group_num[
      match_index
    ]

    df[[paste0(
      output_prefix,
      "_group"
    )]] <- classification_map$group[
      match_index
    ]
  }

  df
}
