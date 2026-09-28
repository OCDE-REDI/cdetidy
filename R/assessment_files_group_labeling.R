# =========================================
# Internal assessment classification map
# =========================================

assessment_classification_map <- function(assessment_type,
                                          data_year) {
  if (length(assessment_type) != 1L ||
      is.na(assessment_type) ||
      trimws(as.character(assessment_type)) == "") {
    stop(
      "`assessment_type` must contain exactly one value.",
      call. = FALSE)
  }

  if (length(data_year) != 1L ||
      is.na(data_year) ||
      data_year < 0L ||
      data_year > 99L) {
    stop(
      "`data_year` must be one two-digit ending year, such as 25.",
      call. = FALSE)
  }

  assessment_type <- toupper(trimws(as.character(assessment_type)))
  data_year       <- as.integer(data_year)

  make_rows <- function(source_value,
                        label,
                        num,
                        group_num,
                        group,
                        variable_type = "student_group") {
    data.frame(
      variable_type = variable_type,
      source_value  = as.character(source_value),
      label         = as.character(label),
      num           = as.integer(num),
      group_num     = as.integer(group_num),
      group         = as.character(group),
      stringsAsFactors = FALSE)
  }

  classification_map <- rbind(
    # ---------------------------------------
    # Race and ethnicity
    # ---------------------------------------

    make_rows(
      c(74, 75, 76, 77, 78, 79, 144, 80),
      c(
        "Black or African American",
        "American Indian or Alaska Native",
        "Asian",
        "Filipino",
        "Hispanic or Latino",
        "Native Hawaiian or Pacific Islander",
        "Two or More Races",
        "White"),
      1:8,
      1L,
      "Race"),

    # ---------------------------------------
    # Grades
    # ---------------------------------------

    make_rows(
      c("KN", 1:14, 99),
      c(
        "Kindergarten",
        "Grade 1",
        "Grade 2",
        "Grade 3",
        "Grade 4",
        "Grade 5",
        "Grade 6",
        "Grade 7",
        "Grade 8",
        "Grade 9",
        "Grade 10",
        "Grade 11",
        "Grade 12",
        "All Grades",
        "All High School Grades",
        "Cohort Grade/Graduating Class"),
      9:24,
      2L,
      "Grade",
      variable_type = "grade"),

    # ---------------------------------------
    # Gender
    # ---------------------------------------

    make_rows(
      c(4, 3),
      c(
        "Female",
        "Male"),
      c(25L, 26L),
      3L,
      "Gender"),

    # ---------------------------------------
    # Student subgroups
    # ---------------------------------------

    make_rows(
      c(
        28, 29,
        52, 53,
        240, 241,
        50, 51,
        1,
        128, 99,
        31, 111),
      c(
        "Migrant Youth",
        "Not Migrant Youth",
        "Homeless Youth",
        "Not Homeless Youth",
        "Foster Youth",
        "Not Foster Youth",
        "Armed Forces Family Member",
        "No Armed Forces Family Member",
        "All Students",
        "Student with a Disability",
        "Student Without a Disability",
        "Socioeconomically Disadvantaged",
        "Not Socioeconomically Disadvantaged"),
      c(
        27L, 28L,
        29L, 30L,
        31L, 32L,
        33L, 34L,
        35L,
        36L, 37L,
        39L, 40L),
      4L,
      "Student Subgroup"),

    # ---------------------------------------
    # English-language acquisition status
    # ---------------------------------------

    make_rows(
      c(
        6, 7, 8,
        120, 142, 160,
        180, 170,
        250, 251, 252, 190),
      c(
        "IFEP, RFEP and EO",
        "IFEP",
        "RFEP",
        "ELs Enrolled Less Than 12 Months",
        "ELs Enrolled 12 Months or More",
        "English Learner",
        "English Only",
        "Ever-EL",
        "Long-Term English Learner",
        "At-Risk of Becoming LTEL",
        "Never an English Learner",
        "English-Language Acquisition Status TBD"),
      c(
        41L, 42L, 43L,
        44L, 45L, 46L,
        48L, 49L,
        50L, 51L, 52L, 53L),
      6L,
      "English Language Acquisition Status"),

    # ---------------------------------------
    # Parent education
    # ---------------------------------------

    make_rows(
      c(90, 91, 92, 93, 94, 121),
      c(
        "Not a High School Graduate",
        "High School Graduate",
        "Some College or Associate Degree",
        "College Graduate",
        "Graduate School or Postgraduate",
        "Declined to State"),
      71:76,
      8L,
      "Parent Education"),

    # ---------------------------------------
    # Race by economic-status crosstabs
    # ---------------------------------------

    make_rows(
      c(
        201, 202, 200, 203,
        204, 205, 206, 207,
        221, 222, 220, 223,
        224, 225, 226, 227),
      c(
        "American Indian or Alaska Native and Economically Disadvantaged",
        "Asian and Economically Disadvantaged",
        "Black or African American and Economically Disadvantaged",
        "Filipino and Economically Disadvantaged",
        "Hispanic or Latino and Economically Disadvantaged",
        "Native Hawaiian or Pacific Islander and Economically Disadvantaged",
        "White and Economically Disadvantaged",
        "Two or More Races and Economically Disadvantaged",
        "American Indian or Alaska Native and Not Economically Disadvantaged",
        "Asian and Not Economically Disadvantaged",
        "Black or African American and Not Economically Disadvantaged",
        "Filipino and Not Economically Disadvantaged",
        "Hispanic or Latino and Not Economically Disadvantaged",
        "Native Hawaiian or Pacific Islander and Not Economically Disadvantaged",
        "White and Not Economically Disadvantaged",
        "Two or More Races and Not Economically Disadvantaged"),
      77:92,
      9L,
      "Crosstabs")
  )

  # Code 243 has different meanings across assessment families.
  if (assessment_type %in% c("SBAC", "CAST")) {
    classification_map <- rbind(
      classification_map,
      make_rows(
        243,
        "Adult English Learner",
        47L,
        6L,
        "English Language Acquisition Status"))
  }

  if (assessment_type == "ELPAC") {
    classification_map <- rbind(
      classification_map,

      # Alternate-assessment participation
      make_rows(
        239,
        "Student with a Disability Tested with Alternate Assessment",
        38L,
        5L,
        "Test Taken"),

      # Time in an English-language program
      make_rows(
        c(242, 243, 244, 245, 246, 247, 248),
        c(
          "English Learner - 1 Year in Program",
          "English Learner - 2 Years in Program",
          "English Learner - 3 Years in Program",
          "English Learner - 4 Years in Program",
          "English Learner - 5 Years in Program",
          "English Learner - 6 Years in Program",
          "English Learner - Less Than 1 Year in Program"),
        c(54L, 55L, 56L, 57L, 58L, 59L, 93L),
        6L,
        "English Language Acquisition Status"),

      # Primary language
      make_rows(
        228:238,
        c(
          "Spanish",
          "Vietnamese",
          "Mandarin (Putonghua)",
          "Arabic",
          "Filipino (Pilipino or Tagalog)",
          "Cantonese",
          "Korean",
          "Hmong",
          "Punjabi",
          "Russian",
          "All Remaining Languages"),
        60:70,
        7L,
        "First Language"))
  }

  classification_map$assessment_type <- assessment_type
  classification_map$data_year       <- data_year

  classification_map <- classification_map[
    ,
    c(
      "assessment_type",
      "data_year",
      "variable_type",
      "source_value",
      "label",
      "num",
      "group_num",
      "group")
  ]

  rownames(classification_map) <- NULL

  classification_map
}


# =========================================
# Internal map validation
# =========================================

validate_assessment_classification_map <- function(classification_map) {
  required_columns <- c(
    "assessment_type",
    "data_year",
    "variable_type",
    "source_value",
    "label",
    "num",
    "group_num",
    "group")

  missing_columns <- setdiff(
    required_columns,
    names(classification_map))

  if (length(missing_columns) > 0L) {
    stop(
      "Assessment classification map is missing: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE)
  }

  valid_variable_types <- c(
    "student_group",
    "grade")

  invalid_variable_types <- setdiff(
    unique(classification_map$variable_type),
    valid_variable_types)

  if (length(invalid_variable_types) > 0L) {
    stop(
      "Invalid assessment variable type(s): ",
      paste(invalid_variable_types, collapse = ", "),
      ".",
      call. = FALSE)
  }

  duplicate_key <- duplicated(
    classification_map[
      ,
      c(
        "assessment_type",
        "data_year",
        "variable_type",
        "source_value")
    ])

  if (any(duplicate_key)) {
    duplicate_rows <- classification_map[
      duplicate_key |
        duplicated(
          classification_map[
            ,
            c(
              "assessment_type",
              "data_year",
              "variable_type",
              "source_value")
          ],
          fromLast = TRUE),
      c(
        "assessment_type",
        "data_year",
        "variable_type",
        "source_value",
        "label")
    ]

    stop(
      paste0(
        "Assessment classification map contains duplicate keys:\n",
        paste(
          utils::capture.output(print(duplicate_rows)),
          collapse = "\n")),
      call. = FALSE)
  }

  invisible(TRUE)
}


# =========================================
# Internal source-value standardization
# =========================================

standardize_assessment_source_value <- function(x) {
  value <- trimws(as.character(x))

  numeric_value <- grepl(
    "^[0-9]+$",
    value)

  value[numeric_value] <- sub(
    "^0+(?=[0-9])",
    "",
    value[numeric_value],
    perl = TRUE)

  value
}


#' Label assessment-file student groups and grades
#'
#' Applies an assessment-, year-, and variable-specific classification map
#' to one or more assessment-file columns.
#'
#' @param df A data frame containing assessment classifications.
#' @param var_names Character vector containing source-column names.
#' @param output_names Character vector containing output-column prefixes.
#' @param variable_types Character vector identifying each source column as
#'   `"student_group"` or `"grade"`.
#' @param assessment_type Assessment family, such as `"SBAC"`, `"CAST"`, or
#'   `"ELPAC"`. The value supplies context for codes whose meanings differ
#'   across assessment families.
#' @param data_year Two-digit ending year, such as `25`.
#' @param validate If `TRUE`, print source frequencies and mapping tables.
#' @param fail_on_unmapped If `TRUE`, stop when a nonmissing source value
#'   cannot be mapped. Otherwise, issue a warning.
#' @param return_map If `TRUE`, return a named list containing the labeled
#'   data in `data` and the applicable classification map in `map`.
#'   If `FALSE`, return only the labeled data frame.
#'
#' @return If `return_map = FALSE`, the original data frame with four new
#'   columns per source variable: `<prefix>_label`, `<prefix>_num`,
#'   `<prefix>_group_num`, and `<prefix>_group`. If `return_map = TRUE`,
#'   a named list containing the labeled data in `data` and the classification
#'   map in `map`.
#'
#' @export
assessment_files_group_labeling <- function(
    df,
    var_names,
    output_names,
    variable_types,
    assessment_type,
    data_year,
    validate = FALSE,
    fail_on_unmapped = TRUE,
    return_map = FALSE) {

  if (!is.data.frame(df)) {
    stop(
      "`df` must be a data frame.",
      call. = FALSE)
  }

  if (length(return_map) != 1L ||
      !is.logical(return_map) ||
      is.na(return_map)) {
    stop(
      "`return_map` must be either `TRUE` or `FALSE`.",
      call. = FALSE)
  }

  argument_lengths <- c(
    var_names       = length(var_names),
    output_names    = length(output_names),
    variable_types  = length(variable_types))

  if (length(unique(argument_lengths)) != 1L) {
    stop(
      paste0(
        "`var_names`, `output_names`, and `variable_types` ",
        "must have the same length."),
      call. = FALSE)
  }

  if (length(var_names) == 0L) {
    stop(
      "At least one source column must be supplied.",
      call. = FALSE)
  }

  missing_columns <- setdiff(
    var_names,
    names(df))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing assessment column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE)
  }

  valid_variable_types <- c(
    "student_group",
    "grade")

  invalid_variable_types <- setdiff(
    variable_types,
    valid_variable_types)

  if (length(invalid_variable_types) > 0L) {
    stop(
      "`variable_types` contains invalid value(s): ",
      paste(invalid_variable_types, collapse = ", "),
      ". Valid values are `student_group` and `grade`.",
      call. = FALSE)
  }

  classification_map <- assessment_classification_map(
    assessment_type = assessment_type,
    data_year       = data_year)

  validate_assessment_classification_map(
    classification_map)

  for (i in seq_along(var_names)) {
    source_column <- var_names[[i]]
    output_prefix <- output_names[[i]]
    variable_type <- variable_types[[i]]

    lookup <- classification_map[
      classification_map$variable_type == variable_type,
      ,
      drop = FALSE
    ]

    source_value <- standardize_assessment_source_value(
      df[[source_column]])

    match_index <- match(
      source_value,
      lookup$source_value)

    source_is_missing <- is.na(df[[source_column]]) |
      source_value == ""

    unmapped <- !source_is_missing &
      is.na(match_index)

    if (any(unmapped)) {
      unmapped_values <- sort(
        unique(source_value[unmapped]))

      error_message <- paste0(
        toupper(as.character(assessment_type)),
        " ",
        2000L + as.integer(data_year),
        " contains unmapped value(s) in `",
        source_column,
        "`: ",
        paste(unmapped_values, collapse = ", "),
        ". Update the assessment classification map before continuing."
      )

      if (isTRUE(fail_on_unmapped)) {
        stop(
          error_message,
          call. = FALSE)
      } else {
        warning(
          error_message,
          call. = FALSE)
      }
    }

    df[[paste0(output_prefix, "_label")]] <-
      lookup$label[match_index]

    df[[paste0(output_prefix, "_num")]] <-
      lookup$num[match_index]

    df[[paste0(output_prefix, "_group_num")]] <-
      lookup$group_num[match_index]

    df[[paste0(output_prefix, "_group")]] <-
      lookup$group[match_index]

    if (isTRUE(validate)) {
      observed_frequency <- table(
        source_value,
        useNA = "no")

      validation_table <- lookup[
        lookup$source_value %in% names(observed_frequency),
        c(
          "source_value",
          "label",
          "num",
          "group_num",
          "group"),
        drop = FALSE
      ]

      validation_table$rows <- as.integer(
        observed_frequency[
          match(
            validation_table$source_value,
            names(observed_frequency))
        ])

      rownames(validation_table) <- NULL

      message(
        "\n--- ",
        source_column,
        " mapping ---")

      print(
        validation_table,
        row.names = FALSE)
    }
  }

  if (isTRUE(return_map)) {
    map_used <- classification_map[
      classification_map$variable_type %in%
        unique(variable_types),
      ,
      drop = FALSE
    ]

    rownames(map_used) <- NULL

    return(list(
      data = df,
      map = tibble::as_tibble(map_used)
    ))
  }

  df
}
