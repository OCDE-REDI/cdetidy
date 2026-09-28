# =============================================================================
# CDE file group labeling helpers
# =============================================================================


# -----------------------------------------------------------------------------
# Standardize source values
# -----------------------------------------------------------------------------

standardize_cde_source_value <- function(value) {

  standardized_value <- as.character(
    value
  )

  standardized_value <- trimws(
    standardized_value
  )

  standardized_value[
    standardized_value == ""
  ] <- NA_character_

  standardized_value <- toupper(
    standardized_value
  )

  standardized_value
}


# -----------------------------------------------------------------------------
# Validate a CDE classification map
# -----------------------------------------------------------------------------

validate_cde_classification_map <- function(
    classification_map) {

  if (!is.data.frame(classification_map)) {
    stop(
      "`classification_map` must be a data frame.",
      call. = FALSE
    )
  }

  if (nrow(classification_map) == 0L) {
    stop(
      "`classification_map` contains no rows.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "dataset",
    "file_type",
    "data_year",
    "variable_type",
    "source_value",
    "label",
    "num",
    "group_num",
    "group",
    "source_note"
  )

  missing_columns <- setdiff(
    required_columns,
    names(classification_map)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "The classification map is missing required column(s): ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  character_columns <- c(
    "dataset",
    "file_type",
    "variable_type",
    "source_value",
    "label",
    "group",
    "source_note"
  )

  invalid_character_columns <- character_columns[
    !vapply(
      classification_map[
        character_columns
      ],
      is.character,
      logical(1)
    )
  ]

  if (length(invalid_character_columns) > 0L) {
    stop(
      "The following classification-map column(s) must be ",
      "character: ",
      paste(
        invalid_character_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  integer_columns <- c(
    "data_year",
    "num",
    "group_num")

  invalid_integer_columns <- integer_columns[
    !vapply(
      classification_map[integer_columns],
      is.integer,
      logical(1))
  ]

  if (length(invalid_integer_columns) > 0L) {
    stop(
      "The following classification-map column(s) must be ",
      "integer: ",
      paste(
        invalid_integer_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  primary_key_columns <- c(
    "dataset",
    "file_type",
    "data_year",
    "variable_type",
    "source_value"
  )

  missing_primary_key <- Reduce(
    `|`,
    lapply(
      classification_map[
        primary_key_columns
      ],
      function(value) {
        is.na(value) |
          (
            is.character(value) &
              trimws(value) == ""
          )
      }
    )
  )

  if (any(missing_primary_key)) {
    stop(
      "The classification map contains ",
      sum(missing_primary_key),
      " row(s) with an incomplete composite primary key.",
      call. = FALSE
    )
  }

  duplicated_primary_key <- duplicated(
    classification_map[
      primary_key_columns
    ]
  ) |
    duplicated(
      classification_map[
        primary_key_columns
      ],
      fromLast = TRUE
    )

  if (any(duplicated_primary_key)) {
    duplicate_keys <- unique(
      classification_map[
        duplicated_primary_key,
        primary_key_columns,
        drop = FALSE
      ]
    )

    duplicate_key_text <- apply(
      duplicate_keys,
      1L,
      function(value) {
        paste(
          value,
          collapse = " | "
        )
      }
    )

    stop(
      "The classification map contains duplicate composite ",
      "primary key(s): ",
      paste(
        duplicate_key_text,
        collapse = "; "
      ),
      ".",
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Validate canonical classification IDs
  # ---------------------------------------

  canonical_classifications <- unique(
    classification_map[
      c(
        "group_num",
        "num",
        "label",
        "group"
      )
    ]
  )

  canonical_key_columns <- c(
    "group_num",
    "num"
  )

  inconsistent_canonical_key <- duplicated(
    canonical_classifications[
      canonical_key_columns
    ]
  ) |
    duplicated(
      canonical_classifications[
        canonical_key_columns
      ],
      fromLast = TRUE
    )

  if (any(inconsistent_canonical_key)) {
    inconsistent_rows <- canonical_classifications[
      inconsistent_canonical_key,
      ,
      drop = FALSE
    ]

    inconsistent_key_text <- paste(
      utils::capture.output(
        print(
          inconsistent_rows,
          row.names = FALSE
        )
      ),
      collapse = "\n"
    )

    stop(
      "The classification map assigns the same canonical ",
      "`group_num` and `num` combination to multiple labels:\n",
      inconsistent_key_text,
      call. = FALSE
    )
  }

  missing_classification <- (
    is.na(classification_map$label) |
      trimws(classification_map$label) == "" |
      is.na(classification_map$num) |
      is.na(classification_map$group_num) |
      is.na(classification_map$group) |
      trimws(classification_map$group) == ""
  )

  if (any(missing_classification)) {
    stop(
      "The classification map contains ",
      sum(missing_classification),
      " row(s) with an incomplete classification.",
      call. = FALSE
    )
  }

  standardized_source_values <-
    standardize_cde_source_value(
      classification_map$source_value
    )

  if (!identical(
    classification_map$source_value,
    standardized_source_values
  )) {
    stop(
      "`classification_map$source_value` contains values that ",
      "have not been standardized.",
      call. = FALSE
    )
  }

  invisible(
    classification_map
  )
}

# -----------------------------------------------------------------------------
# Common CDE classification rows
# -----------------------------------------------------------------------------

cde_common_classification_rows <- function() {

  # ---------------------------------------
  # Race and ethnicity
  # ---------------------------------------

  race_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "reporting_category", "RB",   "African American",                    1L, 1L, "Race",
    "reporting_category", "RE_B", "African American",                    1L, 1L, "Race",
    "reporting_category", "RI",   "American Indian or Alaska Native",    2L, 1L, "Race",
    "reporting_category", "RE_I", "American Indian or Alaska Native",    2L, 1L, "Race",
    "reporting_category", "RA",   "Asian",                               3L, 1L, "Race",
    "reporting_category", "RE_A", "Asian",                               3L, 1L, "Race",
    "reporting_category", "RF",   "Filipino",                            4L, 1L, "Race",
    "reporting_category", "RE_F", "Filipino",                            4L, 1L, "Race",
    "reporting_category", "RH",   "Hispanic or Latino",                  5L, 1L, "Race",
    "reporting_category", "RE_H", "Hispanic or Latino",                  5L, 1L, "Race",
    "reporting_category", "RP",   "Pacific Islander",                    6L, 1L, "Race",
    "reporting_category", "RE_P", "Pacific Islander",                    6L, 1L, "Race",
    "reporting_category", "RT",   "Two or More Races",                   7L, 1L, "Race",
    "reporting_category", "RE_T", "Two or More Races",                   7L, 1L, "Race",
    "reporting_category", "RW",   "White",                               8L, 1L, "Race",
    "reporting_category", "RE_W", "White",                               8L, 1L, "Race",
    "reporting_category", "RD",   "Race Not Reported",                   9L, 1L, "Race",
    "reporting_category", "RE_D", "Race Not Reported",                   9L, 1L, "Race"
  )

  # ---------------------------------------
  # Grade
  # ---------------------------------------

  grade_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "grade", "GRKN",       "Kindergarten",       10L, 2L, "Grade",
    "grade", "GRK",        "Kindergarten",       10L, 2L, "Grade",
    "grade", "KN",         "Kindergarten",       10L, 2L, "Grade",
    "grade", "GR13",       "Grades 1-3",         11L, 2L, "Grade",
    "grade", "GS_46",      "Grades 4-6",         12L, 2L, "Grade",
    "grade", "GR46",       "Grades 4-6",         12L, 2L, "Grade",
    "grade", "GR78",       "Grades 7-8",         13L, 2L, "Grade",
    "grade", "GS_78",      "Grades 7-8",         13L, 2L, "Grade",
    "grade", "GRK8",       "Grades K-8",         14L, 2L, "Grade",
    "grade", "GS_912",     "Grades 9-12",        15L, 2L, "Grade",
    "grade", "GR912",      "Grades 9-12",        15L, 2L, "Grade",
    "grade", "GRTKKN",     "Grades TK-K",        16L, 2L, "Grade",
    "grade", "GRTK8",      "Grades TK-8",        17L, 2L, "Grade",
    "grade", "GRTK",       "Grade TK",           18L, 2L, "Grade",
    "grade", "TK",         "Grade TK",           18L, 2L, "Grade",
    "grade", "GR04",       "Grade 4",            19L, 2L, "Grade",
    "grade", "04",         "Grade 4",            19L, 2L, "Grade",
    "grade", "GR08",       "Grade 8",            20L, 2L, "Grade",
    "grade", "08",         "Grade 8",            20L, 2L, "Grade",
    "grade", "GR09",       "Grade 9",            21L, 2L, "Grade",
    "grade", "09",         "Grade 9",            21L, 2L, "Grade",
    "grade", "GR10",       "Grade 10",           22L, 2L, "Grade",
    "grade", "10",         "Grade 10",           22L, 2L, "Grade",
    "grade", "GR11",       "Grade 11",           23L, 2L, "Grade",
    "grade", "11",         "Grade 11",           23L, 2L, "Grade",
    "grade", "GR12",       "Grade 12",           24L, 2L, "Grade",
    "grade", "12",         "Grade 12",           24L, 2L, "Grade",
    "grade", "GS_PS3",     "Grades PreK-3",      25L, 2L, "Grade",
    "grade", "GR01",       "Grade 1",            80L, 2L, "Grade",
    "grade", "01",         "Grade 1",            80L, 2L, "Grade",
    "grade", "GR02",       "Grade 2",            81L, 2L, "Grade",
    "grade", "02",         "Grade 2",            81L, 2L, "Grade",
    "grade", "GR03",       "Grade 3",            82L, 2L, "Grade",
    "grade", "03",         "Grade 3",            82L, 2L, "Grade",
    "grade", "GR05",       "Grade 5",            83L, 2L, "Grade",
    "grade", "05",         "Grade 5",            83L, 2L, "Grade",
    "grade", "GR06",       "Grade 6",            84L, 2L, "Grade",
    "grade", "06",         "Grade 6",            84L, 2L, "Grade",
    "grade", "GR07",       "Grade 7",            85L, 2L, "Grade",
    "grade", "07",         "Grade 7",            85L, 2L, "Grade",
    "grade", "GR_UNGRADE", "Ungraded",           86L, 2L, "Grade",
    "grade", "UNGRADED",   "Ungraded",           86L, 2L, "Grade"
  )

  # ---------------------------------------
  # Grade span
  # ---------------------------------------

  grade_span_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "grade_span", "ALL",    "All data for all schools",  75L, 9L, "Grade Span",
    "grade_span", "GS_K6",  "School grade span K-6",    76L, 9L, "Grade Span",
    "grade_span", "GRK6",   "School grade span K-6",    76L, 9L, "Grade Span",
    "grade_span", "GS_69",  "School grade span 6-9",    77L, 9L, "Grade Span",
    "grade_span", "GR69",   "School grade span 6-9",    77L, 9L, "Grade Span",
    "grade_span", "GS_912", "School grade span 9-12",   78L, 9L, "Grade Span",
    "grade_span", "GR912",  "School grade span 9-12",   78L, 9L, "Grade Span",
    "grade_span", "GS_K12", "School grade span K-12",   79L, 9L, "Grade Span",
    "grade_span", "GRK12",  "School grade span K-12",   79L, 9L, "Grade Span"
  )

  # ---------------------------------------
  # Stable gender codes
  # ---------------------------------------

  gender_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "reporting_category", "GN_F", "Female",          33L, 4L, "Gender",
    "reporting_category", "GF",   "Female",          33L, 4L, "Gender",
    "reporting_category", "F",    "Female",          33L, 4L, "Gender",
    "reporting_category", "GN_M", "Male",            34L, 4L, "Gender",
    "reporting_category", "GM",   "Male",            34L, 4L, "Gender",
    "reporting_category", "M",    "Male",            34L, 4L, "Gender",
    "reporting_category", "GN_X", "Non-Binary",      35L, 4L, "Gender",
    "reporting_category", "X",    "Non-Binary",      35L, 4L, "Gender",
    "reporting_category", "GN_Z", "Gender Missing",  36L, 4L, "Gender",
    "reporting_category", "GZ",   "Gender Missing",  36L, 4L, "Gender",
    "reporting_category", "Z",    "Gender Missing",  36L, 4L, "Gender"
  )

  # ---------------------------------------
  # Stable staff-gender codes
  # ---------------------------------------

  staff_gender_map <- gender_map |>
    dplyr::mutate(
      variable_type = "staff_gender"
    )

  # ---------------------------------------
  # Age ranges
  # ---------------------------------------

  age_range_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "reporting_category", "AR_03",   "Children K-12 who are 0-3",              26L, 3L, "Age Range",
    "reporting_category", "AR_0418", "Children K-12 who are 4-18",             27L, 3L, "Age Range",
    "reporting_category", "AR_1922", "Continuing Students K-12 who are 19-22", 28L, 3L, "Age Range",
    "reporting_category", "AR_2329", "Adults K-12 who are 23-29",              29L, 3L, "Age Range",
    "reporting_category", "AR_3039", "Adults K-12 who are 30-39",              30L, 3L, "Age Range",
    "reporting_category", "AR_4049", "Adults K-12 who are 40-49",              31L, 3L, "Age Range",
    "reporting_category", "AR_50P",  "Adults K-12 who are 50 plus",            32L, 3L, "Age Range",
    "reporting_category", "AR_35",   "Ages 3-5",                               71L, 3L, "Age Range",
    "reporting_category", "AR_612",  "Ages 6-12",                              72L, 3L, "Age Range",
    "reporting_category", "AR_1318", "Ages 13-18",                             73L, 3L, "Age Range",
    "reporting_category", "AR_19P",  "Ages 19 plus",                           74L, 3L, "Age Range"
  )

  # ---------------------------------------
  # Student subgroups
  # ---------------------------------------

  student_subgroup_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "reporting_category", "SG_EL", "English Learner",                       37L, 5L, "Student Subgroup",
    "reporting_category", "SE",    "English Learner",                       37L, 5L, "Student Subgroup",
    "reporting_category", "ELAS_EL", "English Learner",                     37L, 5L, "Student Subgroup",
    "reporting_category", "SG_DS", "Students with Disabilities",            38L, 5L, "Student Subgroup",
    "reporting_category", "SD",    "Students with Disabilities",            38L, 5L, "Student Subgroup",
    "reporting_category", "SG_SD", "Socioeconomically Disadvantaged",       39L, 5L, "Student Subgroup",
    "reporting_category", "SS",    "Socioeconomically Disadvantaged",       39L, 5L, "Student Subgroup",
    "reporting_category", "SG_MG", "Migrant Youth",                         40L, 5L, "Student Subgroup",
    "reporting_category", "SM",    "Migrant Youth",                         40L, 5L, "Student Subgroup",
    "reporting_category", "SG_FS", "Foster Youth",                          41L, 5L, "Student Subgroup",
    "reporting_category", "SF",    "Foster Youth",                          41L, 5L, "Student Subgroup",
    "reporting_category", "SG_HM", "Homeless Youth",                        42L, 5L, "Student Subgroup",
    "reporting_category", "SH",    "Homeless Youth",                        42L, 5L, "Student Subgroup",
    "reporting_category", "S5",    "Student with a 504 Accommodation Plan", 43L, 5L, "Student Subgroup",
    "reporting_category", "HUYN",  "Not Homeless Unaccompanied Youth",      44L, 5L, "Student Subgroup",
    "reporting_category", "HUYY",  "Homeless Unaccompanied Youth",          45L, 5L, "Student Subgroup",
    "reporting_category", "CAY",   "Is Chronically Absent",                 48L, 5L, "Student Subgroup",
    "reporting_category", "CAN",   "Is Not Chronically Absent",             49L, 5L, "Student Subgroup"
  )

  # ---------------------------------------
  # English-language acquisition status
  # ---------------------------------------

  english_status_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "reporting_category", "EL_Y",      "Is an English Learner",                    46L, 8L, "English Language Acquisition Status",
    "reporting_category", "EL_N",      "Is Not an English Learner",                47L, 8L, "English Language Acquisition Status",
    "reporting_category", "ELAS_ADEL", "Adult English Learner",                    51L, 8L, "English Language Acquisition Status",
    "reporting_category", "ELAS_EO",   "English Only",                             52L, 8L, "English Language Acquisition Status",
    "reporting_category", "ELAS_IFEP", "Initial Fluent English Proficient",        53L, 8L, "English Language Acquisition Status",
    "reporting_category", "ELAS_MISS", "EL Status Missing",                        54L, 8L, "English Language Acquisition Status",
    "reporting_category", "ELAS_RFEP", "Reclassified Fluent English Proficient",   55L, 8L, "English Language Acquisition Status",
    "reporting_category", "ELAS_TBD",  "English Status TBD",                       56L, 8L, "English Language Acquisition Status"
  )

  # ---------------------------------------
  # Disability categories
  # ---------------------------------------

  disability_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "reporting_category", "DC_AUT",  "Autism",                           57L, 7L, "Disability Category",
    "reporting_category", "DC_DB",   "Deaf Blindedness",                 58L, 7L, "Disability Category",
    "reporting_category", "DC_DFHI", "Deaf/Hearing Impairment",          59L, 7L, "Disability Category",
    "reporting_category", "DC_ED",   "Emotional Disturbance",            60L, 7L, "Disability Category",
    "reporting_category", "DC_EMD",  "Established Medical Disability",   61L, 7L, "Disability Category",
    "reporting_category", "DC_HH",   "Hard of Hearing",                  62L, 7L, "Disability Category",
    "reporting_category", "DC_ID",   "Intellectual Disability",          63L, 7L, "Disability Category",
    "reporting_category", "DC_MD",   "Multiple Disabilities",            64L, 7L, "Disability Category",
    "reporting_category", "DC_OHI",  "Other Health Impairment",          65L, 7L, "Disability Category",
    "reporting_category", "DC_OI",   "Orthopedic Health Impairment",      66L, 7L, "Disability Category",
    "reporting_category", "DC_SLD",  "Specific Learning Disability",     67L, 7L, "Disability Category",
    "reporting_category", "DC_SLI",  "Speech or Language Impairment",    68L, 7L, "Disability Category",
    "reporting_category", "DC_TBI",  "Traumatic Brain Injury",           69L, 7L, "Disability Category",
    "reporting_category", "DC_VI",   "Visual Impairment",                70L, 7L, "Disability Category"
  )

  # ---------------------------------------
  # Total-student category
  # ---------------------------------------

  total_student_map <- tibble::tribble(
    ~variable_type, ~source_value, ~label, ~num, ~group_num, ~group,
    "reporting_category", "TA", "Total Number of Students", 50L, 6L,
    "Total Number of Students"
  )

  common_map <- dplyr::bind_rows(
    race_map,
    grade_map,
    grade_span_map,
    gender_map,
    staff_gender_map,
    age_range_map,
    student_subgroup_map,
    english_status_map,
    disability_map,
    total_student_map
  )

  common_map <- common_map |>
    dplyr::mutate(
      source_value =
        standardize_cde_source_value(
          .data$source_value
        ),
      source_note =
        "Existing cdetidy classification map; stable classification."
    )

  common_map
}


# -----------------------------------------------------------------------------
# Context-specific CDE classification rows
# -----------------------------------------------------------------------------

cde_context_classification_rows <- function(
    dataset,
    file_type,
    data_year) {

  context_map <- tibble::tibble(
    variable_type = character(),
    source_value = character(),
    label = character(),
    num = integer(),
    group_num = integer(),
    group = character(),
    source_note = character()
  )

  # ---------------------------------------
  # Graduation and dropout gender drift
  # ---------------------------------------
  #
  # In the 2018-19 files, GX represented missing gender.
  # Beginning with 2019-20, GX represented non-binary gender.

  if (
    dataset == "graduation" &&
    file_type %in%
    c(
      "cohort",
      "dropout"
    )
  ) {

    if (data_year == 19L) {
      context_map <- dplyr::bind_rows(
        context_map,
        tibble::tribble(
          ~variable_type,       ~source_value, ~label,           ~num, ~group_num, ~group,  ~source_note,
          "reporting_category", "GX",          "Gender Missing", 36L,  4L,         "Gender",
          "CDE schema drift: GX represented missing gender in 2018-19 graduation and dropout files."
        )
      )
    }

    if (data_year >= 20L) {
      context_map <- dplyr::bind_rows(
        context_map,
        tibble::tribble(
          ~variable_type,       ~source_value, ~label,       ~num, ~group_num, ~group,  ~source_note,
          "reporting_category", "GX",          "Non-Binary", 35L,  4L,         "Gender",
          "CDE schema drift: GX represented non-binary gender beginning in 2019-20 graduation and dropout files."
        )
      )
    }
  }

  # ---------------------------------------
  # Restraint and seclusion gender codes
  # ---------------------------------------
  #
  # Restraint and seclusion files use GN for non-binary
  # and GX for missing gender.

  if (
    dataset == "restraint_seclusion" &&
    file_type == "overall" &&
    data_year >= 20L
  ) {
    context_map <- dplyr::bind_rows(
      context_map,
      tibble::tribble(
        ~variable_type,       ~source_value, ~label,           ~num, ~group_num, ~group,  ~source_note,
        "reporting_category", "GN",          "Non-Binary",     35L,  4L,         "Gender",
        "CDE schema drift: restraint and seclusion files use GN for non-binary gender.",
        "reporting_category", "GX",          "Gender Missing", 36L,  4L,         "Gender",
        "CDE schema drift: restraint and seclusion files use GX for missing gender."
      )
    )
  }

  # ---------------------------------------
  # Certificated staff all-gender category
  # ---------------------------------------
  #
  # ALL in staff_gender means all genders. It must not
  # inherit the grade-span meaning of ALL.

  if (
    dataset == "certificated_staff" &&
    file_type == "race_ethnicity" &&
    data_year >= 20L
  ) {
    context_map <- dplyr::bind_rows(
      context_map,
      tibble::tribble(
        ~variable_type,
        ~source_value,
        ~label,
        ~num,
        ~group_num,
        ~group,
        ~source_note,

        "staff_gender",
        "ALL",
        "All",
        87L,
        4L,
        "Gender",
        paste(
          "CDE staff race/ethnicity files use ALL",
          "for all staff genders. Canonical ID 87",
          "replaces the legacy assignment of 36."
        ),

        "staff_gender",
        "GX",
        "Non-Binary",
        35L,
        4L,
        "Gender",
        paste(
          "CDE staff race/ethnicity files use GX",
          "for non-binary staff gender."
        )
      )
    )
  }

  context_map <- context_map |>
    dplyr::mutate(
      source_value =
        standardize_cde_source_value(
          .data$source_value
        )
    )

  context_map
}


# -----------------------------------------------------------------------------
# Build a context-specific CDE classification map
# -----------------------------------------------------------------------------

cde_classification_map <- function(
    dataset,
    file_type,
    data_year) {

  if (!is.character(dataset) ||
      length(dataset) != 1L ||
      is.na(dataset) ||
      trimws(dataset) == "") {
    stop(
      "`dataset` must contain exactly one nonmissing character value.",
      call. = FALSE
    )
  }

  if (!is.character(file_type) ||
      length(file_type) != 1L ||
      is.na(file_type) ||
      trimws(file_type) == "") {
    stop(
      "`file_type` must contain exactly one nonmissing character value.",
      call. = FALSE
    )
  }

  if (!is.numeric(data_year) ||
      length(data_year) != 1L ||
      is.na(data_year) ||
      data_year != as.integer(data_year) ||
      data_year < 0L ||
      data_year > 99L) {
    stop(
      "`data_year` must be one two-digit ending year, such as 25.",
      call. = FALSE
    )
  }

  dataset <- tolower(
    trimws(dataset)
  )

  file_type <- tolower(
    trimws(file_type)
  )

  data_year <- as.integer(
    data_year
  )

  common_map <-
    cde_common_classification_rows()

  context_map <-
    cde_context_classification_rows(
      dataset = dataset,
      file_type = file_type,
      data_year = data_year
    )

  if (nrow(context_map) > 0L) {
    common_map <- dplyr::anti_join(
      common_map,
      context_map,
      by = c(
        "variable_type",
        "source_value"))}

  classification_map <- dplyr::bind_rows(
    common_map,
    context_map)

  classification_map <- classification_map |>
    dplyr::mutate(
      dataset = dataset,
      file_type = file_type,
      data_year = data_year,
      .before = 1L) |>
    dplyr::select(
      "dataset",
      "file_type",
      "data_year",
      "variable_type",
      "source_value",
      "label",
      "num",
      "group_num",
      "group",
      "source_note"
    )

  classification_map <- data.frame(
    classification_map,
    stringsAsFactors = FALSE
  )

  classification_map$num <- as.integer(
    classification_map$num
  )

  classification_map$group_num <- as.integer(
    classification_map$group_num
  )

  classification_map$data_year <- as.integer(
    classification_map$data_year
  )

  rownames(classification_map) <- NULL

  validate_cde_classification_map(
    classification_map
  )

  classification_map
}

# -----------------------------------------------------------------------------
# Comprehensive canonical CDE classification lookup
# -----------------------------------------------------------------------------

#' Return the comprehensive CDE classification lookup
#'
#' Returns one row per canonical CDE classification. The combination of
#' `group_num` and `num` uniquely identifies each human-readable label.
#'
#' @return A tibble containing `group_num`, `num`, `label`, and `group`.
#'
#' @export
cde_classification_lookup <- function() {

  common_classifications <-
    cde_common_classification_rows() |>
    dplyr::select(
      "group_num",
      "num",
      "label",
      "group"
    )

  # Canonical classifications introduced by contextual rules
  contextual_classifications <- tibble::tribble(
    ~group_num, ~num, ~label, ~group,
    4L,         87L,  "All",  "Gender"
  )

  classification_lookup <- dplyr::bind_rows(
    common_classifications,
    contextual_classifications
  ) |>
    dplyr::distinct(
      .data$group_num,
      .data$num,
      .data$label,
      .data$group
    ) |>
    dplyr::arrange(
      .data$group_num,
      .data$num
    )

  duplicate_canonical_key <- duplicated(
    classification_lookup[
      c(
        "group_num",
        "num"
      )
    ]
  ) |
    duplicated(
      classification_lookup[
        c(
          "group_num",
          "num"
        )
      ],
      fromLast = TRUE
    )

  if (any(duplicate_canonical_key)) {
    duplicate_rows <- classification_lookup[
      duplicate_canonical_key,
      ,
      drop = FALSE
    ]

    stop(
      "The comprehensive CDE classification lookup contains ",
      "duplicate canonical keys:\n",
      paste(
        utils::capture.output(
          print(
            duplicate_rows,
            row.names = FALSE
          )
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  classification_lookup$group_num <- as.integer(
    classification_lookup$group_num
  )

  classification_lookup$num <- as.integer(
    classification_lookup$num
  )

  rownames(classification_lookup) <- NULL

  tibble::as_tibble(
    classification_lookup
  )
}

#' Label CDE-file classifications using contextual mappings
#'
#' Applies dataset-, file-, year-, and variable-specific classification maps
#' to one or more columns in a CDE data file.
#'
#' @param df A data frame containing CDE classification columns.
#' @param var_names Character vector containing source-column names.
#' @param output_names Character vector containing output-column prefixes.
#' @param variable_types Character vector identifying the semantic type of
#'   each source column. Supported values are `"reporting_category"`,
#'   `"grade"`, `"grade_span"`, and `"staff_gender"`.
#' @param dataset Character value identifying the CDE dataset family.
#' @param file_type Character value identifying the file type within the
#'   dataset family.
#' @param data_year Two-digit ending year, such as `25`.
#' @param validate If `TRUE`, print the observed mappings and row counts.
#' @param fail_on_unmapped If `TRUE`, stop when a nonmissing source value
#'   cannot be mapped. If `FALSE`, issue a warning.
#' @param return_map If `TRUE`, return a named list containing the labeled
#'   data in `data` and the applicable classification map in `map`.
#'
#' @return If `return_map = FALSE`, the original data frame with four new
#'   columns per source variable: `<prefix>_label`, `<prefix>_num`,
#'   `<prefix>_group_num`, and `<prefix>_group`. If `return_map = TRUE`,
#'   a named list containing the labeled data and applicable map.
#'
#' @export
cde_files_group_labeling <- function(
    df,
    var_names,
    output_names,
    variable_types,
    dataset,
    file_type,
    data_year,
    validate = FALSE,
    fail_on_unmapped = TRUE,
    return_map = FALSE) {

  # ---------------------------------------
  # Validate data and column arguments
  # ---------------------------------------

  supported_variable_types <- c(
    "reporting_category",
    "grade",
    "grade_span",
    "staff_gender"
  )

  if (missing(variable_types)) {
    stop(
      paste0(
        "`variable_types` is required.\n\n",
        "Choose one classification type for each entry ",
        "in `var_names`.\n\n",
        "Supported values:\n",
        paste0(
          "  - ",
          supported_variable_types,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  if (
    !is.character(variable_types) ||
    length(variable_types) == 0L ||
    anyNA(variable_types)
  ) {
    stop(
      "`variable_types` must be a nonempty character vector.",
      call. = FALSE
    )
  }

  unsupported_variable_types <- setdiff(
    variable_types,
    supported_variable_types
  )

  if (length(unsupported_variable_types) > 0L) {
    stop(
      paste0(
        "Unsupported `variable_types` value(s):\n",
        paste0(
          "  - ",
          unsupported_variable_types,
          collapse = "\n"
        ),
        "\n\n",
        "Supported values:\n",
        paste0(
          "  - ",
          supported_variable_types,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  if (!is.data.frame(df)) {
    stop(
      "`df` must be a data frame.",
      call. = FALSE
    )
  }

  argument_lengths <- c(
    var_names = length(var_names),
    output_names = length(output_names),
    variable_types = length(variable_types)
  )

  if (length(unique(argument_lengths)) != 1L) {
    stop(
      "`var_names`, `output_names`, and `variable_types` ",
      "must have the same length.",
      call. = FALSE
    )
  }

  if (length(var_names) == 0L) {
    stop(
      "At least one source column must be supplied.",
      call. = FALSE
    )
  }

  character_arguments <- list(
    var_names = var_names,
    output_names = output_names,
    variable_types = variable_types
  )

  invalid_character_arguments <- names(
    character_arguments
  )[
    !vapply(
      character_arguments,
      is.character,
      logical(1)
    )
  ]

  if (length(invalid_character_arguments) > 0L) {
    stop(
      "The following argument(s) must be character vectors: ",
      paste(
        invalid_character_arguments,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  missing_argument_values <- vapply(
    character_arguments,
    function(value) {
      anyNA(value) ||
        any(trimws(value) == "")
    },
    logical(1)
  )

  if (any(missing_argument_values)) {
    stop(
      "The following argument(s) contain missing or blank values: ",
      paste(
        names(missing_argument_values)[
          missing_argument_values
        ],
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(
    var_names,
    names(df)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "Missing CDE source column(s): ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  duplicated_output_names <- unique(
    output_names[
      duplicated(output_names)
    ]
  )

  if (length(duplicated_output_names) > 0L) {
    stop(
      "`output_names` contains duplicate prefix(es): ",
      paste(
        duplicated_output_names,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Validate logical arguments
  # ---------------------------------------

  logical_arguments <- list(
    validate = validate,
    fail_on_unmapped = fail_on_unmapped,
    return_map = return_map
  )

  invalid_logical_arguments <- names(
    logical_arguments
  )[
    !vapply(
      logical_arguments,
      function(value) {
        is.logical(value) &&
          length(value) == 1L &&
          !is.na(value)
      },
      logical(1)
    )
  ]

  if (length(invalid_logical_arguments) > 0L) {
    stop(
      "The following argument(s) must be either `TRUE` or `FALSE`: ",
      paste(
        invalid_logical_arguments,
        collapse = ", "
      ),
      ".",
      call. = FALSE
    )
  }

  # ---------------------------------------
  # Build the applicable classification map
  # ---------------------------------------

  classification_map <-
    cde_classification_map(
      dataset = dataset,
      file_type = file_type,
      data_year = data_year
    )

  # ---------------------------------------
  # Apply each requested classification
  # ---------------------------------------

  for (index in seq_along(
    var_names
  )) {

    source_column <- var_names[[index]]
    output_prefix <- output_names[[index]]
    variable_type <- variable_types[[index]]

    lookup <- classification_map[
      classification_map$variable_type ==
        variable_type,
      ,
      drop = FALSE
    ]

    if (nrow(lookup) == 0L) {
      stop(
        "No classification rows are available for variable type `",
        variable_type,
        "` in dataset `",
        tolower(trimws(dataset)),
        "`, file type `",
        tolower(trimws(file_type)),
        "`, and data year ",
        as.integer(data_year),
        ".",
        call. = FALSE
      )
    }

    source_value <-
      standardize_cde_source_value(
        df[[source_column]]
      )

    match_index <- match(
      source_value,
      lookup$source_value
    )

    source_is_missing <- is.na(
      source_value
    )

    unmapped <- (
      !source_is_missing &
        is.na(match_index)
    )

    if (any(unmapped)) {
      unmapped_values <- sort(unique(
        source_value[
          unmapped
        ]
      ))

      unmapped_message <- paste0(
        "Dataset `",
        tolower(trimws(dataset)),
        "`, file type `",
        tolower(trimws(file_type)),
        "`, and data year ",
        as.integer(data_year),
        " contain unmapped value(s) in `",
        source_column,
        "` for variable type `",
        variable_type,
        "`: ",
        paste(
          unmapped_values,
          collapse = ", "
        ),
        ". Update the contextual classification map before continuing."
      )

      if (isTRUE(fail_on_unmapped)) {
        stop(
          unmapped_message,
          call. = FALSE
        )
      } else {
        warning(
          unmapped_message,
          call. = FALSE
        )
      }
    }

    df[[
      paste0(
        output_prefix,
        "_label"
      )
    ]] <- lookup$label[
      match_index
    ]

    df[[
      paste0(
        output_prefix,
        "_num"
      )
    ]] <- lookup$num[
      match_index
    ]

    df[[
      paste0(
        output_prefix,
        "_group_num"
      )
    ]] <- lookup$group_num[
      match_index
    ]

    df[[
      paste0(
        output_prefix,
        "_group"
      )
    ]] <- lookup$group[
      match_index
    ]

    # -------------------------------------
    # Optional mapping summary
    # -------------------------------------

    if (isTRUE(validate)) {
      observed_frequency <- table(
        source_value,
        useNA = "no"
      )

      observed_values <- names(
        observed_frequency
      )

      validation_table <- lookup[
        lookup$source_value %in%
          observed_values,
        c(
          "source_value",
          "label",
          "num",
          "group_num",
          "group",
          "source_note"
        ),
        drop = FALSE
      ]

      validation_table$rows <- as.integer(
        observed_frequency[
          match(
            validation_table$source_value,
            observed_values
          )
        ]
      )

      validation_table <- validation_table[
        order(
          validation_table$source_value
        ),
        ,
        drop = FALSE
      ]

      rownames(validation_table) <- NULL

      message(
        "\n--- ",
        source_column,
        " mapping ---"
      )

      print(
        validation_table,
        row.names = FALSE
      )
    }
  }

  # ---------------------------------------
  # Return data and optional map
  # ---------------------------------------

  if (isTRUE(return_map)) {
    map_used <- classification_map[
      classification_map$variable_type %in%
        unique(variable_types),
      ,
      drop = FALSE
    ]

    rownames(map_used) <- NULL

    return(
      list(
        data = df,
        map = tibble::as_tibble(
          map_used
        )
      )
    )
  }

  df
}
