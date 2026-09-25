# =============================================================================
# Tests for schema-aware CDE group labeling
# =============================================================================


testthat::test_that(
  "source values are standardized consistently",
  {
    standardized_values <-
      standardize_cde_source_value_v2(
        c(
          " gx ",
          "gn",
          "",
          NA_character_
        )
      )
    
    testthat::expect_identical(
      standardized_values,
      c(
        "GX",
        "GN",
        NA_character_,
        NA_character_
      )
    )
  }
)


testthat::test_that(
  "classification map has a unique composite primary key",
  {
    classification_map <-
      cde_classification_map_v2(
        dataset = "graduation",
        file_type = "cohort",
        data_year = 25L
      )
    
    primary_key_columns <- c(
      "dataset",
      "file_type",
      "data_year",
      "variable_type",
      "source_value"
    )
    
    testthat::expect_false(
      any(
        duplicated(
          classification_map[
            primary_key_columns
          ]
        )
      )
    )
    
    testthat::expect_true(
      is.integer(
        classification_map$data_year
      )
    )
    
    testthat::expect_true(
      is.integer(
        classification_map$num
      )
    )
    
    testthat::expect_true(
      is.integer(
        classification_map$group_num
      )
    )
  }
)


testthat::test_that(
  "variable type resolves reused grade codes",
  {
    classification_map <-
      cde_classification_map_v2(
        dataset = "graduation",
        file_type = "cohort",
        data_year = 25L
      )
    
    reused_code <- classification_map[
      classification_map$source_value ==
        "GS_912",
      ,
      drop = FALSE
    ]
    
    reused_code <- reused_code[
      order(
        reused_code$variable_type
      ),
      ,
      drop = FALSE
    ]
    
    testthat::expect_identical(
      reused_code$variable_type,
      c(
        "grade",
        "grade_span"
      )
    )
    
    testthat::expect_identical(
      reused_code$label,
      c(
        "Grades 9-12",
        "School grade span 9-12"
      )
    )
    
    testthat::expect_identical(
      reused_code$num,
      c(
        15L,
        78L
      )
    )
  }
)


testthat::test_that(
  "GX follows graduation gender schema drift",
  {
    map_2019 <-
      cde_classification_map_v2(
        dataset = "graduation",
        file_type = "cohort",
        data_year = 19L
      )
    
    map_2025 <-
      cde_classification_map_v2(
        dataset = "graduation",
        file_type = "cohort",
        data_year = 25L
      )
    
    gx_2019 <- map_2019[
      map_2019$variable_type ==
        "reporting_category" &
        map_2019$source_value ==
        "GX",
      ,
      drop = FALSE
    ]
    
    gx_2025 <- map_2025[
      map_2025$variable_type ==
        "reporting_category" &
        map_2025$source_value ==
        "GX",
      ,
      drop = FALSE
    ]
    
    testthat::expect_equal(
      nrow(gx_2019),
      1L
    )
    
    testthat::expect_equal(
      nrow(gx_2025),
      1L
    )
    
    testthat::expect_identical(
      gx_2019$label,
      "Gender Missing"
    )
    
    testthat::expect_identical(
      gx_2019$num,
      36L
    )
    
    testthat::expect_identical(
      gx_2025$label,
      "Non-Binary"
    )
    
    testthat::expect_identical(
      gx_2025$num,
      35L
    )
  }
)


testthat::test_that(
  "dropout files use the same documented GX drift",
  {
    map_2019 <-
      cde_classification_map_v2(
        dataset = "graduation",
        file_type = "dropout",
        data_year = 19L
      )
    
    map_2025 <-
      cde_classification_map_v2(
        dataset = "graduation",
        file_type = "dropout",
        data_year = 25L
      )
    
    gx_2019 <- map_2019[
      map_2019$source_value ==
        "GX",
      ,
      drop = FALSE
    ]
    
    gx_2025 <- map_2025[
      map_2025$source_value ==
        "GX",
      ,
      drop = FALSE
    ]
    
    testthat::expect_identical(
      gx_2019$label,
      "Gender Missing"
    )
    
    testthat::expect_identical(
      gx_2025$label,
      "Non-Binary"
    )
  }
)


testthat::test_that(
  "map validation rejects duplicate composite keys",
  {
    classification_map <-
      cde_classification_map_v2(
        dataset = "graduation",
        file_type = "cohort",
        data_year = 25L
      )
    
    invalid_map <- rbind(
      classification_map,
      classification_map[
        1L,
        ,
        drop = FALSE
      ]
    )
    
    testthat::expect_error(
      validate_cde_classification_map_v2(
        invalid_map
      ),
      regexp = "duplicate composite primary key"
    )
  }
)

testthat::test_that(
  "restraint and seclusion gender codes are context specific",
  {
    classification_map <-
      cde_classification_map_v2(
        dataset = "restraint_seclusion",
        file_type = "overall",
        data_year = 20L
      )
    
    gender_rows <- classification_map[
      classification_map$source_value %in%
        c(
          "GN",
          "GX"
        ),
      ,
      drop = FALSE
    ]
    
    gender_rows <- gender_rows[
      order(
        gender_rows$source_value
      ),
      ,
      drop = FALSE
    ]
    
    testthat::expect_identical(
      gender_rows$source_value,
      c(
        "GN",
        "GX"
      )
    )
    
    testthat::expect_identical(
      gender_rows$label,
      c(
        "Non-Binary",
        "Gender Missing"
      )
    )
    
    testthat::expect_identical(
      gender_rows$num,
      c(
        35L,
        36L
      )
    )
    
    testthat::expect_identical(
      gender_rows$group_num,
      c(
        4L,
        4L
      )
    )
  }
)


testthat::test_that(
  "ALL differs between grade span and staff gender",
  {
    classification_map <-
      cde_classification_map_v2(
        dataset = "certificated_staff",
        file_type = "race_ethnicity",
        data_year = 20L
      )
    
    all_rows <- classification_map[
      classification_map$source_value ==
        "ALL",
      ,
      drop = FALSE
    ]
    
    all_rows <- all_rows[
      order(
        all_rows$variable_type
      ),
      ,
      drop = FALSE
    ]
    
    testthat::expect_identical(
      all_rows$variable_type,
      c(
        "grade_span",
        "staff_gender"
      )
    )
    
    testthat::expect_identical(
      all_rows$label,
      c(
        "All data for all schools",
        "All"
      )
    )
    
    testthat::expect_identical(
      all_rows$num,
      c(
        75L,
        87L
      )
    )
    
    testthat::expect_identical(
      all_rows$group,
      c(
        "Grade Span",
        "Gender"
      )
    )
  }
)

testthat::test_that(
  "labeling function applies contextual graduation mappings",
  {
    source_data <- data.frame(
      reporting_category = c(
        "GF",
        "GM",
        "GX",
        NA_character_
      ),
      stringsAsFactors = FALSE
    )
    
    original_data <- source_data
    
    labeled_data <-
      cde_files_group_labeling_v2(
        df = source_data,
        var_names = "reporting_category",
        output_names = "demo_group",
        variable_types = "reporting_category",
        dataset = "graduation",
        file_type = "cohort",
        data_year = 19L
      )
    
    testthat::expect_identical(
      source_data,
      original_data
    )
    
    testthat::expect_identical(
      labeled_data$demo_group_label,
      c(
        "Female",
        "Male",
        "Gender Missing",
        NA_character_
      )
    )
    
    testthat::expect_identical(
      labeled_data$demo_group_num,
      c(
        33L,
        34L,
        36L,
        NA_integer_
      )
    )
    
    testthat::expect_true(
      is.integer(
        labeled_data$demo_group_num
      )
    )
    
    testthat::expect_true(
      is.integer(
        labeled_data$demo_group_group_num
      )
    )
  }
)


testthat::test_that(
  "labeling function distinguishes identical values by variable type",
  {
    source_data <- data.frame(
      school_grade_span = "ALL",
      staff_gender = "ALL",
      stringsAsFactors = FALSE
    )
    
    labeled_data <-
      cde_files_group_labeling_v2(
        df = source_data,
        var_names = c(
          "school_grade_span",
          "staff_gender"
        ),
        output_names = c(
          "grade_span",
          "gender"
        ),
        variable_types = c(
          "grade_span",
          "staff_gender"
        ),
        dataset = "certificated_staff",
        file_type = "race_ethnicity",
        data_year = 20L
      )
    
    testthat::expect_identical(
      c(
        labeled_data$grade_span_label,
        labeled_data$gender_label
      ),
      c(
        "All data for all schools",
        "All"
      )
    )
    
    testthat::expect_identical(
      c(
        labeled_data$grade_span_num,
        labeled_data$gender_num
      ),
      c(
        75L,
        87L
      )
    )
  }
)


testthat::test_that(
  "unmapped values can stop or warn",
  {
    source_data <- data.frame(
      reporting_category = "UNKNOWN_CODE",
      stringsAsFactors = FALSE
    )
    
    testthat::expect_error(
      cde_files_group_labeling_v2(
        df = source_data,
        var_names = "reporting_category",
        output_names = "demo_group",
        variable_types = "reporting_category",
        dataset = "graduation",
        file_type = "cohort",
        data_year = 25L,
        fail_on_unmapped = TRUE
      ),
      regexp = "unmapped value"
    )
    
    testthat::expect_warning(
      cde_files_group_labeling_v2(
        df = source_data,
        var_names = "reporting_category",
        output_names = "demo_group",
        variable_types = "reporting_category",
        dataset = "graduation",
        file_type = "cohort",
        data_year = 25L,
        fail_on_unmapped = FALSE
      ),
      regexp = "unmapped value"
    )
  }
)


testthat::test_that(
  "labeling function can return its applicable map",
  {
    source_data <- data.frame(
      reporting_category = "GX",
      stringsAsFactors = FALSE
    )
    
    result <-
      cde_files_group_labeling_v2(
        df = source_data,
        var_names = "reporting_category",
        output_names = "demo_group",
        variable_types = "reporting_category",
        dataset = "graduation",
        file_type = "cohort",
        data_year = 19L,
        return_map = TRUE
      )
    
    testthat::expect_named(
      result,
      c(
        "data",
        "map"
      )
    )
    
    testthat::expect_s3_class(
      result$data,
      "data.frame"
    )
    
    testthat::expect_s3_class(
      result$map,
      "tbl_df"
    )
    
    testthat::expect_true(
      all(
        c(
          "dataset",
          "file_type",
          "data_year",
          "variable_type",
          "source_value",
          "source_note"
        ) %in%
          names(result$map)
      )
    )
  }
)

testthat::test_that(
  "canonical classification IDs identify one label",
  {
    classification_map <-
      cde_classification_map_v2(
        dataset = "certificated_staff",
        file_type = "race_ethnicity",
        data_year = 20L
      )
    
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
    
    testthat::expect_false(
      any(
        duplicated(
          canonical_classifications[
            c(
              "group_num",
              "num"
            )
          ]
        )
      )
    )
    
    staff_all <- classification_map[
      classification_map$variable_type ==
        "staff_gender" &
        classification_map$source_value ==
        "ALL",
      ,
      drop = FALSE
    ]
    
    testthat::expect_identical(
      staff_all$num,
      87L
    )
    
    testthat::expect_identical(
      staff_all$label,
      "All"
    )
  }
)

testthat::test_that(
  "comprehensive lookup contains unique canonical classifications",
  {
    classification_lookup <-
      cde_classification_lookup_v2()
    
    testthat::expect_s3_class(
      classification_lookup,
      "tbl_df"
    )
    
    testthat::expect_equal(
      nrow(classification_lookup),
      87L
    )
    
    testthat::expect_false(
      any(
        duplicated(
          classification_lookup[
            c(
              "group_num",
              "num"
            )
          ]
        )
      )
    )
    
    gender_lookup <- classification_lookup[
      classification_lookup$group ==
        "Gender",
      ,
      drop = FALSE
    ]
    
    testthat::expect_identical(
      gender_lookup$num,
      c(
        33L,
        34L,
        35L,
        36L,
        87L
      )
    )
    
    testthat::expect_identical(
      gender_lookup$label,
      c(
        "Female",
        "Male",
        "Non-Binary",
        "Gender Missing",
        "All"
      )
    )
  }
)


testthat::test_that(
  "comprehensive lookup makes fact classifications readable",
  {
    example_fact <- data.frame(
      group_num = c(
        1L,
        4L,
        4L,
        9L
      ),
      num = c(
        1L,
        35L,
        87L,
        75L
      ),
      value = c(
        10,
        20,
        30,
        40
      )
    )
    
    readable_fact <- example_fact |>
      dplyr::left_join(
        cde_classification_lookup_v2(),
        by = c(
          "group_num",
          "num"
        )
      )
    
    testthat::expect_false(
      anyNA(
        readable_fact$label
      )
    )
    
    testthat::expect_identical(
      readable_fact$label,
      c(
        "African American",
        "Non-Binary",
        "All",
        "All data for all schools"
      )
    )
  }
)

test_that(
  "certificated staff gender codes use their contextual meanings",
  {
    test_data <- data.frame(
      school_grade_span = c(
        "ALL",
        "GS_912"
      ),
      staff_gender = c(
        "ALL",
        "GX"
      ),
      stringsAsFactors = FALSE
    )
    
    result <- cde_files_group_labeling_v2(
      df = test_data,
      var_names = c(
        "school_grade_span",
        "staff_gender"
      ),
      output_names = c(
        "grade_span",
        "gender"
      ),
      variable_types = c(
        "grade_span",
        "staff_gender"
      ),
      dataset = "certificated_staff",
      file_type = "race_ethnicity",
      data_year = 20L,
      validate = FALSE,
      fail_on_unmapped = TRUE
    )
    
    expect_identical(
      result$grade_span_num,
      c(
        75L,
        78L
      )
    )
    
    expect_identical(
      result$gender_label,
      c(
        "All",
        "Non-Binary"
      )
    )
    
    expect_identical(
      result$gender_num,
      c(
        87L,
        35L
      )
    )
    
    expect_identical(
      result$gender_group_num,
      c(
        4L,
        4L
      )
    )
  }
)

test_that(
  "missing variable_types reports supported options",
  {
    expect_error(
      cde_files_group_labeling_v2(
        df = data.frame(
          reporting_category = "GF"
        ),
        var_names = "reporting_category",
        output_names = "demo_group",
        dataset = "graduation",
        file_type = "cohort",
        data_year = 20L
      ),
      paste0(
        "`variable_types` is required.*",
        "reporting_category.*grade.*",
        "grade_span.*staff_gender"
      )
    )
  }
)

test_that(
  "unsupported variable_types reports supported options",
  {
    captured_error <- expect_error(
      cde_files_group_labeling_v2(
        df = data.frame(
          reporting_category = "GF"
        ),
        var_names = "reporting_category",
        output_names = "demo_group",
        variable_types = "gender",
        dataset = "graduation",
        file_type = "cohort",
        data_year = 20L
      )
    )
    
    error_message <- conditionMessage(
      captured_error
    )
    
    expect_match(
      error_message,
      "Unsupported `variable_types` value(s):",
      fixed = TRUE
    )
    
    expect_match(
      error_message,
      "- gender",
      fixed = TRUE
    )
    
    expect_match(
      error_message,
      "- reporting_category",
      fixed = TRUE
    )
    
    expect_match(
      error_message,
      "- grade",
      fixed = TRUE
    )
    
    expect_match(
      error_message,
      "- grade_span",
      fixed = TRUE
    )
    
    expect_match(
      error_message,
      "- staff_gender",
      fixed = TRUE
    )
  }
)
