test_that("SBAC student groups and grades use separate namespaces", {
  test_data <- data.frame(
    student_group_id = c(3L, 4L, 243L),
    grade = c(3L, 4L, 8L)
  )

  result <- assessment_files_group_labeling(
    test_data,
    var_names = c(
      "student_group_id",
      "grade"),
    output_names = c(
      "demo_group",
      "grade"),
    variable_types = c(
      "student_group",
      "grade"),
    assessment_type = "SBAC",
    data_year = 25L)

  expect_identical(
    result$student_group_id,
    test_data$student_group_id)

  expect_identical(
    result$grade,
    test_data$grade)

  expect_equal(
    result$demo_group_label,
    c(
      "Male",
      "Female",
      "Adult English Learner"))

  expect_equal(
    result$grade_label,
    c(
      "Grade 3",
      "Grade 4",
      "Grade 8"))

  expect_type(
    result$demo_group_num,
    "integer")

  expect_type(
    result$grade_num,
    "integer")
})


test_that("SBAC mapping fails on an unknown student-group code", {
  test_data <- data.frame(
    student_group_id = c(1L, 999L),
    grade = c(3L, 3L)
  )

  expect_error(
    assessment_files_group_labeling(
      test_data,
      var_names = c(
        "student_group_id",
        "grade"),
      output_names = c(
        "demo_group",
        "grade"),
      variable_types = c(
        "student_group",
        "grade"),
      assessment_type = "SBAC",
      data_year = 25L),
    "unmapped value.*999")
})


test_that("SBAC mapping fails on an unknown grade", {
  test_data <- data.frame(
    student_group_id = 1L,
    grade = 98L
  )

  expect_error(
    assessment_files_group_labeling(
      test_data,
      var_names = c(
        "student_group_id",
        "grade"),
      output_names = c(
        "demo_group",
        "grade"),
      variable_types = c(
        "student_group",
        "grade"),
      assessment_type = "SBAC",
      data_year = 25L),
    "unmapped value.*98")
})


test_that("SBAC mapping supports zero-padded character codes", {
  test_data <- data.frame(
    student_group_id = c("001", "074"),
    grade = c("03", "08")
  )

  result <- assessment_files_group_labeling(
    test_data,
    var_names = c(
      "student_group_id",
      "grade"),
    output_names = c(
      "demo_group",
      "grade"),
    variable_types = c(
      "student_group",
      "grade"),
    assessment_type = "SBAC",
    data_year = 25L)

  expect_equal(
    result$demo_group_label,
    c(
      "All Students",
      "Black or African American"))

  expect_equal(
    result$grade_label,
    c(
      "Grade 3",
      "Grade 8"))
})

test_that("SBAC mapping returns data only by default", {
  test_data <- data.frame(
    student_group_id = c(1L, 74L),
    grade = c(3L, 8L)
  )

  result <- assessment_files_group_labeling(
    test_data,
    var_names = c(
      "student_group_id",
      "grade"),
    output_names = c(
      "demo_group",
      "grade"),
    variable_types = c(
      "student_group",
      "grade"),
    assessment_type = "SBAC",
    data_year = 25L)

  expect_s3_class(
    result,
    "data.frame")

  expect_true(
    all(c(
      "demo_group_label",
      "demo_group_num",
      "grade_label",
      "grade_num"
    ) %in% names(result)))
})

test_that("SBAC mapping can return labeled data and its map", {
  test_data <- data.frame(
    student_group_id = c(1L, 74L, 243L),
    grade = c(3L, 8L, 13L)
  )

  result <- assessment_files_group_labeling(
    test_data,
    var_names = c(
      "student_group_id",
      "grade"),
    output_names = c(
      "demo_group",
      "grade"),
    variable_types = c(
      "student_group",
      "grade"),
    assessment_type = "SBAC",
    data_year = 25L,
    return_map = TRUE)

  expect_named(
    result,
    c("data", "map"))

  expect_s3_class(
    result$data,
    "data.frame")

  expect_s3_class(
    result$map,
    "tbl_df")

  expect_equal(
    nrow(result$map),
    74L)

  expect_equal(
    sum(result$map$variable_type == "student_group"),
    58L)

  expect_equal(
    sum(result$map$variable_type == "grade"),
    16L)

  expect_equal(
    result$data$demo_group_label,
    c(
      "All Students",
      "Black or African American",
      "Adult English Learner"))
})


test_that("returned map includes only requested variable types", {
  test_data <- data.frame(
    student_group_id = c(1L, 74L)
  )

  result <- assessment_files_group_labeling(
    test_data,
    var_names = "student_group_id",
    output_names = "demo_group",
    variable_types = "student_group",
    assessment_type = "SBAC",
    data_year = 25L,
    return_map = TRUE)

  expect_equal(
    nrow(result$map),
    58L)

  expect_identical(
    unique(result$map$variable_type),
    "student_group")
})


test_that("return_map requires one nonmissing logical value", {
  test_data <- data.frame(
    student_group_id = 1L
  )

  expect_error(
    assessment_files_group_labeling(
      test_data,
      var_names = "student_group_id",
      output_names = "demo_group",
      variable_types = "student_group",
      assessment_type = "SBAC",
      data_year = 25L,
      return_map = NA),
    "`return_map` must be either `TRUE` or `FALSE`",
    fixed = TRUE)

  expect_error(
    assessment_files_group_labeling(
      test_data,
      var_names = "student_group_id",
      output_names = "demo_group",
      variable_types = "student_group",
      assessment_type = "SBAC",
      data_year = 25L,
      return_map = "yes"),
    "`return_map` must be either `TRUE` or `FALSE`",
    fixed = TRUE)
})

test_that("code 243 is interpreted by assessment family", {
  cast_map <- assessment_classification_map("CAST", 25)
  elpac_map <- assessment_classification_map("ELPAC", 25)

  cast_243 <- subset(
    cast_map,
    variable_type == "student_group" &
      source_value == "243"
  )

  elpac_243 <- subset(
    elpac_map,
    variable_type == "student_group" &
      source_value == "243"
  )

  expect_equal(cast_243$label, "Adult English Learner")
  expect_equal(cast_243$num, 47L)

  expect_equal(
    elpac_243$label,
    "English Learner - 2 Years in Program"
  )
  expect_equal(elpac_243$num, 55L)
})

test_that("expanded grade codes map correctly", {
  test_data <- data.frame(
    grade = c(
      "KN", "01", "02", "09",
      "10", "12", "13", "14", "99")
  )

  result <- assessment_files_group_labeling(
    test_data,
    var_names = "grade",
    output_names = "grade",
    variable_types = "grade",
    assessment_type = "CAST",
    data_year = 25L)

  expect_equal(
    result$grade_label,
    c(
      "Kindergarten",
      "Grade 1",
      "Grade 2",
      "Grade 9",
      "Grade 10",
      "Grade 12",
      "All Grades",
      "All High School Grades",
      "Cohort Grade/Graduating Class"))

  expect_equal(
    result$grade_num,
    c(
      9L, 10L, 11L, 18L,
      19L, 21L, 22L, 23L, 24L))
})

test_that("ELPAC-specific student groups map correctly", {
  test_data <- data.frame(
    student_group_id = c(
      239L,
      242L, 243L, 248L,
      228L, 238L)
  )

  result <- assessment_files_group_labeling(
    test_data,
    var_names = "student_group_id",
    output_names = "demo_group",
    variable_types = "student_group",
    assessment_type = "ELPAC",
    data_year = 25L)

  expect_equal(
    result$demo_group_num,
    c(
      38L,
      54L, 55L, 93L,
      60L, 70L))

  expect_equal(
    result$demo_group_group,
    c(
      "Test Taken",
      rep(
        "English Language Acquisition Status",
        3L),
      rep(
        "First Language",
        2L)))
})

test_that("classification maps are not restricted to hard-coded years", {
  expect_no_error(
    assessment_classification_map(
      "SBAC",
      26L))

  expect_no_error(
    assessment_classification_map(
      "CAST",
      26L))

  expect_no_error(
    assessment_classification_map(
      "ELPAC",
      26L))
})
