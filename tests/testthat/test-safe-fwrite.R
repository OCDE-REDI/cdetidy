test_that("safe_fwrite writes and validates a fact table", {
  test_root <- tempfile(
    pattern = "safe_fwrite_fact_"
  )

  output_path <- file.path(
    test_root,
    "test_assessment_fact.csv"
  )

  on.exit(
    unlink(
      test_root,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  test_data <- data.frame(
    cds = c(
      "00123450000001",
      "06123450000002",
      "30123450000003"
    ),
    year = c(
      25L,
      25L,
      25L
    ),
    score = c(
      501.1,
      502.2,
      503.3
    )
  )

  manifest <- suppressMessages(
    safe_fwrite(
      data = test_data,
      path = output_path,
      table_name = "test_assessment",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Temporary assessment fact-table test.",
      user_note = "Created during package testing.",
      canonical_table_id = "test_assessment",
      dimension_type = NULL,
      n_check = 0L,
      overwrite = FALSE,
      write_log = FALSE
    )
  )

  written_data <- data.table::fread(
    output_path,
    colClasses = list(
      character = "cds"
    )
  )

  expect_true(
    file.exists(
      output_path
    )
  )

  expect_identical(
    written_data$cds,
    test_data$cds
  )

  expect_identical(
    names(
      written_data
    ),
    names(
      test_data
    )
  )

  expect_s3_class(
    manifest,
    "data.frame"
  )

  expect_equal(
    nrow(
      manifest
    ),
    1L
  )

  expect_identical(
    manifest$table_type,
    "fact"
  )

  expect_identical(
    manifest$data_source,
    "Assessment"
  )

  expect_identical(
    manifest$data_category,
    "CAST"
  )

  expect_false(
    manifest$overwritten
  )
})


test_that("safe_fwrite protects and safely replaces existing files", {
  test_root <- tempfile(
    pattern = "safe_fwrite_overwrite_"
  )

  output_path <- file.path(
    test_root,
    "overwrite_fact.csv"
  )

  on.exit(
    unlink(
      test_root,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  original_data <- data.frame(
    cds = "00123450000001",
    score = 501.1
  )

  modified_data <- data.frame(
    cds = "00123450000001",
    score = 601.1
  )

  suppressMessages(
    safe_fwrite(
      data = original_data,
      path = output_path,
      table_name = "overwrite_test",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Temporary overwrite-protection test.",
      n_check = 0L
    )
  )

  expect_error(
    suppressMessages(
      safe_fwrite(
        data = modified_data,
        path = output_path,
        table_name = "overwrite_test",
        table_type = "fact",
        data_year = 2025L,
        data_source = "Assessment",
        data_category = "CAST",
        data_description = "Temporary overwrite-protection test.",
        n_check = 0L,
        overwrite = FALSE
      )
    ),
    "already exists"
  )

  unchanged_data <- data.table::fread(
    output_path,
    colClasses = list(
      character = "cds"
    )
  )

  expect_identical(
    unchanged_data$score,
    original_data$score
  )

  overwrite_manifest <- suppressMessages(
    safe_fwrite(
      data = modified_data,
      path = output_path,
      table_name = "overwrite_test",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Temporary authorized-overwrite test.",
      n_check = 0L,
      overwrite = TRUE
    )
  )

  replaced_data <- data.table::fread(
    output_path,
    colClasses = list(
      character = "cds"
    )
  )

  temporary_files <- list.files(
    test_root,
    pattern = "_(backup|staged)_",
    all.files = TRUE,
    full.names = TRUE
  )

  expect_identical(
    replaced_data$score,
    modified_data$score
  )

  expect_true(
    overwrite_manifest$overwritten
  )

  expect_length(
    temporary_files,
    0L
  )
})


test_that("safe_fwrite writes an annualized dimension table", {
  test_root <- tempfile(
    pattern = "safe_fwrite_dimension_"
  )

  output_path <- file.path(
    test_root,
    "schools_dim.csv"
  )

  on.exit(
    unlink(
      test_root,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  test_data <- data.frame(
    school_code = c(
      "0000001",
      "0000002"
    ),
    year = c(
      2025L,
      2025L
    ),
    school_name = c(
      "Test School One",
      "Test School Two"
    )
  )

  manifest <- suppressMessages(
    safe_fwrite(
      data = test_data,
      path = output_path,
      table_name = "test_schools",
      table_type = "dimension",
      data_year = 2025L,
      data_source = "CDE",
      data_category = "Enrollment",
      data_description = "Temporary annualized school dimension.",
      canonical_table_id = "test_schools",
      dimension_type = "annualized",
      n_check = 0L
    )
  )

  written_data <- data.table::fread(
    output_path,
    colClasses = list(
      character = "school_code"
    )
  )

  expect_identical(
    written_data$school_code,
    test_data$school_code
  )

  expect_identical(
    manifest$table_type,
    "dimension"
  )

  expect_identical(
    manifest$dimension_type,
    "annualized"
  )
})


test_that("safe_fwrite writes a compressed Dashboard export", {
  test_root <- tempfile(
    pattern = "safe_fwrite_compressed_"
  )

  output_path <- file.path(
    test_root,
    "dashboard_science_fact.csv.gz"
  )

  on.exit(
    unlink(
      test_root,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  test_data <- data.frame(
    cds = c(
      "00123450000001",
      "00123450000002"
    ),
    status = c(
      "High",
      "Medium"
    )
  )

  manifest <- suppressMessages(
    safe_fwrite(
      data = test_data,
      path = output_path,
      table_name = "dashboard_science",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Dashboard",
      data_category = "Science",
      data_description = "Temporary compressed Dashboard export.",
      n_check = 0L
    )
  )

  compressed_connection <- gzfile(
    output_path,
    open = "rt"
  )

  written_data <- utils::read.csv(
    compressed_connection,
    colClasses = c(
      cds = "character"
    ),
    stringsAsFactors = FALSE
  )

  close(
    compressed_connection
  )

  expect_true(
    file.exists(
      output_path
    )
  )

  expect_identical(
    written_data$cds,
    test_data$cds
  )

  expect_identical(
    manifest$file_name,
    basename(
      output_path
    )
  )
})


test_that("safe_fwrite creates and selectively updates an export log", {
  test_root <- tempfile(
    pattern = "safe_fwrite_log_"
  )

  log_path <- file.path(
    test_root,
    "export_log.csv"
  )

  first_path <- file.path(
    test_root,
    "first_fact.csv"
  )

  unrelated_path <- file.path(
    test_root,
    "unrelated_fact.csv"
  )

  replacement_path <- file.path(
    test_root,
    "replacement_fact.csv"
  )

  on.exit(
    unlink(
      test_root,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  test_data <- data.frame(
    cds = "00123450000001",
    value = 10
  )

  suppressMessages(
    safe_fwrite(
      data = test_data,
      path = first_path,
      table_name = "first_table",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Original targeted log entry.",
      canonical_table_id = "target_table",
      n_check = 0L,
      write_log = TRUE,
      log_path = log_path
    )
  )

  suppressMessages(
    safe_fwrite(
      data = test_data,
      path = unrelated_path,
      table_name = "unrelated_table",
      table_type = "fact",
      data_year = 2024L,
      data_source = "CDE",
      data_category = "Enrollment",
      data_description = "Unrelated annual log entry.",
      canonical_table_id = "unrelated_table",
      n_check = 0L,
      write_log = TRUE,
      log_path = log_path
    )
  )

  suppressMessages(
    safe_fwrite(
      data = test_data,
      path = replacement_path,
      table_name = "replacement_table",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Replacement targeted log entry.",
      canonical_table_id = "target_table",
      n_check = 0L,
      write_log = TRUE,
      log_path = log_path
    )
  )

  written_log <- data.table::fread(
    log_path,
    showProgress = FALSE
  )

  targeted_entry <- written_log[
    canonical_table_id == "target_table" &
      data_year == 2025L
  ]

  unrelated_entry <- written_log[
    canonical_table_id == "unrelated_table" &
      data_year == 2024L
  ]

  temporary_files <- list.files(
    test_root,
    pattern = "_(backup|staged)_",
    all.files = TRUE,
    full.names = TRUE
  )

  expect_equal(
    nrow(
      written_log
    ),
    2L
  )

  expect_equal(
    nrow(
      targeted_entry
    ),
    1L
  )

  expect_identical(
    targeted_entry$data_description,
    "Replacement targeted log entry."
  )

  expect_equal(
    nrow(
      unrelated_entry
    ),
    1L
  )

  expect_identical(
    unrelated_entry$data_description,
    "Unrelated annual log entry."
  )

  expect_length(
    temporary_files,
    0L
  )
})


test_that("safe_fwrite rejects invalid metadata before writing", {
  test_root <- tempfile(
    pattern = "safe_fwrite_validation_"
  )

  on.exit(
    unlink(
      test_root,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  test_data <- data.frame(
    cds = "00123450000001",
    value = 10
  )

  invalid_dimension_path <- file.path(
    test_root,
    "invalid_dimension.csv"
  )

  invalid_fact_path <- file.path(
    test_root,
    "invalid_fact.csv"
  )

  invalid_source_path <- file.path(
    test_root,
    "invalid_source.csv"
  )

  missing_log_path <- file.path(
    test_root,
    "missing_log.csv"
  )

  empty_data_path <- file.path(
    test_root,
    "empty_data.csv"
  )

  expect_error(
    safe_fwrite(
      data = test_data,
      path = invalid_dimension_path,
      table_name = "invalid_dimension",
      table_type = "dimension",
      data_year = 2025L,
      data_source = "CDE",
      data_category = "Enrollment",
      data_description = "Invalid dimension test.",
      dimension_type = NULL,
      n_check = 0L
    ),
    "`dimension_type` must be one of:",
    fixed = TRUE
  )

  expect_error(
    safe_fwrite(
      data = test_data,
      path = invalid_fact_path,
      table_name = "invalid_fact",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Invalid fact test.",
      dimension_type = "annualized",
      n_check = 0L
    ),
    "`dimension_type` must be `NULL` for fact tables.",
    fixed = TRUE
  )

  expect_error(
    safe_fwrite(
      data = test_data,
      path = invalid_source_path,
      table_name = "invalid_source",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Unknown",
      data_category = "CAST",
      data_description = "Invalid source test.",
      n_check = 0L
    ),
    "`data_source` must be one of: Assessment, Dashboard, CDE.",
    fixed = TRUE
  )

  expect_error(
    safe_fwrite(
      data = test_data,
      path = missing_log_path,
      table_name = "missing_log",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Missing log-path test.",
      n_check = 0L,
      write_log = TRUE,
      log_path = NULL
    ),
    "`log_path` must be an explicit, nonempty file path",
    fixed = TRUE
  )

  expect_error(
    safe_fwrite(
      data = test_data[
        0L,
        ,
        drop = FALSE
      ],
      path = empty_data_path,
      table_name = "empty_data",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Empty-data test.",
      n_check = 0L
    ),
    "`data` must contain at least one row.",
    fixed = TRUE
  )

  expect_false(
    any(
      file.exists(
        c(
          invalid_dimension_path,
          invalid_fact_path,
          invalid_source_path,
          missing_log_path,
          empty_data_path
        )
      )
    )
  )
})


test_that("safe_fwrite rejects an incompatible log before writing data", {
  test_root <- tempfile(
    pattern = "safe_fwrite_log_preflight_"
  )

  output_path <- file.path(
    test_root,
    "preflight_fact.csv"
  )

  log_path <- file.path(
    test_root,
    "incompatible_log.csv"
  )

  on.exit(
    unlink(
      test_root,
      recursive = TRUE,
      force = TRUE
    ),
    add = TRUE
  )

  dir.create(
    test_root,
    recursive = TRUE
  )

  data.table::fwrite(
    data.table::data.table(
      wrong_column = "preserve this log"
    ),
    log_path
  )

  log_before <- readLines(
    log_path
  )

  expect_error(
    safe_fwrite(
      data = data.frame(
        cds = "00123450000001",
        value = 10
      ),
      path = output_path,
      table_name = "preflight_test",
      table_type = "fact",
      data_year = 2025L,
      data_source = "Assessment",
      data_category = "CAST",
      data_description = "Invalid existing-log preflight test.",
      n_check = 0L,
      write_log = TRUE,
      log_path = log_path
    ),
    "The existing export log has an incompatible schema.",
    fixed = TRUE
  )

  log_after <- readLines(
    log_path
  )

  expect_false(
    file.exists(
      output_path
    )
  )

  expect_identical(
    log_after,
    log_before
  )
})
