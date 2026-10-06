test_that("safe_fwrite_legacy remains available but is deprecated", {
  output_path <- tempfile(
    pattern = "safe_fwrite_legacy_",
    fileext = ".csv"
  )
  
  on.exit(
    unlink(
      output_path,
      force = TRUE
    ),
    add = TRUE
  )
  
  test_data <- data.frame(
    cds = "00123450000001",
    value = 10
  )
  
  result <- NULL
  
  expect_warning(
    {
      result <- suppressMessages(
        safe_fwrite_legacy(
          data = test_data,
          path = output_path,
          data_year = 2025L,
          data_source = "Assessment",
          user_note = "Temporary fact test",
          table_name = "legacy_compatibility_test",
          n_check = 0L
        )
      )
    },
    regexp = "retired export interface"
  )
  
  expect_true(
    file.exists(
      output_path
    )
  )
  
  expect_s3_class(
    result,
    "data.frame"
  )
  
  expect_identical(
    result$table_type,
    "fact"
  )
})
