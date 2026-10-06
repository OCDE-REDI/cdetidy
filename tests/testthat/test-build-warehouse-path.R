test_that("build_warehouse_path constructs standard warehouse paths", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_"
  )
  
  assessment_path <- build_warehouse_path(
    data_source = "Assessment",
    data_category = "CAST",
    table_name = "cast_25_core",
    table_type = "fact",
    warehouse_root = warehouse_root
  )
  
  cde_path <- build_warehouse_path(
    data_source = "CDE",
    data_category = "Enrollment",
    table_name = "census_enrollment",
    table_type = "dimension",
    warehouse_root = warehouse_root
  )
  
  dashboard_path <- build_warehouse_path(
    data_source = "Dashboard",
    data_category = "Science",
    table_name = "dashboard_science",
    table_type = "fact",
    warehouse_root = warehouse_root,
    compressed = TRUE
  )
  
  expect_identical(
    assessment_path,
    file.path(
      warehouse_root,
      "Assessment",
      "CAST",
      "cast_25_core_fact.csv"
    )
  )
  
  expect_identical(
    cde_path,
    file.path(
      warehouse_root,
      "CDE",
      "Enrollment",
      "census_enrollment_dim.csv"
    )
  )
  
  expect_identical(
    dashboard_path,
    file.path(
      warehouse_root,
      "Dashboard",
      "Science",
      "dashboard_science_fact.csv.gz"
    )
  )
  
  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path normalizes supported choices", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_normalized_"
  )
  
  normalized_path <- build_warehouse_path(
    data_source = "assessment",
    data_category = "CAST",
    table_name = "cast_25_area",
    table_type = "FACT",
    warehouse_root = warehouse_root
  )
  
  expect_identical(
    normalized_path,
    file.path(
      warehouse_root,
      "Assessment",
      "CAST",
      "cast_25_area_fact.csv"
    )
  )
  
  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path has no filesystem side effects", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_side_effect_"
  )
  
  constructed_path <- build_warehouse_path(
    data_source = "CDE",
    data_category = "Discipline",
    table_name = "suspension",
    table_type = "fact",
    warehouse_root = warehouse_root
  )
  
  expect_false(
    dir.exists(
      warehouse_root
    )
  )
  
  expect_false(
    dir.exists(
      dirname(
        constructed_path
      )
    )
  )
  
  expect_false(
    file.exists(
      constructed_path
    )
  )
})


test_that("build_warehouse_path rejects unsupported sources", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_source_"
  )
  
  expect_error(
    build_warehouse_path(
      data_source = "Unknown",
      data_category = "CAST",
      table_name = "test_table",
      table_type = "fact",
      warehouse_root = warehouse_root
    ),
    "`data_source` must be one of: Assessment, Dashboard, CDE.",
    fixed = TRUE
  )
  
  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path rejects category paths and traversal", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_category_"
  )
  
  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      data_category = "../CAST",
      table_name = "test_table",
      table_type = "fact",
      warehouse_root = warehouse_root
    ),
    "`data_category` must be one folder name, not a path.",
    fixed = TRUE
  )
  
  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      data_category = "CAST/Archive",
      table_name = "test_table",
      table_type = "fact",
      warehouse_root = warehouse_root
    ),
    "`data_category` must be one folder name, not a path.",
    fixed = TRUE
  )
  
  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path rejects pre-suffixed table names", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_suffix_"
  )
  
  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      data_category = "CAST",
      table_name = "test_table_fact",
      table_type = "fact",
      warehouse_root = warehouse_root
    ),
    "`table_name` must not already end in `_fact` or `_dim`",
    fixed = TRUE
  )
  
  expect_error(
    build_warehouse_path(
      data_source = "CDE",
      data_category = "Enrollment",
      table_name = "test_table_dim",
      table_type = "dimension",
      warehouse_root = warehouse_root
    ),
    "`table_name` must not already end in `_fact` or `_dim`",
    fixed = TRUE
  )
  
  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path validates table type and compression", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_arguments_"
  )
  
  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      data_category = "CAST",
      table_name = "test_table",
      table_type = "summary",
      warehouse_root = warehouse_root
    ),
    "`table_type` must be one of: fact, dimension.",
    fixed = TRUE
  )
  
  expect_error(
    build_warehouse_path(
      data_source = "Dashboard",
      data_category = "Science",
      table_name = "test_table",
      table_type = "fact",
      warehouse_root = warehouse_root,
      compressed = "yes"
    ),
    "`compressed` must be TRUE or FALSE.",
    fixed = TRUE
  )
  
  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})
