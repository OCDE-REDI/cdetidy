test_that("build_warehouse_path constructs paths for all data sources", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_sources_"
  )

  assessment_path <- build_warehouse_path(
    data_source = "Assessment",
    table_name = "cast_core",
    table_type = "fact",
    warehouse_root = warehouse_root
  )

  cde_path <- build_warehouse_path(
    data_source = "CDE",
    table_name = "chronic_absenteeism",
    table_type = "fact",
    warehouse_root = warehouse_root
  )

  dashboard_path <- build_warehouse_path(
    data_source = "Dashboard",
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
      "cast_core_fact.csv"
    )
  )

  expect_identical(
    cde_path,
    file.path(
      warehouse_root,
      "CDE",
      "chronic_absenteeism_fact.csv"
    )
  )

  expect_identical(
    dashboard_path,
    file.path(
      warehouse_root,
      "Dashboard",
      "dashboard_science_fact.csv.gz"
    )
  )

  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path constructs dimension paths", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_dimensions_"
  )

  standard_dimension_path <- build_warehouse_path(
    data_source = "Assessment",
    table_name = "assessment_test",
    table_type = "dimension",
    warehouse_root = warehouse_root,
    subdirectories = "Dimensions"
  )

  annualized_dimension_path <- build_warehouse_path(
    data_source = "Assessment",
    table_name = "cast_25_entities",
    table_type = "dimension",
    warehouse_root = warehouse_root,
    subdirectories = c(
      "Dimensions",
      "Annualized Dimensions"
    )
  )

  expect_identical(
    standard_dimension_path,
    file.path(
      warehouse_root,
      "Assessment",
      "Dimensions",
      "assessment_test_dim.csv"
    )
  )

  expect_identical(
    annualized_dimension_path,
    file.path(
      warehouse_root,
      "Assessment",
      "Dimensions",
      "Annualized Dimensions",
      "cast_25_entities_dim.csv"
    )
  )

  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path supports annualized dimensions by source", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_annualized_"
  )

  cde_path <- build_warehouse_path(
    data_source = "CDE",
    table_name = "enrollment_25_entities",
    table_type = "dimension",
    warehouse_root = warehouse_root,
    subdirectories = c(
      "Dimensions",
      "Annualized Dimensions"
    )
  )

  dashboard_path <- build_warehouse_path(
    data_source = "Dashboard",
    table_name = "suspension_25_entities",
    table_type = "dimension",
    warehouse_root = warehouse_root,
    subdirectories = c(
      "Dimensions",
      "Annualized Dimensions"
    )
  )

  expect_identical(
    cde_path,
    file.path(
      warehouse_root,
      "CDE",
      "Dimensions",
      "Annualized Dimensions",
      "enrollment_25_entities_dim.csv"
    )
  )

  expect_identical(
    dashboard_path,
    file.path(
      warehouse_root,
      "Dashboard",
      "Dimensions",
      "Annualized Dimensions",
      "suspension_25_entities_dim.csv"
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
    data_source = " assessment ",
    table_name = "cast_area",
    table_type = " FACT ",
    warehouse_root = warehouse_root
  )

  expect_identical(
    normalized_path,
    file.path(
      warehouse_root,
      "Assessment",
      "cast_area_fact.csv"
    )
  )

  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path accepts empty subdirectory specifications", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_empty_subdirectories_"
  )

  character_path <- build_warehouse_path(
    data_source = "Assessment",
    table_name = "cast_core",
    table_type = "fact",
    warehouse_root = warehouse_root,
    subdirectories = character()
  )

  null_path <- build_warehouse_path(
    data_source = "Assessment",
    table_name = "cast_area",
    table_type = "fact",
    warehouse_root = warehouse_root,
    subdirectories = NULL
  )

  expect_identical(
    character_path,
    file.path(
      warehouse_root,
      "Assessment",
      "cast_core_fact.csv"
    )
  )

  expect_identical(
    null_path,
    file.path(
      warehouse_root,
      "Assessment",
      "cast_area_fact.csv"
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
    pattern = "warehouse_path_side_effects_"
  )

  constructed_path <- build_warehouse_path(
    data_source = "Assessment",
    table_name = "cast_25_entities",
    table_type = "dimension",
    warehouse_root = warehouse_root,
    subdirectories = c(
      "Dimensions",
      "Annualized Dimensions"
    )
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


test_that("build_warehouse_path rejects invalid source and table choices", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_choices_"
  )

  expect_error(
    build_warehouse_path(
      data_source = "Unknown",
      table_name = "test_table",
      table_type = "fact",
      warehouse_root = warehouse_root
    ),
    "`data_source` must be one of: Assessment, Dashboard, CDE.",
    fixed = TRUE
  )

  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      table_name = "test_table",
      table_type = "summary",
      warehouse_root = warehouse_root
    ),
    "`table_type` must be one of: fact, dimension.",
    fixed = TRUE
  )

  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path rejects invalid table names", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_table_names_"
  )

  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      table_name = "test table",
      table_type = "fact",
      warehouse_root = warehouse_root
    ),
    paste0(
      "`table_name` may contain only letters, numbers, ",
      "and underscores."
    ),
    fixed = TRUE
  )

  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      table_name = "test_table_fact",
      table_type = "fact",
      warehouse_root = warehouse_root
    ),
    "`table_name` must not already end in `_fact` or `_dim`.",
    fixed = TRUE
  )

  expect_error(
    build_warehouse_path(
      data_source = "CDE",
      table_name = "test_table_dim",
      table_type = "dimension",
      warehouse_root = warehouse_root
    ),
    "`table_name` must not already end in `_fact` or `_dim`.",
    fixed = TRUE
  )

  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path rejects invalid subdirectories", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_subdirectories_"
  )

  invalid_subdirectories <- list(
    "../Dimensions",
    "Dimensions/Annualized Dimensions",
    "Dimensions\\Annualized Dimensions",
    "..",
    ".",
    "Invalid:Folder"
  )

  for (invalid_subdirectory in invalid_subdirectories) {
    expect_error(
      build_warehouse_path(
        data_source = "Assessment",
        table_name = "test_table",
        table_type = "dimension",
        warehouse_root = warehouse_root,
        subdirectories = invalid_subdirectory
      ),
      paste0(
        "Each `subdirectories` value must be one folder name, ",
        "not a path."
      ),
      fixed = TRUE
    )
  }

  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      table_name = "test_table",
      table_type = "dimension",
      warehouse_root = warehouse_root,
      subdirectories = c(
        "Dimensions",
        ""
      )
    ),
    paste0(
      "`subdirectories` must be a character vector of nonempty ",
      "folder names, `character()`, or `NULL`."
    ),
    fixed = TRUE
  )

  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      table_name = "test_table",
      table_type = "dimension",
      warehouse_root = warehouse_root,
      subdirectories = c(
        "Dimensions",
        "Dimensions"
      )
    ),
    "`subdirectories` must not contain duplicate folder names.",
    fixed = TRUE
  )

  expect_false(
    dir.exists(
      warehouse_root
    )
  )
})


test_that("build_warehouse_path validates root and compression settings", {
  warehouse_root <- tempfile(
    pattern = "warehouse_path_arguments_"
  )

  expect_error(
    build_warehouse_path(
      data_source = "Assessment",
      table_name = "test_table",
      table_type = "fact",
      warehouse_root = "",
      compressed = FALSE
    ),
    "`warehouse_root` must be one nonempty directory path.",
    fixed = TRUE
  )

  expect_error(
    build_warehouse_path(
      data_source = "Dashboard",
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
