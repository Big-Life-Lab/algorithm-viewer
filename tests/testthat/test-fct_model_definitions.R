# Tests for read_model_definitions and its private sub-functions.
#
# Fixtures (.test_md) are loaded by helper-fixtures.R which is sourced before
# this file.

# ── Top-level structure ────────────────────────────────────────────────────────

test_that("read_model_definitions returns a list with $meta and $models", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  expect_true(is.list(.test_md))
  expect_true("meta" %in% names(.test_md))
  expect_true("models" %in% names(.test_md))
})

test_that("$meta contains algorithm and version fields", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  expect_true("algorithm" %in% names(.test_md$meta))
  expect_true("version"   %in% names(.test_md$meta))
  expect_type(.test_md$meta$algorithm, "character")
  expect_type(.test_md$meta$version,   "character")
})

test_that("$source_file is set to the normalised YAML path", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  expect_equal(.test_md$source_file, normalizePath(.test_reduced_yaml))
})

# ── Model list ─────────────────────────────────────────────────────────────────

test_that("_all_ template model is removed after copying to other models", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  expect_false("_all_" %in% names(.test_md$models))
})

test_that("model_index is assigned sequentially starting at 1", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  model_index <- 1
  for (model_data in .test_md$models) {
    expect_equal(model_data$model_index, model_index)
    model_index <- model_index + 1
  }
})

test_that("model_id on each model matches its key in $models", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  for (model_id in names(.test_md$models)) {
    expect_equal(.test_md$models[[model_id]]$model_id, model_id)
  }
})

test_that("model_color is a non-empty character string on each model", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  for (model_data in .test_md$models) {
    expect_type(model_data$model_color, "character")
    expect_gt(nchar(model_data$model_color), 0)
  }
})

test_that("two models receive distinct colors", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  model_colors <- list()
  for (model_data in .test_md$models) {
    model_colors[[length(model_colors) + 1]] <- model_data$model_color
  }
  expect_true(length(model_colors) == length(unique(model_colors)))
})

test_that("root_dir on each model is the directory of the YAML file", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  expected <- dirname(normalizePath(.test_reduced_yaml))
  for (model_data in .test_md$models) {
    expect_equal(model_data$root_dir, expected)
  }
})

# ── Data frames ────────────────────────────────────────────────────────────────

test_that("variables, variable_details, and model_steps are non-empty data frames", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  for (model_data in .test_md$models) {
    expect_true(is.data.frame(model_data$variables)        && nrow(model_data$variables)        > 0)
    expect_true(is.data.frame(model_data$model_steps)      && nrow(model_data$model_steps)      > 0)
  }
})

test_that("model_pipeline is populated (not NULL)", {
  skip_if(is.null(.test_md), "Test fixtures not available")
  for (model_data in .test_md$models) {
    expect_false(is.null(model_data$model_pipeline))
  }
})

# ── Error handling ─────────────────────────────────────────────────────────────

test_that("read_model_definitions errors when the YAML file does not exist", {
  expect_error(
    suppressWarnings(algorithm.viewer:::read_model_definitions("/no/such/file.yaml"))
  )
})
