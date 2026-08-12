# Tests for .parse_jsonschema_error and read_and_validate_yaml.

make_validation_df <- function(n_errors) {
  data.frame(
    valid = rep(FALSE, n_errors),
    instanceLocation = paste0("/models/m", seq_len(n_errors)),
    error = paste0("problem ", seq_len(n_errors)),
    stringsAsFactors = FALSE
  )
}

test_that(".parse_jsonschema_error reports every error row", {
  # Regression test: the loop previously used seq_along() on the data frame,
  # which iterates over its 3 columns, silently dropping errors past row 3.
  df <- make_validation_df(5)
  result <- algorithm.viewer:::.parse_jsonschema_error(df, html = FALSE)
  for (i in 1:5) {
    expect_match(result, paste0("/models/m", i, ": problem ", i), fixed = TRUE)
  }
})

test_that(".parse_jsonschema_error formats plain-text output as a bulleted list", {
  df <- make_validation_df(2)
  result <- algorithm.viewer:::.parse_jsonschema_error(df, html = FALSE)
  expect_match(result, "  - /models/m1: problem 1", fixed = TRUE)
  expect_match(result, "\n", fixed = TRUE)
  expect_false(grepl("<li>", result, fixed = TRUE))
})

test_that(".parse_jsonschema_error formats HTML output as a <ul> list", {
  df <- make_validation_df(2)
  result <- algorithm.viewer:::.parse_jsonschema_error(df, html = TRUE)
  expect_match(result, "^<ul>")
  expect_match(result, "</ul>$")
  expect_match(result, "<li>/models/m1: problem 1</li>", fixed = TRUE)
  expect_match(result, "<li>/models/m2: problem 2</li>", fixed = TRUE)
})

test_that(".parse_jsonschema_error returns NULL when no rows are errors", {
  df <- data.frame(
    valid = c(TRUE, TRUE),
    instanceLocation = c("/a", "/b"),
    error = c("", ""),
    stringsAsFactors = FALSE
  )
  expect_null(algorithm.viewer:::.parse_jsonschema_error(df))
})

test_that(".parse_jsonschema_error skips rows with NA valid", {
  df <- make_validation_df(2)
  df$valid[1] <- NA
  result <- algorithm.viewer:::.parse_jsonschema_error(df, html = FALSE)
  expect_false(grepl("problem 1", result, fixed = TRUE))
  expect_match(result, "problem 2", fixed = TRUE)
})

test_that("read_and_validate_yaml returns parsed data for a valid file", {
  schema_file <- system.file(
    "extdata/schema/config.schema.json",
    package = "algorithm.viewer"
  )
  skip_if(schema_file == "", "Config schema not available")

  yaml_file <- withr::local_tempfile(fileext = ".yaml")
  writeLines(
    c(
      "allow_file_uploads: true",
      "algorithms:",
      "  alg1:",
      "    title: Algorithm One",
      "    file: alg1/alg1.yaml"
    ),
    yaml_file
  )

  data <- algorithm.viewer:::read_and_validate_yaml(yaml_file, schema_file)
  expect_true(data$allow_file_uploads)
  expect_equal(data$algorithms$alg1$title, "Algorithm One")
})

test_that("read_and_validate_yaml throws yaml_validation_error for bad types", {
  schema_file <- system.file(
    "extdata/schema/config.schema.json",
    package = "algorithm.viewer"
  )
  skip_if(schema_file == "", "Config schema not available")

  yaml_file <- withr::local_tempfile(fileext = ".yaml")
  # allow_file_uploads must be a boolean
  writeLines("allow_file_uploads: not-a-boolean", yaml_file)

  expect_error(
    algorithm.viewer:::read_and_validate_yaml(yaml_file, schema_file),
    class = "yaml_validation_error"
  )
})

test_that("read_and_validate_yaml rejects unknown top-level keys", {
  schema_file <- system.file(
    "extdata/schema/config.schema.json",
    package = "algorithm.viewer"
  )
  skip_if(schema_file == "", "Config schema not available")

  yaml_file <- withr::local_tempfile(fileext = ".yaml")
  writeLines("not_a_real_config_key: true", yaml_file)

  expect_error(
    algorithm.viewer:::read_and_validate_yaml(yaml_file, schema_file),
    class = "yaml_validation_error"
  )
})
