# Tests for package_versions_ui (utils_meta.R).

test_that("package_versions_ui shows the Algorithm Viewer version", {
  ui <- algorithm.viewer:::package_versions_ui(algorithm_viewer_only = TRUE)
  html <- as.character(ui)
  expected <- paste0(
    "Algorithm Viewer v", utils::packageVersion("algorithm.viewer")
  )
  expect_match(html, expected, fixed = TRUE)
  expect_false(grepl("Model Parameters Pipeline", html, fixed = TRUE))
})

test_that("package_versions_ui includes the pipeline version by default", {
  skip_if_not_installed("model.parameters.pipeline")
  ui <- algorithm.viewer:::package_versions_ui()
  html <- as.character(ui)
  expected <- paste0(
    "Model Parameters Pipeline v",
    utils::packageVersion("model.parameters.pipeline")
  )
  expect_match(html, expected, fixed = TRUE)
})

test_that("package_versions_ui applies the requested color", {
  ui <- algorithm.viewer:::package_versions_ui(
    algorithm_viewer_only = TRUE, color = "#abcdef"
  )
  expect_match(as.character(ui), "color: #abcdef", fixed = TRUE)
})
