# Tests for the predictor controls module (mod_predictor_controls.R).
#
# .normalize_model_data accepts either a single model data object or an unnamed
# list of them, and always returns an unnamed list.

test_that(".normalize_model_data wraps a single named model", {
  md <- make_synthetic_model_data()
  result <- algorithm.viewer:::.normalize_model_data(md)
  expect_length(result, 1)
  expect_identical(result[[1]], md)
})

test_that(".normalize_model_data passes through an unnamed list", {
  models <- list(make_synthetic_model_data(), make_synthetic_model_data())
  result <- algorithm.viewer:::.normalize_model_data(models)
  expect_identical(result, models)
})
