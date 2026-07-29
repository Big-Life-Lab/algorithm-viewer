# Synthetic (pipeline-free) fixtures and small test utilities shared across
# test files. Unlike .test_md (helper-fixtures.R) these never touch
# model.parameters.pipeline, so tests using them run even when the real
# model fixtures are unavailable.

# A minimal model_data list with one continuous predictor (age), one
# categorical predictor (smoking), and one non-predictor variable.
make_synthetic_model_data <- function() {
  list(
    model_id = "m1",
    title = "Model One",
    model_color = "#112233",
    variables = data.frame(
      variable = c("age", "smoking", "outcome"),
      label = c("Age", "Smoking status", "Outcome"),
      units = c("years", "n/a", "n/a"),
      variableType = c("Continuous", "Categorical", "Continuous"),
      role = c("Predictor", "Predictor", "Outcome"),
      stringsAsFactors = FALSE
    ),
    variable_details = data.frame(
      variable = c("smoking", "smoking", "smoking", "age"),
      recStart = c("1", "2", "9", "[20, 100]"),
      recEnd = c("1", "2", "NA::b", "copy"),
      catLabel = c("Never", "Current", "Missing", ""),
      stringsAsFactors = FALSE
    ),
    predictor_allowable_values = list(
      age = seq(20, 100),
      smoking = c("1", "2")
    ),
    reference_group = list(age = 40, smoking = "1")
  )
}

# Temporarily replace the contents of the global .CONFIG environment for the
# duration of the calling test, restoring the previous contents afterwards.
local_config <- function(values, env = parent.frame()) {
  config_env <- algorithm.viewer:::.CONFIG
  old <- as.list(config_env, all.names = TRUE)
  rm(list = ls(config_env, all.names = TRUE), envir = config_env)
  list2env(values, config_env)
  withr::defer(
    {
      rm(list = ls(config_env, all.names = TRUE), envir = config_env)
      list2env(old, config_env)
    },
    envir = env
  )
}

# A fake Shiny session sufficient for the utils_url functions:
# shiny::getQueryString() reads session$clientData$url_search and
# shiny::updateQueryString() calls session$updateQueryString(qs, mode).
make_fake_url_session <- function(query = "") {
  e <- new.env(parent = emptyenv())
  e$clientData <- list(url_search = query)
  e$updates <- list()
  e$updateQueryString <- function(queryString, mode) {
    e$updates[[length(e$updates) + 1]] <- list(qs = queryString, mode = mode)
  }
  e
}

# Pick the first predictor of the given variableType from a real fixture
# model that also appears in the model's reference_group.
fixture_predictor <- function(model_data, type) {
  vars <- model_data$variables
  candidates <- vars$variable[
    vars$role == "Predictor" & vars$variableType == type
  ]
  candidates <- candidates[candidates %in% names(model_data$reference_group)]
  if (length(candidates) == 0) {
    return(NULL)
  }
  candidates[[1]]
}
