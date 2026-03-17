source("R/modules/predictor_controls.R")

initialize_predictor_controls_env <- function() {
  rlang::env(
    # All predictor controls server modules. These allow us to retrieve the
    # predictor values
    predictor_controls = list(),
    # Every time we call destroy_all_predictor_controls, we increment this number.
    # The number gets appended to the predictor controls IDs, and ensures that
    # we never use the same ID twice.
    predictor_controls_index = 0
  )
}

create_predictor_controls <- function(.env, model_data, extra_tag = NULL, change_trigger = NULL, model_name = NULL, show_model_color = TRUE) {
  id <- get_model_predictor_controls_id(.env, model_data, extra_tag = extra_tag)
  ui <- predictorControlsUI(id, model_data, model_name = model_name, show_model_color = show_model_color)
  server <- predictorControlsServer(id, model_data, change_trigger)

  .env$predictor_controls[[id]] <- server

  list(
    ui = ui,
    server = server
  )
}

get_model_predictor_controls_id <- function(.env, model_data, extra_tag = NULL) {
  id <- paste0(model_data$model_id, "___", .env$predictor_controls_index)
  if (!is.null(extra_tag)) {
    id <- paste0(id, "___", extra_tag)
  }
  id
}

destroy_all_predictor_controls <- function(.env) {
  for (predictor_ctrl in .env$predictor_controls) {
    if (!is.null(predictor_ctrl$destroy_module)) {
      predictor_ctrl$destroy_module()
    }
  }
  .env$predictor_controls <- list()
  .env$predictor_controls_index <- .env$predictor_controls_index + 1
}

get_predictor_controls_values <- function(.env, model_data, extra_tag = NULL, rv_isolate = TRUE) {
  id <- get_model_predictor_controls_id(.env, model_data, extra_tag = extra_tag)
  predictor_ctrl <- .env$predictor_controls[[id]]
  if (rv_isolate) {
    isolate(predictor_ctrl$rv_values())
  } else {
    predictor_ctrl$rv_values()
  }
}
