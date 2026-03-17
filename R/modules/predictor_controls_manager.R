#' Predictor Controls Manager
#'
#' @description
#' Provides a thin management layer around the [predictorControls] Shiny module.
#' An environment created by [initialize_predictor_controls_env()] tracks all
#' active predictor-controls instances and a monotonically increasing index that
#' guarantees unique module IDs even after controls are destroyed and recreated.
#'
#' The typical lifecycle is:
#' \enumerate{
#'   \item Call [initialize_predictor_controls_env()] once (e.g. in the Shiny
#'         server function) to obtain a shared environment.
#'   \item Call [create_predictor_controls()] for each model whose predictors
#'         you want to expose.  Insert the returned \code{$ui} element into the
#'         UI and the \code{$server} element is already registered.
#'   \item Read current predictor values at any time with
#'         [get_predictor_controls_values()].
#'   \item When you need to replace all controls (e.g. after a model reload),
#'         call [destroy_all_predictor_controls()] and then recreate as needed.
#' }
#'
#' @examples
#' \dontrun{
#' ## ----- Shiny server snippet -----
#' library(shiny)
#'
#' server <- function(input, output, session) {
#'
#'   # 1. Create the shared environment once
#'   pc_env <- initialize_predictor_controls_env()
#'
#'   # 2. Suppose model_data is a list describing one loaded model
#'   #    It should have additional members describing the model
#'   #    predictors (not shown here).
#'   model_data <- list(model_id = "htn_model", title = "Hypertension Model")
#'
#'   # 3. Create controls (returns list with $ui and $server)
#'   ctrls <- create_predictor_controls(
#'     pc_env,
#'     model_data     = model_data,
#'     extra_tag      = "primary",
#'     change_trigger = reactive(input$refresh),
#'     model_name     = "Hypertension Risk"
#'   )
#'
#'   # Insert ctrls$ui into the UI (e.g. via renderUI / insertUI)
#'   output$predictor_ui <- renderUI(ctrls$ui)
#'
#'   # 4. Read predictor values (isolated by default)
#'   observeEvent(input$calculate, {
#'     vals <- get_predictor_controls_values(pc_env, model_data,
#'                                           extra_tag = "primary")
#'     print(vals)
#'   })
#'
#'   # 5. Tear down all controls before loading new models
#'   observeEvent(input$reload_models, {
#'     destroy_all_predictor_controls(pc_env)
#'   })
#' }
#' }
#'
#' @name predictor_controls_manager
NULL

source("R/modules/predictor_controls.R")

#' Initialize a predictor controls environment
#'
#' Creates a new rlang environment used to track active predictor controls
#' server modules and a monotonically increasing index that ensures unique
#' module IDs across successive calls to [destroy_all_predictor_controls()].
#'
#' @return An rlang environment with fields:
#'   \describe{
#'     \item{predictor_controls}{Named list of active predictor controls server modules.}
#'     \item{predictor_controls_index}{Integer incremented each time all controls are destroyed.}
#'   }
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

#' Create a predictor controls module for a model
#'
#' Instantiates the UI and server for a [predictorControls] Shiny module,
#' registers the server in the environment, and returns both components.
#'
#' @param .env Environment created by [initialize_predictor_controls_env()].
#' @param model_data List of model metadata used to derive the module ID and
#'   populate the controls.
#' @param extra_tag Optional character string appended to the module ID to
#'   distinguish multiple controls for the same model.
#' @param change_trigger Optional reactive expression passed to the server
#'   module that fires when predictor values should be refreshed.
#' @param model_name Optional display name for the model shown in the UI. If
#'   NULL then the model's title (in \code{model_data}) is used.
#' @param show_model_color Logical; whether to display the model colour swatch
#'   in the UI. Defaults to \code{TRUE}.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{ui}{The UI element returned by \code{predictorControlsUI()}.}
#'     \item{server}{The server module returned by \code{predictorControlsServer()}.}
#'   }
create_predictor_controls <- function(
  .env,
  model_data,
  extra_tag = NULL,
  change_trigger = NULL,
  model_name = NULL,
  show_model_color = TRUE
) {
  id <- get_model_predictor_controls_id(.env, model_data, extra_tag = extra_tag)
  ui <- predictorControlsUI(id, model_data, model_name = model_name, show_model_color = show_model_color)
  server <- predictorControlsServer(id, model_data, change_trigger)

  .env$predictor_controls[[id]] <- server

  list(
    ui = ui,
    server = server
  )
}

#' Build the module ID for a model's predictor controls
#'
#' Combines the model ID, the current environment index, and an optional extra
#' tag to produce a unique Shiny module namespace string.
#'
#' @param .env Environment created by [initialize_predictor_controls_env()].
#' @param model_data List containing at least a \code{model_id} field.
#' @param extra_tag Optional character string appended to further disambiguate
#'   the ID when multiple controls exist for the same model.
#'
#' @return A character string used as the Shiny module ID.
get_model_predictor_controls_id <- function(.env, model_data, extra_tag = NULL) {
  id <- paste0(model_data$model_id, "___", .env$predictor_controls_index)
  if (!is.null(extra_tag)) {
    id <- paste0(id, "___", extra_tag)
  }
  id
}

#' Destroy all active predictor controls
#'
#' Calls each active module's \code{destroy_module()} method (if present),
#' clears the controls list, and increments the index so that any subsequently
#' created controls receive fresh, non-colliding IDs.
#'
#' @param .env Environment created by [initialize_predictor_controls_env()].
#'
#' @return \code{NULL}, invisibly. Called for side effects.
destroy_all_predictor_controls <- function(.env) {
  for (predictor_ctrl in .env$predictor_controls) {
    if (!is.null(predictor_ctrl$destroy_module)) {
      predictor_ctrl$destroy_module()
    }
  }
  .env$predictor_controls <- list()
  .env$predictor_controls_index <- .env$predictor_controls_index + 1
}

#' Retrieve the current predictor values for a model
#'
#' Looks up the active predictor controls server module for the given model and
#' returns its reactive values, optionally wrapped in [shiny::isolate()] to
#' avoid creating reactive dependencies.
#'
#' @param .env Environment created by [initialize_predictor_controls_env()].
#' @param model_data List containing at least a \code{model_id} field.
#' @param extra_tag Optional character string used to locate the correct module
#'   when multiple controls exist for the same model. This is the extra_tag
#'   parameter passed to [get_model_predictor_controls_id].
#' @param rv_isolate Logical; if \code{TRUE} (the default) the reactive values
#'   are returned inside [shiny::isolate()], preventing the caller from taking
#'   a reactive dependency on them.
#'
#' @return The list of predictor values produced by the module's
#'   \code{rv_values()} reactive.
get_predictor_controls_values <- function(
  .env,
  model_data,
  extra_tag = NULL,
  rv_isolate = TRUE
) {
  id <- get_model_predictor_controls_id(.env, model_data, extra_tag = extra_tag)
  predictor_ctrl <- .env$predictor_controls[[id]]
  if (rv_isolate) {
    isolate(predictor_ctrl$rv_values())
  } else {
    predictor_ctrl$rv_values()
  }
}
