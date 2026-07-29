#' Predictor Grouped Controls Shiny Module
#'
#' A Shiny module that renders per-model predictor controls, grouping one set
#' of \code{predictorControls} for every model in \code{model_definitions}.
#' The server returns a \code{shiny::reactiveValues()} object keyed by
#' \code{model_id} so callers can observe changes to any model's predictor
#' values in a single reactive expression.
#'
#' @details
#' Observe changes across **all** models by reacting to the full list:
#' \preformatted{
#'   observe({
#'     all_values <- shiny::reactiveValuesToList(predictor_values)
#'     # all_values is a named list: model_id -> named predictor-value list
#'   })
#' }
#'
#' To watch a single model, observe the named element directly:
#' \preformatted{
#'   observe({ predictor_values[["female"]] })
#' }
#'
#' @examples
#' \dontrun{
#' # In a Shiny app:
#' ui <- shiny::fluidPage(
#'   predictorGroupedControlsUI("group_ctrl")
#' )
#'
#' server <- function(input, output, session) {
#'   model_defs <- shiny::reactive({ load_model_definitions(zip_path) })
#'
#'   predictor_values <- predictorGroupedControlsServer(
#'     "group_ctrl",
#'     model_definitions = model_defs
#'   )
#'
#'   # React whenever any model's predictor values change
#'   shiny::observe({
#'     vals <- shiny::reactiveValuesToList(predictor_values)
#'     message(
#'       "Updated predictor groups: ",
#'       paste(names(vals), collapse = ", ")
#'     )
#'   })
#' }
#' }
#' @name mod_predictor_grouped_controls
#' @noRd
#' @keywords internal
NULL

#' Predictor Grouped Controls UI
#'
#' Creates a scrollable container that hosts all per-model predictor control
#' widgets rendered by \code{predictorGroupedControlsServer()}.
#'
#' @param id Character string. The module namespace ID; must match the
#'   \code{id} passed to \code{predictorGroupedControlsServer()}.
#'
#' @return A \code{shiny::div} UI element.
#'
#' @noRd
#' @keywords internal
predictorGroupedControlsUI <- function(
  id
) {
  ns <- shiny::NS(id)

  shiny::div(
    style = paste(
    "max-height: calc(100vh - 140px); margin-bottom: 30px;",
    "overflow-y: scroll"
    ),
    shiny::uiOutput(ns("group_controls"))
  )
}

#' Predictor Grouped Controls Server
#'
#' Initialises predictor control modules for each model and keeps the
#' returned \code{predictor_values} reactive values in sync with user input.
#' When \code{model_definitions} changes (e.g. a new ZIP is loaded) all
#' existing predictor controls are destroyed and rebuilt, preserving the
#' current UI values as the new initial values where possible.
#'
#' @param id Character string. The module namespace ID; must match the
#'   \code{id} passed to \code{predictorGroupedControlsUI()}.
#' @param model_definitions A zero-argument \code{shiny::reactive()} that
#'   returns the model definitions (eg. the output of
#'   \code{load_model_definitions()}. Must contain a \code{$models} named list
#'   where each element is a model data list with at least \code{model_id} and
#'   \code{reference_group}.
#' @param mode Character string. Controls how predictor controls are laid out.
#'   \code{"split"} (default) renders one \code{predictorControls} widget per
#'   model. \code{"compact"} renders a single widget that covers all models
#'   simultaneously (a more side-by-side layout).
#' @param show_model_color Logical. If \code{TRUE} (default), a colored left
#'   border is added to single-model predictor control widgets.
#'
#' @return A \code{shiny::reactiveValues()} object keyed by \code{model_id}.
#'   Each value is a named list of the current predictor values for that
#'   model. Observe \code{shiny::reactiveValuesToList(rv)} to react to
#'   changes across all models simultaneously.
#'
#' @noRd
#' @keywords internal
predictorGroupedControlsServer <- function(
  id,
  model_definitions,
  mode = "split",
  show_model_color = TRUE
) {
  shiny::moduleServer(id, function(input, output, session) {
    # List of all objects to destroy whenever we recreate the UI (ie. the
    # predictorControlsServer and observers)
    destroyable_objects <- rlang::env(objects = list())

    # Stores the predictor values keyed by model ID.
    # These match the values that are shown in the grouped controls UI.
    # It is returned by this function so that callers can react to changes.
    predictor_values <- shiny::reactiveValues()

    # Handle new model definitions: Destroy any old objects/observers/servers
    # and initialize the predictor_values for each model to the reference group.
    shiny::observe({
      # Clear values from any previously loaded algorithm before setting the new
      # ones. reactiveValues offers no key deletion, so a stale model ID keeps
      # its name with a NULL value; reading it (predictor_values[[id]]) then
      # returns NULL, exactly as if it were absent. Every consumer indexes by the
      # currently selected model IDs, so these emptied keys never surface — the
      # NULL assignment is the correct and sufficient way to retire them.
      for (key in names(predictor_values)) {
        predictor_values[[key]] <- NULL
      }

      # Initialize predictor_values from each model's reference_group,
      # coercing categorical values to character and continuous to double to
      # match the format used by predictorControlsServer.
      for (model_data in model_definitions()$models) {
        vals <- model_data$reference_group
        for (v in names(vals)) {
          if (is_variable_categorical(model_data, v)) {
            vals[[v]] <- as.character(vals[[v]])
          } else {
            vals[[v]] <- as.double(vals[[v]])
          }
        }
        predictor_values[[model_data$model_id]] <- vals
      }
    }, priority = 10000)

    # Populate UI for the "group_controls" predictor grouped controls
    output$group_controls <- shiny::renderUI({
      # Destroy all existing observers and servers
      for (obj in destroyable_objects$objects) obj$destroy()
      destroyable_objects$objects <- list()

      models <- model_definitions()$models
      last_model_id <- utils::tail(names(models), 1)
      group_controls_ui <- shiny::tagList()

      for (model_data in models) {
        model_id <- model_data$model_id
        if (mode == "split") {
          # Split mode, nothing to do here. We add separate
          # predictorControlsUI/Servers per model.
          use_models <- model_data
        } else if (mode == "compact") {
          # In compact mode, we create a single predictorControlsUI/Server
          # that displays the controls for all models simultaneously.
          # We break out of this loop at the end so that we only pass in
          # all models once.
          use_models <- unname(models)
        } else {
          stop(htmltools::htmlEscape(paste0(
            "Unrecognized mode: '",
            mode,
            "'"
          )))
        }

        ui_id <- uuid::UUIDgenerate()
        predictor_ui <- predictorControlsUI(
          session$ns(ui_id),
          use_models,
          model_name = model_data$title,
          show_model_color = show_model_color
        )
        predictor_server <- predictorControlsServer(ui_id, use_models)
        n <- length(destroyable_objects$objects) + 1
        destroyable_objects$objects[[n]] <- predictor_server

        local({
          # rv_values() returns list(model_id -> list(variable -> value)).
          # Spread each model_id entry into predictor_values.
          rv_values <- predictor_server$rv_values
          n <- length(destroyable_objects$objects) + 1
          destroyable_objects$objects[[n]] <-
            shiny::observe(
              {
                vals <- rv_values()
                for (gk in names(vals)) {
                  predictor_values[[gk]] <- vals[[gk]]
                }
              }
            )
        })

        group_controls_ui[[length(group_controls_ui) + 1]] <-
          predictor_ui

        if (mode == "compact") {
          # For compact mode, we added all the models to a single
          # predictorControlsServer/predictorControlsUI
          break()
        }

        if (model_id != last_model_id) {
          group_controls_ui[[length(group_controls_ui) + 1]] <- shiny::hr()
        }
      }

      group_controls_ui
    })

    # Return predictor_values so callers can respond to changes.
    # To respond to a change in any key of predictor_values (keyed by model_id),
    # the caller should observe reactiveValuesToList(predictor_values)
    predictor_values
  })
}
