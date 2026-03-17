#' @title Predictor Controls Shiny Module
#'
#' @description
#' A Shiny module that creates and manages predictor controls for a
#' single model. The UI component renders input controls (radio buttons for
#' categorical variables, sliders for continuous variables) that allow users
#' to adjust predictor values. The server component tracks value
#' changes, supports resetting to defaults, and exposes reactive values
#' for consumption by other parts of the application.
#'
#' @details
#' `predictorControlsServer()` returns a named list with two elements:
#' \describe{
#'   \item{rv_values}{A [shiny::reactiveVal()] containing the current
#'     predictor values as a named list keyed by variable name.
#'     Call `predictor_ctrl$rv_values()` to read values reactively, or wrap
#'     in [shiny::isolate()] when a non-reactive snapshot is needed.}
#'   \item{destroy_module}{A function that destroys the module's
#'     observers and UI elements via. Call this before recreating the
#'     module to avoid orphaned observers.}
#' }
#'
#' There are two ways to reactively respond to changes in the predictor
#' values:
#' \itemize{
#'   \item Pass in a reactiveVal as the change_trigger parameter to
#'         `predictorControlsServer()`. This integer reactive value will be
#'         incremented whenever the predictor values change.
#'   \item React to `$rv_values()` in the named list returned by
#'         `predictorControlsServer()`
#' }
#'
#' It is best to always use a unique ID whenever destroying and creating
#' predictor controls. While duplicate IDs will work (as long as the
#' previous predictor controls are first destroyed with `$destroy_module()`)
#' there might be redundant reactive changes to the predictor values
#' caused by deleting and destroying input controls with the same
#' duplicate IDs.
#'
#' @examples
#' \dontrun{
#' source("R/modules/predictor_controls.R")
#'
#' # In a Shiny app's server function, after loading model definitions:
#' model_data <- session$userData$model_definitions$models[["female"]]
#' redraw_trigger <- shiny::reactiveVal(0)
#'
#' # Create the UI (typically inserted via shiny::insertUI)
#' ui <- predictorControlsUI("predictor_ctrl_female", model_data)
#'
#' # Create the server and get back a list with rv_values and
#' # destroy_module
#' predictor_ctrl <- predictorControlsServer(
#'   "predictor_ctrl_female",
#'   model_data,
#'   change_trigger = redraw_trigger
#' )
#'
#' # Access the current predictor values reactively
#' current_values <- predictor_ctrl$rv_values()
#'
#' # Destroy the module when no longer needed
#' predictor_ctrl$destroy_module()
#' }
#' @name predictor_controls
NULL

# The ID of the generated HTML element that contains the predictor controls
.predictor_controls_container_id <- "predictor_controls_container"

#' Create Predictor Controls UI (Internal)
#'
#' Builds the UI for a single model's predictor controls. Generates
#' radio buttons for categorical variables and sliders for continuous
#' variables, along with a sticky header showing the model title and a
#' sticky footer with a reset button. The entire panel is styled with a
#' colored left border matching the model color.
#'
#' @param id Character string. The module namespace ID.
#' @param model_data List. Model data containing reference group values,
#'   variable metadata, and model display properties.
#'
#' @return A [shiny::tagList()] containing the predictor controls input controls.
#'
#' @keywords internal
.predictorControlsUI_internal <- function(id, model_data, model_name = NULL, show_model_color = TRUE) {
  ns <- shiny::NS(id)

  # Create the UI by saving them in predictor_controls_input
  predictor_controls_input <- tagList()

  model_id <- model_data$model_id

  # Add heading
  if (is.null(model_name)) {
    model_name <- model_data$title
  }
  model_heading <- cleanup_string(model_name)
  if (show_model_color) {
    model_heading <- shiny::HTML(add_model_color(
      model_data,
      cleanup_string(model_name),
      "20px",
      "20px",
      after = FALSE
    ))
  }
  model_heading <- h4(
    model_heading,
    style = paste(
      "position: sticky; top: 0; background-color: #fff;",
      "padding: 10px 0 8px 0; margin: 0; z-index: 10"
    )
  )
  predictor_controls_input[[length(predictor_controls_input) + 1]] <- model_heading

  # Create a UI control for each variable in the reference group
  for (variable in names(model_data$reference_group)) {
    reference_value <- model_data$reference_group[[variable]]
    label <- get_variable_info(model_data, variable, "label")
    variable_range <- get_predictor_range(model_data, variable)
    input_id <- ns(variable)

    if (is_variable_categorical(model_data, variable)) {
      # For categorical variables, add a radioButtons

      # Get the labels for the full variable range
      labels <- get_variable_label_from_value(
        model_data,
        variable,
        as.vector(variable_range)
      )

      # Get the selected label (corresponding to reference_value)
      selected <- get_variable_label_from_value(
        model_data,
        variable,
        reference_value
      )
      if (!(selected %in% labels)) {
        labels_str <- paste0("'", labels, "'", collapse = ", ")
        warning(glue::glue(
          "Reference group value '{selected}' is not a valid value for ",
          "variable {variable}. Must be one of {labels_str}."
        ))
        selected <- labels[[1]]
      }

      input_control <- radioButtons(
        inputId = input_id,
        label = label,
        choices = labels,
        selected = selected
      )
    } else {
      # For continuous variables, add a sliderInput

      # Calculate the range and step information
      min_range <- min(variable_range)
      max_range <- max(variable_range)

      is_integer_range <- is.integer(min_range) &&
        is.integer(max_range) &&
        all(min_range:max_range == sort(variable_range))

      # For integer ranges, the step is 1, otherwise the
      # step is the difference between the first two values
      # in the range
      if (is_integer_range) {
        step <- 1
      } else {
        step <- signif(variable_range[2] - variable_range[1], 5)
      }

      input_control <- shiny::sliderInput(
        inputId = input_id,
        label = label,
        min = min_range,
        max = max_range,
        value = reference_value,
        step = step
      )
    }

    predictor_controls_input[[length(predictor_controls_input) + 1]] <- input_control
  }

  # Create the reset button for the model
  reset_button <- shiny::actionButton(
    ns("reset_button"),
    label = glue::glue("Reset {model_name}"),
    icon = icon("arrow-rotate-left")
  )
  # Put the reset button in a sticky footer
  reset_button <- shiny::div(
    style = paste(
      "position: sticky; bottom: 0; background-color: #fff;",
      "padding: 10px 0 7px 0; margin: 0; z-index: 9"
    ),
    reset_button
  )
  predictor_controls_input[[length(predictor_controls_input) + 1]] <- reset_button

  # Add a colored left margin that matches the model color
  model_color <- unname(get_model_colors(list(model_data)))[[1]]
  style <- glue::glue(
    "width: 100%; ",
    if (show_model_color) "border-left: solid 6px {model_color}; " else "",
    "padding: 0 10px 0 10px;"
  )
  predictor_controls_input <- shiny::div(
    style = style,
    id = ns(.predictor_controls_container_id),
    predictor_controls_input
  )

  tagList(predictor_controls_input)
}

#' Predictor Controls Server Logic (Internal)
#'
#' Implements the server-side logic for a single model's predictor controls
#' module. Creates observers for each input control, tracks current values
#' reactively, handles reset-to-defaults, and provides a destroy function
#' for clean teardown.
#'
#' @param id Character string. The module namespace ID (must match the ID
#'   used in [.predictorControlsUI_internal()]).
#' @param model_data List. Model data containing reference group values,
#'   variable metadata, and model display properties.
#' @param change_trigger A [shiny::reactiveVal()] that is incremented
#'   whenever any predictor value changes. This allows external code
#'   to react to changes. Default is `NULL` (no external trigger).
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{destroy_module}{Function. Call to destroy the module, its
#'       observers, and its UI elements.}
#'     \item{rv_values}{A [shiny::reactiveVal()] containing the current
#'       predictor values as a named list.}
#'   }
#'
#' @keywords internal
.predictorControlsServer_internal <- function(
  id,
  model_data,
  change_trigger = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    # List of all observers for the predictor controls
    # We need these so we can destroy them later
    observers <- list()

    default_predictor_values <- model_data$reference_group
    # Convert all categorical variables to strings
    for (variable in names(default_predictor_values)) {
      if (is_variable_categorical(model_data, variable)) {
        default_predictor_values[[variable]] <- as.character(
          default_predictor_values[[variable]]
        )
      }
    }

    # The current predictor values (matching what we see in the UI)
    predictor_values_internal <-
      shiny::reactiveVal(default_predictor_values)
    
    # Create all observers for the predictor controls
    for (variable in names(default_predictor_values)) {
      input_id <- variable
      cur_env <- rlang::env(
        input_id = input_id,
        variable = variable
      )
      observers[[length(observers) + 1]] <- observeEvent(input[[input_id]],
        {
          save_values_from_ui(c(variable))
        },
        event.env = cur_env,
        handler.env = cur_env,
        ignoreInit = TRUE
      )
    }

    # Create the observer for the reset button
    cur_env <- rlang::env(
      model_data = model_data,
      model_id = model_data$model_id
    )
    observers[[length(observers) + 1]] <- observeEvent(input$reset_button,
      {
        predictor_values_internal(default_predictor_values)
        set_ui_from_values()
      },
      event.env = cur_env,
      handler.env = cur_env
    )

    #' Save the Values in the UI to the Internal Predictor Values.
    #'
    #' Retrieves current predictor values from UI input controls.
    #'
    #' @param variables A list of variables to save from the UI. Since
    #'   accessing the UI can be slow (especially in Shinylive) we
    #'   should only save the UI values that have changed. If NULL
    #'   then all variables are saved from the UI. Default is NULL.
    #'
    #' @return NULL
    #'
    #' @keywords internal
    save_values_from_ui <- function(variables = NULL) {
      saved_predictor_values <- predictor_values_internal()

      # Gather the variables to save the values for
      if (is.null(variables)) {
        variables <- names(default_predictor_values)
      } else {
        variables <- intersect(variables, names(default_predictor_values))
      }

      # Save each variable in variables
      for (variable in variables) {
        ui_id <- variable
        val <- input[[ui_id]]

        if (is_variable_categorical(model_data, variable)) {
          # For categorical variables, convert the UI label to the
          # actual value in the model (eg. convert "Yes" to 2)
          val <- get_variable_value_from_label(model_data, variable, val)
        }

        saved_predictor_values[[variable]] <- val
      }

      predictor_values_internal(saved_predictor_values)
    }

    #' Repopulate Predictor Controls With Currently Saved Values
    #'
    #' Repopulates the existing predictor controls with the last
    #' saved internal values. This will not destroy or create controls,
    #' but instead update their values.
    #'
    #' @return NULL (called for side effects on UI).
    #'
    #' @keywords internal
    set_ui_from_values <- function() {
      cur_predictor_values <- predictor_values_internal()

      for (variable in names(cur_predictor_values)) {
        val <- cur_predictor_values[[variable]]
        ui_id <- variable

        if (is_variable_categorical(model_data, variable)) {
          # For categorical variables, convert the value to a label
          val <- get_variable_label_from_value(model_data, variable, val)
          val <- as.character(val)
          shiny::updateRadioButtons(session, ui_id, selected = val)
        } else {
          shiny::updateSliderInput(session, ui_id, value = val)
        }
      }
    }

    observe({
      predictor_values_internal()
      if (!is.null(change_trigger)) {
        shiny::isolate(change_trigger(change_trigger() + 1))
      }
    })

    #' Destroy the Module, its UI Elements, and its Observers.
    #'
    #' This should be called whenever the module is no longer required.
    #' This function is returned in the server's returned named list
    #' (under the name "rv_values"), so that users can call this function.
    #'
    #' @return NULL
    #'
    #' @keywords internal
    destroy_module <- function() {
      # Destroy all observers
      for (obs in observers) {
        obs$destroy()
      }
      observers <- list()

      # Remove UI
      removeUI(
        selector = paste0("#", shiny::NS(id, .predictor_controls_container_id)),
        immediate = TRUE
      )

      # Destroy the module (if using shiny.destroy)
      # nolint start
      # shiny.destroy::destroyModule(id)
      # nolint end
    }

    list(
      destroy_module = destroy_module,
      rv_values = predictor_values_internal
    )
  })
}

predictorControlsServer <- .predictorControlsServer_internal
predictorControlsUI <- .predictorControlsUI_internal
