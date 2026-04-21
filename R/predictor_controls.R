#' Predictor Controls Shiny Module
#'
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
#'   \item{rv_values}{A \code{shiny::reactiveVal()} containing the current
#'     predictor values as a named list keyed by variable name.
#'     Call `predictor_ctrl$rv_values()` to read values reactively, or wrap
#'     in \code{shiny::isolate()} when a non-reactive snapshot is needed.}
#'   \item{destroy_module}{A function that destroys the module's
#'     observers and UI elements via. Call this before recreating the
#'     module to avoid orphaned observers.}
#' }
#'
#' To reactively respond to changes in the predictor react to `$rv_values()`
#' in the named list returned by `predictorControlsServer()`
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
#' # In a Shiny app's server function, after loading model definitions:
#' model_data <- model_definitions$models[["female"]]
#'
#' # Create the UI (can be returned for a renderUI call or inserted via
#' # shiny::insertUI)
#' ui <- predictorControlsUI("predictor_ctrl_female", model_data)
#'
#' # Create the server and get back a list with rv_values and
#' # destroy_module
#' predictor_ctrl <- predictorControlsServer(
#'   "predictor_ctrl_female",
#'   model_data
#' )
#'
#' # Access the current predictor values reactively
#' current_values <- predictor_ctrl$rv_values()
#'
#' # Destroy the module when no longer needed
#' predictor_ctrl$destroy_module()
#' }
#' @name predictor_controls
#' @noRd
#' @keywords internal
NULL

# The ID of the generated HTML element that contains the predictor controls
.predictor_controls_container_id <- "predictor_controls_container"

#' Create Predictor Controls UI
#'
#' Builds the UI for a single model's predictor controls. Generates
#' radio buttons for categorical variables and sliders for continuous
#' variables, along with a sticky header showing the model title and a
#' sticky footer with a reset button.
#'
#' @param id Character string. The module namespace ID.
#' @param model_data Named list of model data containing reference group
#'   values, variable metadata, and model display properties.
#' @param initial_predictor_values Named list of initial predictor values keyed
#'   by variable name. If \code{NULL}, the reference group from
#'   \code{model_data$reference_group} is used.
#' @param model_name Character string. Display name shown in the sticky
#'   header and reset button label. If \code{NULL}, the model's title from
#'   \code{model_data$title} is used.
#' @param show_model_color Logical. If \code{TRUE} (the default), a colored
#'   left border and color swatch are added to the panel using the model's
#'   assigned color.
#'
#' @return A \code{shiny::tagList()} containing the predictor controls input
#'   controls.
#'
#' @noRd
#' @keywords internal
predictorControlsUI <- function(
  id,
  model_data,
  initial_predictor_values = NULL,
  model_name = NULL,
  show_model_color = TRUE
) {
  ns <- shiny::NS(id)

  # Create the UI by saving them in predictor_controls_input
  predictor_controls_input <- shiny::tagList()

  left_pad <- ifelse(show_model_color, "10px", "0")

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
  model_heading <- shiny::h4(
    model_heading,
    style = paste(
      "position: sticky; top: 0; background-color: #fff;",
      "margin: 0; z-index: 10;",
      glue::glue("padding: 10px 0 8px {left_pad};")
    )
  )
  predictor_controls_input[[length(predictor_controls_input) + 1]] <-
    model_heading

  if (is.null(initial_predictor_values)) {
    initial_predictor_values <- model_data$reference_group
  }
  # Create a UI control for each variable in initial_predictor_values
  # The controls are added to the items list
  items <- list()
  for (variable in names(initial_predictor_values)) {
    predictor_value <- initial_predictor_values[[variable]]
    label <- get_variable_info(model_data, variable, "label")
    variable_allowable_values <- get_predictor_allowable_values(
      model_data, variable
    )
    input_id <- ns(variable)

    if (is_variable_categorical(model_data, variable)) {
      # For categorical variables, add a radioButtons

      # Get the labels for all allowable values of the variable
      labels <- get_variable_label_from_value(
        model_data,
        variable,
        as.vector(variable_allowable_values)
      )

      # Get the selected label (corresponding to predictor_value)
      selected <- get_variable_label_from_value(
        model_data,
        variable,
        predictor_value
      )
      if (!(selected %in% labels)) {
        labels_str <- paste0("'", labels, "'", collapse = ", ")
        warning(glue::glue(
          "Initial value '{selected}' is not a valid value for ",
          "variable {variable}. Must be one of {labels_str}."
        ))
        selected <- labels[[1]]
      }

      input_control <- shiny::radioButtons(
        inputId = input_id,
        label = label,
        choices = labels,
        selected = selected
      )
    } else {
      # For continuous variables, add a sliderInput

      # Calculate the min, max and step information
      min_range <- min(variable_allowable_values)
      max_range <- max(variable_allowable_values)

      is_integer_range <- is.integer(min_range) &&
        is.integer(max_range) &&
        all(min_range:max_range == sort(variable_allowable_values))

      # For integer ranges, the step is 1, otherwise the
      # step is the difference between the first two values
      if (is_integer_range) {
        step <- 1
      } else {
        step <- signif(
          variable_allowable_values[2] - variable_allowable_values[1],
          5
        )
      }

      input_control <- shiny::sliderInput(
        inputId = input_id,
        label = label,
        min = min_range,
        max = max_range,
        value = predictor_value,
        step = step
      )
    }

    items[[length(items) + 1]] <-
      input_control
  }

  predictor_controls_input[[length(predictor_controls_input) + 1]] <-
    shiny::div(
      style = paste(
        "width: 100%;",
        "overflow-x: hidden;",
        glue::glue("padding: 0 12px 0 {left_pad};")
      ),
      items
    )

  # Create the reset button for the model
  reset_button <- shiny::actionButton(
    ns("reset_button"),
    label = glue::glue("Reset {model_name}"),
    icon = shiny::icon("arrow-rotate-left")
  )
  # Put the reset button in a sticky footer
  reset_button <- shiny::div(
    style = paste(
      "position: sticky; bottom: 0; background-color: #fff;",
      "margin: 0; z-index: 9; overflow-x: hidden; width: 100%;",
      glue::glue("padding: 10px 0 7px {left_pad};")
    ),
    reset_button
  )
  predictor_controls_input[[length(predictor_controls_input) + 1]] <-
    reset_button

  # Ceate the style for the div containing the controls.
  # If show_model_color is TRUE then we add a left margin with the model color.
  model_color <- unname(get_model_colors(list(model_data)))[[1]]
  style <- paste(
    "width: 100%;",
    ifelse(show_model_color,
      glue::glue("border-left: solid 6px {model_color}; "),
      ""
    )
  )

  # Create the div containing the controls
  predictor_controls_input <- shiny::div(
    style = style,
    id = ns(.predictor_controls_container_id),
    predictor_controls_input
  )

  shiny::tagList(predictor_controls_input)
}

#' Predictor Controls Server Logic (Internal)
#'
#' Implements the server-side logic for a single model's predictor controls
#' module. Creates observers for each input control, tracks current values
#' reactively, handles reset-to-defaults, and provides a destroy function
#' for clean teardown.
#'
#' @param id Character string. The module namespace ID (must match the ID
#'   used in \code{predictorControlsUI()}).
#' @param model_data Named list of model data containing reference group
#'   values, variable metadata, and model display properties.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{destroy_module}{Function. Call to destroy the module, its
#'       observers, and its UI elements.}
#'     \item{rv_values}{A \code{shiny::reactiveVal()} containing the current
#'       predictor values as a named list.}
#'   }
#'
#' @noRd
#' @keywords internal
predictorControlsServer <- function(
  id,
  model_data
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
      # Saves the updated value to internal state whenever this predictor's
      # input changes.
      observers[[length(observers) + 1]] <-
        shiny::observeEvent(
          input[[input_id]],
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
    observers[[length(observers) + 1]] <-
      shiny::observeEvent(
        input$reset_button,
        {
          predictor_values_internal(default_predictor_values)
          set_ui_from_values()
        },
        event.env = cur_env,
        handler.env = cur_env,
        ignoreInit = TRUE
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

    #' Destroy the Module, its UI Elements, and its Observers.
    #'
    #' This should be called whenever the module is no longer required.
    #' This function is returned in the server's returned named list
    #' under the name \code{"destroy_module"}.
    #'
    #' @return NULL
    destroy_module <- function() {
      # Destroy all observers
      for (obs in observers) {
        obs$destroy()
      }
      observers <- list()

      # Remove UI
      shiny::removeUI(
        selector = paste0("#", session$ns(.predictor_controls_container_id)),
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
