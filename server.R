source("R/model_definitions/model_definitions.R")

# Remove scientific notation from plots
options(scipen = 8)
# Maximum upload size in bytes
options(shiny.maxRequestSize = 30 * 1024^2)

# Load all curve calculation source files
curve_files <- list.files(path = "R/curves", pattern = "^curve-.*\\.R$")
lapply(file.path("R/curves", curve_files), source)

# ID and name for the empty predictor (eg. to specify nothing in the
# "Interaction Predictor" selector)
empty_predictor <- "<empty>"

# Prefix used for the reference group controls and the reference group reset
# button. These are also used to destroy previous observeEvents when
# repopulating the reference gorup controls/buttons (we destroy them by
# matching the IDs with a regex starting with these values)
reference_group_control_id_prefix <- "ref_control__"
reference_group_reset_id_prefix <- "refreset__"

#' Create a Plot Consisting of a Single String Message
#'
#' Generates a minimal plotly plot displaying a centered text message.
#' Used to show error or status messages in place of a data visualization.
#'
#' @param label Character string. The message text to display in the plot.
#' @param color Character string. The color of the message text. Default is
#'   "black".
#'
#' @return A plotly object containing an empty plot with centered text.
#'
#' @examples
#' .make_message_plot("Please select at least one model")
#' .make_message_plot("Error loading data", color = "red")
.make_message_plot <- function(label, color = "black") {
  label <- label |>
    cli::ansi_strip() |>
    stringr::str_wrap(width = 50)

  df <- data.frame(label = label)
  p <- ggplot2::ggplot() +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(label = label),
      x = 0.5,
      y = 0.5,
      color = color
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.line = ggplot2::element_blank())

  plotly::ggplotly(p, tooltip = NULL) |>
    plotly::style(hoverinfo = "none") |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    )
}

#' Build Aesthetic Mapping
#'
#' Combines base aesthetic arguments with additional mappings into a single
#' aes() call for ggplot2.
#'
#' @param aes_args List of aesthetic mappings to include.
#' @param ... Additional aesthetic mappings to append.
#'
#' @return A ggplot2 aesthetic mapping object.
#'
#' @keywords internal
.make_aes <- function(aes_args, ...) {
  # Append ... to aes_args, then past as params to aes function
  aes_args <- c(aes_args, list(...))
  do.call(ggplot2::aes, aes_args)
}

#' Shiny Server Function
#'
#' Main server logic for the Algorithm Viewer Shiny application. Handles
#' reactive UI updates, model selection, and odds ratio curve visualization.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return NULL (called for side effects).
#'
#' @export
server <- function(input, output, session) {
  # React to redraw_trigger when the plots need to be redrawn
  redraw_trigger <- reactiveVal(0)
  # React to reload_trigger when something needs to be updated due to a loading
  # of an algorithm file.
  reload_trigger <- reactiveVal(0)
  # React to initial_load_trigger to respond to the very first load
  initial_load_trigger <- reactiveVal(0)

  # A named list of all dynamic observers. These are created by observeEvent,
  # and can be created and destroyed later if their corresponding UI element
  # gets destroyed (R Shiny doesn't do this automatically). The names
  # are the IDs of the UI element that the observeEvent is for.
  # Dynamic observers are only used for the controls in the Reference tab.
  # In the future these should be removed from the main server function and
  # moved into a separate R Shiny module for reference group controls.
  all_dynamic_observers <- list()

  #' Destroy a Dynamic Observer
  #'
  #' Removes and destroys a previously registered dynamic observer by its
  #' ID. If no observer exists with the given ID, this is a no-op.
  #'
  #' @param observe_id Character string identifying the observer to destroy.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  destroy_dynamic_observer <- function(observe_id) {
    if (observe_id %in% names(all_dynamic_observers)) {
      all_dynamic_observers[[observe_id]]$event$destroy()
      all_dynamic_observers[[observe_id]] <<- NULL
    }
  }

  #' Destroy Dynamic Observers by Regex
  #'
  #' Removes and destroys all previously registered dynamic observers whose
  #' IDs match the given regular expression pattern.
  #'
  #' @param observe_id_regex Character string containing a regular expression
  #'   pattern to match against observer IDs.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  destroy_dynamic_observer_regex <- function(observe_id_regex) {
    for (cur_id in names(all_dynamic_observers)) {
      if (grepl(observe_id_regex, cur_id)) {
        destroy_dynamic_observer(cur_id)
      }
    }
  }

  #' Register a Dynamic Observer
  #'
  #' Registers a new dynamic observer, destroying any existing observer
  #' with the same ID first. Stores the observer event and associated UI
  #' item for later cleanup.
  #'
  #' @param observe_id Character string identifying the observer.
  #' @param observe_item The UI input control associated with this observer.
  #' @param observe_event The Shiny observer event object to register.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  add_dynamic_observer <- function(observe_id, observe_item, observe_event) {
    destroy_dynamic_observer(observe_id)
    all_dynamic_observers[[observe_id]] <<- list(
      event = observe_event,
      item = observe_item
    )
  }

  #' Generate Reference Group Input ID
  #'
  #' Creates a unique HTML input ID for a reference group variable slider.
  #'
  #' @param model_id Character string specifying the model identifier.
  #' @param variable Character string specifying the variable name.
  #'
  #' @return Character string with the formatted input ID.
  #'
  #' @keywords internal
  get_refgroup_input_id <- function(model_id, variable, index) {
    glue::glue("{reference_group_control_id_prefix}{model_id}_{variable}")
  }

  #' Generate Reset Button ID
  #'
  #' Creates a unique HTML ID for a reference group reset button.
  #'
  #' @param model_id Character string specifying the model identifier.
  #'
  #' @return Character string with the formatted button ID.
  #'
  #' @keywords internal
  get_refgroup_reset_button_id <- function(model_id, index) {
    glue::glue("{reference_group_reset_id_prefix}{model_id}")
  }


  #' Get And Save Last Reference Group Values from UI
  #'
  #' Retrieves current reference group values from UI slider controls for
  #' a specific model and caches them in the model data as 
  #' $last_reference_group.
  #'
  #' @param model_id Character string specifying the model identifier.
  #'
  #' @return Named list of reference group values, or NULL if no model
  #'   definitions are loaded.
  #'
  #' @keywords internal
  save_last_reference_group_from_ui <- function(model_id, variables = NULL) {
    if (is.null(session$userData$model_definitions)) {
      return()
    }

    reference_group <- session$userData$model_definitions$
      models[[model_id]]$last_reference_group
    if (is.null(reference_group)) {
      reference_group <- list()
    }
    model_data <- session$userData$model_definitions$models[[model_id]]

    # Gather the variables to save the values for
    if (is.null(variables)) {
      variables <- names(model_data$reference_group)
    } else {
      variables <- intersect(variables, names(model_data$reference_group))
    }

    for (variable in variables) {
      ui_id <- get_refgroup_input_id(model_id, variable)
      val <- input[[ui_id]]

      if (is.null(val)) {
        val <- model_data$reference_group[[variable]]
      } else {
        if (is_variable_categorical(model_data, variable)) {
          val <- get_variable_value_from_label(model_data, variable, val)
        }
      }

      reference_group[[variable]] <- val
    }

    session$userData$model_definitions$
      models[[model_id]]$last_reference_group <- reference_group

    reference_group
  }

  #' Get Currently Selected Models
  #'
  #' @return Named list of model data objects for the selected models,
  #'   or an empty list if no model definitions are loaded.
  #'
  #' @keywords internal
  selected_models <- function() {
    if (is.null(session$userData$model_definitions)) {
      list()
    } else {
      model_defs <- session$userData$model_definitions
      models <- model_defs$
        models[as.vector(session$userData$selected_model_ids)]
      models <- models[!is.na(names(models))]
      models
    }
  }

  #' Create the Reference Group UI Controls
  #'
  #' Rebuilds the reference group input controls in the UI for all loaded
  #' models. Removes existing controls and creates new controls for each
  #' predictor variable, with appropriate input types for categorical vs
  #' continuous variables.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  create_refgroup_controls <- function() {
    destroy_dynamic_observer_regex(
      glue::glue("^{stringr::str_escape(reference_group_control_id_prefix)}.+$")
    )
    destroy_dynamic_observer_regex(
      glue::glue("^{stringr::str_escape(reference_group_reset_id_prefix)}.+$")
    )

    refgroups_container_id <- "#refgroups"
    removeUI(
      selector = paste0(refgroups_container_id, " > *"),
      immediate = TRUE
    )

    # Create the reference group inputs, save them in reference_group_input
    reference_group_input <- list()

    append_ui <- function(lst, item) {
      lst[[length(lst) + 1]] <- item
      lst
    }

    reference_group_input <- append_ui(reference_group_input, br())

    # Go through all models and create the reference group controls
    last_model_id <- tail(names(session$userData$model_definitions$models), 1)
    for (model_data in session$userData$model_definitions$models) {
      current_model_input <- list()

      model_id <- model_data$model_id

      # Add heading
      model_heading <- h4(
        shiny::HTML(add_model_color(
          model_data,
          cleanup_string(model_data$title),
          "20px",
          "20px",
          after = FALSE
        )),
        style = paste(
          "position: sticky; top: 0; background-color: #fff;",
          "padding: 10px 0 8px 0; margin: 0; z-index: 10"
        )
      )
      current_model_input <- append_ui(current_model_input, model_heading)

      # Get the reference group values for the model.
      # If last_reference_group is set for the model, then use those
      # values instead.
      reference_group <- get_last_refgroup_values(model_data)

      # For each value in the reference group, create a slider
      for (variable in names(reference_group)) {
        reference_value <- reference_group[[variable]]
        label <- get_variable_info(model_data, variable, "label")
        variable_range <- get_predictor_range(model_data, variable)
        input_id <- get_refgroup_input_id(model_id, variable)

        if (is_variable_categorical(model_data, variable)) {
          # For categorical variables, add a sliderTextInput
          # Get the labels for the full variable range
          labels <- get_variable_label_from_value(
            model_data,
            variable,
            as.vector(variable_range)
          )

          # Get the selected label
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

          if (is_integer_range) {
            step <- 1
          } else {
            step <- signif(variable_range[2] - variable_range[1], 5)
          }

          input_control <- sliderInput(
            inputId = input_id,
            label = label,
            min = min_range,
            max = max_range,
            value = reference_value,
            step = step
          )
        }

        # Call save_last_reference_group_from_ui any time a control changes
        # This will save the last set reference group values. We also
        # redraw with redraw_trigger when the reference group changes.
        cur_env <- rlang::env(
          model_id = model_id,
          input_id = input_id,
          variable = variable
        )
        add_dynamic_observer(
          input_id,
          input_control,
          observeEvent(
            input[[input_id]],
            {
              save_last_reference_group_from_ui(model_id, c(variable))
              redraw_trigger(redraw_trigger() + 1)
            },
            event.env = cur_env,
            handler.env = cur_env,
            ignoreInit = TRUE
          )
        )

        current_model_input <- append_ui(current_model_input, input_control)
      }

      # Add reset button for the model
      reset_button_id <- get_refgroup_reset_button_id(model_id)

      reset_button <- actionButton(
        reset_button_id,
        label = glue::glue("Reset {model_data$title}"),
        icon = icon("arrow-rotate-left")
      )
      reset_button <- div(
        style = paste(
          "position: sticky; bottom: 0; background-color: #fff;",
          "padding: 10px 0 7px 0; margin: 0; z-index: 9"
        ),
        reset_button
      )

      current_model_input <- append_ui(current_model_input, reset_button)

      cur_env <- rlang::env(model_id = model_id, reset_button_id = reset_button_id)
      add_dynamic_observer(
        reset_button_id,
        reset_button,
        observeEvent(
          input[[reset_button_id]],
          {
            session$userData$model_definitions$models[[model_id]]$
              last_reference_group <- NULL
            set_refgroup_control_values(model_id)
            redraw_trigger(redraw_trigger() + 1)
          },
          handler.env = cur_env,
          event.env = cur_env,
          ignoreInit = TRUE
        )
      )

      # Add a colored left margin that matches the model color
      model_color <- unname(get_model_colors(list(model_data)))[[1]]
      style <- glue::glue(
        "width: 100%; ",
        "border-left: solid 6px {model_color}; ",
        "padding: 0 10px 0 10px;"
      )
      current_model_input <- list(div(style = style, current_model_input))

      # Add an hr between each group
      if (model_id != last_model_id) {
        current_model_input <- append_ui(current_model_input, hr())
      }

      reference_group_input <- append_ui(reference_group_input,
                                         current_model_input)
    }

    insertUI(
      selector = refgroups_container_id,
      where = "afterBegin",
      ui = div(reference_group_input),
      immediate = TRUE
    )
  }

  #' Repopulate Reference Group Controls With Last Saved Values
  #'
  #' Repopulates the existing reference group controls with the last
  #' saved values for a specific model. This will not destroy or create
  #' controls, but instead update their values.
  #'
  #' @param model_id Character string specifying the model identifier to
  #'   repopulate the reference group controls for.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  set_refgroup_control_values <- function(model_id) {
    if (is.null(session$userData$model_definitions)) {
      return()
    }

    model_data <- session$userData$model_definitions$models[[model_id]]
    reference_group <- get_last_refgroup_values(model_data)

    for (variable in names(reference_group)) {
      val <- reference_group[[variable]]
      ui_id <- get_refgroup_input_id(model_id, variable)

      if (is_variable_categorical(model_data, variable)) {
        val <- get_variable_label_from_value(model_data, variable, val)
        updateRadioButtons(session, ui_id, selected = val)
      } else {
        updateSliderInput(session, ui_id, value = val)
      }
    }
  }

  #' Update Predictor Dropdown Choices
  #'
  #' Populates the predictor selectInput with available predictor variables
  #' from the currently selected models.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  update_predictor_choices <- function() {
    if (is.null(session$userData$model_definitions)) {
      updateSelectInput(
        session,
        "predictor",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    models <- selected_models()

    # Create list of all possible choices (from all models)
    predictor_choices <- gather_predictor_choices(models)

    if (length(models) > 0) {
      selected <- predictor_choices[[names(predictor_choices)[[1]]]]
    } else {
      selected <- character(0)
    }

    session$userData$predictor <- selected
    updateSelectInput(
      session,
      "predictor",
      choices = predictor_choices,
      selected = selected
    )
  }

  #' Update Interaction Predictor Dropdown Choices
  #'
  #' Populates the interaction predictor selectInput with available predictor
  #' variables from the currently selected models, including an empty option.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  update_interaction_predictor_choices <- function() {
    if (is.null(session$userData$model_definitions)) {
      updateSelectInput(
        session,
        "interaction_predictor",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    models <- selected_models()

    # Create list of all possible choices (from all models)
    predictor_choices <- gather_predictor_choices(models)

    # If at least one model is selected, then add the empty predictor
    if (length(models) > 0) {
      new_list <- list()
      new_list[[empty_predictor]] <- empty_predictor
      predictor_choices <- c(new_list, predictor_choices)
    }

    if (length(models) > 0) {
      selected <- empty_predictor
    } else {
      selected <- character(0)
    }

    session$userData$interaction_predictor <- selected
    updateSelectInput(
      session,
      "interaction_predictor",
      choices = predictor_choices,
      selected = selected
    )
  }

  #' Create Plotly Visualization
  #'
  #' Generates a plotly plot from curve data for multiple models. Handles
  #' both categorical (bar chart) and continuous (line chart) predictor
  #' types, with optional logarithmic scaling.
  #'
  #' @param all_curve_data List of curve data objects from calculate_or_curve
  #'   or similar functions. Each object should contain df, aes_args,
  #'   x_axis_label, y_axis_label, and x_axis_type fields.
  #'
  #' @return A plotly object for rendering in the UI.
  #'
  #' @keywords internal
  make_plot <- function(all_curve_data) {
    # If no models are selected then tell the user to select one
    if (is.null(session$userData$model_definitions)) {
      msg <- "No algorithm loaded.<br />Please upload some data."
      return(.make_message_plot(msg))
    } else if (is.null(all_curve_data) || length(all_curve_data) == 0) {
      return(.make_message_plot("Please select at least one model."))
    }

    # Combine all data frames
    df <- all_curve_data |>
      lapply(function(x) x$df) |>
      dplyr::bind_rows()

    # Factoring by Model will force the legend to be listed in
    # the order that they appear in df$Model (which should match the
    # order in the model definitions file). If we did not do this
    # then the legend would be sorted alphabetically.
    df$Model <- factor(df$Model, levels = unique(df$Model))

    curve_data <- all_curve_data[[length(all_curve_data)]]
    logarithmic <- session$userData$logarithmic

    tryCatch(
      {
        # log10 vs identity transform
        transform <- ifelse(logarithmic, "log10", "identity")

        ylabel <- ifelse(
          logarithmic,
          glue::glue("{curve_data$y_axis_label} (Logarithmic)"),
          curve_data$y_axis_label
        )

        # Create plot
        if (curve_data$x_axis_type == "Categorical") {
          # Maintain the order of the x-axis categories
          df[[curve_data$x_axis_label]] <-
            factor(
              df[[curve_data$x_axis_label]],
              levels = unique(df[[curve_data$x_axis_label]])
            )

          model_colors <- get_model_colors(
            session$userData$model_definitions$models
          )
          p <- ggplot2::ggplot(
            data = df,
            .make_aes(curve_data$aes_args, fill = dplyr::sym("Model"))
          ) +
            ggplot2::geom_col(position = "dodge") +
            ggplot2::scale_fill_manual(values = model_colors, aesthetics = "fill")
        } else {
          model_colors <- get_model_colors(
            session$userData$model_definitions$models
          )
          p <- ggplot2::ggplot(
            data = df,
            .make_aes(curve_data$aes_args, color = dplyr::sym("Model"))
          ) +
            ggplot2::geom_line(linewidth = 1.2) +
            ggplot2::scale_color_manual(values = model_colors, aesthetics = "color")
        }

        y_limits <- NULL
        if (!is.null(curve_data$ylim) && !logarithmic) {
          y_limits <- curve_data$ylim
        }

        p <- p +
          ggplot2::scale_y_continuous(transform = transform, limits = y_limits) +
          ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
          ggplot2::labs(
            title = curve_data$title,
            subtitle = curve_data$title,
            x = curve_data$x_axis_label,
            y = ylabel
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            legend.position = "right",
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 12),
            axis.title = ggplot2::element_text(size = 11)
          )

        plotly::ggplotly(p) |>
          plotly::layout(hovermode = "x unified")
      },
      error = function(e) {
        .make_message_plot(
          paste("Error making plot:", e$message),
          color = "red"
        )
      }
    )
  }

  # Handle change in "Logarithmic" checkbox item
  observeEvent(input$logarithmic, {
    session$userData$logarithmic <- input$logarithmic
    redraw_trigger(redraw_trigger() + 1)
  })

  # Handle change in "Predictor" drop down box
  observeEvent(input$predictor, {
    session$userData$predictor <- input$predictor
    redraw_trigger(redraw_trigger() + 1)
  })

  # Handle change in "Interaction Predictor" drop down box
  observeEvent(input$interaction_predictor, {
    session$userData$interaction_predictor <- input$interaction_predictor
    redraw_trigger(redraw_trigger() + 1)
  })

  # Predicted Risk plot
  output$pr_plot <- plotly::renderPlotly({
    redraw_trigger()

    req(session$userData$predictor)

    if (is.null(session$userData$model_definitions)) {
      return(make_plot(NULL))
    }

    tryCatch(
      {
        predictor <- session$userData$predictor
        all_curve_data <- list()

        # Go through all models and calculate the OR curves
        # We concatenate them (with bind_rows) to show one curve per model
        for (model_data in selected_models()) {
          # Get predictor type (Categorical or Continuous)
          predictor_type <- model_data$variables |>
            dplyr::filter(variable == predictor) |>
            dplyr::pull(variableType)

          reference_group <- get_last_refgroup_values(model_data)

          tic <- Sys.time()

          # Calculate the OR curve for the model
          curve_data <- calculate_pr_curve(
            predictor,
            model_data,
            reference_group = reference_group
          )

          elapsed <- Sys.time() - tic
          message(paste0(
            "Elapsed time for PR curve ", model_data$model_id, ": ", elapsed
          ))

          all_curve_data[[length(all_curve_data) + 1]] <- curve_data
        }

        make_plot(all_curve_data)
      },
      error = function(e) {
        .make_message_plot(
          glue::glue("<b>Error</b>: {e$message}"),
          color = "red"
        )
      }
    )
  })

  # Odss Ratios plots
  output$or_plot <- plotly::renderPlotly({
    redraw_trigger()

    req(session$userData$predictor)

    if (is.null(session$userData$model_definitions)) {
      return(make_plot(NULL))
    }

    tryCatch(
      {
        all_curve_data <- list()
        predictor <- session$userData$predictor
        interaction_predictor <- session$userData$interaction_predictor

        # Go through all models and calculate the OR curves
        # We concatenate them (with bind_rows) to show one curve per model
        for (model_data in selected_models()) {
          # Get predictor type (Categorical or Continuous)
          predictor_type <- model_data$variables |>
            dplyr::filter(variable == predictor) |>
            dplyr::pull(variableType)

          reference_group <- get_last_refgroup_values(model_data)

          tic <- Sys.time()

          # Calculate the OR curve for the model
          if (interaction_predictor == empty_predictor) {
            curve_data <- calculate_or_curve(
              predictor,
              model_data,
              reference_group = reference_group
            )
          } else {
            curve_data <- calculate_or_curve_interaction(
              predictor,
              interaction_predictor,
              model_data,
              reference_group = reference_group
            )
          }

          elapsed <- Sys.time() - tic
          message(paste0(
            "Elapsed time for OR curve ", model_data$model_id, ": ", elapsed
          ))

          all_curve_data[[length(all_curve_data) + 1]] <- curve_data
        }

        make_plot(all_curve_data)
      },
      error = function(e) {
        .make_message_plot(
          glue::glue("<b>Error</b>: {e$message}"),
          color = "red"
        )
      }
    )
  })

  #' Load Model Definitions from File
  #'
  #' Reads model definitions from a YAML file and stores them in the session
  #' user data.
  #'
  #' @param file Character string specifying the path to the YAML file.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  load_model_definitions <- function(file) {
    session$userData$selected_model_ids <- NULL
    tryCatch(
      {
        session$userData$model_definitions <- read_model_definitions(file)
      },
      error = function(e) {
        session$userData$model_definitions <- NULL
        message(paste("Error loading model definition:", e$message))
      }
    )
  }

  #' Recreate All UI Components and data and trigger a reload all.
  #'
  #' Refreshes all model-dependent UI components including model selections,
  #' reference group controls, and predictor dropdowns. Once everything is
  #' created we trigger downstream reloading and redrawing.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  recreate_and_trigger_reload <- function() {
    session$userData$selected_model_ids <- NULL
    update_model_selections()
    create_refgroup_controls()
    update_predictor_choices()
    update_interaction_predictor_choices()
    reload_trigger(reload_trigger() + 1)
    redraw_trigger(redraw_trigger() + 1)
  }

  #' Process Uploaded Data File
  #'
  #' Handles uploaded algorithm data files. Supports direct YAML files or
  #' archive formats (ZIP, TAR) containing a YAML configuration file.
  #' Displays error modals for invalid or corrupt files.
  #'
  #' @param file Character string specifying the path to the uploaded file.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  process_data_file <- function(file) {
    if (grepl("^(yaml|yml)$", tools::file_ext(file), ignore.case = TRUE)) {
      load_model_definitions(file)
    } else {
      temp_dir_path <- tempdir()

      session$userData$model_definitions <- NULL
      archive_success <- FALSE
      config_files <- c()

      tryCatch(
        {
          files_in_archive <- archive::archive_extract(file, dir = temp_dir_path)
          yaml_pattern <- "(\\.yaml|\\.yml)$"
          config_files <- files_in_archive[
            grepl(yaml_pattern, files_in_archive, ignore.case = TRUE)
          ]
          # Ignore config files in the __MACOSX directory
          # (added automatically on a Mac)
          config_files <- config_files[!grepl("__MACOSX", config_files)]
          archive_success <- TRUE
        },
        error = function(e) {
          warning(glue::glue("Error loading file: {e$message}"))
        }
      )

      if (!archive_success) {
        err_msg <- paste0(
          "Could not extract the contents of the uploaded file, ",
          "it may be corrupt or in an unsupported format. ",
          "No models were loaded."
        )
        showModal(errorModal("Error Loading Data", err_msg))
      } else if (length(config_files) == 0) {
        err_msg <- paste0(
          "A YAML configuration file was not found in your archive. ",
          "No models were loaded."
        )
        showModal(errorModal("Error Loading Data", err_msg))
      } else if (length(config_files) > 1) {
        # The names of selections are the full path to the config file within
        # the temporary directory. The values of selections are the values
        # shown to the user in the dialog box (the values can be anything).
        escaped_files <- htmltools::htmlEscape(config_files)
        selections <- setNames(
          file.path(temp_dir_path, config_files),
          escaped_files
        )
        message <- tagList(
          paste0(
            "Multiple YAML model definitions were found in your archive, ",
            "please select the one to load:"
          ),
          br(),
          br(),
          radioButtons(
            inputId = "select_yaml_radio",
            label = NULL,
            choices = selections,
            selected = unname(selections)[[1]]
          )
        )

        showModal(yesNoModal(
          "Select Model Definitions File",
          message,
          "select_yaml_ok",
          show_no = FALSE
        ))
      } else {
        config_file <- file.path(temp_dir_path, config_files[[1]])
        load_model_definitions(config_file)
      }
    }

    recreate_and_trigger_reload()
  }

  # Handle file that has been uploaded
  observeEvent(input$upload, {
    if (!config$allow_file_uploads) {
      return()
    }

    if (!is.null(input$upload)) {
      file <- input$upload$datapath
      process_data_file(file)
    }
  }, ignoreInit = TRUE)

  # Show a message to the user at the top of the "Models" tab
  output$model_message <- renderUI({
    reload_trigger()
    if (is.null(session$userData$model_definitions)) {
      msg <- paste0(
        "No algorithm has been loaded. Click the \"Browse\" button below ",
        "to upload your data as a ZIP file or other archive."
      )
      div(msg, style = "color: #ff0000;", br(), br())
    }
  })

  # Populate the main title of the page
  output$ui_title <- renderUI({
    reload_trigger()

    if (!is.null(session$userData$model_definitions)) {
      meta <- session$userData$model_definitions$meta
      glue::glue("{meta$algorithm} v{meta$version} Algorithm Viewer")
    } else {
      "Algorithm Viewer"
    }
  })

  # Handle initial loading of the page
  # (load the initial algorithm file if there is one)
  observe({
    if (initial_load_trigger() > 0) {
      return()
    }
    if (!is.null(config$initial_algorithm_file)) {
      process_data_file(config$initial_algorithm_file)
    }
    initial_load_trigger(initial_load_trigger() + 1)
  })

  # Redraw when the selected Models changes.
  # The checkbox group returns NULL if nothing is selected, so we must
  # set ignoreNULL = FALSE to make sure that we redraw when no model
  # is selected
  observeEvent(input$model_id, {
    session$userData$selected_model_ids <- input$model_id
    redraw_trigger(redraw_trigger() + 1)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  #' Update Model Selection Checkboxes
  #'
  #' Populates the model selection checkbox group with available models
  #' from the loaded model definitions. Selects all models by default
  #' and triggers a reload of dependent UI components.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  update_model_selections <- function() {
    model_defs <- session$userData$model_definitions
    if (!is.null(model_defs) && length(model_defs$models) > 0) {
      selected <- unname(get_model_choices(model_defs$models))
      choice_names <- get_model_titles(
        model_defs$models,
        include_model_colors = TRUE,
        escape_html = TRUE
      )
      choice_values <- get_model_ids(model_defs$models)
    } else {
      # No model definitions loaded, so show an empty checkbox group
      selected <- character(0)
      choice_names <- character(0)
      choice_values <- character(0)
    }

    updateCheckboxGroupInput(
      session,
      "model_id",
      label = "Models:",
      selected = selected,
      choiceNames = choice_names,
      choiceValues = choice_values
    )
    session$userData$selected_model_ids <- selected
  }

  #' Create Error Modal Dialog
  #'
  #' Creates a modal dialog for displaying error messages to the user.
  #'
  #' @param title Character string specifying the modal title.
  #' @param message Character string or HTML content for the modal body.
  #'
  #' @return A modalDialog object for use with showModal().
  #'
  #' @keywords internal
  errorModal <- function(title, message) {
    modalDialog(
      title = title,
      message,
      easyClose = TRUE,
      footer = tagList(
        modalButton("Dismiss")
      )
    )
  }

  #' Create Yes/No Modal Dialog
  #'
  #' Creates a modal dialog with OK and optional Cancel buttons for
  #' user confirmation prompts.
  #'
  #' @param title Character string specifying the modal title.
  #' @param message Character string or HTML content for the modal body.
  #' @param ok_button_id Character string specifying the input ID for the
  #'   OK button, used to observe click events.
  #' @param show_no Logical indicating whether to show the Cancel button.
  #'   Default is TRUE.
  #' @param size Character string specifying the modal size. One of "s",
  #'   "m", "l", or "xl". Default is "m".
  #'
  #' @return A modalDialog object for use with showModal().
  #'
  #' @keywords internal
  yesNoModal <- function(title,
                         message,
                         ok_button_id,
                         show_no = TRUE,
                         size = "m") {
    modalDialog(
      title = title,
      message,
      footer = tagList(
        if (show_no) modalButton("Cancel"),
        actionButton(ok_button_id, "OK")
      ),
      size = size
    )
  }

  # OK pressed for dialog box asking to choose which YAML model definitions
  # file to load when multiple YAML files found in an uploaded archive
  observeEvent(input$select_yaml_ok, {
    selected <- input$select_yaml_radio
    if (!is.null(selected) && selected != "") {
      load_model_definitions(selected)
      recreate_and_trigger_reload()
    }
    removeModal()
  })

  # Help tab
  output$help <- renderUI({
    htmltools::includeMarkdown("data/help/main.md")
  })
}

server
