library(dplyr)
library(rlang)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)
library(cli)

# Load all curve calculation source files
curve_files <- list.files(path = "R/curves", pattern = "^curve-.*\\.R$")
lapply(file.path("R/curves", curve_files), source)

# Remove scientific notation from plots
options(scipen = 8)

# ID and name for the empty predictor (eg. to specify nothing in the
# "Interaction Predictor" selector)
empty_predictor <- "<empty>"

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
.get_refgroup_input_id <- function(model_id, variable) {
  glue::glue("ref_{model_id}_{variable}")
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
.get_refgroup_reset_button_id <- function(model_id) {
  glue::glue("refreset_{model_id}")
}

#' Gather Predictor Choices
#'
#' Get all the possible predictor choices for the specified models.
#'
#' @param models List of model data objects.
#'
#' @return Named list where names are predictor labels and values are predictor
#'   variable names (e.g., list(Diabetes = "diabx")).
.gather_predictor_choices <- function(models) {
  predictor_choices <- list()

  for (model_data in models) {
    # Get predictors from variables table (role == "Predictor")
    predictors <- model_data$variables |>
      filter(role == "Predictor") |>
      select(variable, label)

    # Create named vector for selectInput
    cur_predictor_choices <- setNames(predictors$variable, predictors$label)
    predictor_choices <- c(predictor_choices, cur_predictor_choices)
  }

  predictor_choices <- predictor_choices[unique(names(predictor_choices))]
  predictor_choices
}

#' Create a Plot Consisting of a Single String Message
#'
#' Generates a minimal plotly plot displaying a centered text message.
#' Used to show error or status messages in place of a data visualization.
#'
#' @param label Character string. The message text to display in the plot.
#' @param color Character string. The color of the message text. Default is
#'  "black".
#'
#' @return A plotly object containing an empty plot with centered text.
#'
#' @examples
#' .make_message_plot("Please select at least one model")
#' .make_message_plot("Error loading data", color = "red")
.make_message_plot <- function(label, color = "black") {
  label <- label |>
    cli::ansi_strip() |>
    stringr::str_wrap(width = 40)

  df <- data.frame(label = label)
  p <- ggplot() +
    geom_text(data = df, aes(label = label), x = 0.5, y = 0.5, color = color) +
    theme_void()

  ggplotly(p, tooltip = NULL) |>
    style(hoverinfo = "none")
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
  aes_args <- c(aes_args, list(...))
  do.call(aes, aes_args)
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
  .get_refgroup_values_from_ui <- function(model_id) {
    reference_group <- list()
    model_data <- model_definitions$models[[model_id]]

    for (variable in names(model_data$reference_group)) {
      ui_id <- .get_refgroup_input_id(model_id, variable)
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

    model_definitions$models[[model_id]]$last_reference_group <<-
      reference_group

    reference_group
  }

  #' Repopulate the existing reference group controls with the last values.
  #'
  #' @param model_id Character string specifying the model identifier to
  #'  repopulate the reference group controls for.
  .set_refgroup_control_values <- function(model_id) {
    model_data <- model_definitions$models[[model_id]]
    reference_group <- get_last_refgroup_values(model_data)

    for (variable in names(reference_group)) {
      val <- reference_group[[variable]]
      ui_id <- .get_refgroup_input_id(model_id, variable)

      if (is_variable_categorical(model_data, variable)) {
        val <- get_variable_label_from_value(model_data, variable, val)
        updateSliderTextInput(session, ui_id, selected = val)
      } else {
        updateSliderInput(session, ui_id, value = val)
      }
    }
  }

  # Reactive expression to get currently selected models, based on the
  # input$model_id checkboxGroupInput
  selected_models <- reactive({
    model_definitions$models[as.vector(input$model_id)]
  })

  # HTML for specifying the reference groups
  output$refgroups <- renderUI({
    # Create the reference group inputs, save them in reference_group_input
    reference_group_input <- list()

    # Add a UI element to reference_group_input
    append_ui <- function(item) {
      reference_group_input[[length(reference_group_input) + 1]] <- item
      reference_group_input
    }

    reference_group_input <- append_ui(br())

    # Go through all models and create the reference group controls
    for (model_data in model_definitions$models) {
      model_id <- model_data$model_id

      # Add a line (hr) between each model controls
      if (length(reference_group_input) > 1) {
        reference_group_input <- append_ui(hr())
      }

      # Add heading
      model_heading <- h4(shiny::HTML(add_model_color(
        model_data,
        glue::glue("Model: {model_data$title}"),
        "20px",
        "20px",
        after = FALSE
      )))
      reference_group_input <- append_ui(model_heading)

      # Get the reference group values for the model.
      # If last_reference_group is set for the model, then use those
      # values instead.
      reference_group <- get_last_refgroup_values(model_data)

      # For each value in the reference group, create a slider
      for (variable in names(reference_group)) {
        reference_value <- reference_group[[variable]]
        label <- get_variable_info(model_data, variable, "label")
        variable_range <- get_predictor_range(model_data, variable)
        input_id <- .get_refgroup_input_id(model_id, variable)

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

          slider <- sliderTextInput(
            inputId = input_id,
            label = label,
            choices = labels,
            grid = TRUE,
            selected = selected
          )
        } else {
          # For continuous variables, add a sliderInput
          # Calculate the range and step information
          min_range <- min(variable_range)
          max_range <- max(variable_range)

          if (is.integer(min_range) &&
                is.integer(max_range) &&
                all(min_range:max_range == sort(variable_range))) {
            step <- 1
          } else {
            step <- signif(variable_range[2] - variable_range[1], 5)
          }

          slider <- sliderInput(
            inputId = input_id,
            label = label,
            min = min_range,
            max = max_range,
            value = reference_value,
            step = step
          )
        }

        # Call .get_refgroup_values_from_ui any time a slider changes
        # This will save the last set reference group values
        # (ie. last_reference_group at
        # model_definitions$models[[model_id]]$last_reference_group)
        cur_env <- env(model_id = model_id, input_id = input_id)

        observeEvent(
          input[[input_id]],
          {
            .get_refgroup_values_from_ui(model_id)
          },
          event.env = cur_env,
          handler.env = cur_env
        )

        reference_group_input <- append_ui(slider)
      }

      # Add reset button for the model
      reset_button_id <- .get_refgroup_reset_button_id(model_id)

      reset_button <- actionButton(
        reset_button_id,
        label = "Reset",
        icon = icon("arrow-rotate-left")
      )

      reference_group_input <- append_ui(reset_button)
      cur_env <- env(model_id = model_id, reset_button_id = reset_button_id)

      observeEvent(
        input[[reset_button_id]],
        {
          model_definitions$models[[model_id]]$last_reference_group <<- NULL
          .set_refgroup_control_values(model_id)
        },
        handler.env = cur_env,
        event.env = cur_env
      )
    }

    div(reference_group_input)
  })

  # Populate predictor choices when model_id changes
  observe({
    models <- selected_models()

    # Create list of all possible choices (from all models)
    predictor_choices <- .gather_predictor_choices(models)

    # Determine the selected predictor. If the currently selected predictor
    # exists then keep it selected, otherwise choose the first one
    if (input$predictor %in% unname(unlist(predictor_choices))) {
      selected <- input$predictor
    } else if (length(models) > 0) {
      selected <- predictor_choices[[names(predictor_choices)[[1]]]]
    } else {
      selected <- NULL
    }

    updateSelectInput(
      session,
      "predictor",
      choices = predictor_choices,
      selected = selected
    )
  })

  # Populate interaction predictor choices when model_id changes
  observe({
    models <- selected_models()

    # Create list of all possible choices (from all models)
    predictor_choices <- .gather_predictor_choices(models)

    # If at least one model is selected, then add the empty predictor
    if (length(models) > 0) {
      predictor_choices[[empty_predictor]] <- empty_predictor
    }

    # Determine the selected predictor. If the currently selected one exists
    # then keep it selected.
    if (input$interaction_predictor %in% unname(unlist(predictor_choices))) {
      selected <- input$interaction_predictor
    } else if (length(models) > 0) {
      selected <- empty_predictor
    } else {
      selected <- NULL
    }

    updateSelectInput(
      session,
      "interaction_predictor",
      choices = predictor_choices,
      selected = selected
    )
  })

  make_plot <- function(all_curve_data) {
    # If no models are selected then tell the user to select one
    if (length(all_curve_data) == 0) {
      return(.make_message_plot("Please select at least one model."))
    }

    # Combine all data frames
    df <- all_curve_data |>
      lapply(function(x) x$df) |>
      dplyr::bind_rows()

    curve_data <- all_curve_data[[length(all_curve_data)]]

    tryCatch(
      {
        # log10 vs identity transform, based on input$logarithmic.
        transform <- ifelse(input$logarithmic, "log10", "identity")

        ylabel <- ifelse(
          input$logarithmic,
          glue::glue("{curve_data$y_axis_label} (Logarithmic)"),
          curve_data$y_axis_label
        )

        # Create plot
        if (curve_data$x_axis_type == "Categorical") {
          p <- ggplot(
            data = df,
            .make_aes(curve_data$aes_args, fill = dplyr::sym("Model"))
          ) +
            geom_col(position = "dodge") +
            scale_fill_manual(
              values = get_model_colors(model_definitions$models),
              aesthetics = "fill"
            )
        } else {
          p <- ggplot(
            data = df,
            .make_aes(curve_data$aes_args, color = dplyr::sym("Model"))
          ) +
            geom_line(linewidth = 1.2) +
            scale_color_manual(
              values = get_model_colors(model_definitions$models),
              aesthetics = "color"
            )
        }

        p <- p +
          scale_y_continuous(transform = transform) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
          labs(
            title = curve_data$title,
            subtitle = curve_data$title,
            x = curve_data$x_axis_label,
            y = ylabel
          ) +
          theme_minimal() +
          theme(
            legend.position = "right",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 11)
          )

        ggplotly(p) |>
          layout(hovermode = "x unified")
      },
      error = function(e) {
        .make_message_plot(
          paste("Error calculating OR:", e$message),
          color = "red"
        )
      }
    )
  }

  output$pr_plot <- renderPlotly({
    req(input$predictor)

    predictor <- input$predictor
    all_curve_data <- list()

    # Go through all models and calculate the OR curves
    # We concatenate them (with bind_rows) to show one curve per model
    for (model_data in selected_models()) {
      # Get predictor type (Categorical or Continuous)
      predictor_type <- model_data$variables |>
        filter(variable == predictor) |>
        pull(variableType)

      reference_group <- .get_refgroup_values_from_ui(model_data$model_id)

      tic = Sys.time()

      # Calculate the OR curve for the model
      curve_data <- calculate_pr_curve(
        predictor,
        model_data,
        reference_group = reference_group
      )

      print(paste0("Elapsed time for PR curve ", model_data$model_id, ": ", Sys.time() - tic))

      all_curve_data[[length(all_curve_data) + 1]] <- curve_data
    }

    make_plot(all_curve_data)
  })

  # Calculate and plot OR curves
  output$or_plot <- renderPlotly({
    req(input$predictor)

    all_curve_data <- list()
    predictor <- input$predictor
    interaction_predictor <- input$interaction_predictor

    # Go through all models and calculate the OR curves
    # We concatenate them (with bind_rows) to show one curve per model
    for (model_data in selected_models()) {
      # Get predictor type (Categorical or Continuous)
      predictor_type <- model_data$variables |>
        filter(variable == predictor) |>
        pull(variableType)

      reference_group <- .get_refgroup_values_from_ui(model_data$model_id)

      tic = Sys.time()

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
      model_definitions$models[[model_data$model_id]] <<- curve_data$model_data

      print(paste0("Elapsed time for OR curve ", model_data$model_id, ": ", Sys.time() - tic))

      all_curve_data[[length(all_curve_data) + 1]] <- curve_data
    }

    make_plot(all_curve_data)
  })

  # Help modal
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Algorithm Viewer Help",
      shiny::HTML("
        <h4>What is the Algorithm Viewer?</h4>
        <p>This application helps you visualize various features of a model.</p>

        <h4>How to use:</h4>
        <ol>
          <li>Select a model</li>
          <li>Choose a predictor to visualize</li>
          <li>View the odds ratio curve showing how the effect changes with
              the predictor</li>
        </ol>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

server