source("R/model_definitions/model_definitions.R")
source("R/modules/predictor_controls.R")
source("R/modules/predictor_controls_manager.R")
source("R/utils/cached_curve_data.R")
source("R/utils/config.R")
source("R/utils/url.R")

# Remove scientific notation from plots
options(scipen = 8)
# Maximum upload size in bytes
options(shiny.maxRequestSize = 30 * 1024^2)

# Load all curve calculation source files
curve_files <- list.files(path = "R/curves", pattern = "^curve-.*\\.R$")
lapply(file.path("R/curves", curve_files), source)

# ID and name for the empty value for UI selections (eg. in the
# "Interaction Predictor" dropdown to specify that we want no interaction
# predictor)
empty_selection <- "<empty>"

# Tags to add to the exposed and unexposed predictor control IDs
exposed_group_extra_tag <- "exposed"
unexposed_group_extra_tag <- "unexposed"

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

  # The environment containing all variables associated with the predictor
  # controls
  predictor_controls_env <- initialize_predictor_controls_env()
  # The envionment containing all variables associated with cached curve data
  cached_curve_env <- initialize_cached_curve_data_env()
  # The environment containing all model definitions
  model_definitions_env <- rlang::env(model_definitions = NULL)

  #' Create Predictor Controls
  #'
  #' Destroys any existing predictor controls modules and UI elements, then
  #' recreates them from the current model definitions. This includes
  #' the reference group controls and any other predictor controls
  #'
  #' @return NULL (called for side effects on UI and module state).
  #'
  #' @keywords internal
  create_all_predictor_controls <- function() {
    # Destroy existing predictor controls
    destroy_all_predictor_controls(predictor_controls_env)

    # Empty the refeence group controls container
    refgroup_controls_container_id <- "#refgroup_controls"
    shiny::removeUI(
      selector = paste0(refgroup_controls_container_id, " > *"),
      immediate = TRUE,
      multiple = TRUE
    )

    # Create the reference group controls UI (plus module servers)
    # refgroup_controls_ui is a list of the UI controls that we insert
    # with shiny::insertUI
    last_model_id <- tail(names(model_definitions_env$model_definitions$models), 1)
    refgroup_controls_ui <- tagList()
    for (model_data in model_definitions_env$model_definitions$models) {
      model_id <- model_data$model_id
      predictor_ctrl <- create_predictor_controls(
        predictor_controls_env,
        model_data,
        change_trigger = redraw_trigger
      )

      refgroup_controls_ui[[length(refgroup_controls_ui) + 1]] <- predictor_ctrl$ui

      if (model_id != last_model_id) {
        refgroup_controls_ui[[length(refgroup_controls_ui) + 1]] <- hr()
      }
    }

    # Add the reference group controls UI
    shiny::insertUI(
      selector = refgroup_controls_container_id,
      where = "afterBegin",
      ui = refgroup_controls_ui,
      immediate = TRUE
    )

    # Ceate exposed and unexposed group predictor controls
    exposed_container_id <- "#rr_plot_exposed_vs_unexposed_group"
    shiny::removeUI(
      selector = paste0(exposed_container_id, " > *"),
      immediate = TRUE,
      multiple = TRUE
    )
    # Use the first model's reference group data for the default
    # predictor values
    model_data <- head(model_definitions_env$model_definitions$models, 1)
    model_data <- model_data[[names(model_data)[1]]]

    # Exposed group controls
    exposed_predictor_ctrl <- create_predictor_controls(
      predictor_controls_env,
      model_data,
      extra_tag = exposed_group_extra_tag,
      change_trigger = redraw_trigger,
      model_name = "Exposed Group",
      show_model_color = FALSE
    )
    # Unexposed group controls
    unexposed_predictor_ctrl <- create_predictor_controls(
      predictor_controls_env,
      model_data,
      extra_tag = unexposed_group_extra_tag,
      change_trigger = redraw_trigger,
      model_name = "Unexposed Group",
      show_model_color = FALSE
    )

    # Insert exposed/unexposed groups
    shiny::insertUI(
      selector = exposed_container_id,
      where = "afterBegin",
      ui = tagList(exposed_predictor_ctrl$ui, unexposed_predictor_ctrl$ui),
      immediate = TRUE
    )

  }

  #' Get Currently Selected Models
  #'
  #' @return Named list of model data objects for the selected models,
  #'   or an empty list if no model definitions are loaded.
  #'
  #' @keywords internal
  selected_models <- function() {
    if (is.null(model_definitions_env$model_definitions)) {
      list()
    } else {
      model_defs <- model_definitions_env$model_definitions
      models <- model_defs$
        models[as.vector(session$userData$selected_model_ids)]
      models <- models[!is.na(names(models))]
      models
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
    if (is.null(model_definitions_env$model_definitions)) {
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
    if (is.null(model_definitions_env$model_definitions)) {
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
      new_list[[empty_selection]] <- empty_selection
      predictor_choices <- c(new_list, predictor_choices)
    }

    if (length(models) > 0) {
      selected <- empty_selection
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
  #' @param flip_coords If TRUE then flip the x and y axes. Defaults to FALSE.
  #' @param theme_args If not NULL then a named list of arguments to pass to
  #'   ggplot2::theme
  #' @param plot_type The type of plot to create. Can be "bar", "line", "scatter"
  #'
  #' @return A plotly object for rendering in the UI.
  #'
  #' @keywords internal
  make_plot <- function(all_curve_data, flip_coords = FALSE, theme_args = NULL, plot_type = NULL) {
    # If no models are selected then tell the user to select one
    if (is.null(model_definitions_env$model_definitions)) {
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

    # The hover mode, as passed to plotly::layout()
    hovermode = "x unified"

    tryCatch(
      {
        # log10 vs identity transform
        transform <- ifelse(logarithmic, "log10", "identity")

        ylabel <- ifelse(
          logarithmic,
          glue::glue("{curve_data$y_axis_label} (Logarithmic)"),
          curve_data$y_axis_label
        )

        y_limits <- NULL
        if (!is.null(curve_data$ylim) && !logarithmic) {
          y_limits <- curve_data$ylim
        }

        # Create plot
        if (curve_data$x_axis_type == "Categorical") {
          # Maintain the order of the x-axis categories
          levels <- unique(df[[curve_data$x_axis_label]])
          if (flip_coords) {
            # For vertical graphs (where the categories are on the y axis),
            # we want the to reverse the order of the categories, so they
            # are sorted from top to bottom (instead of bottom to top)
            levels <- rev(levels)
          }
          df[[curve_data$x_axis_label]] <-
            factor(
              df[[curve_data$x_axis_label]],
              levels = levels
            )

          if (is.null(plot_type)) {
            plot_type <- "bar"
          }
        } else {
          if (is.null(plot_type)) {
            plot_type <- "line"
          }
        }

        model_colors <- get_model_colors(
          model_definitions_env$model_definitions$models
        )

        if (plot_type == "bar") {
          p <- ggplot2::ggplot(
            data = df,
            .make_aes(curve_data$aes_args, fill = dplyr::sym("Model"))
          )
          p <- p +
            ggplot2::geom_col(position = "dodge") +
            ggplot2::scale_fill_manual(
              values = model_colors,
              aesthetics = "fill"
            )
        } else if (plot_type == "line") {
          p <- ggplot2::ggplot(
            data = df,
            .make_aes(curve_data$aes_args, color = dplyr::sym("Model"))
          )
          p <- p +
            ggplot2::geom_line(linewidth = 1.2) +
            ggplot2::scale_color_manual(
              values = model_colors,
              aesthetics = "color"
            )
        } else if (plot_type == "scatter") {
          p <- ggplot2::ggplot(
            data = df,
            .make_aes(curve_data$aes_args, fill = dplyr::sym("Model"))
          )
          p <- p +
            ggplot2::geom_point(
              position = ggplot2::position_dodge(width = 0.3),
              size = 4,
              stroke = 0.1
            ) +
            ggplot2::scale_fill_manual(
              values = model_colors,
              aesthetics = "fill"
            )
          # @TODO: REMOVE THIS!!!
          y_limits <- c(0.001, 100)
        }

        p <- p +
          ggplot2::scale_y_continuous(
            transform = transform,
            limits = y_limits
          ) +
          ggplot2::geom_hline(
            yintercept = 1,
            linetype = "dashed",
            color = "gray50"
          ) +
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

        if (!is.null(theme_args)) {
          # Apply the addition theme arguments
          p <- p +
            do.call(ggplot2::theme, theme_args)
        }
        if (flip_coords) {
          # Flip the x and y axes
          p <- p +
            ggplot2::coord_flip()
          hovermode = "y unified"
        }

        plotly::ggplotly(p) |>
          plotly::layout(hovermode = hovermode)
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

    if (is.null(model_definitions_env$model_definitions)) {
      return(make_plot(NULL))
    }

    tryCatch(
      {
        predictor <- session$userData$predictor
        all_curve_data <- list()

        # Go through all models and calculate the OR curves
        # We concatenate them (with bind_rows) to show one curve per model
        for (model_data in selected_models()) {
          predictor_values <- get_predictor_controls_values(predictor_controls_env, model_data)

          # Check if we can use the cached old data for the current model
          model_params <- list(
            predictor = predictor,
            reference_group = predictor_values
          )
          if (
            is_reusable_cached_curve_data(
              cached_curve_env,
              "pr",
              model_data$model_id, model_params
            )
          ) {
            # Reuse the old data
            all_curve_data[[length(all_curve_data) + 1]] <-
              get_cached_curve_data(cached_curve_env, "pr", model_data$model_id)
          } else {
            # Get predictor type (Categorical or Continuous)
            predictor_type <- model_data$variables |>
              dplyr::filter(variable == predictor) |>
              dplyr::pull(variableType)

            tic <- Sys.time()

            # Calculate the OR curve for the model
            curve_data <- calculate_pr_curve(
              predictor,
              model_data,
              reference_group = predictor_values
            )

            elapsed <- Sys.time() - tic
            message(paste0(
              "Elapsed time for PR curve ", model_data$model_id, ": ", elapsed
            ))

            all_curve_data[[length(all_curve_data) + 1]] <- curve_data

            # Save the data to our cache
            set_cached_curve_data(
              cached_curve_env,
              "pr",
              model_data$model_id,
              model_params,
              curve_data
            )
          }
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

  # Odds Ratios plots
  output$or_plot <- plotly::renderPlotly({
    redraw_trigger()

    req(session$userData$predictor)

    if (is.null(model_definitions_env$model_definitions)) {
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
          predictor_values <- get_predictor_controls_values(predictor_controls_env, model_data)

          # Check if we can use the cached old data for the current model
          model_params <- list(
            predictor = predictor,
            interaction_predictor = interaction_predictor,
            reference_group = predictor_values
          )
          if (
            is_reusable_cached_curve_data(
              cached_curve_env,
              "or",
              model_data$model_id,
              model_params
            )
          ) {
            # Reuse the old data
            all_curve_data[[length(all_curve_data) + 1]] <-
              get_cached_curve_data(cached_curve_env, "or", model_data$model_id)
          } else {
            # Get predictor type (Categorical or Continuous)
            predictor_type <- model_data$variables |>
              dplyr::filter(variable == predictor) |>
              dplyr::pull(variableType)

            tic <- Sys.time()

            # Calculate the OR curve for the model
            if (interaction_predictor == empty_selection) {
              curve_data <- calculate_or_curve(
                predictor,
                model_data,
                reference_group = predictor_values
              )
            } else {
              curve_data <- calculate_or_curve_interaction(
                predictor,
                interaction_predictor,
                model_data,
                reference_group = predictor_values
              )
            }

            elapsed <- Sys.time() - tic
            message(paste0(
              "Elapsed time for OR curve ", model_data$model_id, ": ", elapsed
            ))

            all_curve_data[[length(all_curve_data) + 1]] <- curve_data

            # Save the data to our cache
            set_cached_curve_data(
              cached_curve_env,
              "or",
              model_data$model_id,
              model_params,
              curve_data
            )
          }
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

  output$rr_plot_exposed_vs_unexposed <- plotly::renderPlotly({
    redraw_trigger()

    req(session$userData$predictor)

    if (is.null(model_definitions_env$model_definitions)) {
      return(make_plot(NULL))
    }

    tryCatch(
      {
        all_curve_data <- list()
        predictor <- session$userData$predictor

        # Go through all models and calculate the OR curves
        # We concatenate them (with bind_rows) to show one curve per model
        for (model_data in selected_models()) {
          # Use the first model's reference group data for the default
          # predictor values
          exposed_model_data <- head(model_definitions_env$model_definitions$models, 1)
          exposed_model_data <- exposed_model_data[[names(exposed_model_data)[1]]]

          exposed_group <- get_predictor_controls_values(
            predictor_controls_env,
            exposed_model_data,
            extra_tag = exposed_group_extra_tag
          )
          unexposed_group <- get_predictor_controls_values(
            predictor_controls_env,
            exposed_model_data,
            extra_tag = unexposed_group_extra_tag
          )

          # Check if we can use the cached old data for the current model
          model_params <- list(
            exposed_group = exposed_group,
            unexposed_group = unexposed_group
          )
          if (
            is_reusable_cached_curve_data(
              cached_curve_env,
              "rr_exposed_vs_unexposed",
              model_data$model_id,
              model_params
            )
          ) {
            # Reuse the old data
            all_curve_data[[length(all_curve_data) + 1]] <-
              get_cached_curve_data(cached_curve_env, "rr_exposed_vs_unexposed", model_data$model_id)
          } else {
            # Get predictor type (Categorical or Continuous)
            tic <- Sys.time()

            # Calculate the RR curve for the model
            curve_data <- calculate_rr_exposed_vs_unexposed_curve(
              model_data = model_data,
              exposed_group = exposed_group,
              unexposed_group = unexposed_group
            )

            elapsed <- Sys.time() - tic
            message(paste0(
              "Elapsed time for RR Multi curve ", model_data$model_id, ": ", elapsed
            ))

            all_curve_data[[length(all_curve_data) + 1]] <- curve_data

            # Save the data to our cache
            set_cached_curve_data(
              cached_curve_env,
              "rr_exposed_vs_unexposed",
              model_data$model_id,
              model_params,
              curve_data
            )
          }
        }

        make_plot(
          all_curve_data,
          flip_coords = TRUE,
          theme_args = list(axis.title.y = ggplot2::element_blank()),
          plot_type = "scatter"
        )
      },
      error = function(e) {
        .make_message_plot(
          glue::glue("<b>Error</b>: {e$message}"),
          color = "red"
        )
      }
    )
  })

  # Relative Risk plots
  output$rr_plot <- plotly::renderPlotly({
    redraw_trigger()

    req(session$userData$predictor)

    if (is.null(model_definitions_env$model_definitions)) {
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
          predictor_values <- get_predictor_controls_values(predictor_controls_env, model_data)

          # Check if we can use the cached old data for the current model
          model_params <- list(
            predictor = predictor,
            interaction_predictor = interaction_predictor,
            reference_group = predictor_values
          )
          if (
            is_reusable_cached_curve_data(
              cached_curve_env,
              "rr",
              model_data$model_id,
              model_params
            )
          ) {
            # Reuse the old data
            all_curve_data[[length(all_curve_data) + 1]] <-
              get_cached_curve_data(cached_curve_env, "rr", model_data$model_id)
          } else {
            # Get predictor type (Categorical or Continuous)
            predictor_type <- model_data$variables |>
              dplyr::filter(variable == predictor) |>
              dplyr::pull(variableType)

            tic <- Sys.time()

            # Calculate the RR curve for the model
            if (interaction_predictor == empty_selection) {
              curve_data <- calculate_rr_curve(
                predictor,
                model_data,
                reference_group = predictor_values
              )
            } else {
              curve_data <- calculate_rr_curve_interaction(
                predictor,
                interaction_predictor,
                model_data,
                reference_group = predictor_values
              )
            }

            elapsed <- Sys.time() - tic
            message(paste0(
              "Elapsed time for RR curve ", model_data$model_id, ": ", elapsed
            ))

            all_curve_data[[length(all_curve_data) + 1]] <- curve_data

            # Save the data to our cache
            set_cached_curve_data(
              cached_curve_env,
              "rr",
              model_data$model_id,
              model_params,
              curve_data
            )
          }
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
        model_definitions_env$model_definitions <- read_model_definitions(file)
      },
      error = function(e) {
        model_definitions_env$model_definitions <- NULL
        showModal(errorModal("Error Loading Model Definitions", e$message))
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
    clear_cached_curve_data(cached_curve_env)
    update_preloaded_algorithms()
    update_model_selections()
    update_predictor_choices()
    update_interaction_predictor_choices()
    create_all_predictor_controls()
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
    if (is.null(file)) {
      # Empty file
      model_definitions_env$model_definitions <- NULL
    } else if (
      grepl("^(yaml|yml)$", tools::file_ext(file), ignore.case = TRUE)
    ) {
      # YAML file
      load_model_definitions(file)
    } else {
      # An archive
      temp_dir_path <- tempdir()

      model_definitions_env$model_definitions <- NULL
      archive_success <- FALSE
      config_files <- c()

      tryCatch(
        {
          files_in_archive <-
            archive::archive_extract(file, dir = temp_dir_path)
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
    if (!config_allow_file_uploads()) {
      return()
    }

    # Since the file was uploaded, it does not represent any preloaded algorithms
    # specified in the config, so we clear the URL query string.
    if (config_allow_algorithm_in_url()) {
      url_update_query_string("?", mode = "replace", session = session)
    }
    if (!is.null(input$upload)) {
      file <- input$upload$datapath
      process_data_file(file)
    }
  })

  # Show a message to the user at the top of the "Models" tab
  output$model_message <- renderUI({
    reload_trigger()
    if (is.null(model_definitions_env$model_definitions)) {
      has_algorithms <- config_has_algorithms()
      allow_file_uploads <- config_allow_file_uploads()
      if (has_algorithms && allow_file_uploads) {
        msg <- paste(
          "Select an algorithm from the \"Preloaded Algorithms\" dropdown",
          "or click \"Browse\" to upload your own algorithm."
        )
      } else if (has_algorithms) {
        msg <- "Select an algorithm from the \"Preloaded Algorithms\" dropdown."
      } else if (allow_file_uploads) {
        msg <- "Click \"Browse\" to upload an algorithm."
      } else {
        msg <- ""
      }
      msg <- paste0(
        "No algorithm has been loaded.  ",
        msg
      )
      div(msg, style = "color: #ff0000;", br(), br())
    }
  })

  # Populate the main title of the page
  output$ui_title <- renderUI({
    reload_trigger()

    if (!is.null(model_definitions_env$model_definitions)) {
      meta <- model_definitions_env$model_definitions$meta
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
    if (url_has_algorithm_id(session)) {
      # The URL has an algorithm specified (eg
      # "example.com/?algorithm=htnport-reduced"), so try to load the
      # algorithm specified in the URL.
      process_data_file(config_get_algorithm_file(
        url_get_algorithm_id(session)
      ))
    } else if (!is.null(config_get_initial_algorithm_file())) {
      # Load initial algorithm
      process_data_file(config_get_initial_algorithm_file())
    } else {
      # Did not load an initial algorithm, but we still need
      # to create all the UI elements and perform other initial
      # setup
      recreate_and_trigger_reload()
    }

    initial_load_trigger(initial_load_trigger() + 1)
  })

  # Redraw when the selected Models changes.
  # The checkbox group returns NULL if nothing is selected, so we must
  # set ignoreNULL = FALSE to make sure that we redraw when no model
  # is selected
  observeEvent(input$model_id,
    {
      session$userData$selected_model_ids <- input$model_id
      redraw_trigger(redraw_trigger() + 1)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )

  # Handle selection from the "Preloaded Algorithms" dropdown
  observeEvent(input$algorithms, {
    selected_file <- config_get_algorithm_file(input$algorithms)
    if (!is.null(selected_file)) {
      if (config_allow_algorithm_in_url()) {
        # Update the query string so users can bookmark the page/share
        # the url and have the selected algorithm automatically loaded.
        url_set_algorithm_id(input$algorithms, session = session)
      }

      # A file is selected. If it is not the currently loaded file then
      # load it.
      current_source_file <- model_definitions_env$model_definitions$source_file
      if (
        is.null(current_source_file) || selected_file != current_source_file
      ) {
        process_data_file(selected_file)
        # Clear the text in the file upload UI control
        shinyjs::reset(id = "upload")
      }
    }
  })

  #' Update Preloaded Algorithms Dropdown
  #'
  #' Refreshes the algorithms dropdown with the list of preloaded algorithms
  #' from the app config, and sets the selected value to the algorithm whose
  #' source file matches the currently loaded model definitions. If no match
  #' is found, the selection is cleared.
  #'
  #' Does nothing if no preloaded algorithms are defined in the config.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  update_preloaded_algorithms <- function() {
    if (config_has_algorithms()) {
      # Find the preloaded algorithm ID that has the same config file as
      # the currently loaded one. If the source file exists then we
      # need to select it, otherwise we select nothing
      selected <- config_get_algorithm_id_from_file(
        model_definitions_env$model_definitions$source_file
      )
      if (is.null(selected)) {
        selected <- character(0)
        choices <- config_get_algorithm_choices()

        # When we select nothing in the dropdown, R Shiny doesn't recognize
        # this as a change in the selected value. Instead, it assumes that
        # whatever was previously selected remains the selected value.
        # If the user then reselects the previously selected value then
        # the "algorithms" dropdown will not trigger observers. To avoid this,
        # we add an extra fake ID to the dropdown, select that fake ID,
        # then clear the selection with selected = character(0).

        # Make a fake ID consisting of "x"s so that its length is one
        # character longer than the longest existing ID. This will
        # guarantee our fake ID doesn't clash with an existing
        # algorithm ID
        max_choice_id_length <- unname(unlist(choices)) |>
          stringr::str_length()
        max_choice_id_length <- max(max_choice_id_length)
        fake_choice <- strrep("x", max_choice_id_length + 1)
        choices[[fake_choice]] <- fake_choice
        updateSelectInput(
          session,
          "algorithms",
          choices = choices,
          selected = fake_choice
        )
      }
      updateSelectInput(
        session,
        "algorithms",
        choices = config_get_algorithm_choices(),
        selected = selected
      )
    }
  }

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
    model_defs <- model_definitions_env$model_definitions
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
