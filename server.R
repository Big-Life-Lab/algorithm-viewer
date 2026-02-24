source("R/model_definitions/model_definitions.R")
source("R/modules/reference_group.R")

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

  # All reference group modules (one module per model). These allow us to retrieve
  # the reference group values (from reference_groups[[model_id]]$rv_values()) and
  # destroy the reference groups when no longer needed (with
  # reference_groups[[model_id]]$destroy_module())
  reference_groups <- list()
  # Every time we create new reference groups, we increment and append this number
  # to the reference group IDs. This ensures that we never reuse the same IDs,
  # any avoids getting redundant change calls from destroying then creating
  # the controls with the same IDs.
  reference_groups_index <- 0

  #' Create Reference Group Controls
  #'
  #' Destroys any existing reference group modules and UI elements, then
  #' recreates them from the current model definitions. Each model gets a
  #' \code{\link{referenceGroupUI}}/\code{\link{referenceGroupServer}} pair
  #' inserted into the \code{#refgroups} container.
  #'
  #' @return NULL (called for side effects on UI and module state).
  #'
  #' @keywords internal
  create_refgroup_controls <- function() {
    # Destroy existing reference groups
    for (refgroup in reference_groups) {
      if (!is.null(refgroup$destroy_module)) {
        refgroup$destroy_module()
      }
    }
    reference_groups <- list()

    # Empty the reference group container
    refgroups_container_id <- "#refgroups"
    shiny::removeUI(
      selector = paste0(refgroups_container_id, " > *"),
      immediate = TRUE
    )

    # Create the reference group UI (plus module servers)
    # Incrementing and adding reference_groups_index to the ID ensures that
    # when we create new reference group controls we always have a unique ID
    # for the modules. This avoids receiving extra redraw_trigger calls
    # or other reactive changes on creation when duplicate IDs are used.
    last_model_id <- tail(names(session$userData$model_definitions$models), 1)
    refgroup_ui <- list()
    reference_groups_index <- reference_groups_index + 1
    for (model_data in session$userData$model_definitions$models) {
      model_id <- model_data$model_id
      refgroup_id <- paste0(model_id, "__", reference_groups_index)
      refgroup_ui[[length(refgroup_ui) + 1]] <- referenceGroupUI(refgroup_id, model_data)
      reference_groups[[model_id]] <<- referenceGroupServer(refgroup_id, model_data, redraw_trigger)

      if (model_id != last_model_id) {
        refgroup_ui[[length(refgroup_ui) + 1]] <- hr()
      }
    }

    # Add the reference group UI
    shiny::insertUI(
      selector = refgroups_container_id,
      where = "afterBegin",
      ui = refgroup_ui,
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

          reference_group <- isolate(reference_groups[[model_data$model_id]]$rv_values())

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

          reference_group <- isolate(reference_groups[[model_data$model_id]]$rv_values())

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
