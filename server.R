library(dplyr)
library(rlang)
library(ggplot2)
library(plotly)
library(shiny)
library(shinyWidgets)
library(cli)
library(htmltools)
library(archive)
if (FALSE) {
  # Required by ggplot2 when exporting for Shinylive
  library(munsell)
  # Required by htmltools::includeMarkdown when exporting for Shinylive
  library(markdown)
}

source("R/model_definitions/model_definitions.R")

# The initial algorithm file to load automatically by default on startup
# (eg. zip or yaml file).
# Set this to NULL to not load a default algorithm (user must upload themself)
initial_algorithm_file <- file.path(
  "data", "models", "htnport-mpp", "htnport-reduced.yaml"
)
# initial_algorithm_file <- NULL

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
    stringr::str_wrap(width = 50)

  df <- data.frame(label = label)
  p <- ggplot() +
    geom_text(
      data = df, aes(label = label), x = 0.5, y = 0.5, color = color
    ) +
    theme_void() +
    theme(axis.line = element_blank())

  ggplotly(p, tooltip = NULL) |>
    style(hoverinfo = "none") |>
    config(displayModeBar = FALSE) |>
    layout(
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
  # React to reload_trigger when something needs to be updated due to a loading
  # of an algorithm zip file. In most cases, we can simply call
  # selected_models() to react to a reload.
  # selected_model_ids should generally not be used except for special cases. It
  # is reacted to in selected_models() to make all other functions that access
  # the selected models react automatically.
  selected_model_ids <- reactiveVal(c())
  reload_trigger <- reactiveVal(0)
  initial_load_trigger <- reactiveVal(0)

  #' Get Reference Group Values from UI
  #'
  #' Retrieves current reference group values from UI slider controls for
  #' a specific model and caches them in the model data.
  #'
  #' @param model_id Character string specifying the model identifier.
  #'
  #' @return Named list of reference group values, or NULL if no model
  #'   definitions are loaded.
  #'
  #' @keywords internal
  get_refgroup_values_from_ui <- function(model_id) {
    if (is.null(session$userData$model_definitions)) {
      return()
    }

    reference_group <- list()
    model_data <- session$userData$model_definitions$models[[model_id]]

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

    session$userData$model_definitions$models[[model_id]]$
      last_reference_group <<-
      reference_group

    reference_group
  }

  #' Repopulate the existing reference group controls with the last values.
  #'
  #' @param model_id Character string specifying the model identifier to
  #'  repopulate the reference group controls for.
  set_refgroup_control_values <- function(model_id) {
    if (is.null(session$userData$model_definitions)) {
      return()
    }

    model_data <- session$userData$model_definitions$models[[model_id]]
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


  # Reactive expression to get currently selected models based on
  # rv$selected_model_ids
  selected_models <- reactive({
    reload_trigger()
    if (is.null(session$userData$model_definitions)) {
      list()
    } else {
      model_defs <- session$userData$model_definitions
      models <- model_defs$models[as.vector(selected_model_ids())]
      models <- models[!is.na(names(models))]
      models
    }
  })

  #' Update Reference Group UI Controls
  #'
  #' Rebuilds the reference group slider controls in the UI for all loaded
  #' models. Removes existing controls and creates new sliders for each
  #' predictor variable, with appropriate input types for categorical vs
  #' continuous variables.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  update_refgroups <- function() {
    # ID of the div containing the reference group sliders. This is for removing
    # then adding the div.
    refgroup_input_id <- "refgroup_input_id"

    removeUI(selector = paste0("#", refgroup_input_id))

    # Create the reference group inputs, save them in reference_group_input
    reference_group_input <- list()

    # Add a UI element to reference_group_input
    append_ui <- function(item) {
      reference_group_input[[length(reference_group_input) + 1]] <- item
      reference_group_input
    }

    reference_group_input <- append_ui(br())

    # Go through all models and create the reference group controls
    for (model_data in session$userData$model_definitions$models) {
      model_id <- model_data$model_id

      # Add a line (hr) between each model controls
      if (length(reference_group_input) > 1) {
        reference_group_input <- append_ui(hr())
      }

      # Add heading
      model_heading <- h4(shiny::HTML(add_model_color(
        model_data,
        cleanup_string(glue::glue("Model: {model_data$title}")),
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

        # Call get_refgroup_values_from_ui any time a slider changes
        # This will save the last set reference group values
        cur_env <- env(model_id = model_id, input_id = input_id)
        observeEvent(
          input[[input_id]],
          {
            get_refgroup_values_from_ui(model_id)
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
        label = glue::glue("Reset {model_data$title}"),
        icon = icon("arrow-rotate-left")
      )

      reference_group_input <- append_ui(reset_button)
      cur_env <- env(
        model_id = model_id, reset_button_id = reset_button_id
      )

      observeEvent(
        input[[reset_button_id]],
        {
          session$userData$model_definitions$models[[model_id]]$
            last_reference_group <<- NULL
          set_refgroup_control_values(model_id)
        },
        handler.env = cur_env,
        event.env = cur_env
      )
    }

    content <- div(id = refgroup_input_id, reference_group_input)
    insertUI(selector = "#refgroups", where = "afterBegin", ui = content)
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
          model_colors <- get_model_colors(
            session$userData$model_definitions$models
          )
          p <- ggplot(
            data = df,
            .make_aes(curve_data$aes_args, fill = dplyr::sym("Model"))
          ) +
            geom_col(position = "dodge") +
            scale_fill_manual(values = model_colors, aesthetics = "fill")
        } else {
          model_colors <- get_model_colors(
            session$userData$model_definitions$models
          )
          p <- ggplot(
            data = df,
            .make_aes(curve_data$aes_args, color = dplyr::sym("Model"))
          ) +
            geom_line(linewidth = 1.2) +
            scale_color_manual(values = model_colors, aesthetics = "color")
        }

        y_limits <- NULL
        if (!is.null(curve_data$ylim) && !input$logarithmic) {
          y_limits <- curve_data$ylim
          # if (input$logarithmic) {
          #   y_limits <- sapply(y_limits, function(x) ifelse (x <= 0, 1e-8, x))
          # }
        }

        p <- p +
          scale_y_continuous(transform = transform, limits = y_limits) +
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
    reload_trigger()

    if (is.null(session$userData$model_definitions)) {
      return(make_plot(NULL))
    }

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

      reference_group <- get_refgroup_values_from_ui(model_data$model_id)

      tic <- Sys.time()

      # Calculate the OR curve for the model
      curve_data <- calculate_pr_curve(
        predictor,
        model_data,
        reference_group = reference_group
      )

      elapsed <- Sys.time() - tic
      print(paste0(
        "Elapsed time for PR curve ", model_data$model_id, ": ", elapsed
      ))

      all_curve_data[[length(all_curve_data) + 1]] <- curve_data
    }

    make_plot(all_curve_data)
  })

  # Calculate and plot OR curves
  output$or_plot <- renderPlotly({
    reload_trigger()

    if (is.null(session$userData$model_definitions)) {
      return(make_plot(NULL))
    }

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

      reference_group <- get_refgroup_values_from_ui(model_data$model_id)

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
      print(paste0(
        "Elapsed time for OR curve ", model_data$model_id, ": ", elapsed
      ))

      all_curve_data[[length(all_curve_data) + 1]] <- curve_data
    }

    make_plot(all_curve_data)
  })

  #' Load Model Definitions from File
  #'
  #' Reads model definitions from a YAML file and stores them in the session
  #' user data. Optionally triggers UI updates after loading.
  #'
  #' @param file Character string specifying the path to the YAML file.
  #' @param call_update_all Logical indicating whether to call update_all()
  #'   after loading. Default is TRUE.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  load_model_definitions <- function(file, call_update_all = TRUE) {
    tryCatch(
      {
        session$userData$model_definitions <- read_model_definitions(file)
      },
      error = function(e) {
        print(paste("Error loading model definition:", e$message))
      }
    )
    if (call_update_all) {
      update_all()
    }
  }

  #' Update All UI Components
  #'
  #' Refreshes all model-dependent UI components including model selections,
  #' reference group controls, and predictor dropdowns.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  update_all <- function() {
    update_model_selections()
    update_refgroups()
    update_predictor_choices()
    update_interaction_predictor_choices()
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
    if (grepl(file_ext(file), "^(yaml|yml)$", ignore.case = TRUE)) {
      load_model_definitions(file)
    } else {
      temp_dir_path <- tempdir()

      session$userData$model_definitions <- NULL
      archive_success <- FALSE
      config_files <- c()

      tryCatch(
        {
          files_in_archive <- archive_extract(file, dir = temp_dir_path)
          yaml_pattern <- "(\\.yaml|\\.yml)$"
          config_files <- files_in_archive[
            grepl(yaml_pattern, files_in_archive, ignore.case = TRUE)
          ]
          # Ignore config files in the __MACOSX directory
          # (added automatically on a Mac)
          config_files <- config_files[!grepl("__MACOSX", config_files)]
          archive_success <- TRUE
        },
        error = function(e) {}
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
        escaped_files <- htmltools::htmlEscape(basename(config_files))
        message <- paste0("<li>", escaped_files, "</li>") |>
          stringr::str_c(collapse = "\n")
        message <- paste0("<ul>\n", message, "\n</ul>")
        message <- glue::glue(
          "<p>Multiple YAML configuration files were found in your archive, ",
          "only one is allowed:</p>{message}<p>No models were loaded.</p>"
        )
        showModal(errorModal("Error Loading Data", shiny::HTML(message)))
      } else {
        config_file <- file.path(temp_dir_path, config_files[[1]])
        load_model_definitions(config_file, call_update_all = FALSE)
      }
      update_all()
    }
  }

  # Handle file that has been uploaded
  observeEvent(input$upload, {
    if (!allow_file_uploads) {
      return()
    }

    if (!is.null(input$upload)) {
      file <- input$upload$datapath
      process_data_file(file)
    }
  })

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
    if (!is.null(initial_algorithm_file)) {
      process_data_file(initial_algorithm_file)
    }
    initial_load_trigger(initial_load_trigger() + 1)
  })

  # When the Models checkboxes change selection then update the reactive values
  # select_model_ids with a string vector of the selected model IDs. This
  # will trigger all listeners on selected_model_ids.
  observe({
    selected_model_ids(input$model_id)
  })

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
      updateCheckboxGroupInput(
        session,
        "model_id",
        label = "Models:",
        selected = selected,
        choiceNames = choice_names,
        choiceValues = get_model_ids(model_defs$models)
      )
      selected_model_ids(selected)
    } else {
      # No model definitions loaded, so show an empty checkbox group
      updateCheckboxGroupInput(
        session,
        "model_id",
        label = "Models:",
        selected = character(0),
        choiceNames = character(0),
        choiceValues = character(0)
      )
      selected_model_ids(c())
    }

    reload_trigger(reload_trigger() + 1)
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

  # Help tab
  output$help <- renderUI({
    htmltools::includeMarkdown("data/help/main.md")
  })
}

server
