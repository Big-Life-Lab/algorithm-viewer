source("R/model_definitions/model_definitions.R")
source("R/modules/predictor_controls.R")
source("R/modules/predictor_controls_manager.R")
source("R/utils/cached_curve_data.R")
source("R/utils/config.R")
source("R/utils/url.R")
source("R/utils/general_plot.R")

source("R/plots/plot_or.R")
source("R/plots/plot_pr.R")
source("R/plots/plot_rr.R")
source("R/plots/plot_rr_exposed_vs_unexposed.R")

# Standard height of the plots
plot_height <- "calc(100vh - 170px)"

#' Shiny Server Function
#'
#' Main server logic for the Algorithm Viewer Shiny application.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return NULL (called for side effects).
#'
#' @export
server <- function(input, output, session) {
  # The environment containing all variables associated with the predictor
  # controls
  predictor_controls_env <- initialize_predictor_controls_env()
  # The envionment containing all variables associated with cached curve data
  cached_curve_env <- initialize_cached_curve_data_env()

  # Stores the currently loaded model definitions (NULL when no algorithm is loaded).
  # model_definitions()$models contains all the models (keyed by model ID).
  # Once loaded with load_model_definitions, the values in model_definitions
  # remain unchanged until the next algorithm is loaded.
  model_definitions <- reactiveVal(NULL)
  # Stores the reference group predictor values keyed by model ID.
  # These match the values that are shown in the reference groups UI.
  reference_groups <- reactiveValues()

  # Filters model_definitions to only the models the user has selected via checkboxes.
  # Use selected_reference_groups to get the corresponding reference group values for
  # each selected model.
  selected_models <- reactive({
    if (has_model_definitions()) {
      model_defs <- model_definitions()
      models <- model_defs$models[as.vector(input$selected_model_ids)]
      models <- models[!is.na(names(models))]
      models
    } else {
      list()
    }
  })

  # Filters reference_groups to only the groups corresponding to selected models.
  # Use selected_models to get the corresponding models for each selected
  # reference group.
  selected_reference_groups <- reactive({
    if (has_model_definitions()) {
      groups <- list()
      for (model_id in input$selected_model_ids) {
        groups[[model_id]] <- reference_groups[[model_id]]
      }
      groups
    } else {
      list()
    }
  })

  # Returns TRUE when model definitions are loaded and non-empty.
  has_model_definitions <- reactive({
    val <- !is.null(model_definitions()) && length(model_definitions()) > 0
  })

  # Renders the container UI for the Relative Risk plot tab.
  output$rr_plot <- renderUI({
    plotRRUI("rr_plot", plot_height, model_definitions)
  })

  # Renders the container UI for the Odds Ratio plot tab.
  output$or_plot <- renderUI({
    plotORUI("or_plot", plot_height, model_definitions)
  })

  # Renders the container UI for the Predicted Risk plot tab.
  output$pr_plot <- renderUI({
    plotPRUI("pr_plot", plot_height, model_definitions)
  })

  # Renders the container UI for the Exposed vs Unexposed Relative Risk plot tab.
  output$rr_exposed_vs_unexposed_plot <- renderUI({
    plotRRExposedUnexposedUI("rr_exposed_vs_unexposed_plot", plot_height, model_definitions)
  })

  # Handle initial loading of the page, loading the initial algorithm (if
  # there is one) and creating all plots.
  # This is called only once per session
  observeEvent(TRUE,
    {
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
        # Did not load an initial algorithm
      }

      create_all_plots()
    },
    once = TRUE
  )
  
  # Populate the UI for available models in "selected_model_ids" checkbox group
  observe({
    model_defs <- model_definitions()
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
      "selected_model_ids",
      label = "Models:",
      selected = selected,
      choiceNames = choice_names,
      choiceValues = choice_values
    )
  })

  # Populate UI for "interaction_predictor" dropdown
  observe({
    if (!has_model_definitions()) {
      updateSelectInput(
        session,
        "interaction_predictor",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    # Create list of all possible choices (from all models)
    predictor_choices <-
      gather_predictor_choices(model_definitions()$models)

    # If at least one predictor is available, then add the empty predictor and
    # select it
    if (length(predictor_choices) > 0) {
      new_list <- list()
      new_list[[config_get_empty_selection()]] <- config_get_empty_selection()
      predictor_choices <- c(new_list, predictor_choices)
      selected <- config_get_empty_selection()
    } else {
      # No predictors, so select nothing
      selected <- character(0)
    }

    updateSelectInput(
      session,
      "interaction_predictor",
      choices = predictor_choices,
      selected = selected
    )
  })

  # Populate UI for the "predictor" dropdown
  observe({
    if (!has_model_definitions()) {
      updateSelectInput(
        session,
        "predictor",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    # Create list of all possible choices (from all models)
    predictor_choices <-
      gather_predictor_choices(model_definitions()$models)

    # If at least one predictor is available then select the first one
    if (length(predictor_choices) > 0) {
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
  })

  # Populate the UI for the preloaded "algorithms" dropdown
  observe({
    if (config_has_algorithms()) {
      # Find the preloaded algorithm ID that has the same config file as
      # the currently loaded one. If the source file exists then we
      # need to select it, otherwise we select nothing
      selected <- config_get_algorithm_id_from_file(
        model_definitions()$source_file
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
  })

  # Populate UI for the "refgroup_controls" reference group controls
  output$refgroup_controls <- renderUI({
    # Create the reference group controls UI (plus module servers)
    # refgroup_controls_ui is a list of the UI controls that we return
    last_model_id <- tail(names(model_definitions()$models), 1)
    refgroup_controls_ui <- tagList()
    for (model_data in model_definitions()$models) {
      predictor_ctrl <- create_predictor_controls(
        predictor_controls_env,
        session,
        model_data,
        initial_predictor_values = isolate(reference_groups[[model_data$model_id]])
      )
      predictor_server <- predictor_ctrl$server

      # Updates reference_groups whenever this model's predictor control values change.
      cur_env <- rlang::env(
        rv_values = predictor_server$rv_values,
        model_id = model_data$model_id
      )
      observe(
        {
          reference_groups[[model_id]] <- rv_values()
        },
        env = cur_env
      )

      refgroup_controls_ui[[length(refgroup_controls_ui) + 1]] <- predictor_ctrl$ui # predictor_ui

      if (model_data$model_id != last_model_id) {
        refgroup_controls_ui[[length(refgroup_controls_ui) + 1]] <- hr()
      }
    }

    refgroup_controls_ui
  })

  #' Create All Plot Servers
  #'
  #' This is called once when the page is first loaded.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  create_all_plots <- function() {
    plotRRExposedUnexposedServer(
      "rr_exposed_vs_unexposed_plot",
      reactive(input$predictor),
      reactive(input$interaction_predictor),
      reactive(input$logarithmic),
      selected_models,
      selected_reference_groups,
      model_definitions,
      cached_curve_env
    )
    plotRRServer(
      "rr_plot",
      reactive(input$predictor),
      reactive(input$interaction_predictor),
      reactive(input$logarithmic),
      selected_models,
      selected_reference_groups,
      model_definitions,
      cached_curve_env
    )
    plotORServer(
      "or_plot",
      reactive(input$predictor),
      reactive(input$interaction_predictor),
      reactive(input$logarithmic),
      selected_models,
      selected_reference_groups,
      model_definitions,
      cached_curve_env
    )
    plotPRServer(
      "pr_plot",
      reactive(input$predictor),
      reactive(input$interaction_predictor),
      reactive(input$logarithmic),
      selected_models,
      selected_reference_groups,
      model_definitions,
      cached_curve_env
    )
  }

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
    tryCatch(
      {
        # Destroy all predictor controls
        destroy_all_predictor_controls(predictor_controls_env)

        # Clear all cached curve data, since they will no longer apply after
        # loading the new definitions
        clear_cached_curve_data(cached_curve_env)

        model_definitions(read_model_definitions(file))

        # Initialize the reference group values. These values will be automatically
        # updated by changes in the reference groups UI, but the UI does not exist
        # until it first gets rendered with renderUI, so we must initialize
        # reference_groups here.
        clear_reference_groups()
        for (model_data in model_definitions()$models) {
          reference_groups[[model_data$model_id]] <- model_data$reference_group
        }
      },
      error = function(e) {
        model_definitions(NULL)
        clear_reference_groups()
        showModal(errorModal("Error Loading Model Definitions", e$message))
      }
    )
  }

  #' Clear All Reference Groups
  #'
  #' Removes all entries from the \code{reference_groups} reactive values
  #' object by setting each keyed value to \code{NULL}. Typically called
  #' before loading new model definitions or when a load error occurs, to
  #' ensure stale reference group data does not persist.
  #'
  #' @return NULL (called for side effects).
  #'
  #' @keywords internal
  clear_reference_groups <- function() {
    for (key in names(reference_groups)) {
      reference_groups[[key]] <- NULL
    }
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
      model_definitions(NULL)
    } else if (
      grepl("^(yaml|yml)$", tools::file_ext(file), ignore.case = TRUE)
    ) {
      # YAML file
      load_model_definitions(file)
    } else {
      # An archive
      temp_dir_path <- tempdir()

      model_definitions(NULL)
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
    if (!has_model_definitions()) {
      has_algorithm_selection <- config_allow_algorithms_selection()
      allow_file_uploads <- config_allow_file_uploads()
      if (has_algorithm_selection && allow_file_uploads) {
        msg <- paste(
          "Select an algorithm from the \"Preloaded Algorithms\" dropdown",
          "or click \"Browse\" to upload your own algorithm."
        )
      } else if (has_algorithm_selection) {
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
    bare_title <- "Algorithm Viewer"
    if (has_model_definitions()) {
      meta <- model_definitions()$meta
      glue::glue("{meta$algorithm} v{meta$version} {bare_title}")
    } else {
      bare_title
    }
  })

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
      current_source_file <- model_definitions()$source_file
      if (
        is.null(current_source_file) || selected_file != current_source_file
      ) {
        process_data_file(selected_file)
        # Clear the text in the file upload UI control
        shinyjs::reset(id = "upload")
      }
    }
  })

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
    }
    removeModal()
  })

  # Help tab
  output$help <- renderUI({
    htmltools::includeMarkdown("data/help/main.md")
  })
}

server
