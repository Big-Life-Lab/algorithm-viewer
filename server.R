source("R/model_definitions/model_definitions.R")
source("R/modules/predictor_controls.R")
source("R/modules/predictor_controls_manager.R")
source("R/utils/cached_curve_data.R")
source("R/utils/config.R")
source("R/utils/url.R")
source("R/utils/general_plot.R")
source("R/plots/plot_manager.R")

# Remove scientific notation from plots
options(scipen = 8)
# Maximum upload size in bytes
options(shiny.maxRequestSize = 30 * 1024^2)

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
  # The environment for the plot manager
  plot_man_env <- initialize_plot_manager_env()

  #' Load and Register Plot Modules
  #'
  #' Discovers all plot definition files matching \code{plot-*.R} under
  #' \code{R/plots/}, sources each file, and calls the returned registration
  #' function with the shared plot manager environment. After this call,
  #' every plot module is available to the plot manager for rendering.
  #'
  #' Plot files must follow the naming convention \code{plot-<name>.R} and must
  #' return a single-argument function from \code{source()} that accepts the
  #' plot manager environment and performs registration as a side effect.
  #'
  #' @return NULL (called for side effects; modifies \code{plot_man_env}).
  #'
  #' @keywords internal
  load_and_register_plots <- function() {
    # Load all the plots and register them with the plot manager
    plot_files <- list.files(path = "R/plots", pattern = "^plot-.*\\.R$")
    plot_files <- file.path("R", "plots", plot_files)
    for (plot_file in plot_files) {
      fn <- source(plot_file, local = new.env())
      # Call the registration function
      fn$value(plot_man_env)
    }
  }

  #' Check Whether Model Definitions Are Loaded
  #'
  #' Returns TRUE if model definitions have been loaded into the session
  #' environment and the definitions list is non-empty.
  #'
  #' @return Logical scalar; TRUE if model definitions are available, FALSE
  #'   otherwise.
  #'
  #' @keywords internal
  has_model_definitions <- function() {
    !is.null(model_definitions_env$model_definitions) && length(model_definitions_env$model_definitions) > 0
  }

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
  }

  #' Get Currently Selected Models
  #'
  #' @return Named list of model data objects for the selected models,
  #'   or an empty list if no model definitions are loaded.
  #'
  #' @keywords internal
  selected_models <- function() {
    if (has_model_definitions()) {
      model_defs <- model_definitions_env$model_definitions
      models <- model_defs$
        models[as.vector(session$userData$selected_model_ids)]
      models <- models[!is.na(names(models))]
      models
    } else {
      list()
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
      gather_predictor_choices(model_definitions_env$model_definitions$models)

    # If at least one predictor is available then select the first one
    if (length(predictor_choices) > 0) {
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
      gather_predictor_choices(model_definitions_env$model_definitions$models)

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

    session$userData$interaction_predictor <- selected
    updateSelectInput(
      session,
      "interaction_predictor",
      choices = predictor_choices,
      selected = selected
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

  #' Add Plot Tabs to Main UI
  #'
  #' Iterates over all registered plot IDs and insert a \code{tabPanel} for
  #' each into \code{main_tabs}, selecting the first plot's tab on
  #' completion. Each tab contains the plot's panel UI and a
  #' \code{plotly::renderPlotly} output bound to the plot ID.
  #' 
  #' This is called once when the page is first loaded.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  add_plot_tabs_to_ui <- function() {
    get_panel_id <- function(id) {
      as.character(glue::glue("plot_panel_{id}"))
    }

    all_plot_ids <- plot_man_all_plot_ids(plot_man_env)

    prev_tab_id <- NULL
    for (id in plot_man_all_plot_ids(plot_man_env)) {
      panel_id <- get_panel_id(id)
      
      # Create the tab panel containing the plot's panel UI
      new_tab <- tabPanel(
        icon = icon("chart-line"),
        title = plot_man_get_title(plot_man_env, id),
        value = panel_id,
        plot_man_call_panel_ui_fn(
          plot_man_env,
          id,
          plot_height = "calc(100vh - 170px)"
        )
      )

      # Insert the tab. They are all inserted at the start of the tabsetPanel (before any
      # pre-existing tab, such as the "Help" tab)
      insertTab(
        inputId = "main_tabs",
        tab = new_tab,
        select = is.null(prev_tab_id),
        target = prev_tab_id,
        position = ifelse(is.null(prev_tab_id), "before", "after")
      )

      # Save the ID of the last added tab
      prev_tab_id <- panel_id

      # Add the renderPlotly function to render the plot
      cur_env <- rlang::env(
        redraw_trigger = redraw_trigger,
        plot_man_env = plot_man_env,
        id = id,
        session = session,
        selected_models = selected_models,
        model_definitions_env = model_definitions_env,
        predictor_controls_env = predictor_controls_env,
        cached_curve_env = cached_curve_env
      )
      output[[id]] <- plotly::renderPlotly({
        redraw_trigger()

        plot_man_call_make_plot_fn(
          plot_man_env,
          id,
          session,
          selected_models(),
          model_definitions_env$model_definitions,
          predictor_controls_env,
          cached_curve_env
        )
      }, env = cur_env)
    }
  }

  #' Create UI Controls for All Plots
  #'
  #' Iterates over all registered plot IDs and invokes each plot's model UI
  #' function, inserting predictor controls and wiring up the redraw trigger.
  #' 
  #' This is called once when an algorithm file is loaded. The UI controls
  #' might be specific to the loaded models.
  #'
  #' @return NULL (called for side effects on UI).
  #'
  #' @keywords internal
  create_all_plot_ui <- function() {
    for (id in plot_man_all_plot_ids(plot_man_env)) {
      plot_man_call_model_ui_fn(
        plot_man_env,
        id,
        model_definitions_env$model_definitions,
        predictor_controls_env,
        redraw_trigger
      )
    }
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
    create_all_plot_ui()
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
    if (!has_model_definitions()) {
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

    if (has_model_definitions()) {
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

    # Load and register the plots
    load_and_register_plots()

    # Add all the plot tabs
    add_plot_tabs_to_ui()

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
