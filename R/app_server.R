#' Main R Shiny Server Function
#'
#' The main server function for the Algorithm Viewer R Shiny App.
#'
#' @name app_server
#' @noRd
#' @keywords internal
NULL

# Height, in pixels, of the area outside of the plot area (in the main tabs).
# If a plot wants to fill up the height of the page, without causing overflow
# at the bottom (and hence scrolling), then a plot's height should be
# \code{calc(100vh - {.external_height}px)}
.external_height <- 170

#' Create Error Modal Dialog
#'
#' Creates a modal dialog for displaying error messages to the user.
#'
#' @param title Character string specifying the modal title.
#' @param message Character string or HTML content for the modal body.
#'
#' @return A modalDialog object for use with showModal().
#'
#' @noRd
#' @keywords internal
errorModal <- function(title, message) {
  message <- gsub("\n", "<br />", message)
  shiny::modalDialog(
    title = title,
    shiny::HTML(message),
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Dismiss")
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
#' @noRd
#' @keywords internal
yesNoModal <- function(title,
                       message,
                       ok_button_id,
                       show_no = TRUE,
                       size = "m") {
  shiny::modalDialog(
    title = title,
    message,
    footer = shiny::tagList(
      if (show_no) shiny::modalButton("Cancel"),
      shiny::actionButton(ok_button_id, "OK")
    ),
    size = size
  )
}

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
#' @noRd
#' @keywords internal
app_server <- function(input, output, session) {
  # Stores the currently loaded model definitions (NULL when no algorithm is
  # loaded).
  # model_definitions()$models contains all the models (keyed by model ID).
  # Once loaded with load_model_definitions, the values in model_definitions
  # remain unchanged until the next algorithm is loaded.
  model_definitions <- shiny::reactiveVal(NULL)

  # Tracks the temp directory created for the most-recent archive upload.
  # Used by the select_yaml_ok observer to validate the user's YAML selection
  # against the specific directory for that upload (not the shared tempdir()).
  upload_temp_dir <- NULL

  # Stores the reference group predictor values keyed by model ID.
  # These match the values that are shown in the reference groups UI.
  reference_groups <- predictorGroupedControlsServer(
    "refgroup", model_definitions
  )

  # Handle initial loading of the page, loading the initial algorithm (if
  # there is one) and creating all plots.
  # This is called only once per session
  shiny::observeEvent(TRUE,
    {
      create_all_plot_servers()

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
    },
    once = TRUE,
    priority = 1000
  )

  # Filters model_definitions to only the models the user has selected via
  # checkboxes. Use selected_reference_groups to get the corresponding
  # reference group values for each selected model.
  selected_models <- shiny::reactive({
    if (has_model_definitions()) {
      model_defs <- model_definitions()
      models <- model_defs$models[as.vector(input$selected_model_ids)]
      models <- models[!is.na(names(models))]
      models
    } else {
      list()
    }
  })

  # Filters reference_groups to only the groups corresponding to selected
  # models.
  # Use selected_models to get the corresponding models for each selected
  # reference group.
  selected_reference_groups <- shiny::reactive({
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

  # Model definitions for a vs b predictor control panel
  a_vs_b_model_definitions <- shiny::reactive({
    if (has_model_definitions()) {
      defns <- model_definitions()
      a_model <- defns$models[[names(defns$models)[[1]]]]
      b_model <- a_model
      a_model$model_id <- "a"
      b_model$model_id <- "b"
      a_model$title <- "Me"
      b_model$title <- "Ref"
      defns$models <- list(
        a = a_model,
        b = b_model
      )
      defns
    } else {
      NULL
    }
  })

  a_vs_b_groups <- predictorGroupedControlsServer(
    "a_vs_b_groups",
    a_vs_b_model_definitions,
    mode = "compact",
    show_model_color = FALSE
  )

  # Returns TRUE when model definitions are loaded and non-empty.
  has_model_definitions <- shiny::reactive({
    !is.null(model_definitions()) && length(model_definitions()) > 0
  })

  # Populate the UI for available models in "selected_model_ids" checkbox group
  shiny::observe({
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

    shiny::updateCheckboxGroupInput(
      session,
      "selected_model_ids",
      label = "Models:",
      selected = selected,
      choiceNames = choice_names,
      choiceValues = choice_values
    )
  })

  # Populate UI for "interaction_predictor" dropdown
  shiny::observe({
    if (!has_model_definitions()) {
      shiny::updateSelectInput(
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

    shiny::updateSelectInput(
      session,
      "interaction_predictor",
      choices = predictor_choices,
      selected = selected
    )
  })

  # Populate UI for the "predictor" dropdown
  shiny::observe({
    if (!has_model_definitions()) {
      shiny::updateSelectInput(
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

    shiny::updateSelectInput(
      session,
      "predictor",
      choices = predictor_choices,
      selected = selected
    )
  })

  # Populate the UI for the preloaded "algorithms" dropdown
  shiny::observe({
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
        shiny::updateSelectInput(
          session,
          "algorithms",
          choices = choices,
          selected = fake_choice
        )
      }
      shiny::updateSelectInput(
        session,
        "algorithms",
        choices = config_get_algorithm_choices(),
        selected = selected
      )
    }
  })

  # Show/hide settings sub-tabs based on the active main tab
  observeEvent(input$main_tabs, {
    hideable_tabs <- c("a_vs_b", "reference_groups")
    if (input$main_tabs %in% c("a_vs_b")) {
      showTab(
        "settings_tabs", "a_vs_b",
        select = input$settings_tabs %in% hideable_tabs
      )
      hideTab("settings_tabs", "reference_groups")
    } else {
      hideTab("settings_tabs", "a_vs_b")
      showTab(
        "settings_tabs", "reference_groups",
        select = input$settings_tabs %in% hideable_tabs
      )
    }
  })

  #' Create All Plot Servers
  #'
  #' This is called once when the page is first loaded.
  #'
  #' @return NULL (called for side effects).
  create_all_plot_servers <- function() {
    plotRRAvsBServer(
      "rr_a_vs_b_plot",
      shiny::reactive(input$predictor),
      shiny::reactive(input$interaction_predictor),
      shiny::reactive(input$logarithmic),
      selected_models,
      a_vs_b_groups,
      model_definitions
    )
    plotRRServer(
      "rr_plot",
      shiny::reactive(input$predictor),
      shiny::reactive(input$interaction_predictor),
      shiny::reactive(input$logarithmic),
      selected_models,
      selected_reference_groups,
      model_definitions
    )
    plotORServer(
      "or_plot",
      shiny::reactive(input$predictor),
      shiny::reactive(input$interaction_predictor),
      shiny::reactive(input$logarithmic),
      selected_models,
      selected_reference_groups,
      model_definitions
    )
    plotPRServer(
      "pr_plot",
      shiny::reactive(input$predictor),
      shiny::reactive(input$interaction_predictor),
      shiny::reactive(input$logarithmic),
      selected_models,
      selected_reference_groups,
      model_definitions
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
  load_model_definitions <- function(file) {
    tryCatch(
      {
        # Load the file
        model_definitions(read_model_definitions(file))
      },
      error = function(e) {
        model_definitions(NULL)
        shiny::showModal(errorModal(
          "Error Loading Model Definitions",
          conditionMessage(e)
        ))
      }
    )
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
  process_data_file <- function(file) {
    if (is.null(file) || length(file) == 0) {
      # Empty file
      model_definitions(NULL)
    } else if (
      # Validating a YAML (based on a JSON schema) using S7schema only
      # supports lower-case extensions (.yaml or .yml, but not .YAML or
      # .YML). Because we use S7schema elsewhere, we only allow lowercase
      # extensions here to remain consistent.
      grepl("^(yaml|yml)$", tools::file_ext(file), ignore.case = FALSE)
    ) {
      # YAML file
      load_model_definitions(file)
    } else {
      # An archive — extract into a per-upload directory so that concurrent
      # uploads from different sessions don't share or overwrite each other's
      # files, and so the YAML selection check below is scoped to this upload.
      temp_dir_path <- tempfile("upload_")
      dir.create(temp_dir_path, recursive = TRUE)
      upload_temp_dir <<- temp_dir_path

      model_definitions(NULL)
      archive_success <- FALSE
      config_files <- c()

      tryCatch(
        {
          files_in_archive <-
            archive::archive_extract(file, dir = temp_dir_path)
          yaml_pattern <- "(\\.yaml|\\.yml)$"
          # Validating a YAML (based on a JSON schema) using S7schema only
          # supports lower-case extensions (.yaml or .yml, but not .YAML or
          # .YML). Because we use S7schema elsewhere, we only allow lowercase
          # extensions here to remain consistent.
          config_files <- files_in_archive[
            grepl(yaml_pattern, files_in_archive, ignore.case = FALSE)
          ]
          # Ignore config files in the __MACOSX directory
          # (added automatically on a Mac)
          config_files <- config_files[!grepl("__MACOSX", config_files)]
          archive_success <- TRUE
        },
        error = function(e) {
          warning(glue::glue("Error loading file: {conditionMessage(e)}"))
        }
      )

      if (!archive_success) {
        err_msg <- paste0(
          "Could not extract the contents of the uploaded file, ",
          "it may be corrupt or in an unsupported format. ",
          "No models were loaded."
        )
        shiny::showModal(errorModal("Error Loading Data", err_msg))
      } else if (length(config_files) == 0) {
        err_msg <- paste0(
          "A YAML configuration file was not found in your archive. ",
          "Please make sure there is at least one .yaml or .yml file ",
          "in your archive (case-sensitive). ",
          "No models were loaded."
        )
        shiny::showModal(errorModal("Error Loading Data", err_msg))
      } else if (length(config_files) > 1) {
        # The names of selections are the full path to the config file within
        # the temporary directory. The values of selections are the values
        # shown to the user in the dialog box (the values can be anything).
        escaped_files <- htmltools::htmlEscape(config_files)
        selections <- stats::setNames(
          file.path(temp_dir_path, config_files),
          escaped_files
        )
        message <- shiny::tagList(
          paste0(
            "Multiple YAML model definitions were found in your archive, ",
            "please select the one to load:"
          ),
          shiny::br(),
          shiny::br(),
          shiny::radioButtons(
            inputId = "select_yaml_radio",
            label = NULL,
            choices = selections,
            selected = unname(selections)[[1]]
          )
        )

        shiny::showModal(yesNoModal(
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
  shiny::observeEvent(input$upload, {
    if (!config_allow_file_uploads()) {
      return()
    }

    # Since the file was uploaded, it does not represent any preloaded
    # algorithms
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
  output$model_message <- shiny::renderUI({
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
      shiny::div(msg, style = "color: #ff0000;", shiny::br(), shiny::br())
    }
  })

  # Populate the main title of the page
  output$ui_title <- shiny::renderText({
    bare_title <- "Algorithm Viewer"
    if (has_model_definitions()) {
      meta <- model_definitions()$meta
      paste0(
        meta$algorithm,
        " v",
        meta$version,
        " ",
        bare_title
      )
    } else {
      bare_title
    }
  })

  # Handle selection from the "Preloaded Algorithms" dropdown
  shiny::observeEvent(input$algorithms, {
    selected_file <- config_get_algorithm_file(input$algorithms)
    if (!is.null(selected_file) && length(selected_file) > 0) {
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
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # OK pressed for dialog box asking to choose which YAML model definitions
  # file to load when multiple YAML files found in an uploaded archive
  shiny::observeEvent(input$select_yaml_ok, {
    selected <- input$select_yaml_radio
    shiny::removeModal()

    # Validate against the specific upload directory, not the shared tempdir(),
    # so the check is scoped to exactly this upload's extracted files.
    if (
      length(selected) > 0 &&
      !is.null(selected) &&
      !is.null(upload_temp_dir) &&
      is_file_descendant_of(selected, upload_temp_dir)
    ) {
      load_model_definitions(selected)
    }
  })

  # Help tab
  output$help <- shiny::renderUI({
    htmltools::includeMarkdown(
      system.file("extdata/help/main.md", package = utils::packageName())
    )
  })
}

app_server
