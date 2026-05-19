#' Predictor Controls Shiny Module
#'
#' A Shiny module that creates and manages predictor controls for one or more
#' models. When multiple models are provided, each becomes a labelled group
#' column in the controls. When a single model is provided, no group column
#' headers are shown and the model title appears as a sticky heading.
#'
#' @details
#' `predictorControlsServer()` returns a named list:
#' \describe{
#'   \item{rv_values}{A \code{shiny::reactiveVal()} containing a named list
#'     keyed by \code{model_id}. Each value is itself a named list keyed by
#'     variable name holding the current selected value for that
#'     model/variable combination.}
#'   \item{destroy}{A function that destroys the module's observers and UI
#'     elements. Call this before recreating the module.}
#' }
#'
#' @param model_data Either a single model data named list (with at least
#'   \code{model_id}, \code{title}, and \code{reference_group}), or an
#'   \emph{unnamed} list of such model data objects (one per group column).
#'   A named list is always treated as a single model. An unnamed list of
#'   length 1 is also treated as a single model. An unnamed list of length > 1
#'   activates multi-group mode.
#'
#' @examples
#' \dontrun{
#' # Single model
#' model_data <- model_definitions$models[["female"]]
#' ui <- predictorControlsUI("ctrl", model_data)
#' ctrl <- predictorControlsServer("ctrl", model_data)
#' ctrl$rv_values()  # list(female = list(age = 45, sex = "2", ...))
#'
#' # Multiple models as groups
#' models <- unname(model_definitions$models)
#' ui <- predictorControlsUI("ctrl", models)
#' ctrl <- predictorControlsServer("ctrl", models)
#' ctrl$rv_values()  # list(female = list(...), male = list(...))
#'
#' ctrl$destroy()
#' }
#' @name predictor_controls
#' @noRd
#' @keywords internal
NULL

# The ID of the generated HTML element that contains the predictor controls
.predictor_controls_container_id <- "predictor_controls_container"

# Normalize model_data to an unnamed list of one or more model data objects.
# A named list (single model) is wrapped; an unnamed list is returned as-is.
.normalize_model_data <- function(model_data) {
  if (!is.null(names(model_data))) list(model_data) else model_data
}

#' Create Predictor Controls UI
#'
#' Builds the UI for one or more models' predictor controls. Initial values
#' are taken from each model's \code{reference_group}.
#'
#' @param id Character string. The module namespace ID.
#' @param model_data Single model data named list, or an unnamed list of model
#'   data objects. See \code{?predictor_controls} for details.
#' @param model_name Character string. Display name in the sticky heading and
#'   reset button label. Used only in single-model mode; defaults to
#'   \code{model_data$title}.
#' @param show_model_color Logical. If \code{TRUE} (default), adds a colored
#'   left border in single-model mode.
#'
#' @return A \code{shiny::tagList()} containing the predictor controls UI.
#'
#' @noRd
#' @keywords internal
predictorControlsUI <- function(
  id,
  model_data,
  model_name = NULL,
  show_model_color = TRUE
) {
  ns <- shiny::NS(id)

  models        <- .normalize_model_data(model_data)
  is_multi      <- length(models) > 1
  primary       <- models[[1]]
  model_ids     <- vapply(models, function(m) m$model_id, character(1))

  # Sub-module groups: names = model_ids, values = column header labels
  groups <- if (is_multi) {
    stats::setNames(
      vapply(models, function(m) m$title, character(1)), model_ids
    )
  } else {
    stats::setNames("", model_ids[[1]])
  }

  # Initial values per model, from each model's reference_group
  init_by_model <- stats::setNames(
    lapply(models, function(m) m$reference_group),
    model_ids
  )

  left_pad <- ifelse(!is_multi && show_model_color, "10px", "0")
  ui_parts <- shiny::tagList()

  # Sticky heading: single-model mode only
  if (!is_multi) {
    display_name <-
      if (!is.null(model_name))
        model_name
      else
        primary$title
    heading_content <- cleanup_string(display_name)
    if (show_model_color) {
      heading_content <- shiny::HTML(add_model_color(
        primary, cleanup_string(display_name), "20px", "20px", after = FALSE
      ))
    }
    ui_parts[[length(ui_parts) + 1]] <- shiny::h4(
      heading_content,
      style = paste(
        "position: sticky; top: 0; background-color: #fff;",
        "margin: 0; z-index: 10;",
        glue::glue("padding: 10px 0 8px {left_pad};")
      )
    )
  }

  # Variable controls
  variable_names <- names(init_by_model[[model_ids[[1]]]])
  items <- list()
  for (variable in variable_names) {
    initial_values <- stats::setNames(
      lapply(model_ids, function(mid) init_by_model[[mid]][[variable]]),
      model_ids
    )
    input_id <- ns(variable)
    if (is_variable_categorical(primary, variable)) {
      items[[length(items) + 1]] <- categoricalRadioTableUI(
        input_id, primary,
        variable = variable, groups = groups, initial_values = initial_values
      )
    } else {
      items[[length(items) + 1]] <- continuousSliderGroupUI(
        input_id, primary,
        variable = variable, groups = groups, initial_values = initial_values
      )
    }
  }

  ui_parts[[length(ui_parts) + 1]] <- shiny::div(
    style = paste(
      "width: 100%;", "overflow-x: hidden;",
      glue::glue(
        "padding: {if (is_multi) '20px' else '5px'} 15px 0 {left_pad};"
      )
    ),
    items
  )

  # Sticky reset button
  display_name <-
    if (!is.null(model_name))
      model_name
        else
      primary$title
  reset_label  <- 
    if (is_multi)
      "Reset"
    else
      glue::glue("Reset {display_name}")
  ui_parts[[length(ui_parts) + 1]] <- shiny::div(
    style = paste(
      "position: sticky; bottom: 0; background-color: #fff;",
      "margin: 0; z-index: 9; overflow-x: hidden; width: 100%;",
      glue::glue("padding: 10px 0 7px {left_pad};")
    ),
    shiny::actionButton(
      ns("reset_button"),
      label = reset_label,
      icon  = shiny::icon("arrow-rotate-left"),
      style = "height: 34px;"
    )
  )

  # Outer container (colored border in single-model mode)
  container_style <- "width: 100%; "
  if (!is_multi && show_model_color) {
    model_color    <- unname(get_model_colors(list(primary)))[[1]]
    container_style <- paste(
      container_style,
      glue::glue("border-left: solid 6px {model_color}; ")
    )
  }

  shiny::tagList(shiny::div(
    style = container_style,
    id    = ns(.predictor_controls_container_id),
    ui_parts
  ))
}

#' Predictor Controls Server Logic
#'
#' @param id Character string. The module namespace ID (must match
#'   \code{predictorControlsUI()}).
#' @param model_data Single model data named list, or an unnamed list of model
#'   data objects. See \code{?predictor_controls} for details.
#'
#' @return A named list:
#'   \describe{
#'     \item{destroy}{Function. Destroys the module's observers and UI.}
#'     \item{rv_values}{A \code{shiny::reactiveVal()} holding a named list
#'       keyed by \code{model_id}; each value is a named list keyed by
#'       variable name.}
#'   }
#'
#' @noRd
#' @keywords internal
predictorControlsServer <- function(id, model_data) {
  shiny::moduleServer(id, function(input, output, session) {
    models    <- .normalize_model_data(model_data)
    is_multi  <- length(models) > 1
    primary   <- models[[1]]
    model_ids <- vapply(models, function(m) m$model_id, character(1))

    groups <- if (is_multi) {
      stats::setNames(vapply(models, function(m) m$title, character(1)), model_ids)
    } else {
      stats::setNames("", model_ids[[1]])
    }

    # Default values: model_id -> variable -> value
    # Categorical values are coerced to character.
    default_predictor_values <- stats::setNames(
      lapply(models, function(m) {
        vals <- m$reference_group
        for (v in names(vals)) {
          if (is_variable_categorical(m, v)) {
            vals[[v]] <- as.character(vals[[v]])
          }
        }
        vals
      }),
      model_ids
    )

    variable_names <- names(default_predictor_values[[model_ids[[1]]]])
    objects <- rlang::env(
      observers = list(),
      servers = list()
    )
    predictor_values_internal <- shiny::reactiveVal(default_predictor_values)

    # Sub-module servers (one per variable)
    for (variable in variable_names) {
      initial_values <- stats::setNames(
        lapply(
          model_ids,
          function(mid) default_predictor_values[[mid]][[variable]]
        ),
        model_ids
      )
      input_id <- variable

      if (is_variable_categorical(primary, variable)) {
        objects$servers[[variable]] <- categoricalRadioTableServer(
          input_id, primary,
          variable = variable, groups = groups, initial_values = initial_values
        )
      } else {
        objects$servers[[variable]] <- continuousSliderGroupServer(
          input_id, primary,
          variable = variable, groups = groups, initial_values = initial_values
        )
      }

      local({
        # Capture the current value for variable, to be used in the observer.
        # If we don't do this, all observers will use the value of variable from
        # the last iteration of the for loop
        variable <- variable
        objects$observers[[length(objects$observers) + 1]] <- shiny::observeEvent(
          objects$servers[[variable]]$rv_values(),
          {
            # Save the UI value
            save_values_from_ui(c(variable))
          },
          ignoreInit  = TRUE
        )
      })
    }

    # Reset button observer
    objects$observers[[length(objects$observers) + 1]] <- shiny::observeEvent(
      input$reset_button,
      {
        predictor_values_internal(default_predictor_values)
        set_ui_from_values()
      },
      ignoreInit  = TRUE
    )

    # Pull the current UI values for the given variables and store them.
    save_values_from_ui <- function(variables = NULL) {
      # isolate() prevents reading rv_values() and predictor_values_internal()
      # from establishing reactive dependencies inside this helper, which is
      # already called from within an observeEvent (itself reactive).
      isolate({
        variables   <- if (is.null(variables)) variable_names
                       else intersect(variables, variable_names)
        saved_values <- predictor_values_internal()
        for (variable in variables) {
          group_vals <- objects$servers[[variable]]$rv_values()
          for (mid in model_ids) {
            val <- group_vals[[mid]]
            if (!is.null(val)) saved_values[[mid]][[variable]] <- val
          }
        }
      })
      predictor_values_internal(saved_values)
    }

    # Push the internally stored values back into the UI sub-modules.
    set_ui_from_values <- function() {
      cur_values <- predictor_values_internal()
      for (variable in variable_names) {
        group_vals <- stats::setNames(
          lapply(model_ids, function(mid) cur_values[[mid]][[variable]]),
          model_ids
        )
        objects$servers[[variable]]$update_values(group_vals)
      }
    }

    destroy <- function() {
      for (obs in objects$observers) obs$destroy()
      for (srv in objects$servers) srv$destroy()
      objects$observers <- list()
      objects$servers <- list()
      shiny::removeUI(
        selector  = paste0("#", session$ns(.predictor_controls_container_id)),
        immediate = TRUE
      )
    }

    list(destroy = destroy, rv_values = predictor_values_internal)
  })
}
