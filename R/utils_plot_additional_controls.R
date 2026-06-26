#' Plot Additional Controls
#'
#' Utility functions for building the row of additional controls (dropdowns
#' and checkboxes) displayed above or alongside a plot, and for populating
#' their predictor choices from the available models.
#'
#' @name utils_plot_additional_controls
#' @noRd
#' @keywords internal
NULL

#' Height of the Additional Controls Row
#'
#' Returns the fixed height, in pixels, reserved for the additional controls
#' row so that plot layouts can account for it.
#'
#' @return A numeric scalar giving the height in pixels.
#'
#' @noRd
#' @keywords internal
plot_additional_controls_height <- function() {
  46
}

#' Container for Additional Plot Controls
#'
#' Wraps one or more control cells (e.g. created by
#' \code{plot_additional_controls_dropdown} or
#' \code{plot_additional_controls_checkbox}) in a single-row table laid out
#' across the full width of the container.
#'
#' @param ... Control cells (\code{shiny.tag} \code{<td>} elements) to place
#'   in the controls row.
#'
#' @return A \code{shiny.tag} \code{<div>} containing the controls table.
#'   The \code{<div>} has the class "plot-additional-controls"
#'
#' @noRd
#' @keywords internal
plot_additional_controls_container <- function(...) {
  shiny::div(
    class = "plot-additional-controls",
    shiny::tags$table(
      style = "width: 100%",
      shiny::tags$tr(
        ...
      )
    )
  )
}

#' Dropdown Cell for the Additional Controls Row
#'
#' Builds a table cell containing a labelled \code{selectInput}, sized to
#' occupy an equal share of the controls row.
#'
#' @param id Character string. The \code{inputId} for the select input.
#' @param label Character string. The label shown to the left of the input;
#' @param choices The choices passed to \code{shiny::selectInput}.
#' @param num_columns Integer. The total number of control cells in the row,
#'   used to compute the cell's width as an equal fraction.
#'
#' @return A \code{shiny.tag} \code{<td>} element.
#'
#' @noRd
#' @keywords internal
plot_additional_controls_dropdown <- function(id, label, choices, num_columns) {
  column_width <- 100 / num_columns
  shiny::tags$td(
    style = paste0("width: ", column_width, "%;"),
    shiny::tags$table(
      style = "width: 100%;",
      shiny::tags$tr(
        shiny::tags$td(
          style = paste(
            "padding-right: 10px; width: 1px;",
            "white-space: nowrap;",
            "font-weight: bold;"
          ),
          paste0(label, ":")
        ),
        shiny::tags$td(
          style = "max-width: 100%;",
          shiny::selectInput(
            inputId = id,
            label = NULL,
            choices = choices
          )
        )
      )
    )
  )
}

#' Checkbox Cell for the Additional Controls Row
#'
#' Builds a table cell containing a \code{checkboxInput}, sized to occupy an
#' equal share of the controls row.
#'
#' @param id Character string. The \code{inputId} for the checkbox input.
#' @param label Character string. The label shown next to the checkbox.
#' @param value Logical. The initial checked state of the checkbox.
#' @param num_columns Integer. The total number of control cells in the row,
#'   used to compute the cell's width as an equal fraction.
#'
#' @return A \code{shiny.tag} \code{<td>} element.
#'
#' @noRd
#' @keywords internal
plot_additional_controls_checkbox <- function(id, label, value, num_columns) {
  column_width <- 100 / num_columns
  shiny::tags$td(
    style = paste0("width: ", column_width, "%;"),
    shiny::checkboxInput(
      id,
      label,
      value = value
    )
  )
}

#' Populate a Predictor Dropdown
#'
#' Updates a select input with the predictor choices gathered from the given
#' models. When no selection is supplied, the first available predictor is
#' selected. When \code{empty} is \code{TRUE}, the input is cleared instead.
#'
#' @param session The Shiny session object.
#' @param id Character string. The \code{inputId} of the select input to
#'   update.
#' @param models The models from which predictor choices are gathered (see
#'   \code{gather_predictor_choices}).
#' @param selected The predictor to select. If \code{NULL}, the first
#'   available predictor is selected. Default is \code{NULL}.
#' @param empty Logical. If \code{TRUE}, clear the input's choices and
#'   selection. Default is \code{FALSE}.
#'
#' @return Invisibly, the result of \code{shiny::updateSelectInput}.
#'
#' @noRd
#' @keywords internal
populate_dropdown_predictors <- function(
  session,
  id,
  models,
  selected = NULL,
  empty = FALSE
) {
  if (empty) {
    shiny::updateSelectInput(
      session,
      id,
      choices = character(0),
      selected = character(0)
    )
    return()
  }

  # Create list of all possible choices (from all models)
  predictor_choices <- gather_predictor_choices(models)

  # If at least one predictor is available, keep the requested selection or
  # default to the first predictor
  if (length(predictor_choices) > 0) {
    selected <- selected %||% predictor_choices[[names(predictor_choices)[[1]]]]
  } else {
    selected <- character(0)
  }

  shiny::updateSelectInput(
    session,
    id,
    choices = predictor_choices,
    selected = selected
  )
}

#' Populate an Interaction Predictor Dropdown
#'
#' Updates a select input with the predictor choices gathered from the given
#' models, prepended with an "empty" selection so that no interaction
#' predictor can be chosen. When no selection is supplied, the empty
#' selection is chosen. When \code{empty} is \code{TRUE}, the input is
#' cleared instead.
#'
#' @param session The Shiny session object.
#' @param id Character string. The \code{inputId} of the select input to
#'   update.
#' @param models The models from which predictor choices are gathered (see
#'   \code{gather_predictor_choices}).
#' @param selected The predictor to select. If \code{NULL}, the empty
#'   selection is chosen. Default is \code{NULL}.
#' @param empty Logical. If \code{TRUE}, clear the input's choices and
#'   selection. Default is \code{FALSE}.
#'
#' @return Invisibly, the result of \code{shiny::updateSelectInput}.
#'
#' @noRd
#' @keywords internal
populate_dropdown_interaction_predictors <- function(
  session,
  id,
  models,
  selected = NULL,
  empty = FALSE
) {
  if (empty) {
    shiny::updateSelectInput(
      session,
      id,
      choices = character(0),
      selected = character(0)
    )
    return()
  }

  # Create list of all possible choices (from all models)
  predictor_choices <- gather_predictor_choices(models)

  # If at least one predictor is available, then add the empty predictor and
  # select it
  if (length(predictor_choices) > 0) {
    new_list <- list()
    new_list[[config_get_empty_selection()]] <- config_get_empty_selection()
    predictor_choices <- c(new_list, predictor_choices)
    selected <- selected %||% config_get_empty_selection()
  } else {
    # No predictors, so select nothing
    selected <- character(0)
  }

  shiny::updateSelectInput(
    session,
    id,
    choices = predictor_choices,
    selected = selected
  )
}