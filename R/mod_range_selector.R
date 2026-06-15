#' Range Selector Module
#'
#' A Shiny module providing a numeric min input, a range slider, and a numeric
#' max input for selecting an axis range. Supports logarithmic and linear modes.
#'
#' @name mod_range_selector
#' @noRd
#' @keywords internal
NULL

#' Range Selector UI
#'
#' @param id Character string. Shiny module namespace ID.
#' @param mode Character. \code{"logarithmic"} or \code{"linear"}.
#' @param label Character string, or \code{NULL} to use the mode default label.
#' @param min Numeric. Override the default slider/numeric bounds.
#'   \code{NULL} uses the mode default.
#' @param max Numeric. Override the default slider/numeric bounds.
#'   \code{NULL} uses the mode default.
#' @param value Numeric vector of length 2. Override the default slider initial
#'   value. \code{NULL} uses the mode default.
#' @param step Numeric. Override the default slider and numeric step.
#'   \code{NULL} uses the mode default.
#'
#' @return A \code{\link[shiny]{tagList}} with the range selector controls.
#'
#' @noRd
#' @keywords internal
rangeSelectorUI <- function(
  id,
  mode = c("logarithmic", "linear"),
  label = NULL,
  min   = NULL,
  max   = NULL,
  value = NULL,
  step  = NULL
) {
  mode <- match.arg(mode)
  ns <- shiny::NS(id)

  # Initialize all variables to define the controls
  if (mode == "logarithmic") {
    label    <- label %||% "X Axis Range (log10 scale):"
    init_min <- min   %||% -5
    init_max <- max   %||%  5
    init_val <- value %||% c(-3, 3)
    init_step <- step %||% 0.1
  } else {
    label    <- label %||% "X Axis Range:"
    init_min <- min   %||%  0
    init_max <- max   %||%  500
    init_val <- value %||% c(0, 300)
    init_step <- step %||% 1
  }

  shiny::tagList(
    shiny::tags$label(class = "control-label", label),
    shiny::div(
      style = "display: flex; align-items: center; gap: 8px",
      shiny::numericInput(
        inputId = ns("min"),
        label = NULL, value = init_min, step = init_step, width = "80px"
      ),
      shiny::div(
        style = "flex: 1",
        shiny::sliderInput(
          inputId = ns("range"),
          label = NULL,
          min = init_min, max = init_max, value = init_val, step = init_step,
          width = "100%"
        )
      ),
      shiny::numericInput(
        inputId = ns("max"),
        label = NULL, value = init_max, step = init_step, width = "80px"
      )
    )
  )
}

#' Range Selector Server
#'
#' Manages the observer that keeps the slider bounds in sync with the min/max
#' numeric inputs. Registers its own cleanup on session end, and also returns
#' a \code{destroy} function for explicit early teardown.
#'
#' @param id Character string. Shiny module namespace ID.
#' @param mode Character. \code{"logarithmic"} or \code{"linear"}.
#'
#' @return A list with:
#'   \describe{
#'     \item{\code{range}}{Reactive returning the current slider value.
#'        The value is a vector of two doubles (lower and upper ends of the
#'        range)}
#'     \item{\code{destroy}}{Function that destroys the module's observer.}
#'   }
#'
#' @noRd
#' @keywords internal
rangeSelectorServer <- function(id, mode = c("logarithmic", "linear")) {
  match.arg(mode)
  shiny::moduleServer(id, function(input, output, session) {
    obs <- shiny::observe({
      new_min <- input$min
      new_max <- input$max
      if (is.null(new_min) || is.null(new_max) ||
          is.na(new_min) || is.na(new_max) ||
          new_min >= new_max) return()
      current_val <- shiny::isolate(input$range)
      new_val <- if (!is.null(current_val)) {
        pmax(pmin(current_val, new_max), new_min)
      } else {
        c(new_min, new_max)
      }
      shiny::updateSliderInput(
        session, "range",
        min = new_min, max = new_max, value = new_val
      )
    })

    destroy <- function() obs$destroy()
    session$onSessionEnded(destroy)

    list(
      range   = shiny::reactive(input$range),
      destroy = destroy
    )
  })
}
