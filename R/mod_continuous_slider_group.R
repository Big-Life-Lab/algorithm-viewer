#' Continuous Slider Group Shiny Module
#'
#' A Shiny module that renders a stack of independent sliders for a single
#' continuous variable — one per group (e.g., "Me" and "Ref").  All sliders
#' share the same min/max/step range derived from
#' \code{predictor_allowable_values}.  Any number of groups is supported.
#'
#' @details
#' `continuousSliderGroupServer()` returns a named list:
#' \describe{
#'   \item{rv_values}{A \code{shiny::reactiveVal()} containing the current
#'     slider positions as a named list of group key -> numeric value.
#'     E.g., `list(me = 45, ref = 60)`.}
#'   \item{update_values}{A function accepting a named list of
#'     group key -> numeric value.  Updates both the UI sliders and the
#'     internal reactive state.}
#'   \item{destroy}{A function that destroys all observers created by the
#'     module.}
#' }
#'
#' @examples
#' \dontrun{
#' # groups: names = group keys (input ID suffix), values = slider labels
#' groups <- c(me = "Me", ref = "Ref")
#' initial_values <- list(me = 45, ref = 60)
#'
#' ui <- continuousSliderGroupUI(
#'   "age_ctrl",
#'   model_data,
#'   variable       = "age",
#'   groups         = groups,
#'   initial_values = initial_values
#' )
#'
#' ctrl <- continuousSliderGroupServer(
#'   "age_ctrl",
#'   model_data,
#'   variable       = "age",
#'   groups         = groups,
#'   initial_values = initial_values
#' )
#'
#' # React to changes
#' shiny::observe({
#'   vals <- ctrl$rv_values()   # list(me = 45, ref = 60)
#' })
#'
#' # Reset
#' ctrl$update_values(list(me = 50, ref = 50))
#'
#' # Tear down
#' ctrl$destroy()
#' }
#'
#' @name mod_continuous_slider_group
#' @noRd
#' @keywords internal
NULL

# Compose the local (un-namespaced) input ID for one group of one variable.
.csg_group_input_id <- function(variable, group_key) {
  paste0(variable, "__", group_key)
}

# Derive the slider step from the allowable values, matching the logic used
# in predictorControlsUI.
.csg_step <- function(allowable_values) {
  min_val <- min(allowable_values)
  max_val <- max(allowable_values)
  # Detect a consecutive integer range (e.g. 1:100) and use step = 1 so
  # Shiny renders a clean integer slider without floating-point noise.
  is_integer_range <-
    (is.numeric(min_val) && min_val %% 1 == 0) &&
    (is.numeric(max_val) && max_val %% 1 == 0) &&
    (length(allowable_values) == max_val - min_val + 1) &&
    all(min_val:max_val == sort(allowable_values))
  if (is_integer_range) {
    1L
  } else if (length(allowable_values) >= 2) {
    # The slider step is the minimum difference between any consecutive values
    # found in allowable_values (after sorting)
    # signif(5) avoids floating-point representation artifacts.
    gaps <- diff(sort(allowable_values))
    signif(min(gaps), 5)
  } else {
    1L
  }
}

#' Continuous Slider Group UI
#'
#' Renders a variable label followed by one \code{sliderInput} per group,
#' all sharing the same min/max/step range.
#'
#' @param id Character. Module namespace ID.
#' @param model_data Named list of model data.
#' @param variable Character. Name of the continuous variable.
#' @param groups Named character vector. Names are group keys used as input
#'   ID suffixes; values are the slider labels shown to the left of each
#'   slider.  E.g., \code{c(me = "Me", ref = "Ref")}.  Any number of groups
#'   is supported.  All label cells share the same width (sized to the
#'   widest label) so all slider tracks have equal length.
#' @param initial_values Named list. Group key -> initial numeric value.
#'   Values outside \[min, max\] are clamped to the nearest boundary.
#'   Defaults to \code{min(allowable_values)} for omitted groups.
#' @param show_label Logical. If \code{TRUE} (default), display the variable
#'   label (and units, if present) above the sliders.
#'
#' @return A \code{shiny::tagList} containing the label and sliders.
#'
#' @noRd
#' @keywords internal
continuousSliderGroupUI <- function(
  id,
  model_data,
  variable,
  groups,
  initial_values = NULL,
  show_label = TRUE
) {
  ns <- shiny::NS(id)

  allowable_values <- get_predictor_allowable_values(model_data, variable)
  min_val <- min(allowable_values)
  max_val <- max(allowable_values)
  step <- .csg_step(allowable_values)

  group_keys <- names(groups)
  has_labels <- any(nzchar(trimws(unname(groups))))

  resolve_init <- function(g) {
    iv <- if (!is.null(initial_values)) initial_values[[g]] else NULL
    if (!is.null(iv) && is.numeric(iv)) {
      max(min_val, min(max_val, iv))  # clamp to [min, max]
    } else {
      min_val
    }
  }

  var_label <- if (show_label) {
    shiny::tags$div(
      style = "font-weight: 700; font-size: 14px; margin-bottom: 5px;",
      get_variable_label_and_units(model_data, variable)
    )
  } else {
    NULL
  }

  # Each group contributes two consecutive grid cells: a label div (column 1)
  # and a slider wrapper div (column 2).  grid-template-columns: max-content 1fr
  # makes column 1 exactly as wide as the widest label across all rows, so
  # every slider track has the same length regardless of label text.
  # When all labels are empty the label cells are omitted entirely and the grid
  # collapses to a single column so sliders take the full width.
  grid_items <- unlist(
    lapply(group_keys, function(g) {
      label_item <- if (has_labels) {
        list(shiny::tags$div(class = "csg-group-label", groups[[g]]))
      } else {
        list()
      }
      c(
        label_item,
        list(shiny::tags$div(
          class = "csg-slider-wrapper",
          shiny::sliderInput(
            inputId = ns(.csg_group_input_id(variable, g)),
            label = NULL,
            min = min_val,
            max = max_val,
            value = resolve_init(g),
            step = step
          )
        ))
      )
    }),
    recursive = FALSE
  )

  grid_cols <- if (has_labels) "max-content 1fr" else "1fr"
  sliders_grid <- do.call(
    function(...) shiny::tags$div(
      class = "csg-sliders-grid",
      style = paste0("grid-template-columns: ", grid_cols, ";"),
      ...
    ),
    grid_items
  )

  shiny::tagList(
    shiny::tags$div(
      class = "csg-slider-group",
      var_label,
      sliders_grid
    )
  )
}

#' Continuous Slider Group Server
#'
#' Server-side logic for the continuous slider group.  Tracks each slider's
#' current position reactively and exposes a function for programmatic updates
#' (e.g., Reset).
#'
#' @param id Character. Module namespace ID (must match the UI).
#' @param model_data Named list of model data.
#' @param variable Character. Name of the continuous variable.
#' @param groups Named character vector. Same as passed to the UI function.
#' @param initial_values Named list. Group key -> initial numeric value.
#'
#' @return A named list:
#'   \describe{
#'     \item{rv_values}{A \code{shiny::reactiveVal()} of group key -> current
#'       numeric value.}
#'     \item{update_values}{Function accepting a named list of group key ->
#'       numeric value.  Calls \code{updateSliderInput} and updates
#'       \code{rv_values} immediately.}
#'     \item{destroy}{Function that destroys all module observers.}
#'   }
#'
#' @noRd
#' @keywords internal
continuousSliderGroupServer <- function(
  id,
  model_data,
  variable,
  groups,
  initial_values = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    group_keys <- names(groups)
    allowable_values <- get_predictor_allowable_values(model_data, variable)
    min_val <- min(allowable_values)
    max_val <- max(allowable_values)

    resolve_init <- function(g) {
      iv <- if (!is.null(initial_values)) initial_values[[g]] else NULL
      if (!is.null(iv) && is.numeric(iv)) {
        max(min_val, min(max_val, iv))
      } else {
        min_val
      }
    }

    init_vals <- stats::setNames(
      lapply(group_keys, resolve_init),
      group_keys
    )

    rv_values <- shiny::reactiveVal(init_vals)

    # One observer per group: slider value -> rv_values.
    observers <- list()
    for (g in group_keys) {
      local({
        # Capture the current value for g, to be used in the observer.
        # If we don't do this, all observers will use the value of g from
        # the last iteration of the for loop
        g <- g
        input_id <- .csg_group_input_id(variable, g)
        observers[[length(observers) + 1]] <- shiny::observeEvent(
          input[[input_id]],
          {
            val <- input[[input_id]]
            if (!is.null(val)) {
              cur <- rv_values()
              if (!identical(cur[[g]], val)) {
                cur[[g]] <- as.double(val)
                rv_values(cur)
              }
            }
          },
          ignoreInit = TRUE
        )
      })
    }

    # updateSliderInput is called within the module session, so Shiny
    # applies session$ns() automatically — pass the local ID only.
    update_values <- function(new_values) {
      cur <- rv_values()
      for (g in names(new_values)) {
        if (!g %in% group_keys) next
        val <- new_values[[g]]
        if (!is.numeric(val)) next
        val <- max(min_val, min(max_val, val))
        shiny::updateSliderInput(
          session,
          inputId = .csg_group_input_id(variable, g),
          value = val
        )
        cur[[g]] <- val
      }
      rv_values(cur)
    }

    destroy <- function() {
      for (obs in observers) obs$destroy()
    }

    list(
      rv_values = rv_values,
      update_values = update_values,
      destroy = destroy
    )
  })
}
