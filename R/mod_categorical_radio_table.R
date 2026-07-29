#' Categorical Radio Table Shiny Module
#'
#' A Shiny module that renders a multi-column radio button table for a single
#' categorical variable. Each column is an independent radio group (e.g.,
#' "Me" and "Ref"). Rows align correctly even when level labels wrap, and
#' any number of groups is supported.
#'
#' @details
#' `categoricalRadioTableServer()` returns a named list:
#' \describe{
#'   \item{rv_values}{A \code{shiny::reactiveVal()} containing the current
#'     selections as a named list of group key -> internal model value.
#'     E.g., `list(me = "2", ref = "1")`.}
#'   \item{update_values}{A function accepting a named list of
#'     group key -> internal value. Updates both the UI radio buttons and
#'     the internal reactive state.}
#'   \item{destroy}{A function that destroys all observers created by the
#'     module.}
#' }
#'
#' @examples
#' \dontrun{
#' # groups: names = group keys (input ID suffix), values = column headers
#' groups <- c(me = "Me", ref = "Ref")
#' initial_values <- list(me = "2", ref = "1")
#'
#' ui <- categoricalRadioTableUI(
#'   "smoking_ctrl",
#'   model_data,
#'   variable = "smoking_status",
#'   groups = groups,
#'   initial_values = initial_values
#' )
#'
#' ctrl <- categoricalRadioTableServer(
#'   "smoking_ctrl",
#'   model_data,
#'   variable = "smoking_status",
#'   groups = groups,
#'   initial_values = initial_values
#' )
#'
#' # React to changes
#' shiny::observe({
#'   vals <- ctrl$rv_values()   # list(me = "2", ref = "1")
#' })
#'
#' # Reset
#' ctrl$update_values(list(me = "1", ref = "1"))
#'
#' # Tear down
#' ctrl$destroy()
#' }
#'
#' @name mod_categorical_radio_table
#' @noRd
#' @keywords internal
NULL

# Compose the local (un-namespaced) input ID for one group of one variable.
.crt_group_input_id <- function(variable, group_key) {
  paste0(variable, "__", group_key)
}

# One-time singleton: shared JS injected at most once per HTML document.
# Uses htmltools::singleton so Shiny deduplicates it across multiple module
# instances and across renderUI re-renders.
.crt_singleton <- function() {
  shiny::tagList(
    htmltools::singleton(shiny::tagList(
      shiny::tags$script(shiny::HTML("
        (function() {
          /* Guard: run setup once per page,
             even if the tag is rendered again. */
          if (window._crtInitialized) return;
          window._crtInitialized = true;

          /*
          * Read the currently-checked radio for every group in a table and
          * push the values to Shiny.  The data-crt-inited attribute prevents
          * duplicate initialisation for the same table instance.
          */
          function initCrtTable(table) {
            if (!table) return;
            if (table.getAttribute('data-crt-inited')) return;
            if (!window.Shiny || !Shiny.setInputValue) return;
            var raw = table.getAttribute('data-crt-groups');
            if (!raw) return;
            table.setAttribute('data-crt-inited', '1');
            JSON.parse(raw).forEach(function(inputId) {
              var checked = table.querySelector(
                'input[name=\"' + inputId + '\"]:checked'
              );
              if (checked) Shiny.setInputValue(inputId, checked.value);
            });
          }

          /* Initialise tables already in the DOM when Shiny connects. */
          $(document).on('shiny:connected', function() {
            document.querySelectorAll('.crt-radio-table').forEach(initCrtTable);
          });

          /*
          * Watch for tables added dynamically (e.g. via renderUI).
          * MutationObserver is more reliable than shiny:recalculated because
          * it fires as soon as nodes are inserted, before Shiny's own flush.
          */
          $(document).ready(function() {
            var observer = new MutationObserver(function(mutations) {
              if (!window.Shiny || !Shiny.setInputValue) return;
              mutations.forEach(function(mutation) {
                mutation.addedNodes.forEach(function(node) {
                  if (node.nodeType !== 1) return;
                  var tables = (
                    node.classList &&
                    node.classList.contains('crt-radio-table')
                  )
                    ? [node]
                    : Array.from(node.querySelectorAll('.crt-radio-table'));
                  tables.forEach(initCrtTable);
                });
              });
            });
            observer.observe(document.body, { childList: true, subtree: true });
          });

          /* Propagate every user radio-click to Shiny. */
          $(document).on(
            'change',
            '.crt-radio-table input[type=\"radio\"]',
            function() {
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue(this.name, this.value);
              }
            }
          );

          /*
          * Server-driven update: select a radio by (name, value) and
          * immediately push the new value to Shiny so the server observer
          * fires without waiting for a user interaction.
          */
          Shiny.addCustomMessageHandler('crt_update_radio', function(msg) {
            var radio = document.querySelector(
              'input[name=\"' + msg.name + '\"][value=\"' + msg.value + '\"]'
            );
            if (!radio) return;
            radio.checked = true;
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue(msg.name, msg.value);
            }
          });
        })();
      ")))
    ))
}

#' Categorical Radio Table UI
#'
#' Renders a labelled multi-group radio button table for one categorical
#' variable.
#'
#' @param id Character. Module namespace ID.
#' @param model_data Named list of model data.
#' @param variable Character. Name of the categorical variable.
#' @param groups Named character vector. Names are group keys used as input
#'   ID suffixes; values are the column header labels shown in the table.
#'   E.g., \code{c(me = "Me", ref = "Ref")}.  Any number of groups is
#'   supported.
#' @param initial_values Named list. Group key -> initial selected internal
#'   model value (e.g. \code{list(me = "2", ref = "1")}).  Defaults to the
#'   first allowable value for groups that are missing or invalid.
#' @param show_label Logical. If \code{TRUE} (default), display the variable
#'   label above the table.
#'
#' @return A \code{shiny::tagList} containing the table and its required JS/CSS.
#'
#' @noRd
#' @keywords internal
categoricalRadioTableUI <- function(
  id,
  model_data,
  variable,
  groups,
  initial_values = NULL,
  show_label = TRUE
) {
  ns <- shiny::NS(id)

  allowable_values <- get_predictor_allowable_values(model_data, variable)
  level_labels <- get_variable_label_from_value(
    model_data, variable, as.vector(allowable_values)
  )
  group_keys <- names(groups)

  # Full namespaced input ID for every group (used as the HTML name attribute
  # and as the Shiny input key reported via Shiny.setInputValue).
  input_ids <- stats::setNames(
    vapply(
      group_keys,
      function(g) ns(.crt_group_input_id(variable, g)),
      character(1)
    ),
    group_keys
  )

  # Resolve the initially-selected display label for each group.
  selected_labels <- stats::setNames(
    lapply(group_keys, function(g) {
      iv <- if (!is.null(initial_values)) initial_values[[g]] else NULL
      if (!is.null(iv)) {
        lbl <- get_variable_label_from_value(model_data, variable, iv)
        if (!is.null(lbl) && lbl %in% level_labels) return(lbl)
      }
      level_labels[[1]]
    }),
    group_keys
  )

  var_label <- if (show_label) {
    shiny::tags$div(
      style = "font-weight: 700; font-size: 14px;",
      get_variable_label(model_data, variable)
    )
  } else {
    NULL
  }

  # Header row: empty first cell, then one <th> per group.
  header_cells <- c(
    list(shiny::tags$th(var_label)),
    lapply(group_keys, function(g) shiny::tags$th(groups[[g]]))
  )

  get_id <- function(input_id, group_key) {
    glue::glue("{input_id}_{group_key}")
  }

  # One <tr> per allowable level.
  data_rows <- lapply(seq_along(level_labels), function(i) {
    lbl <- level_labels[[i]]
    label_cell <- lbl
    if (length(group_keys) == 1) {
      first_id <- get_id(input_ids[[1]], i)
      label_cell <- shiny::tags$label(
        label_cell, `for` = first_id,
        style = "width: 100%; margin: 0; padding: 0; font-weight: 400"
      )
    }
    label_cell <- shiny::tags$td(label_cell, class = "crt-label-cell")
    radio_cells <- lapply(group_keys, function(g) {
      is_checked <- identical(selected_labels[[g]], lbl)
      extra <- if (is_checked) list(checked = "checked") else list()
      radio <- do.call(
        shiny::tags$input,
        c(
          list(
            type = "radio", name = input_ids[[g]],
            value = lbl, id = get_id(input_ids[[g]], i)
          ),
          extra
        )
      )
      shiny::tags$td(radio, class = "crt-radio-cell")
    })
    do.call(shiny::tags$tr, c(list(label_cell), radio_cells))
  })

  # Encode group input IDs as a JSON array for the MutationObserver init.
  # IDs only contain [A-Za-z0-9_-] so no JSON escaping is needed.
  groups_json <- paste0(
    '["', paste(unname(input_ids), collapse = '","'), '"]'
  )

  table_tag <- shiny::tags$table(
    class = "crt-radio-table",
    `data-crt-groups` = groups_json,
    shiny::tags$thead(do.call(shiny::tags$tr, header_cells)),
    do.call(shiny::tags$tbody, data_rows)
  )

  shiny::tagList(
    .crt_singleton(),
    table_tag
  )
}

#' Categorical Radio Table Server
#'
#' Server-side logic for the categorical radio table.  Tracks which level is
#' selected in each group, converts display labels back to internal model
#' values, and exposes a function for programmatic updates (e.g., Reset).
#'
#' @param id Character. Module namespace ID (must match the UI).
#' @param model_data Named list of model data.
#' @param variable Character. Name of the categorical variable.
#' @param groups Named character vector. Same as passed to the UI function.
#' @param initial_values Named list. Group key -> initial selected internal
#'   model value.  Defaults to the first allowable value.
#'
#' @return A named list:
#'   \describe{
#'     \item{rv_values}{A \code{shiny::reactiveVal()} of group key -> current
#'       internal value.}
#'     \item{update_values}{Function accepting a named list of group key ->
#'       internal value.  Sends a custom message to update the UI and causes
#'       the observers to update \code{rv_values}.}
#'     \item{destroy}{Function that destroys all module observers.}
#'   }
#'
#' @noRd
#' @keywords internal
categoricalRadioTableServer <- function(
  id,
  model_data,
  variable,
  groups,
  initial_values = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    group_keys <- names(groups)
    allowable_values <- get_predictor_allowable_values(model_data, variable)
    allowable_char <- as.character(allowable_values)

    # Internal initial values: fall back to first allowable level if
    # the supplied value is missing or not in the allowable set.
    init_vals <- stats::setNames(
      lapply(group_keys, function(g) {
        # as.character(NULL) is character(0), not NULL, so check length too —
        # a group missing from initial_values must fall back rather than
        # crash the `&&` below with a zero-length condition.
        iv <- if (!is.null(initial_values)) {
          as.character(initial_values[[g]])
        } else {
          NULL
        }
        if (length(iv) == 1 && iv %in% allowable_char) {
          iv
        } else {
          allowable_char[[1]]
        }
      }),
      group_keys
    )

    rv_values <- shiny::reactiveVal(init_vals)

    # One observer per group: label -> internal value -> rv_values.
    observers <- list()
    for (g in group_keys) {
      local({
        # Capture the current value for g, to be used in the observer.
        # If we don't do this, all observers will use the value of g from
        # the last iteration of the for loop
        g <- g
        input_id <- .crt_group_input_id(variable, g)
        # <<- so the observer lands in the module-level `observers` list;
        # a plain <- inside local() would modify a local copy and destroy()
        # would never see (or destroy) these observers.
        observers[[length(observers) + 1]] <<- shiny::observeEvent(
          input[[input_id]],
          {
            val <- get_variable_value_from_label(
              model_data, variable, input[[input_id]]
            )
            # An unknown label yields NA (named-vector subscript miss), not
            # NULL — guard against it so a bad client value can't poison
            # rv_values with NA.
            if (length(val) == 1 && !is.na(val)) {
              cur <- rv_values()
              val_char <- as.character(val)
              if (!identical(cur[[g]], val_char)) {
                cur[[g]] <- val_char
                rv_values(cur)
              }
            }
          },
          ignoreInit = TRUE
        )
      })
    }

    # Send a custom message to JS to update one radio button and let the
    # change propagate back through the normal observer path.
    update_values <- function(new_values) {
      for (g in names(new_values)) {
        if (!g %in% group_keys) next
        lbl <- get_variable_label_from_value(
          model_data, variable, new_values[[g]]
        )
        if (!is.null(lbl)) {
          session$sendCustomMessage("crt_update_radio", list(
            name = session$ns(.crt_group_input_id(variable, g)),
            value = lbl
          ))
        }
      }
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
