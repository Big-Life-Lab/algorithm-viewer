#' Main UI Tab Panels
#'
#' Defines the main tabset panel structure and helpers for
#' showing, hiding, and constructing the application's plot tabs.
#'
#' @name tab_panels
#' @noRd
#' @keywords internal
NULL

.tabset_id <- "main_tabs"

#' Panel metadata for each plot tab.
#'
#' A named list where each entry describes one tab: its display \code{title},
#' its \code{icon}, and the \code{ui_output_id} of the \code{shiny::uiOutput}
#' that renders the plot.  Keys are short identifiers used by
#' \code{show_tab_panels}.
#'
#' @format A named list of lists with elements \code{title}, \code{icon}, and
#'   \code{ui_output_id}.
#'
#' @noRd
#' @keywords internal
tab_panels <- list(
  or = list(
    title = "Odds Ratio",
    icon = shiny::icon("chart-line"),
    ui_output_id = "or_plot"
  ),
  rr = list(
    title = "Relative Risk",
    icon = shiny::icon("chart-line"),
    ui_output_id = "rr_plot"
  ),
  pr = list(
    title = "Predicted Risk",
    icon = shiny::icon("chart-line"),
    ui_output_id = "pr_plot"
  ),
  rr_exposed_vs_unexposed = list(
    title = "Exposed vs Unexposed",
    icon = shiny::icon("chart-line"),
    ui_output_id = "rr_exposed_vs_unexposed_plot"
  )
)

#' Show and hide plot tabs in the main tabset.
#'
#' Calls \code{shiny::showTab} / \code{shiny::hideTab} so that exactly the tabs
#' in \code{show_panels \\ hide_panels} are visible.
#'
#' @param show_panels Character vector of panel IDs (keys of \code{tab_panels})
#'   to show. Pass \code{NULL} to show all panels (minus those listed in
#'   hide_panels).
#' @param hide_panels Character vector of panel IDs to hide. If an entry is
#'   found in both \code{show_panels} and \code{hide_panels}, then the panel
#'   is hidden (ie. Entries in this vector take precedence over
#'   \code{show_panels}).
#'
#' @return Invisibly \code{NULL}. Called for its side-effects on the UI.
#'
#' @seealso \code{\link{tab_panels}}, \code{\link{make_tab_panels}}
#'
#' @noRd
#' @keywords internal
show_tab_panels <- function(show_panels, hide_panels) {
  # If show_panels is NULL, then show all panels
  if (is.null(show_panels)) {
    show_panels <- names(tab_panels)
  }
  if (is.null(hide_panels)) {
    hide_panels <- list()
  }

  # Check for invalid IDs
  invalid <- c(
    setdiff(show_panels, names(tab_panels)),
    setdiff(hide_panels, names(tab_panels))
  )
  if (length(invalid) > 0) {
    stop(glue::glue(
      "Unrecognized panel IDs: ",
      paste(invalid, collapse = ", ")
    ))
  }

  # Show/hide all panels. We show the panels that are in the
  # set show_panels - hide_panels, and hide all other panels
  show_panels <- setdiff(show_panels, hide_panels)
  for (id in names(tab_panels)) {
    if (id %in% show_panels) {
      shiny::showTab(.tabset_id, tab_panels[[id]]$title)
    } else {
      shiny::hideTab(.tabset_id, tab_panels[[id]]$title)
    }
  }
}

#' Build the main tabset panel UI element.
#'
#' Constructs a \code{shiny::tabsetPanel} containing one tab per entry in
#' \code{tab_panels} plus a trailing Help tab. Intended to be called once
#' during UI construction.
#'
#' @return A \code{shiny.tag} representing the complete tabset, suitable for
#'   embedding directly in the UI definition.
#'
#' @seealso \code{\link{tab_panels}}, \code{\link{show_tab_panels}}
#'
#' @noRd
#' @keywords internal
make_tab_panels <- function() {
  # The arguments passed to tabsetPanel. We will add each tabPanel to this
  panel_args <- list(
    type = "tabs",
    id = .tabset_id
  )

  # Create each of the plots
  for (info in tab_panels) {
    panel_args[[length(panel_args) + 1]] <- shiny::tabPanel(
      info$title,
      icon = info$icon,
      shiny::uiOutput(info$ui_output_id)
    )
  }

  # Add additional panels at the end
  panel_args[[length(panel_args) + 1]] <- shiny::tabPanel(
    "Help",
    icon = shiny::icon("circle-question"),
    style = "max-width: 800px",
    shiny::br(),
    shiny::htmlOutput("help")
  )

  do.call(shiny::tabsetPanel, panel_args)
}
