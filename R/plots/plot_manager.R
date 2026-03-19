#' Plot Manager
#'
#' A lightweight registry for generating plots and UI controls related to
#' all the supported plots (eg. Relative Risk plots, Odds Ratio plots, etc.).
#'
#' The UI includes the tabPanels and additional UI elements associated
#' with the plot type (eg. additional controls to specify an exposure
#' group).
#'
#' Each plot module in R/plots/plot-*.R calls \code{plot_man_add_plot()}
#' at load time (via its \code{plot_register} function) to self-register.
#' The \code{plot_register} is returned from each source file as the last
#' line, to indicate which function to call for registration.
#'
#' @section Typical usage:
#' \preformatted{
#' # 1. Create the manager environment (once, at app startup)
#' plot_env <- initialize_plot_manager_env()
#'
#' # 2. Register plots — each plot module exposes a plot_register function
#' #    that is sourced and called with the environment:
#' source("R/plots/plot-10-or.R")   # returns plot_register
#' plot_register(plot_env)
#'
#' # 3. Enumerate all registered plot IDs
#' ids <- plot_man_all_plot_ids(plot_env)
#'
#' # 4. Build the panel UI for a plot
#' new_tab <- tabPanel(
#'   icon = icon("chart-line"),
#'   title = plot_man_get_title(plot_env, "or_plot"),
#'   value = panel_id,
#'   plot_man_call_panel_ui_fn(
#'     plot_env,
#'     "or_plot",
#'     plot_height = "calc(100vh - 170px)"
#'   )
#' )
#'
#' # 5. Render a plot (in a plotly::renderPlotly call)
#' plot_man_call_make_plot_fn(
#'   plot_env,
#'   "or_plot",
#'   session,
#'   models,
#'   model_definitions,
#'   predictor_controls_env,
#'   cached_curve_env
#' )
#' }
#'
#' @name plot_manager
NULL

#' Plot Manager Key Constants
#'
#' Named list of string keys used to store and retrieve fields within each
#' plot entry in the manager environment. Centralising the key names here
#' avoids hard-coded strings throughout the codebase.
#'
#' @format A named list with elements:
#' \describe{
#'   \item{plot_id_key}{Key for the plot's unique identifier string.}
#'   \item{make_plot_fn_key}{Key for the plot-rendering function.}
#'   \item{model_ui_fn_key}{Key for the per-model UI function
#'      (may be \code{NULL}).}
#'   \item{panel_ui_fn_key}{Key for the panel UI function
#'      (may be \code{NULL}).}
#'   \item{title_key}{Key for the human-readable plot title.}
#' }
plot_man_keys <- list(
  plot_id_key = "plot_id",
  make_plot_fn_key = "make_plot_fn",
  model_ui_fn_key = "model_ui_fn",
  panel_ui_fn_key = "panel_ui_fn",
  title_key = "title"
)

#' Initialize a Plot Manager Environment
#'
#' Creates a new \code{rlang} environment that acts as the mutable store for
#' all registered plots. Pass this environment to every other
#' \code{plot_man_*()} function.
#'
#' @return An \code{environment} containing an empty \code{plots} list.
#'
#' @seealso \code{\link{plot_man_add_plot}}
initialize_plot_manager_env <- function() {
  rlang::env(
    plots = list()
  )
}

#' Register a Plot with the Plot Manager
#'
#' Adds a new plot entry to the manager environment.  Attempting to register
#' a plot whose ID is already present raises an error.
#'
#' @param .env The plot manager environment created by
#'   \code{\link{initialize_plot_manager_env}}.
#' @param plot_id A unique character string identifying the plot
#'   (e.g. \code{"or_plot"}).
#' @param title A human-readable title shown in the UI
#'   (e.g. \code{"Odds Ratio"}).
#' @param make_plot_fn A function that renders the plot and returns a
#'   \code{ggplot} (or equivalent) object.  It will be called with whatever
#'   arguments are forwarded by \code{\link{plot_man_call_make_plot_fn}}.
#' @param model_ui_fn Optional function that creates the per-model Shiny UI
#'   elements. Pass \code{NULL} if the plot has no model-specific controls.
#' @param panel_ui_fn Optional function that returns the Shiny panel UI
#'   (e.g. a \code{plotlyOutput}).  Pass \code{NULL} if not needed.
#'
#' @return \code{NULL} invisibly; modifies \code{.env} in place.
#'
#' @seealso \code{\link{plot_man_call_make_plot_fn}},
#'   \code{\link{plot_man_call_panel_ui_fn}}
plot_man_add_plot <- function(
  .env,
  plot_id,
  title,
  make_plot_fn,
  model_ui_fn = NULL,
  panel_ui_fn = NULL
) {
  if (plot_id %in% names(.env$plots)) {
    stop(glue::glue(
      "A plot with ID '{plot_id}' already exists in the plot manager. ",
      "Title of plot being added is '{title}'."
    ))
  }

  # Create the new plot entry
  entry <- list()
  entry[[plot_man_keys$plot_id_key]] <- plot_id
  entry[[plot_man_keys$title_key]] <- title
  entry[[plot_man_keys$make_plot_fn_key]] <- make_plot_fn
  entry[[plot_man_keys$model_ui_fn_key]] <- model_ui_fn
  entry[[plot_man_keys$panel_ui_fn_key]] <- panel_ui_fn

  .env$plots[[plot_id]] <- entry
}

#' Retrieve the Make-Plot Function for a Plot
#'
#' This is the function that generates and returns the plot.
#'
#' @param .env The plot manager environment.
#' @param plot_id Character string identifying the plot.
#'
#' @return The \code{make_plot_fn} registered for \code{plot_id}, or
#'   \code{NULL} if \code{plot_id} is not found.
plot_man_get_make_plot_fn <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$make_plot_fn_key]]
}

#' Retrieve the Model UI Function for a Plot
#'
#' This is the function that creates the UI controls for a plot,
#' that is specific for the loaded models.
#'
#' @param .env The plot manager environment.
#' @param plot_id Character string identifying the plot.
#'
#' @return The \code{model_ui_fn} registered for \code{plot_id}, or
#'   \code{NULL} if none was registered.
plot_man_get_model_ui_fn <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$model_ui_fn_key]]
}

#' Retrieve the Panel UI Function for a Plot
#'
#' This is the function that returns the UI elements that get added
#' to the tabPanel for the plot.
#'
#' @param .env The plot manager environment.
#' @param plot_id Character string identifying the plot.
#'
#' @return The \code{panel_ui_fn} registered for \code{plot_id}, or
#'   \code{NULL} if none was registered.
plot_man_get_panel_ui_fn <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$panel_ui_fn_key]]
}

#' Retrieve the Display Title for a Plot
#'
#' @param .env The plot manager environment.
#' @param plot_id Character string identifying the plot.
#'
#' @return The title string registered for \code{plot_id}, or \code{NULL} if
#'   \code{plot_id} is not found.
plot_man_get_title <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$title_key]]
}

#' List All Registered Plot IDs
#'
#' @param .env The plot manager environment.
#'
#' @return A character vector of all plot IDs currently registered, in
#'   registration order. These IDs can be used in calls to other
#'   plot manager functions.
plot_man_all_plot_ids <- function(.env) {
  names(.env$plots)
}

#' Call the Make-Plot Function for a Plot
#'
#' Looks up \code{make_plot_fn} for the given plot ID and invokes it, passing
#' \code{...} as arguments.
#'
#' The make-plot function generates the plot and is typically called within
#' a plotly::renderPlotly handler.
#'
#' @param .env The plot manager environment.
#' @param .plot_id Character string identifying the plot.  Named with a leading
#'   dot to avoid clashing with arguments forwarded via \code{...}.
#' @param ... Arguments forwarded directly to the plot's \code{make_plot_fn}.
#'
#' @return The return value of the registered \code{make_plot_fn} (typically a
#'   \code{ggplot} object).
plot_man_call_make_plot_fn <- function(.env, .plot_id, ...) {
  plot_man_get_make_plot_fn(.env, .plot_id)(...)
}

#' Call the Model UI Function for a Plot
#'
#' Looks up \code{model_ui_fn} for the given plot ID and invokes it with
#' \code{...}.  Does nothing if no \code{model_ui_fn} was registered.
#'
#' This is the function that creates the UI controls for a plot,
#' that is specific for the loaded models.
#'
#' @param .env The plot manager environment.
#' @param .plot_id Character string identifying the plot.  Named with a leading
#'   dot to avoid clashing with arguments forwarded via \code{...}.
#' @param ... Arguments forwarded directly to the plot's \code{model_ui_fn}.
#'
#' @return The return value of the registered \code{model_ui_fn} (typically a
#'   Shiny tag list), or \code{NULL} if none was registered.
plot_man_call_model_ui_fn <- function(.env, .plot_id, ...) {
  fn <- plot_man_get_model_ui_fn(.env, .plot_id)
  if (!is.null(fn)) {
    fn(...)
  }
}

#' Call the Panel UI Function for a Plot
#'
#' Looks up \code{panel_ui_fn} for the given plot ID and invokes it with
#' \code{...}.  Does nothing if no \code{panel_ui_fn} was registered.
#'
#' This is the function that returns the UI elements that get added
#' to the tabPanel for the plot.
#'
#' @param .env The plot manager environment.
#' @param .plot_id Character string identifying the plot.  Named with a leading
#'   dot to avoid clashing with arguments forwarded via \code{...}.
#' @param ... Arguments forwarded directly to the plot's \code{panel_ui_fn}.
#'
#' @return The return value of the registered \code{panel_ui_fn} (typically a
#'   Shiny tag list), or \code{NULL} if none was registered.
plot_man_call_panel_ui_fn <- function(.env, .plot_id, ...) {
  fn <- plot_man_get_panel_ui_fn(.env, .plot_id)
  if (!is.null(fn)) {
    fn(...)
  }
}
