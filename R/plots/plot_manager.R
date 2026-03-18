plot_man_keys <- list(
  plot_id_key = "plot_id",
  make_plot_fn_key = "make_plot_fn",
  model_ui_fn_key = "model_ui_fn",
  panel_ui_fn_key = "panel_ui_fn",
  title_key = "title"
)

initialize_plot_manager_env <- function() {
  rlang::env(
    plots = list()
  )
}

plot_man_add_plot <- function(.env, plot_id, title, make_plot_fn, model_ui_fn = NULL, panel_ui_fn = NULL) {
  if (plot_id %in% names(.env$plots)) {
    stop(glue::glue(
      "A plot with ID '{plot_id}' already exists in the plot manager. ",
      "Title of plot being added is '{title}'."
    ))
  }

  entry <- list()

  entry[[plot_man_keys$plot_id_key]] <- plot_id
  entry[[plot_man_keys$title_key]] <- title
  entry[[plot_man_keys$make_plot_fn_key]] <- make_plot_fn
  entry[[plot_man_keys$model_ui_fn_key]] <- model_ui_fn
  entry[[plot_man_keys$panel_ui_fn_key]] <- panel_ui_fn

  .env$plots[[plot_id]] <- entry
}

plot_man_get_make_plot_fn <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$make_plot_fn_key]]
}

plot_man_get_model_ui_fn <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$model_ui_fn_key]]
}

plot_man_get_panel_ui_fn <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$panel_ui_fn_key]]
}

plot_man_get_title <- function(.env, plot_id) {
  .env$plots[[plot_id]][[plot_man_keys$title_key]]
}

plot_man_all_plot_ids <- function(.env) {
  names(.env$plots)
}

plot_man_call_make_plot_fn <- function(.env, .plot_id, ...) {
  plot_man_get_make_plot_fn(.env, .plot_id)(...)
}

plot_man_call_model_ui_fn <- function(.env, .plot_id, ...) {
  fn <- plot_man_get_model_ui_fn(.env, .plot_id)
  if (!is.null(fn)) {
    fn(...)
  }
}

plot_man_call_panel_ui_fn <- function(.env, .plot_id, ...) {
  fn <- plot_man_get_panel_ui_fn(.env, .plot_id)
  if (!is.null(fn)) {
    fn(...)
  }
}