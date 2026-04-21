#' Launch the Algorithm Viewer Shiny Application
#'
#' Main entry point for the Algorithm Viewer app. Initializes configuration
#' and starts the Shiny server.
#'
#' @param config Path to a YAML configuration file, or \code{NULL} to use
#'   the built-in example HTNPoRT configuration file (located at
#'   inst/extdata/config.yaml).
#' @param port Port number for the Shiny server. Defaults to
#'   \code{getOption("shiny.port")}.
#' @param host Host address for the Shiny server. Defaults to
#'   \code{getOption("shiny.host", "127.0.0.1")}.
#'
#' @return A Shiny app object (invisibly), as returned by
#'   \code{\link[shiny]{shinyApp}}.
#'
#' @export
run_app <- function(
  config = NULL,
  port = getOption("shiny.port"),
  host = getOption("shiny.host", "127.0.0.1")
) {
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = function() load_config(config),
    options = list(
      port = port,
      host = host
    )
  )
}
