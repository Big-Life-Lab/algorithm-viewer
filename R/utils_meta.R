#' Package Versions UI
#'
#' Returns a \code{shiny::div} displaying the current version of the Algorithm
#' Viewer package and, optionally, other important packages such as the
#' \pkg{model.parameters.pipeline} package.
#'
#' @param algorithm_viewer_only Logical. If \code{TRUE}, only the Algorithm
#'   Viewer version is shown.  If \code{FALSE} (default), then the version of
#'   other important packages are also included (on separate lines).
#' @param color Character. CSS colour applied to the version text.
#'   Defaults to \code{"#999"}.
#'
#' @return A \code{shiny.tag} (\code{<div>}) containing the version string(s),
#'   e.g.: Algorithm Viewer v0.1.1
#'
#' @noRd
#' @keywords internal
package_versions_ui <- function(
  algorithm_viewer_only = FALSE,
  color = "#999"
) {
  shiny::div(
    style = paste("color:", color),
    paste0("Algorithm Viewer v", utils::packageVersion(utils::packageName())),
    if (!algorithm_viewer_only) {
      shiny::tagList(
        shiny::br(),
        paste0(
          "Model Parameters Pipeline v",
          utils::packageVersion("model.parameters.pipeline")
        )
      )
    }
  )
}