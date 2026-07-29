# List of packages for package_versions_ui function to always show
# (whether algorithm_viewer_only is TRUE or FALSE). Fields are:
#   \code{$title} descriptive title to show
#   \code{$package} package name (eg. model.parameters.pipeline)
.package_versions_ui_main_packages <- list(
  list(
    title = "Algorithm Viewer",
    package = utils::packageName()
  )
)

# List of packages for package_versions_ui function to show (in addition to
# main_packages) only when algorithm_viewer_only is FALSE. Fields are:
#   \code{$title} descriptive title to show
#   \code{$package} package name (eg. model.parameters.pipeline)
.package_versions_ui_other_packages <- list(
  list(
    title = "Model Parameters Pipeline",
    package = "model.parameters.pipeline"
  )
)

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
  # Gather HTML of packages and versions associated with the packages named
  # list. Each element in the packages list is a named list with names
  # $title (descriptive title to show for the package) and $package
  # (R package to get the version of eg. "model.parameters.pipeline")
  make_packages_html <- function(packages) {
    v <- shiny::tagList()
    for (info in packages) {
      if (length(v) > 0) v[[length(v) + 1]] <- shiny::br()
      v[[length(v) + 1]] <- paste0(
        info$title,
        " v",
        utils::packageVersion(info$package)
      )
    }
    v
  }

  shiny::div(
    style = paste("color:", color),
    make_packages_html(.package_versions_ui_main_packages),
    if (!algorithm_viewer_only) {
      shiny::tagList(
        shiny::br(),
        make_packages_html(.package_versions_ui_other_packages)
      )
    }
  )
}