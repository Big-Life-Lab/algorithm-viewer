#' General HTML/CSS utilities
#'
#' General-purpose helpers for generating HTML tags and working with CSS
#' assets in the Shiny app (e.g. building cache-busting stylesheet links).
#'
#' @name utils_html
#' @noRd
#' @keywords internal
NULL

#' Build a Stylesheet Link Tag with a Content-Hash Cache Buster
#'
#' Creates a \code{<link rel="stylesheet">} tag whose href carries a query
#' string derived from the file's contents, so browsers re-fetch the
#' stylesheet whenever it changes, without manual "?N" version bumps.
#'
#' @param href Character string. The stylesheet path relative to the
#'   resource prefix registered in \code{app_ui} (e.g. "www/csg.css", which
#'   maps to inst/extdata/www/csg.css).
#'
#' @return A \code{shiny.tag} link element.
#'
#' @noRd
#' @keywords internal
stylesheet_link <- function(href) {
  file <- system.file("extdata", href, package = utils::packageName())
  if (nzchar(file)) {
    hash <- substr(digest::digest(file = file), 1, 8)
    href <- paste0(href, "?", hash)
  }
  shiny::tags$link(rel = "stylesheet", href = href)
}
