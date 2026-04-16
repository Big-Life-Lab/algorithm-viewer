#' @file url.R
#' @title URL query string utilities
#'
#' @description
#' Helpers for reading and writing the Shiny app's URL query string, with a
#' focus on the algorithm-selection parameter controlled by the app config.
#'
#' @examples
#' \dontrun{
#' # In a Shiny server function, activate an algorithm specified in the URL:
#' observe({
#'   if (url_has_algorithm_id(session)) {
#'     id <- url_get_algorithm_id(session) # e.g. "htnport-full"
#'     # … activate the matching algorithm
#'   }
#' })
#'
#' # Reflect the active algorithm in the browser URL, without triggering a
#' # reload:
#' url_set_algorithm_id("htnport-full", session = session)
#' }
NULL

source("R/utils/config.R")

# When the config file allows specifying algorithms in the URL,
# algorithm_query_param is the parameter name for specifying the
# algorithm ID. eg. If algorithm_query_param <- "algorithm", then
# an example URL would be http://example.com/?algorithm=htnport-full
algorithm_query_param <- "algorithm"

#' Check whether the current URL contains an algorithm ID query parameter
#'
#' This does not verify if the algorithm ID is valid and found in the config,
#' instead it just checks for the presence of the algorithm in the URL.
#'
#' Returns `FALSE` immediately if URL-based algorithm selection is disabled.
#' Otherwise inspects the Shiny session's query string for the given parameter.
#'
#' @param session The Shiny session object.
#' @return `TRUE` if the parameter is present in the URL, `FALSE` otherwise.
url_has_algorithm_id <- function(session) {
  # If algorithms in the URL are not allowed then always return FALSE
  if (!config_allow_algorithm_in_url()) {
    return(FALSE)
  }
  # Check if the URL has a query parameter named algorithm_query_param
  params <- shiny::getQueryString(session)
  !is.null(params[[algorithm_query_param]])
}

#' Get the algorithm ID from the current URL query string
#'
#' Returns `NULL` if URL-based algorithm selection is disabled, if no algorithms
#' are defined in the config, if the query parameter is absent, or if the
#' supplied ID does not match any known algorithm.
#'
#' @param session The Shiny session object.
#' @return The algorithm ID as a character string, or `NULL`.
url_get_algorithm_id <- function(session) {
  # If algorithms in the URL are not allowed, or if a list of algorithms is
  # not available in the config, then always return NULL (no algorithm in URL)
  if (!config_allow_algorithm_in_url() || !config_has_algorithms()) {
    return(NULL)
  }

  # Get the algorithm ID from the URL, and if it is a valid algorithm ID
  # found in the config file then return the ID
  params <- shiny::getQueryString(session)
  query_algorithm_id <- params[[algorithm_query_param]]
  if (is.null(query_algorithm_id)) {
    return(NULL)
  } else if (config_algorithm_id_exists(query_algorithm_id)) {
    return(query_algorithm_id)
  }

  NULL
}

#' Set the algorithm ID in the browser URL query string
#'
#' Constructs a query string from the given algorithm ID and replaces the
#' current browser URL via [url_update_query_string()]. Does not trigger a
#' full page reload.
#'
#' @param algorithm_id A character string with the algorithm ID to set,
#'   e.g. `"htnport-full"`.
#' @param session The Shiny session object.
#' @return Called for its side-effect; returns `NULL` invisibly.
url_set_algorithm_id <- function(algorithm_id, session) {
  query_string <- paste0("?", algorithm_query_param, "=", algorithm_id)
  url_update_query_string(query_string, mode = "replace", session = session)
}

#' Update the browser URL query string
#'
#' A thin wrapper around [shiny::updateQueryString()] that updates the current
#' URL in the browser without triggering a full page reload.
#'
#' @param query_string A character string with the new query string to set,
#'   e.g. `"?algorithm=htnport-full"`.
#' @param mode Either `"replace"` (default, replaces the current history entry)
#'   or `"push"` (adds a new entry to the browser history).
#' @param session The Shiny session object. Defaults to the current reactive
#'   domain via [shiny::getDefaultReactiveDomain()].
#' @return Called for its side-effect; returns `NULL` invisibly.
url_update_query_string <- function(
  query_string,
  mode = "replace",
  session = getDefaultReactiveDomain()
) {
  shiny::updateQueryString(query_string, mode = mode, session = session)
}
