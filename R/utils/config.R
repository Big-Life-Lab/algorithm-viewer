#' Configuration utility functions
#'
#' Helper functions for reading and querying the global application
#' configuration. These utilities abstract access to algorithm definitions,
#' file paths, and feature flags stored in the global config.
#'
#' @name config
NULL

source("R/utils/path_utils.R")

# Load the global configuration. .CONFIG should not be modified with
# user-specific data, it is shared by ALL active sessions so changes
# to the config will affect all users.
if (!exists(".CONFIG")) {
  .CONFIG <- yaml::read_yaml(file.path("data", "config.yaml"))
}

#' Get the file path of the initial algorithm to display on startup
#'
#' Resolves the initial algorithm file by checking, in order:
#' \enumerate{
#'   \item `.CONFIG$initial_algorithm_file` (explicit file path)
#'   \item `.CONFIG$initial_algorithm_id` (look up file by algorithm ID)
#'   \item The first algorithm defined in `.CONFIG$algorithms`
#' }
#'
#' @return A character string with the file path, or `NULL` if none is found.
config_get_initial_algorithm_file <- function() {
  file <- NULL
  if (!is.null(.CONFIG$initial_algorithm_file)) {
    file <- .CONFIG$initial_algorithm_file
  } else if (!is.null(.CONFIG$initial_algorithm_id)) {
    file <- .CONFIG$algorithms[[.CONFIG$initial_algorithm_id]]$file
  } else if (!is.null(.CONFIG$algorithms) && length(.CONFIG$algorithms) > 0) {
    file <- .CONFIG$algorithms[[1]]$file
  }

  file
}

#' Check whether file uploads are permitted
#'
#' Reads `.CONFIG$allow_file_uploads`. If the value is not a logical, the
#' function defaults to `FALSE`.
#'
#' @return `TRUE` if file uploads are enabled, `FALSE` otherwise.
config_allow_file_uploads <- function() {
  config_get_bool("allow_file_uploads", FALSE)
}

#' Get a boolean value from the global config
#'
#' Returns the logical value stored at `.CONFIG[[key]]`. If the value is not a
#' logical, `default` is returned instead.
#'
#' @param key A character string naming the config key to look up.
#' @param default The value to return when the key is absent or not logical.
#' @return A logical value.
config_get_bool <- function(key, default) {
  if (!is.logical(.CONFIG[[key]])) {
    return(default)
  }
  return(.CONFIG[[key]])
}

#' Check whether algorithm selection is permitted (ie. we can display a list of
#' algorithms for the user to select from).
#'
#' Returns `TRUE` when the config defines at least one algorithm and
#' `.CONFIG$allow_algorithms_selection` is `TRUE` (the default).
#'
#' @return `TRUE` if algorithm selection is enabled, `FALSE` otherwise.
config_allow_algorithms_selection <- function() {
  # Users can select from a dropdown of algorithms if the config has a list of
  # algorithms to choose from and allow_algorithms_selection has been set to TRUE
  config_has_algorithms() && config_get_bool("allow_algorithms_selection", TRUE)
}

#' Check whether an algorithm ID may be passed via the URL.
#'
#' Reads `.CONFIG$allow_algorithm_in_url`. Defaults to `TRUE` when the value is
#' absent or not a logical.
#'
#' @return `TRUE` if URL-based algorithm selection is enabled, `FALSE` otherwise.
config_allow_algorithm_in_url <- function() {
  config_get_bool("allow_algorithm_in_url", TRUE)
}

#' Check whether an algorithm ID is defined in the config
#'
#' @param algorithm_id A character string to look up in `.CONFIG$algorithms`.
#' @return `TRUE` if the ID is present, `FALSE` otherwise.
config_algorithm_id_exists <- function(algorithm_id) {
  # Check if the algorithm ID is in the list of algorithms in the config
  # file
  algorithm_id %in% names(.CONFIG$algorithms)
}

#' Check whether any algorithms are defined in the config
#'
#' @return `TRUE` if `.CONFIG$algorithms` contains at least one entry,
#'   `FALSE` otherwise.
config_has_algorithms <- function() {
  length(.CONFIG$algorithms) > 0
}

#' Get the normalised file path for a given algorithm ID
#'
#' Looks up the raw file path stored under `.CONFIG$algorithms[[algorithm_id]]`
#' and attempts to normalise it with [base::normalizePath()]. Returns `NULL` if
#' the path cannot be resolved (e.g. the file does not exist).
#'
#' @param algorithm_id A character string identifying the algorithm.
#' @return A normalised character file path, or `NULL` if resolution fails.
config_get_algorithm_file <- function(algorithm_id) {
  expand_and_normalize_path(.CONFIG$algorithms[[algorithm_id]]$file)
}

#' Get algorithm choices for display in a selection input
#'
#' Builds a named list suitable for use with Shiny's `selectInput()` (or
#' similar). The names are human-readable algorithm titles; the values are the
#' corresponding algorithm IDs taken from `.CONFIG$algorithms`.
#'
#' @return A named list where each name is a display title and each value is
#'   the algorithm ID string.
config_get_algorithm_choices <- function() {
  # Get all choices for algorithms (eg. to show in a "Preloaded Algorithms"
  # dropdown)
  # The returned value is a list, the names are displayed text and the values
  # are the algorithm IDs
  choices <- .CONFIG$algorithms |>
    lapply(function(x) x$title)
  choices <- setNames(names(choices), unname(unlist(choices)))

  choices
}

#' Look up the algorithm ID that corresponds to a given file path
#'
#' Normalises `file` and then compares it against the normalised paths of all
#' algorithms in `.CONFIG$algorithms`. Returns the ID of the first match.
#'
#' @param file A character string containing the (possibly un-normalised) path
#'   to an algorithm file.
#' @return The algorithm ID as a character string, or `NULL` if no matching
#'   algorithm is found or the path cannot be normalised.
config_get_algorithm_id_from_file <- function(file) {
  # Try to normalize the file path
  file <- expand_and_normalize_path(file)

  if (is.null(file)) {
    # Could not normalize the file (path doesn't exist)
    return(NULL)
  }

  # Get all files specified in the config
  files <- .CONFIG$algorithms |>
    lapply(function(x) normalizePath(x$file))
  # Get the matching indices
  matches <- which(files == file)
  if (length(matches) > 0) {
    # An algorithm with a matching file was found, return its id
    return(names(matches)[[1]])
  }

  NULL
}
