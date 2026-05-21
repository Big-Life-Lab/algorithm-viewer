#' Configuration utility functions
#'
#' Helper functions for reading and querying the global application
#' configuration. These utilities abstract access to algorithm definitions,
#' file paths, and feature flags stored in the global config.
#'
#' @name fct_config
#' @noRd
#' @keywords internal
NULL

.CONFIG <- new.env(parent = emptyenv())

#' Load the global application configuration
#'
#' Reads a YAML config file into the shared \code{.CONFIG} environment.
#' \code{.CONFIG} is shared across all active sessions, so changes affect
#' every user; it should not be modified with session-specific data.
#'
#' @param config Path to a YAML configuration file, or \code{NULL} to use
#'   the built-in default at \code{inst/extdata/config.yaml}.
#'
#' @return NULL (called for side effects).
#'
#' @noRd
#' @keywords internal
load_config <- function(config) {
  if (is.null(config)) {
    config <- system.file("extdata/config.yaml", package = utils::packageName())
  }

  schema_file <- system.file(
    "extdata/schema/config.schema.json",
    package = utils::packageName()
  )
  tryCatch(
    {
      # Load and validate the file
      data <- read_and_validate_yaml(config, schema_file, error_html = FALSE)
    },
    error = function(e) {
      stop(paste0(
        "Error with configuration file ",
        htmltools::htmlEscape(basename(config)),
        ":\n",
        conditionMessage(e)
      ))
    }
  )

  list2env(data, .CONFIG)

  # Save the path, so we can retrieve it with config_get_path().
  .CONFIG$config_path__ <- config
}

#' Get the path to the loaded configuration file
#'
#' @return Character string with the path to the currently loaded config file.
#'
#' @noRd
#' @keywords internal
config_get_path <- function() {
  .CONFIG$config_path__
}

#' Get the string that represents an empty selection.
#'
#' This is the ID and name for the empty value for UI selections (eg. in the
#' "Interaction Predictor" dropdown to specify that we want no interaction
#' predictor)
#'
#' @return A character string used for the empty selection (eg. "<empty>")
#'
#' @noRd
#' @keywords internal
config_get_empty_selection <- function() {
  "<empty>"
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
#'
#' @noRd
#' @keywords internal
config_get_initial_algorithm_file <- function() {
  file <- NULL
  if (!is.null(.CONFIG$initial_algorithm_file)) {
    file <- .CONFIG$initial_algorithm_file
  } else if (!is.null(.CONFIG$initial_algorithm_id)) {
    file <- .CONFIG$algorithms[[.CONFIG$initial_algorithm_id]]$file
  } else if (!is.null(.CONFIG$algorithms) && length(.CONFIG$algorithms) > 0) {
    file <- .CONFIG$algorithms[[1]]$file
  }

  file <- config_expand_and_normalize_algorithm_file(file)

  file
}

#' Resolve and normalise an algorithm file path
#'
#' If `file` is a relative path, it is resolved relative to the directory
#' containing the config file (via \code{config_get_path()}). The result is then
#' expanded and normalised with \code{expand_and_normalize_path()}.
#'
#' @param file A character string giving an absolute or relative file path.
#' @return A normalised absolute character file path, or `NULL` if normalisation
#'   fails (e.g. the file does not exist).
#'
#' @noRd
#' @keywords internal
config_expand_and_normalize_algorithm_file <- function(file) {
  if (is.null(file)) {
    return(NULL)
  }

  # If the file is not an absolute path, then make it relative
  # to the directory containing the config file.
  if (!R.utils::isAbsolutePath(file)) {
    file <- file.path(dirname(config_get_path()), file)
  }

  expand_and_normalize_path(file)
}

#' Check whether file uploads are permitted
#'
#' Reads `.CONFIG$allow_file_uploads`. If the value is not a logical, the
#' function defaults to `FALSE`.
#'
#' @return `TRUE` if file uploads are enabled, `FALSE` otherwise.
#'
#' @noRd
#' @keywords internal
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
#'
#' @noRd
#' @keywords internal
config_get_bool <- function(key, default) {
  if (!is.logical(.CONFIG[[key]])) {
    return(default)
  }
  .CONFIG[[key]]
}

#' Check whether algorithm selection is permitted (ie. we can display a list of
#' algorithms for the user to select from).
#'
#' Returns `TRUE` when the config defines at least one algorithm and
#' `.CONFIG$allow_algorithms_selection` is `TRUE` (the default).
#'
#' @return `TRUE` if algorithm selection is enabled, `FALSE` otherwise.
#'
#' @noRd
#' @keywords internal
config_allow_algorithms_selection <- function() {
  # Users can select from a dropdown of algorithms if the config has a list of
  # algorithms to choose from and allow_algorithms_selection has been set to
  # TRUE
  config_has_algorithms() && config_get_bool("allow_algorithms_selection", TRUE)
}

#' Check whether an algorithm ID may be passed via the URL.
#'
#' Reads `.CONFIG$allow_algorithm_in_url`. Defaults to `TRUE` when the value is
#' absent or not a logical.
#'
#' @return `TRUE` if URL-based algorithm selection is enabled, `FALSE`
#'   otherwise.
#'
#' @noRd
#' @keywords internal
config_allow_algorithm_in_url <- function() {
  config_get_bool("allow_algorithm_in_url", TRUE)
}

#' Check whether an algorithm ID is defined in the config
#'
#' @param algorithm_id A character string to look up in `.CONFIG$algorithms`.
#' @return `TRUE` if the ID is present, `FALSE` otherwise.
#'
#' @noRd
#' @keywords internal
config_algorithm_id_exists <- function(algorithm_id) {
  # Check if the algorithm ID is in the list of algorithms in the config
  # file
  algorithm_id %in% names(.CONFIG$algorithms)
}

#' Check whether any algorithms are defined in the config
#'
#' @return `TRUE` if `.CONFIG$algorithms` contains at least one entry,
#'   `FALSE` otherwise.
#'
#' @noRd
#' @keywords internal
config_has_algorithms <- function() {
  length(.CONFIG$algorithms) > 0
}

#' Get the normalised file path for a given algorithm ID
#'
#' Looks up the raw file path stored under `.CONFIG$algorithms[[algorithm_id]]`
#' and attempts to normalise it with \code{base::normalizePath()}. Returns
#' `NULL` if the path cannot be resolved (e.g. the file does not exist).
#'
#' @param algorithm_id A character string identifying the algorithm.
#' @return A normalised character file path, or `NULL` if resolution fails.
#'
#' @noRd
#' @keywords internal
config_get_algorithm_file <- function(algorithm_id) {
  if (is.null(algorithm_id) || !algorithm_id %in% names(.CONFIG$algorithms)) {
    return(NULL)
  }
  config_expand_and_normalize_algorithm_file(
    .CONFIG$algorithms[[algorithm_id]]$file
  )
}

#' Get algorithm choices for display in a selection input
#'
#' Builds a named list suitable for use with Shiny's `selectInput()` (or
#' similar). The names are human-readable algorithm titles; the values are the
#' corresponding algorithm IDs taken from `.CONFIG$algorithms`.
#'
#' @return A named list where each name is a display title and each value is
#'   the algorithm ID string.
#'
#' @noRd
#' @keywords internal
config_get_algorithm_choices <- function() {
  # Get all choices for algorithms (eg. to show in a "Preloaded Algorithms"
  # dropdown)
  # The returned value is a list, the names are displayed text and the values
  # are the algorithm IDs
  choices <- .CONFIG$algorithms |>
    lapply(function(x) x$title)
  choices <- stats::setNames(names(choices), unname(unlist(choices)))

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
#'
#' @noRd
#' @keywords internal
config_get_algorithm_id_from_file <- function(file) {
  # Try to normalize the file path
  file <- config_expand_and_normalize_algorithm_file(file)

  if (is.null(file)) {
    # Could not normalize the file (path doesn't exist)
    return(NULL)
  }

  # Get all files specified in the config
  files <- .CONFIG$algorithms |>
    lapply(function(x) config_expand_and_normalize_algorithm_file(x$file))
  # Get the matching indices
  matches <- which(files == file)
  if (length(matches) > 0) {
    # An algorithm with a matching file was found, return its id
    return(names(matches)[[1]])
  }

  NULL
}
