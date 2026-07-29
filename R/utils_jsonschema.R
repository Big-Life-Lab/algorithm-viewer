#' Parse JSON Schema Validation Errors
#'
#' Formats a data frame of validation results from
#' \code{rjsoncons::j_schema_validate} into a human-readable error string.
#'
#' @param validate_result A data frame returned by
#'   \code{rjsoncons::j_schema_validate} with at least \code{valid},
#'   \code{instanceLocation}, and \code{error} columns.
#' @param html Logical. If \code{TRUE}, wraps errors in an HTML \code{<ul>/<li>}
#'   list; otherwise formats as a plain-text bulleted list. Default \code{TRUE}.
#'
#' @return A character string of formatted errors, or \code{NULL} if there are
#'   none.
#'
#' @noRd
#' @keywords internal
.parse_jsonschema_error <- function(validate_result, html = TRUE) {
  results <- list()
  for (i in seq_len(nrow(validate_result))) {
    row <- validate_result[i,]
    if (is.na(row$valid) || !is.logical(row$valid) || row$valid) {
      next()
    }
    result <- paste0(row$instanceLocation, ": ", row$error)
    results[[length(results) + 1]] <- result
  }

  if (length(results) == 0) {
    return(NULL)
  }

  if (html) {
    results_str <- paste0("<li>", results, "</li>", collapse = "")
    results_str <- paste0("<ul>", results_str, "</ul>")
  } else {
    results_str <- paste0("  - ", results, collapse = "\n")
  }

  results_str
}

#' Read and Validate a YAML File Against a JSON Schema
#'
#' Reads a YAML file, converts it to JSON, and validates it against a JSON
#' Schema. Stops with a formatted error message if validation fails.
#'
#' @param yaml_file Path to the YAML file to read.
#' @param json_schema_file Path to the JSON Schema file used for validation.
#' @param error_html Logical. If \code{TRUE}, validation errors are formatted as
#'   HTML; otherwise as plain text. Default \code{TRUE}.
#'
#' @return The parsed YAML data (as an R list) if validation passes.
#'   Calls \code{stop()} with a condition of class \code{yaml_validation_error}
#'   if validation fails.
#'
#' @noRd
#' @keywords internal
read_and_validate_yaml <- function(
  yaml_file,
  json_schema_file,
  error_html = TRUE
) {
  data <- yaml::read_yaml(yaml_file)
  data_str <- jsonlite::toJSON(data, auto_unbox = TRUE)
  res <- rjsoncons::j_schema_validate(
    data_str,
    json_schema_file,
    as = "data.frame"
  )
  if (nrow(res) == 0) {
    return(data)
  }
  stop(make_error(
    "yaml_validation_error",
    .parse_jsonschema_error(res, html = error_html)
  ))
}
