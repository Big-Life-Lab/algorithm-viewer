#' @title Function Parser
#'
#' @description
#' The function parser parses very basic strings representing function calls.
#' It can parse single function calls, where each parameter is either a single
#' or double quoted string or a number, with named parameters allowed. For
#' example, the following can be parsed:
#'
#'  seq(1, 100, by = 0.1)
#'
#' More complicated parameters are not allowed (eg, 10*10 instead of 100 will
#' not work).
#'
#' @examples
#' \dontrun{
#' # The following call
#' get_function_and_params("seq(1, 10, by = 2)")
#'
#' # Returns:
#' list(
#'   func = "seq",
#'   params = list(
#'     1,
#'     10,
#'     by = 2
#'   )
#' )
#'
#' # The following will return NULL, since 5*2 is not recognized, it must be 10
#' get_function_and_params("seq(1, 5*2, by = 2)")
#' }
#' @name function_parser
NULL

#' Extract Function Name and Parameters from String
#'
#' Parses a string expression in the format "function_name(param1, param2)"
#' and extracts the function name and its parameters.
#'
#' @param s Character string containing a function call expression.
#'
#' @return A list with "func" (function name) and "params" (named list of
#'   parameters), or NULL if parsing fails.
#'
#' @export
get_function_and_params <- function(s) {
  expr <- "^([A-Za-z_\\.][A-Za-z_0-9\\.]*)\\(([^\\(\\)]*)\\)$"
  res <- stringr::str_match(s, expr)
  if (length(res) == 3 && !any(is.na(res))) {
    func_name <- stringr::str_trim(res[2])
    params <- stringr::str_trim(res[3])

    if (stringr::str_length(params) > 0) {
      params <- params |>
        stringr::str_split(",") |>
        unlist() |>
        stringr::str_trim()
      tryCatch(
        {
          params <- .convert_params_to_named_list(params)
        },
        error = function(e) {
          func_name <<- NULL
        }
      )
    } else {
      params <- c()
    }

    if (!is.null(func_name)) {
      return(list(
        "func" = func_name,
        "params" = params
      ))
    }
  }
  NULL
}

#' Parse a Parameter Value from String
#'
#' Converts a string representation of a parameter value to its appropriate
#' R type (string, double, or integer).
#'
#' @param value Character string to parse.
#'
#' @return The parsed value as character, double, or integer.
#'
#' @keywords internal
.parse_param_value <- function(value) {
  if (stringr::str_length(value) >= 2) {
    if ((stringr::str_starts(value, "'") && stringr::str_ends(value, "'")) ||
      (stringr::str_starts(value, '"') && stringr::str_ends(value, '"'))) {
      value <- substr(value, 2, stringr::str_length(value) - 1)
      return(value)
    }
  }
  if (stringr::str_detect(value, ".")) {
    casted_value <- as.double(value)
    if (!is.na(casted_value)) {
      return(casted_value)
    }
    stop(paste("Invalid value", value))
  } else {
    casted_value <- as.integer(value)
    if (!is.na(casted_value)) {
      return(casted_value)
    }
    stop(paste("Invalid value", value))
  }
  stop(paste("Invalid value", value))
}

#' Convert Parameter Strings to Named List
#'
#' Parses a vector of parameter strings into a named list, handling both
#' named (key=value) and positional parameters.
#'
#' @param params Character vector of parameter strings.
#'
#' @return A named list of parsed parameter values.
#'
#' @keywords internal
.convert_params_to_named_list <- function(params) {
  named_list <- list()
  for (param in params) {
    num_equals <- stringr::str_count(param, "=")
    if (num_equals == 1) {
      parts <- param |>
        stringr::str_split("=") |>
        unlist() |>
        stringr::str_trim() |>
        as.list()
      named_list[parts[[1]]] <- .parse_param_value(parts[[2]])
    } else if (num_equals == 0) {
      named_list <- append(named_list, .parse_param_value(param))
    }
  }
  named_list
}
