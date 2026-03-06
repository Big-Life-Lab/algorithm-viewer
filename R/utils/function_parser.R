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
#' not work). In the future we may want to replace this with a more complete
#' function parse, eg. by using an AST parsing library.
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
  # This expression matches 1. The function name, and 2. The parameters
  # found within brackets (the parameters match does not include the
  # brackets)
  expr <- "^([A-Za-z_\\.][A-Za-z_0-9\\.]*)\\(([^\\(\\)]*)\\)$"
  res <- stringr::str_match(s, expr)
  if (length(res) == 3 && !any(is.na(res))) {
    # Match was found. Get the function name and parse the parameters.
    # At this point the parameters is a single string, such as:
    # 1, 2, by = 3
    func_name <- stringr::str_trim(res[2])
    params <- stringr::str_trim(res[3])

    if (stringr::str_length(params) > 0) {
      # Split by commas. This currently does not ignore commas
      # in quoted strings.
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
#' @param value Character string to parse (eg. "3.5", "'test'", "4").
#'
#' @return The parsed value as character, double, or integer.
#'
#' @keywords internal
.parse_param_value <- function(value) {
  if (stringr::str_length(value) >= 2) {
    if (
      (stringr::str_starts(value, "'") && stringr::str_ends(value, "'")) ||
        (stringr::str_starts(value, '"') && stringr::str_ends(value, '"'))
    ) {
      # A string. It starts or ends with double quotes or single quotes, so
      # remove the quotes.
      value <- substr(value, 2, stringr::str_length(value) - 1)
      return(value)
    }
  }
  if (stringr::str_detect(value, ".")) {
    # There is a decimal point, so cast to a double
    casted_value <- as.double(value)
    if (!is.na(casted_value)) {
      return(casted_value)
    }
    stop(paste("Invalid value", value))
  } else {
    # There is no decimal point, so cast to an integer
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
#' @param params Character vector of parameter strings. These are
#'   the individual parameters, eg. c("1", "2", "by = 3")
#'
#' @return A named list of parsed parameter values.
#'
#' @keywords internal
.convert_params_to_named_list <- function(params) {
  named_list <- list()
  for (param in params) {
    num_equals <- stringr::str_count(param, "=")
    if (num_equals == 1) {
      # This is a named parameter, that has one equal sign.
      # Get the parameter name (eg. "by") and the value
      # (eg. "3")
      parts <- param |>
        stringr::str_split("=") |>
        unlist() |>
        stringr::str_trim() |>
        as.list()
      named_list[parts[[1]]] <- .parse_param_value(parts[[2]])
    } else if (num_equals == 0) {
      # This is an unnamed parameter
      named_list <- append(named_list, .parse_param_value(param))
    }
  }
  named_list
}
