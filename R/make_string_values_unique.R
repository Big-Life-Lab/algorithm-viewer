#' Make string values unique
#'
#' Modifies a character vector so that all values are unique. Duplicate values
#' are disambiguated by appending a number using the provided template.
#'
#' For example, if the list list("Female", "Female", "Male", "Female")
#' is passed in, then the returned value will be
#' list("Female", "Female (2)", "Male", "Female (3)")
#'
#' @param values A character vector to make unique.
#' @param template A \code{glue::glue()} template string used to rename
#'   duplicates. The template has access to `value` (the original string) and
#'   `num` (1-based counter, incremented until the result is unique among
#'   preceding values). The template can also contain addition and subtraction
#'   in the string interpolated values. Defaults to `"{value} ({num+1})"`.
#'
#' @return A character vector the same length as `values` with all elements
#'   unique.
#'
#' @noRd
#' @keywords internal
make_string_values_unique <- function(values, template = "{value} ({num+1})") {
  # env is the environment for string interpolating the template.
  # We allow addition and subtraction. We also allow the variables
  # "value" and "num"
  env <- new.env(parent = emptyenv())
  env[["+"]] <- get("+")
  env[["-"]] <- get("-")
  for (idx in seq_along(values)) {
    env$value <- values[[idx]]
    env$num <- 0
    # Generate a new value, based on the template, until the value
    # at idx is not a duplicate of any of the values occurring
    # earlier on in the list
    while (duplicated(values[seq_len(idx)])[[idx]]) {
      env$num <- env$num + 1
      values[[idx]] <- as.character(glue::glue(template, .envir = env))
    }
  }

  values
}
