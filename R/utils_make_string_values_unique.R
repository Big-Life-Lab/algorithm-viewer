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
  # env is the evaluation environment for glue::glue(). Using an isolated
  # environment (parent = emptyenv()) prevents accidental access to the
  # calling frame's variables. The `+` and `-` operators are added explicitly
  # so that arithmetic in the template (e.g. `{num+1}`) still works.
  env <- new.env(parent = emptyenv())
  env[["+"]] <- get("+")
  env[["-"]] <- get("-")
  for (idx in seq_along(values)) {
    env$value <- values[[idx]]
    env$num <- 0
    # Keep generating new values until the value at idx is no longer a
    # duplicate of any earlier element. duplicated() on the prefix
    # [1..idx] returns TRUE for idx only when it matches a prior element.
    while (duplicated(values[seq_len(idx)])[[idx]]) {
      env$num <- env$num + 1
      values[[idx]] <- as.character(glue::glue(template, .envir = env))
    }
  }

  values
}
