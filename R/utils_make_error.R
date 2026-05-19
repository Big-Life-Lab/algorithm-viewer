#' Create a classed error condition
#'
#' Constructs an error condition object with a custom class, suitable for use
#' with [stop()] to raise typed exceptions that can be caught selectively with
#' [tryCatch()] or [withCallingHandlers()].
#'
#' @param error_class A character string naming the error class (e.g.
#'   `"my_package_error"`). This is prepended to `c("error", "condition")` so
#'   that the condition inherits from the base error class.
#' @param ... Character strings that are concatenated (with no separator) to
#'   form the human-readable error message, analogous to the `...` argument of
#'   [stop()].
#'
#' @return A condition object of class `c(error_class, "error", "condition")`.
#'
#' @examples
#' \dontrun{
#' # Raise a typed error
#' stop(make_error("validation_error", "Value must be positive"))
#'
#' # Test that a specific error class was raised
#' tryCatch(
#'   stop(make_error("validation_error", "Value must be ", "positive")),
#'   validation_error = function(e) {
#'     message("Caught a validation_error: ", conditionMessage(e))
#'   }
#' )
#'
#' # In testthat, assert that the correct error class is thrown
#' testthat::expect_error(
#'   stop(make_error("validation_error", "Value must be positive")),
#'   class = "validation_error"
#' )
#' }
#'
#' @noRd
#' @keywords internal
make_error <- function(error_class, ...) {
  structure(
    class = c(error_class, "error", "condition"),
    list(
      message = paste0(...)
    )
  )
}
