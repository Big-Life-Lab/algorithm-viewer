#' Parse an S7 schema validation error into a formatted string
#'
#' Extracts the human-readable message from an S7 schema validation error
#' condition and formats it as either HTML or plain text. When the error body
#' contains multiple items, the first is used as a lead-in sentence and the
#' remainder are rendered as a list. ANSI colour escape codes are stripped so
#' the result is safe to display in a browser.
#'
#' @param e An S7 schema validation error condition. Expected to have a
#'   `$body` element: either a single string or a character/list vector
#'   where the first element is the main message and subsequent elements
#'   are sub-messages.
#' @param with_html If `TRUE` (the default), sub-messages are formatted as an
#'   HTML `<ul>` list. If `FALSE`, they are formatted as plain-text bullet
#'   points (`  - item`).
#'
#' @return A character string containing the formatted error message.
#'
#' @noRd
#' @keywords internal
parse_s7schema_error <- function(e, with_html = TRUE) {
  msg <- e$body

  if (length(msg) > 1) {
    # The first item in msg is the main error message.
    # After that, we make an HTML list of all the other elements in msg.
    main_msg <- paste0(unlist(msg)[[1]], ":")
    if (with_html) {
      # Make unordered HTML list of all items. Escape each item's text so that
      # schema error messages containing '<', '>', or '&' (e.g. from value
      # comparisons) are not rendered as HTML tags.
      items <- paste0(
        "<li>", htmltools::htmlEscape(unlist(msg)[-1]), "</li>",
        collapse = ""
      )
      msg <- paste0(main_msg, "<br/><ul>", items, "</ul>")
    } else {
      # Make an ASCII text list of all items (eg. "- item 1")
      items <- paste0("  - ", unlist(msg)[-1], collapse = "\n")
      msg <- paste0(main_msg, "\n", items)
    }
  }

  # Remove ANSI/colour codes included by S7schema
  msg <- cli::ansi_strip(msg)

  msg
}