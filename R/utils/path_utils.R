#' Expand and normalize a file path.
#'
#' Symbolic links and ".." will be followed and expanded.
#'
#' @param p Character. The path to expand and normalize.
#' @param add_trailing_slash Logical. If `TRUE`, a trailing slash is appended to
#'   the normalized path if it does not already have one. This is useful if the
#'   path is known to be a directory. Defaults to `FALSE`.
#' @return Character. The normalized path, or `NULL` if the path is invalid or
#'   does not exist.
#' @keywords internal
expand_and_normalize_path <- function(p, add_trailing_slash = FALSE) {
  # Try to normalize the path. If the path does not exist then
  # we return NULL
  normalized <- NULL
  tryCatch(
    {
      normalized <- normalizePath(
        p,
        winslash = .Platform$file.sep,
        mustWork = TRUE
      )
    },
    error = function(e) {
      # This error handler stops normalizePath from printing out an error
    }
  )
  if (is.null(normalized)) {
    return(NULL)
  }

  if (add_trailing_slash) {
    # Add a trailing slash if there isn't one. This is useful
    # for directories
    len <- stringr::str_length(normalized)
    if (len > 0 && substr(normalized, len, len) != .Platform$file.sep) {
      normalized <- paste0(normalized, .Platform$file.sep)
    }
  }

  normalized
}

#' Check if a file is a descendant of a directory, and that both the file and
#' directory exist.
#'
#' Symbolic links and ".." will be followed and expanded.
#'
#' @param file Character. The path to the file to check.
#' @param top_level_directory Character. The path to the directory that `file`
#'   should be a descendant of.
#' @return Logical. `TRUE` if `file` is a descendant of `top_level_directory`,
#'   `FALSE` otherwise. Returns `FALSE` if either `file` or
#'   `top_level_directory` do not exist on the file system.
#' @keywords internal
is_file_descendant_of <- function(file, top_level_directory) {
  file <- expand_and_normalize_path(file)
  top_level_directory <- expand_and_normalize_path(
    top_level_directory,
    add_trailing_slash = TRUE
  )

  # Check if file or top_level_directory are invalid or do not exist
  if (is.null(file) || is.null(top_level_directory)) {
    return(FALSE)
  }

  # Make sure file is within top_level_directory
  startsWith(file, top_level_directory)
}

#' Format the file path to be relative to relative_to_path
#'
#' This is generally for informational purposes to report to the user. It is
#' meant to hide the full paths of files on the system from a user so that
#' attackers cannot gather information about the system's directory structure.
#' Usually, the relative_to_path parameter would be the sandbox path.
#'
#' @param file The file path to format.
#' @param relative_to_path The path that we want the file to be
#'   relative to.
#' @return The formatted file path. If either `file` or `relative_to_path`
#'   do not exist, or if `file` is not a descendant of `relative_to_path` then
#'   simply the basename of `file` is returned.
#' @keywords internal
file_relative_to_path <- function(file, relative_to_path) {
  if (!is.null(relative_to_path)) {
    relative_to_path <- expand_and_normalize_path(
      relative_to_path,
      add_trailing_slash = TRUE
    )
    if (!is.null(relative_to_path)) {
      norm_file <- expand_and_normalize_path(file)
      if (!is.null(norm_file) && startsWith(norm_file, relative_to_path)) {
        rel_start <- nchar(relative_to_path) + 1
        return(substr(norm_file, rel_start, nchar(norm_file)))
      }
    }
    return(basename(file))
  }
  file
}
