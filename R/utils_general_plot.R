#' General Plotting
#'
#' Utility functions for general-purpose plot generation, including
#' helper plots used to display messages or status information in place of
#' data visualizations.
#'
#' @name utils_general_plot
#' @noRd
#' @keywords internal
NULL

#' Create a Plot Consisting of a Single String Message
#'
#' Generates a minimal plotly plot displaying a centered text message.
#' Used to show error or status messages in place of a data visualization.
#'
#' @param label Character string. The message text to display in the plot.
#' @param color Character string. The color of the message text. Default is
#'   "black".
#'
#' @return A plotly object containing an empty plot with centered text.
#'
#' @examples
#' \dontrun{
#' make_message_plot("Please select at least one model")
#' make_message_plot("Error loading data", color = "red")
#' }
#'
#' @noRd
#' @keywords internal
make_message_plot <- function(label, color = "black") {
  label <- label |>
    cli::ansi_strip() |>
    stringr::str_wrap(width = 50)

  df <- data.frame(label = label)
  p <- ggplot2::ggplot() +
    ggplot2::geom_text(
      data = df,
      ggplot2::aes(label = label),
      x = 0.5,
      y = 0.5,
      color = color
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.line = ggplot2::element_blank())

  plotly::ggplotly(p, tooltip = NULL) |>
    plotly::style(hoverinfo = "none") |>
    plotly::config(displayModeBar = FALSE) |>
    plotly::layout(
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE)
    )
}

.format_sys_calls <- function(calls) {
  calls <- utils::tail(calls, 15)
  formatted <- sapply(seq_along(calls), function(i) {
    number_prefix <- paste0(i, ": ")
    indent_prefix <- strrep(" ", stringr::str_length(number_prefix))
    val <- deparse(calls[[i]])
    val <- paste0(val, collapse = paste0("\n", indent_prefix))
    paste0(number_prefix, val)
  })
  paste0(formatted, collapse = "\n")
}

#' Safely Render a Plot with Structured Error Logging
#'
#' Wraps a plot-computation closure with two layers of error handling:
#'
#' \enumerate{
#'   \item \code{withCallingHandlers} — fires \emph{before} the call stack
#'     unwinds, so \code{conditionCall()} correctly identifies the frame that
#'     signalled the error. This makes it possible to distinguish an expected
#'     domain error (\code{stop("no predictor selected")}) from an unexpected
#'     programming error (e.g. \code{NULL[[1]]}) by inspecting the logged
#'     call site.
#'   \item \code{tryCatch} — catches the error after the stack has unwound and
#'     renders it as inline red text in the plot area so the user sees a
#'     readable message rather than a broken widget.
#' }
#'
#' @param fn A zero-argument function containing the plot computation. Reactive
#'   dependencies inside \code{fn} are tracked normally because
#'   \code{withCallingHandlers} and \code{tryCatch} do not alter the reactive
#'   domain.
#'
#' @return The return value of \code{fn()}, or a \code{make_message_plot()}
#'   error plot if \code{fn()} throws.
#'
#' @noRd
#' @keywords internal
plot_render_safely <- function(fn) {
  tryCatch(
    withCallingHandlers(
      fn(),
      error = function(e) {
        call_str <- if (!is.null(conditionCall(e))) {
          paste0(" [", deparse(conditionCall(e), nlines = 1, width.cutoff = 80), "]")
        } else {
          ""
        }
        message(.format_sys_calls(sys.calls()))
        message("Plot error", call_str, ": ", conditionMessage(e))
      }
    ),
    error = function(e) {
      make_message_plot(
        paste0("<b>Error</b>: ", htmltools::htmlEscape(conditionMessage(e))),
        color = "red"
      )
    }
  )
}

#' Create Plotly Visualization
#'
#' Generates a plotly plot from curve data for multiple models. Handles
#' both categorical and continuous predictor types.
#'
#' @param all_curve_data List of curve data objects from calculate_or_curve
#'   or similar functions. Each object should contain df, aes_args,
#'   x_axis_label, y_axis_label, and x_axis_type fields.
#' @param model_definitions The loaded model definitions object. If NULL, a
#'   message plot prompting the user to upload data is returned.
#' @param scale Character string controlling the y-axis scale.
#'   \code{"log10"} applies a standard base-10 logarithmic transform (only
#'   valid for strictly positive values). \code{"pseudo_log"} uses
#'   \code{scales::transform_pseudo_log(sigma = 1, base = 10)}, a smooth
#'   bijection through zero that handles negative values and approximates
#'   \code{log10} for large magnitudes — suitable for absolute difference axes.
#'   \code{"linear"} uses no transform. Defaults to \code{"log10"}.
#' @param flip_coords If TRUE then flip the x and y axes. Defaults to FALSE.
#' @param theme_args If not NULL then a named list of arguments to pass to
#'   ggplot2::theme. Defaults to NULL.
#' @param plot_type The type of plot to create. Can be "bar", "line", or
#'   "point". If NULL, defaults to "bar" for categorical predictors and
#'   "line" for continuous predictors. Defaults to NULL.
#' @param extra_plot List of additional ggplot2 layers (e.g. geom_hline calls)
#'   to add to the plot. If NULL, no extra layers are added. Defaults to NULL.
#' @param ylim_override Numeric vector of length 2 specifying explicit y-axis
#'   limits (e.g. \code{c(0.5, 2.0)}). Overrides any ylim values from the
#'   curve data. If NULL, limits are taken from curve data or left unset.
#'   Defaults to NULL.
#' @param show_reference_line Double. If NULL then do not show a dashed
#'   reference line. If a double then show a dashed horizontal line at
#'   y = show_reference_line. An example usage would be to set
#'   show_reference_line = 1 to show where a null-effect would be on the plot
#'   for a relative risk or odds ratio plot. Defaults to 1.
#' @param source A character string passed to \code{plotly::ggplotly} as the
#'   \code{source} argument. This identifies the plot so that
#'   \code{plotly::event_data} can be used to handle click events on the plot.
#'   Defaults to "A".
#'
#' @return A plotly object for rendering in the UI.
#'
#' @noRd
#' @keywords internal
make_general_plot <- function(
  all_curve_data,
  model_definitions,
  scale = c("log10", "pseudo_log", "linear"),
  flip_coords = FALSE,
  theme_args = NULL,
  plot_type = NULL,
  extra_plot = NULL,
  ylim_override = NULL,
  show_reference_line = 1,
  source = "A"
) {
  if (is.null(model_definitions)) {
    # No algorithm file has been loaded
    return(make_message_plot("Please upload an algorithm to view this plot."))
  } else if (is.null(all_curve_data) || length(all_curve_data) == 0) {
    # No models have been selected
    return(make_message_plot("Please select at least one model."))
  }

  # Combine all data frames
  df <- all_curve_data |>
    lapply(function(x) x$df) |>
    dplyr::bind_rows()

  # Factoring by Model will force the legend to be listed in
  # the order that they appear in df$Model (which should match the
  # order in the model definitions file). If we did not do this
  # then the legend would be sorted alphabetically.
  df$Model <- factor(df$Model, levels = unique(df$Model))

  curve_data <- all_curve_data[[1]]

  # The hover mode, as passed to plotly::layout()
  hovermode <- "x unified"

  tryCatch(
    {
      scale <- match.arg(scale)

      transform <- switch(scale,
        log10      = "log10",
        pseudo_log = scales::transform_pseudo_log(sigma = 1, base = 10),
        linear     = "identity"
      )

      ylabel <- switch(scale,
        log10      = glue::glue("{curve_data$y_axis_label} (Logarithmic)"),
        pseudo_log = glue::glue("{curve_data$y_axis_label} (Signed Logarithmic)"),
        linear     = curve_data$y_axis_label
      )

      # Set y limits from override or curve data
      logarithmic <- scale != "linear"
      y_limits <- ylim_override
      if (is.null(y_limits)) {
        if (!is.null(curve_data[["ylim_logarithmic"]]) && logarithmic) {
          y_limits <- curve_data[["ylim_logarithmic"]]
        } else if (!is.null(curve_data[["ylim_linear"]]) && !logarithmic) {
          y_limits <- curve_data[["ylim_linear"]]
        } else if (!is.null(curve_data[["ylim"]])) {
          y_limits <- curve_data[["ylim"]]
        }
      }

      # Factor the x axis categorical variables
      if (curve_data$x_axis_type == "Categorical") {
        # Maintain the order of the x-axis categories
        levels <- unique(df[[curve_data$x_axis_label]])
        if (flip_coords) {
          # For vertical graphs (where the categories are on the y axis),
          # we want the to reverse the order of the categories, so they
          # are sorted from top to bottom (instead of bottom to top)
          levels <- rev(levels)
        }
        df[[curve_data$x_axis_label]] <-
          factor(
            df[[curve_data$x_axis_label]],
            levels = levels
          )
      }

      # Determine the plot type (if plot_type is NULL)
      if (is.null(plot_type)) {
        if (curve_data$x_axis_type == "Categorical") {
          plot_type <- "bar"
        } else {
          plot_type <- "line"
        }
      }

      # Determine the colors assigned to each subplot
      model_colors <- get_model_colors(
        model_definitions$models
      )

      if (plot_type == "bar") {
        # Create the bar plot (p)
        p <- ggplot2::ggplot(
          data = df,
          make_aes(curve_data$aes_args, fill = rlang::sym("Model"))
        )
        p <- p +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::scale_fill_manual(
            values = model_colors,
            aesthetics = "fill"
          )
      } else if (plot_type == "line") {
        # Create the line plot (p)
        p <- ggplot2::ggplot(
          data = df,
          make_aes(curve_data$aes_args, color = rlang::sym("Model"))
        )
        p <- p +
          ggplot2::geom_line(linewidth = 1.2) +
          ggplot2::scale_color_manual(
            values = model_colors,
            aesthetics = "color"
          )
      } else if (plot_type == "point") {
        # Create the point plot (p)
        p <- ggplot2::ggplot(
          data = df,
          make_aes(curve_data$aes_args, fill = rlang::sym("Model"))
        )
        p <- p +
          ggplot2::geom_point(
            position = ggplot2::position_dodge(width = 0.3),
            size = 4,
            stroke = 0.1
          ) +
          ggplot2::scale_fill_manual(
            values = model_colors,
            aesthetics = "fill"
          )
      }

      # Add general options to the plot
      ref_line <- if (!is.null(show_reference_line)) {
        ggplot2::geom_hline(yintercept = show_reference_line, linetype = "dashed", color = "gray50")
      } else {
        NULL
      }
      p <- p +
        ggplot2::scale_y_continuous(transform = transform) +
        ref_line +
        ggplot2::labs(
          title = curve_data$title,
          x = curve_data$x_axis_label,
          y = ylabel
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "right",
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 11)
        )

      if (!is.null(theme_args)) {
        # Apply the additional theme arguments
        p <- p +
          do.call(ggplot2::theme, theme_args)
      }
      if (flip_coords) {
        # Flip the x and y axes, zooming to y_limits without dropping data
        p <- p +
          ggplot2::coord_flip(ylim = y_limits)
        hovermode <- "y unified"
      } else if (!is.null(y_limits)) {
        p <- p + ggplot2::coord_cartesian(ylim = y_limits)
      }

      if (!is.null(extra_plot)) {
        for (cur_p in extra_plot) {
          p <- p + cur_p
        }
      }

      # Generate and return the Plotly plot from the ggplot2.
      # Suppress hline traces (labelled "yintercept" by ggplotly) from the
      # unified tooltip; "skip" excludes them entirely unlike "none".
      plotly::ggplotly(p, source = source) |>
        (\(plt) {
          # geom_point layers (eg the user-value dots) become scatter traces
          # with a `mode` attribute. ggplotly leaks that attribute onto the
          # bar traces, which triggers the harmless rendering warning:
          # "'bar' objects don't have these attributes: 'mode'". Strip it.
          plt$x$data <- lapply(plt$x$data, function(tr) {
            if (isTRUE(tr$type == "bar")) tr$mode <- NULL
            tr
          })

          hline_traces <- which(vapply(plt$x$data, function(tr) {
            any(grepl("yintercept", tr$text, fixed = TRUE)) ||
            any(grepl("hidden_", tr$text, fixed = TRUE))
          }, logical(1)))
          if (length(hline_traces) > 0) {
            plt <- plotly::style(plt, hoverinfo = "skip", traces = hline_traces)
          }
          plt
        })() |>
        plotly::layout(hovermode = hovermode) |>
        # Register the click event so plotly::event_data("plotly_click", ...)
        # can retrieve it without emitting an "event ... is not registered"
        # warning.
        plotly::event_register("plotly_click")
    },
    error = function(e) {
      message(conditionMessage(e))
      make_message_plot(
        paste("Error making plot:", conditionMessage(e)),
        color = "red"
      )
    }
  )
}

#' Build Aesthetic Mapping
#'
#' Combines base aesthetic arguments with additional mappings into a single
#' aes() call for ggplot2.
#'
#' @param aes_args List of aesthetic mappings to include.
#' @param ... Additional aesthetic mappings to append.
#'
#' @return A ggplot2 aesthetic mapping object.
#'
#' @noRd
#' @keywords internal
make_aes <- function(aes_args, ...) {
  # Append ... to aes_args, then past as params to aes function
  aes_args <- c(aes_args, list(...))
  do.call(ggplot2::aes, aes_args)
}
