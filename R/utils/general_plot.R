#' @file general_plot.R
#' @description Utility functions for general-purpose plot generation, including
#'   helper plots used to display messages or status information in place of
#'   data visualizations.
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
#' make_message_plot("Please select at least one model")
#' make_message_plot("Error loading data", color = "red")
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
#' @param logarithmic If TRUE then plot with a logarithmic scale. Defaults
#'   to TRUE.
#' @param flip_coords If TRUE then flip the x and y axes. Defaults to FALSE.
#' @param theme_args If not NULL then a named list of arguments to pass to
#'   ggplot2::theme. Defaults to NULL.
#' @param plot_type The type of plot to create. Can be "bar", "line", or
#'   "point". If NULL, defaults to "bar" for categorical predictors and
#'   "line" for continuous predictors. Defaults to NULL.
#'
#' @return A plotly object for rendering in the UI.
#'
#' @keywords internal
make_general_plot <- function(
  all_curve_data,
  model_definitions,
  logarithmic = TRUE,
  flip_coords = FALSE,
  theme_args = NULL,
  plot_type = NULL
) {
  # If no models are selected then tell the user to select one
  if (is.null(model_definitions)) {
    msg <- "No algorithm loaded.<br />Please upload some data."
    return(make_message_plot(msg))
  } else if (is.null(all_curve_data) || length(all_curve_data) == 0) {
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

  curve_data <- all_curve_data[[length(all_curve_data)]]

  # The hover mode, as passed to plotly::layout()
  hovermode = "x unified"

  tryCatch(
    {
      # log10 vs identity transform
      transform <- ifelse(logarithmic, "log10", "identity")

      # Add "(Logarithmic)" to y-axis label if required
      ylabel <- ifelse(
        logarithmic,
        glue::glue("{curve_data$y_axis_label} (Logarithmic)"),
        curve_data$y_axis_label
      )

      # Set y limits for non-logarithmic plots, if specified in
      # the curve data
      y_limits <- NULL
      if (!is.null(curve_data$ylim) && !logarithmic) {
        y_limits <- curve_data$ylim
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
          make_aes(curve_data$aes_args, fill = dplyr::sym("Model"))
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
          make_aes(curve_data$aes_args, color = dplyr::sym("Model"))
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
          make_aes(curve_data$aes_args, fill = dplyr::sym("Model"))
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
        # @TODO: REMOVE THIS!!!
        y_limits <- c(0.001, 100)
      }

      # Add general options to the plot
      p <- p +
        ggplot2::scale_y_continuous(
          transform = transform,
          limits = y_limits
        ) +
        ggplot2::geom_hline(
          yintercept = 1,
          linetype = "dashed",
          color = "gray50"
        ) +
        ggplot2::labs(
          title = curve_data$title,
          subtitle = curve_data$title,
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
        # Flip the x and y axes
        p <- p +
          ggplot2::coord_flip()
        hovermode = "y unified"
      }

      # Generate and return the Plotly plot from the ggplot2
      plotly::ggplotly(p) |>
        plotly::layout(hovermode = hovermode)
    },
    error = function(e) {
      make_message_plot(
        paste("Error making plot:", e$message),
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
#' @keywords internal
make_aes <- function(aes_args, ...) {
  # Append ... to aes_args, then past as params to aes function
  aes_args <- c(aes_args, list(...))
  do.call(ggplot2::aes, aes_args)
}
