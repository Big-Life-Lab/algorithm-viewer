#' A vs B Curve
#'
#' Functions for computing and rendering A vs B relative risk curves.
#' Predictor values for group A and group B can be specified in the UI.
#'
#' @name mod_plot_rr_a_vs_b
#' @noRd
#' @keywords internal
NULL

#' Build an A vs B Plot Module Server
#'
#' Shiny module server that renders an interactive A vs B
#' relative risk plot. The module also owns the group controls UI
#' (\code{output$group_controls}) which lets users set the predictor values
#' for the A and B groups.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotRRAvsBUI}}).
#' @param predictor A reactive expression returning the currently selected
#'   predictor variable name (character string).
#' @param interaction_predictor A reactive expression returning the currently
#'   selected interaction predictor variable name.
#' @param logarithmic A reactive expression returning a logical indicating
#'   whether to use a logarithmic y-axis scale.
#' @param selected_models A reactive expression returning the list of model
#'   data objects to plot curves for. This is a subset of
#'   \code{model_definitions()$models}.
#' @param exposure_groups A \code{shiny::reactiveValues()} object with
#'   \code{$a} and \code{$b} named lists of predictor values,
#'   representing the A and B group profiles respectively.
#' @param model_definitions A reactive expression (or \code{reactiveVal})
#'   returning the top-level model definitions object, or \code{NULL} if no
#'   algorithm is loaded.
#'
#' @return \code{NULL}, called for side effects.
#'
#' @noRd
#' @keywords internal
plotRRAvsBServer <- function(
  id,
  predictor,
  interaction_predictor,
  logarithmic,
  selected_models,
  exposure_groups,
  model_definitions
) {
  shiny::moduleServer(id, function(input, output, session) {
    # Cached curve data, to avoid unnecessary recalculation of curves that have
    # already been calculated
    cached_curves <- initialize_cached_data()

    shiny::observe(
      {
        # React whenever new model definitions are loaded
        model_definitions()
        # Clear the cached curve data, since they are no longer valid.
        clear_cached_data(cached_curves)
      },
      priority = 10000
    )

    # Change in min/max range for logarithmic x-range slider
    shiny::observe({
      new_min <- input$x_range_log_min
      new_max <- input$x_range_log_max
      if (is.null(new_min) || is.null(new_max) ||
          is.na(new_min) || is.na(new_max) ||
          new_min >= new_max) return()
      current_val <- shiny::isolate(input$x_range_log)
      new_val <- if (!is.null(current_val)) {
        pmax(pmin(current_val, new_max), new_min)
      } else {
        c(new_min, new_max)
      }
      shiny::updateSliderInput(
        session, "x_range_log",
        min = new_min, max = new_max, value = new_val
      )
    })

    # Change in min/max range for linear x-range slider
    shiny::observe({
      new_min <- input$x_range_linear_min
      new_max <- input$x_range_linear_max
      if (is.null(new_min) || is.null(new_max) ||
          is.na(new_min) || is.na(new_max) ||
          new_min >= new_max) return()
      current_val <- shiny::isolate(input$x_range_linear)
      new_val <- if (!is.null(current_val)) {
        pmax(pmin(current_val, new_max), new_min)
      } else {
        c(new_min, new_max)
      }
      shiny::updateSliderInput(
        session, "x_range_linear",
        min = new_min, max = new_max, value = new_val
      )
    })

    # Make the plot
    output$plot <- plotly::renderPlotly({
      if (is.null(model_definitions())) {
        return(make_general_plot(
          NULL,
          model_definitions()
        ))
      }

      plot_render_safely(function() {
        extra_plot <- list()
        all_curve_data <- list()

        # Go through all models and calculate the A vs B curves
        for (model_data in selected_models()) {
          # Check if we can use the cached old data for the current model
          model_params <- list(
            a_group = exposure_groups$a,
            b_group = exposure_groups$b
          )
          cache_key <- list("rr_a_vs_b", model_data$model_id)
          if (
            is_reusable_cached_data(
              cached_curves,
              cache_key,
              model_params
            )
          ) {
            # Reuse the old data
            curve_data <-
              get_cached_data(cached_curves, cache_key)
            all_curve_data[[length(all_curve_data) + 1]] <- curve_data
          } else {
            # Get predictor type (Categorical or Continuous)
            tic <- Sys.time()

            # Calculate the RR curve for the model
            curve_data <- .calculate_rr_a_vs_b_curve(
              model_data = model_data,
              a_group = exposure_groups$a,
              b_group = exposure_groups$b
            )

            elapsed <- Sys.time() - tic
            message(paste0(
              "Elapsed time for A vs B curve ",
              model_data$model_id,
              ": ",
              elapsed
            ))

            all_curve_data[[length(all_curve_data) + 1]] <- curve_data

            # Save the data to our cache
            set_cached_data(
              cached_curves,
              cache_key,
              model_params,
              curve_data
            )
          }

          extra_plot[[length(extra_plot) + 1]] <- ggplot2::geom_hline(
            yintercept = curve_data$overall_rr,
            linetype = "dotted",
            linewidth = 0.3,
            color = model_data$model_color
          )
        }

        # The log-scale slider operates on the exponent (e.g. -3 to 3),
        # so convert back to actual axis limits by raising 10 to those values.
        x_limits <- if (logarithmic()) {
          if (!is.null(input$x_range_log)) 10^input$x_range_log else NULL
        } else {
          input$x_range_linear
        }
        
        # Pad the labels to maintain a constant x axis label width (to avoid
        # having the y axis change scales when fitting the x axis labels). Note
        # that because flip_coords = TRUE, what we apply to the x axis is
        # actually what is shown as the y axis in the final plot.
        if (length(all_curve_data) > 0) {
          max_label_len <- max(
            sapply(all_curve_data, function(x) x$max_x_axis_label_len)
          )
          max_label_len <- max_label_len + 10
          extra_plot[[length(extra_plot) + 1]] <- 
            ggplot2::scale_x_discrete(labels = function(x) stringr::str_pad(x, max_label_len, side = "left"))
        }

        make_general_plot(
          all_curve_data,
          model_definitions(),
          logarithmic(),
          flip_coords = TRUE,
          theme_args = list(axis.title.y = ggplot2::element_blank()),
          plot_type = "point",
          extra_plot = extra_plot,
          ylim_override = x_limits
        )
      })
    })
  })
}

#' Calculate Relative Risk Curve: A vs B
#'
#' Builds a curve dataset comparing the relative risk of the A group
#' against the B reference group across all categorical predictors in
#' the model. For each categorical predictor, a row is added for every possible
#' value of that predictor (with the A group otherwise held fixed).
#' Continuous predictors are omitted from the per-predictor rows because they
#' do not vary between the A and B groups and would be redundant;
#' their contribution is captured in the overall "A vs B" row.
#' Relative risks are computed by running the model pipeline on all rows and
#' dividing each row's predicted risk by the B group's predicted risk.
#'
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param a_group A named list of predictor values representing the
#'   A group profile.
#' @param b_group A named list of predictor values representing the
#'   B (reference) group profile. This group's predicted risk is used
#'   as the denominator for all relative risk calculations.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
#'
#' @noRd
#' @keywords internal
.calculate_rr_a_vs_b_curve <- function(
  model_data,
  a_group,
  b_group
) {
  # Get the x-axis label given the specified main label (eg. the predictor name)
  # and the specified sub label (eg. the predictor categorical value)
  # If html is TRUE then an HTML string is returned, otherwise a plain-text
  # string is returned.
  get_x_axis_label <- function(main_label, sub_label, html = TRUE) {
    label <- main_label
    if (html) {
      label <- paste0("<b>", label, "</b>")
    }
    if (!is.null(sub_label) && stringr::str_length(sub_label) > 0) {
      label <- paste0(label, ": ", sub_label)
    }
    label
  }

  rows <- list()
  row_names <- list()
  row_comparisons <- list()
  predictors <- list()
  a_values <- list()
  b_values <- list()
  overall_label <- "Overall Relative Risk"

  # The is the maximum length (in characters) of an x axis label
  # (eg. "Diabetes: Yes"). This allows us to determine how wide these labels
  # can possibly be, for any combination of predictor and predictor value.
  # We can then make the plot have a label region that is a constant width,
  # avoiding resizing of the plot to fit the labels. Have a constant width
  # makes it easier to read the plot and to see how the points move as
  # the predictor values change.
  max_x_axis_label_len <- 0

  # First row is the unmodified A group (the overall risk)
  # We will then add additional rows for each categorical variable.
  # For all levels of the categorical variable that is not in A, we
  # will add a row using the A group values, but with the categorical
  # variable set to the new level.
  rows[[length(rows) + 1]] <- a_group
  row_names[[length(row_names) + 1]] <- get_x_axis_label(
    overall_label,
    NULL,
    html = TRUE
  )
  max_x_axis_label_len <- max(
    max_x_axis_label_len,
    stringr::str_length(get_x_axis_label(overall_label, NULL, html = FALSE))
  )
  row_comparisons[[length(row_comparisons) + 1]] <- "A vs B"
  predictors[[length(predictors) + 1]] <- ""
  a_values[[length(a_values) + 1]] <- ""
  b_values[[length(b_values) + 1]] <- ""

  for (idx in seq_along(names(a_group))) {
    predictor <- names(a_group)[[idx]]
    predictor_label <- get_variable_label(
      model_data,
      predictor,
      escape_html = TRUE
    )

    if (is_variable_categorical(model_data, predictor)) {
      # Add a row for each allowable value of the predictor. The group will
      # be a_group but with each allowable value of the current predictor
      # set in the A group (eg. for marital status, we could have one row
      # for "Married", one for "Single", and one for
      # "Widowed/separated/divorced")
      predictor_allowable_values <-
        get_predictor_allowable_values(model_data, predictor)
      for (cur_alt_value in predictor_allowable_values) {
        cur_alt_label <- get_variable_label_from_value(
          model_data,
          predictor,
          cur_alt_value,
          escape_html = TRUE
        )
        # Update maximum possible length of the x axis labels (includes the
        # categories that are not displayed)
        max_x_axis_label_len <- max(
          max_x_axis_label_len,
          stringr::str_length(
            get_x_axis_label(predictor_label, cur_alt_label, html = FALSE)
          )
        )

        # Skip if cur_alt_value is the same as the B group value.
        # This is because the predicted risk (or rr) will be identical
        # to the overall risk.
        if (cur_alt_value == a_group[[predictor]])
          next

        # Add the A row, with the predictor set to cur_alt_value
        cur_group <- a_group
        cur_group[[predictor]] <- cur_alt_value
        rows[[length(rows) + 1]] <- cur_group

        # Calculate the name (eg. "Marital status (Married)") of the new row and
        # the comparison label (eg. "Marital status (Married vs Single)")
        a_label <- get_variable_label_from_value(
          model_data,
          predictor,
          a_group[[predictor]],
          escape_html = TRUE
        )
        row_name <- get_x_axis_label(
          predictor_label,
          cur_alt_label,
          html = TRUE
        )
        row_names[[length(row_names) + 1]] <- row_name
        row_comparison <- as.character(glue::glue(
          "{predictor_label} ({cur_alt_label} vs {a_label})"
        ))
        row_comparisons[[length(row_comparisons) + 1]] <- row_comparison
        predictors[[length(predictors) + 1]] <- predictor
        a_values[[length(a_values) + 1]] <- cur_alt_value
        b_values[[length(b_values) + 1]] <-
          b_group[[predictor]]
      }
    }
  }

  # The B group is the last row. All risks calculated from previous
  # rows are compared to this one.
  rows[[length(rows) + 1]] <- b_group

  df <- do.call(rbind.data.frame, rows)

  # Run the pipeline with the input matrix and calculate the relative risk
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df
  )

  # Calculate the relative risk. The risks are relative to the
  # risk in the last row (the reference group).
  rr <- dat[seq_len(nrow(dat) - 1), ] / dat[nrow(dat), ]

  output_df <- data.frame(
    x = rr,
    RR = rr,
    Model = cleanup_string(model_data$title),
    Label = unlist(row_names[seq_len(nrow(dat) - 1)]),
    Comparison = unlist(row_comparisons[seq_len(nrow(dat) - 1)]),
    predictor = unlist(predictors[seq_len(nrow(dat) - 1)]),
    a_value = unlist(a_values[seq_len(nrow(dat) - 1)]),
    b_value = unlist(b_values[seq_len(nrow(dat) - 1)])
  )

  list(
    df = output_df,
    overall_rr = rr[[1]],
    max_x_axis_label_len = max_x_axis_label_len,
    x_axis_label = "Label",
    y_axis_label = "Relative Risk",
    title = "Relative Risk",
    x_axis_type = "Categorical",
    aes_args = list(
      x = rlang::sym("Label"),
      y = rlang::sym("RR")
    )
  )
}

#' Build the A vs B Plot UI
#'
#' Returns the UI elements to insert into the A vs B tab panel.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotRRAvsBServer}}).
#' @param external_height Height, in pixels, of the area outside of the plot
#'   area (in the main tabs). If a plot wants to fill up the height of the page,
#'   without causing overflow at the bottom (and hence scrolling), then a plot's
#'   height should be "calc(100vh - {external_height}px)".
#'
#' @return A \code{\link[shiny]{tagList}} containing the panel UI elements.
#'
#' @noRd
#' @keywords internal
plotRRAvsBUI <- function(
  id,
  external_height
) {
  # This is the height of the x-range controls in pixels. We do not set the
  # x-range controls to this height, but this value is used to determine how
  # much extra space we have for the plot (once the height of the x-range
  # controls are taken into account)
  x_range_controls_height <- 75

  shiny::tagList(
    shiny::br(),
    shiny::fluidRow(
      style = "width: 100%",
      plotly::plotlyOutput(
        shiny::NS(id, "plot"),
        # +75 is to remove the height of the X axis range controls below
        height = glue::glue(
          "calc(100vh - {external_height + x_range_controls_height}px)"
        )
      )
    ),
    shiny::div(
      style = "padding: 0 15px",
      # This panel is for the x-axis range controls when in logarithmic mode
      shiny::conditionalPanel(
        condition = "input.logarithmic",
        shiny::tags$label(
          class = "control-label", "X Axis Range (log10 scale):"
        ),
        shiny::div(
          style = "display: flex; align-items: center; gap: 8px",
          shiny::numericInput(
            inputId = shiny::NS(id, "x_range_log_min"),
            label = NULL, value = -5, step = 0.1, width = "80px"
          ),
          shiny::div(
            style = "flex: 1",
            shiny::sliderInput(
              inputId = shiny::NS(id, "x_range_log"),
              label = NULL,
              min = -5, max = 5, value = c(-3, 3), step = 0.1,
              width = "100%"
            )
          ),
          shiny::numericInput(
            inputId = shiny::NS(id, "x_range_log_max"),
            label = NULL, value = 5, step = 0.1, width = "80px"
          )
        )
      ),
      # This panel is for the x-axis range controls when in linear mode
      shiny::conditionalPanel(
        condition = "!input.logarithmic",
        shiny::tags$label(class = "control-label", "X Axis Range:"),
        shiny::div(
          style = "display: flex; align-items: center; gap: 8px",
          shiny::numericInput(
            inputId = shiny::NS(id, "x_range_linear_min"),
            label = NULL, value = 0, step = 1, width = "80px"
          ),
          shiny::div(
            style = "flex: 1",
            shiny::sliderInput(
              inputId = shiny::NS(id, "x_range_linear"),
              label = NULL,
              min = 0, max = 500, value = c(0, 300), step = 1,
              width = "100%"
            )
          ),
          shiny::numericInput(
            inputId = shiny::NS(id, "x_range_linear_max"),
            label = NULL, value = 500, step = 1, width = "80px"
          )
        )
      )
    )
  )
}
