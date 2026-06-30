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
  selected_models,
  exposure_groups,
  model_definitions
) {
  shiny::moduleServer(id, function(input, output, session) {
    # Cached curve data, to avoid unnecessary recalculation of curves that have
    # already been calculated
    cached_curves <- initialize_cached_data()

    # List of all the calculated curve data, including the RR and AD values.
    # Used for accessing click data and rendering the info panel (where
    # the user's risk, reference risk, and overall RR are found)
    curve_data_rv <- shiny::reactiveVal(NULL)

    # The predictor to use for the sub plot. This is equal to the predictor of
    # the last clicked row
    sub_plot_predictor <- shiny::reactiveVal(NULL)

    range_rr_log    <- rangeSelectorServer("x_range_rr_log",    "logarithmic")
    range_rr_linear <- rangeSelectorServer("x_range_rr_linear", "linear")
    range_ad_log    <- rangeSelectorServer("x_range_ad_log",    "logarithmic")
    range_ad_linear <- rangeSelectorServer("x_range_ad_linear", "linear")

    session$onSessionEnded(function() {
      range_rr_log$destroy()
      range_rr_linear$destroy()
      range_ad_log$destroy()
      range_ad_linear$destroy()
    })

    shiny::observe(
      {
        # React whenever new model definitions are loaded
        model_definitions()
        # Clear the cached curve data, since they are no longer valid.
        clear_cached_data(cached_curves)
        # Clear the curve data
        curve_data_rv(NULL)
        # Clear sub plot predictor
        sub_plot_predictor(NULL)
      },
      priority = 10000
    )

    # Instructions panel (tell the user what the subplot is showing, and to
    # click the main plot to select a different predictor for the subplot)
    output$sub_plot_instructions <- shiny::renderUI({
      div_style <- paste(
        "width: 100%; height: 300px; display: table-cell;",
        "vertical-align: middle; text-align: center;"
      )

      if (is.null(sub_plot_predictor())) {
        html <- shiny::div(
          style = div_style,
          "No predictor selected."
        )
        return(html)
      }

      model_data <- selected_models()
      if (is.null(model_data) || length(model_data) == 0) {
        html <- shiny::div(
          style = div_style,
          "No model selected."
        )
        return(html)
      }

      model_data <- model_data[[names(model_data)[[1]]]]
      predictor_label <- get_variable_label(
        model_data,
        sub_plot_predictor(),
        escape_html = TRUE
      )
      
      shiny::div(
        style = div_style,
        paste0(
          "This subplot shows the relative risk of you vs the ",
          "reference group, as you take on all values of the predictor '",
          predictor_label,
          "'."
        ),
        shiny::br(),
        shiny::br(),
        paste0(
          "Click another row in the main plot to change the predictor ",
          "displayed in the subplot."  
        )
      )
    })

    output$sub_plot <- plotly::renderPlotly({
      if (
        is.null(model_definitions()) ||
        is.null(exposure_groups$a) ||
        is.null(sub_plot_predictor())
      ) {
        return(make_message_plot(paste(
          "Click a row in the main plot above to see a",
          "predictor's relative risk plot."
        )))
      }

      plot_render_safely(function() {
        all_curve_data <- list()
        extra_plot <- list()
        # Accumulate the user-value dots across models into one data frame so
        # they can be drawn as a single, dodged geom_point layer below.
        overlay_rows <- list()
        predictor_is_categorical <- FALSE

        # Go through all selected_models and calculate the RR curves
        # We concatenate them (with bind_rows) to show one curve per model
        for (model_data in selected_models()) {
          group_a <- exposure_groups$a
          group_b <-  exposure_groups$b

          # Check if we can use the cached old data for the current model
          model_params <- list(
            predictor = sub_plot_predictor(),
            interaction_predictor = NULL,
            group_a = group_a,
            group_b = group_b
          )
          cache_key <- list(
            "sub_plot",
            model_data$model_id,
            sub_plot_predictor()
          )
          if (
            is_reusable_cached_data(
              cached_curves,
              cache_key,
              model_params
            )
          ) {
            # Reuse the old data
            curve_data <- get_cached_data(cached_curves, cache_key)
            all_curve_data[[length(all_curve_data) + 1]] <- curve_data
          } else {
            tic <- Sys.time()

            # Calculate the RR curve for the model
            curve_data <- .calculate_rr_curve(
              sub_plot_predictor(),
              model_data,
              target_group = group_a,
              reference_group = group_b
            )

            # Add the "Me" and "Ref" values as subtitles
            a_value <- group_a[[sub_plot_predictor()]]
            b_value <- group_b[[sub_plot_predictor()]]
            if (is_variable_categorical(model_data, sub_plot_predictor())) {
              b_value <- get_variable_label_from_value(
                model_data,
                sub_plot_predictor(),
                b_value
              )
              a_value <- get_variable_label_from_value(
                model_data,
                sub_plot_predictor(),
                a_value
              )
            }
            curve_data$subtitle <- list(
              paste0("Your value = ", a_value),
              paste0("Reference value = ", b_value)
            )

            elapsed <- Sys.time() - tic
            message(paste0(
              "Elapsed time for RR curve ", model_data$model_id, ": ", elapsed
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

          # Add a dot at the user's value for the main predictor
          predictor_is_categorical <-
            is_variable_categorical(model_data, sub_plot_predictor())
          if (predictor_is_categorical) {
            user_label <- get_variable_label_from_value(
              model_data,
              sub_plot_predictor(),
              group_a[[sub_plot_predictor()]]
            )
          } else {
            user_label <- group_a[[sub_plot_predictor()]]
          }

          overlay_rows[[length(overlay_rows) + 1]] <- data.frame(
            hidden_x = user_label,
            hidden_y = curve_data$overall_rr,
            # Must match df$Model in make_general_plot so the dots share the
            # bars' grouping/dodge order.
            Model = cleanup_string(model_data$title),
            model_color = model_data$model_color,
            stringsAsFactors = FALSE
          )
        }

        # Draw the user-value dots as a single geom_point layer. For bar
        # (categorical) plots the bars are dodged by Model, so the dots must be
        # dodged with the same grouping and width (geom_col's default 0.9) to
        # land in the centre of each model's bar. For line (continuous) plots
        # no dodging is applied so each dot sits on its curve.
        if (length(overlay_rows) > 0) {
          overlay_df <- do.call(rbind, overlay_rows)
          if (predictor_is_categorical) {
            # Match the bar grouping order: make_general_plot factors df$Model
            # by unique(df$Model), and overlay_rows are built in the same model
            # order, so unique() here yields the same level order.
            overlay_df$Model <-
              factor(overlay_df$Model, levels = unique(overlay_df$Model))
            extra_plot[[length(extra_plot) + 1]] <- ggplot2::geom_point(
              data = overlay_df,
              ggplot2::aes(
                x = hidden_x,
                y = hidden_y,
                group = Model,
                color = model_color
              ),
              size = 3,
              shape = 19,
              inherit.aes = FALSE,
              position = ggplot2::position_dodge(width = 0.9)
            )
            # model_color already holds literal colours; the identity scale
            # avoids clashing with the bars' fill scale and adds no legend.
            extra_plot[[length(extra_plot) + 1]] <-
              ggplot2::scale_color_identity()
          } else {
            extra_plot[[length(extra_plot) + 1]] <- ggplot2::geom_point(
              data = overlay_df,
              ggplot2::aes(x = hidden_x, y = hidden_y),
              # No dodge here, so rows keep their order and a per-row colour
              # vector aligns correctly. Set outside aes() to avoid a second
              # colour scale clashing with the line plot's color = Model.
              color = overlay_df$model_color,
              size = 3,
              shape = 19,
              inherit.aes = FALSE
            )
          }
        }

        make_general_plot(
          all_curve_data,
          model_definitions(),
          extra_plot = extra_plot,
          scale = dplyr::case_when(
            input$logarithmic   ~ "log10",
            TRUE                ~ "linear"
          ),
        )
      })
    })

    # Create and dynamically size the plot container based on the number of
    # predictor rows.
    output$plot_container <- shiny::renderUI({
      curve_data <- curve_data_rv()

      # Pixels per displayed row in the plot (ie per predictor)
      px_per_row      <- 26
      # Extra height in the plot, to take into account things like the
      # title, axis labels, etc
      base_overhead   <- 120
      # Minimum height of the plot
      min_fallback_px <- 300

      # Calculate number of rows
      n_rows <- if (!is.null(curve_data) && length(curve_data) > 0) {
        max(sapply(curve_data, function(cd) nrow(cd$df)))
      } else {
        0L
      }

      # Create the height string
      height_str <- paste0(
        max(min_fallback_px, n_rows * px_per_row + base_overhead),
        "px"
      )

      plotly::plotlyOutput(session$ns("main_plot"), height = height_str)
    })

    # Local copy of the main plot's click event, used to drive the sub plot.
    #
    # Why this exists: plotly::event_data() caches the last click and keeps
    # replaying it on every reactive invalidation, even after the plot is
    # redrawn. Reading it directly would re-fire a stale click (e.g. selecting
    # a predictor again) whenever the plot updates. Copying it into our own
    # reactiveVal lets us consume the click once and then reset it to NULL,
    # so a given click is only ever acted on a single time.
    click_data <- shiny::reactiveVal(NULL)

    # Mirror plotly's click event into our reactiveVal as clicks come in.
    shiny::observe({
      data <- plotly::event_data(
        "plotly_click",
        source = shiny::NS(id, "main_plot")
      )

      # Skip if curve_data_rv is not available. We do this after calling
      # plotly::event_data, to make sure we will always react to the
      # click events (calling plotly::event_data will register us to
      # receive the clicks). If we called shiny::req first, then if
      # curve_data_rv is not available, we will never reach the call
      # to event_data. We isolate curve_data_rv so that we only respond
      # to click events (and not updates to curve_data_rv)
      shiny::req(isolate(curve_data_rv()))

      click_data(data)
    })

    # Act on a click: find the predictor for the clicked point and update the
    # sub plot, then clear click_data so this click isn't processed again.
    shiny::observe({
      data <- click_data()

      if (is.null(data)) {
        return()
      }

      for (i in seq_len(nrow(data))) {
        row <- data[i, ]
        curve_number <- row$curveNumber + 1
        point_number <- row$pointNumber + 1

        curve_data <- curve_data_rv()[[curve_number]]
        point_data <- curve_data$df[point_number, ]
        predictor <- point_data$predictor
        if (!is.null(predictor)) {
          sub_plot_predictor(predictor)
          break()
        }
      }

      # Reset so the same (now-consumed) click won't be replayed on the next
      # reactive invalidation.
      click_data(NULL)
    })

    # Make the main plot
    output$main_plot <- plotly::renderPlotly({
      if (is.null(model_definitions())) {
        return(make_general_plot(
          NULL,
          model_definitions()
        ))
      }

      plot_render_safely(function() {
        all_curve_data <- list()

        # Go through all models and calculate the A vs B curves
        for (model_data in selected_models()) {
          # Check if we can use the cached old data for the current model
          model_params <- list(
            a_group = exposure_groups$a,
            b_group = exposure_groups$b
          )
          cache_key <-
            list("rr_a_vs_b", model_data$model_id, input$display_mode)
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
              b_group = exposure_groups$b,
              input$display_mode
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
        }

        x_limits <- if (input$display_mode == "rr") {
          if (input$logarithmic) {
            # Log-scale slider stores exponents; convert back via 10^e.
            log_range <- range_rr_log$range()
            if (!is.null(log_range)) 10^log_range else NULL
          } else {
            range_rr_linear$range()
          }
        } else {
          if (input$logarithmic) {
            # AD values can be negative, so the slider stores signed exponents.
            # Conversion: sign(e) * 10^|e|, so -2 -> -100 pp, +2 -> +100 pp.
            log_range <- range_ad_log$range()
            if (!is.null(log_range)) {
              ifelse(log_range == 0, 0, sign(log_range) * 10^abs(log_range))
            } else NULL
          } else {
            range_ad_linear$range()
          }
        }
        
        curve_data_rv(all_curve_data)

        make_general_plot(
          all_curve_data,
          model_definitions(),
          scale = dplyr::case_when(
            !input$logarithmic              ~ "linear",
            input$display_mode == "ad"      ~ "pseudo_log",
            TRUE                            ~ "log10"
          ),
          flip_coords = TRUE,
          theme_args = list(axis.title.y = ggplot2::element_blank()),
          plot_type = "point",
          ylim_override = x_limits,
          show_reference_line = dplyr::case_when(
            input$display_mode == "ad" ~ 0,
            TRUE ~ 1
          ),
          source = shiny::NS(id, "main_plot")
        )
      })
    })

    # The info section showing "Your estimated risk", "Reference risk",
    # and "Overall RR"
    output$info <- shiny::renderUI({
      html <- list()
      for (curve_data in curve_data_rv()) {
        # Gather all data
        model_id <- curve_data[["model_id"]]
        model_title <- selected_models()[[model_id]][["title"]]
        a_pr <- curve_data[["a_pr"]]
        b_pr <- curve_data[["b_pr"]]
        delta_pr <- a_pr - b_pr
        overall_rr <- curve_data[["overall_rr"]]

        # Create the HTML
        format <- "%.1f"
        html[[length(html) + 1]] <- shiny::div(
            shiny::tags$table(
              style = "width: 100%;",
              shiny::tags$tr(
                shiny::tags$td(
                  style = paste(
                    "width: 15%; vertical-align: top;",
                    "padding-right: 10px;"
                  ),
                  shiny::tags$b(
                    paste0(model_title, ":")
                  )
                ),
                shiny::tags$td(
                  style = paste(
                    "width: 28.33%; vertical-align: top;
                    padding-right: 10px;"
                  ),
                  paste0(
                    "Your estimated risk: ",
                    sprintf(format, a_pr * 100),
                    "%"
                  )
                ),
                shiny::tags$td(
                  style = paste(
                    "width: 28.33%; vertical-align: top;
                    padding-right: 10px;"
                  ),
                  paste0("Reference risk: ", sprintf(format, b_pr * 100), "%")
                ),
                shiny::tags$td(
                  style = "width: 28.33%; vertical-align: top",
                  "Overall RR:",
                  shiny::HTML(paste0(sprintf(format, overall_rr), "&#215;")),
                  paste0(
                    " (",
                    ifelse(delta_pr < 0, "", "+"),
                    sprintf(format, delta_pr * 100), " pts)"
                  )
                )
              )
            )
        )
      }

      html
    })
  })
}

#' Calculate Relative Risk Curve: A vs B
#'
#' Builds a dataset that decomposes the difference between the A group and the
#' B (reference) group into per-predictor contributions. One row is added for
#' every predictor in the model (both categorical and continuous): that row is
#' the A group with only that single predictor swapped to its B value, holding
#' all other A values fixed. The unmodified A and B groups are also evaluated so
#' the overall A-vs-B comparison and each group's predicted risk can be reported.
#'
#' The model pipeline is run on all rows at once. For each per-predictor row,
#' the relative risk is the A group's predicted risk divided by that row's
#' predicted risk, and the absolute difference is the same subtraction times
#' 100 (percentage points); both measures therefore isolate the effect of
#' swapping that one predictor to its B value. The overall relative risk is the
#' A group's predicted risk divided by the B group's predicted risk.
#'
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param a_group A named list of predictor values representing the
#'   A group profile.
#' @param b_group A named list of predictor values representing the
#'   B (reference) group profile. This group's predicted risk is used
#'   as the denominator for all relative risk calculations.
#' @param display_mode A string selecting which measure is the primary
#'   plotted value: \code{"rr"} for relative risk or \code{"ad"} for absolute
#'   difference. Both measures are always computed; this only controls which is
#'   used as the primary value (with the other exposed alongside it) and the
#'   axis and title labels.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
#'
#' @noRd
#' @keywords internal
.calculate_rr_a_vs_b_curve <- function(
  model_data,
  a_group,
  b_group,
  display_mode
) {
  # Get the x-axis label given the specified main label (eg. the predictor name)
  # and the specified sub labels (eg. the predictor categorical values)
  # If html is TRUE then an HTML string is returned, otherwise a plain-text
  # string is returned.
  get_x_axis_label <- function(main_label, a_value, b_value, html = TRUE) {
    label <- main_label
    if (html) {
      label <- paste0("<b>", label, "</b>")
      arrow <- "&#8594;"
      # We use "\n" instead of <br /> for the new line. Both are treated
      # as new lines when rendering, but Plotly doesn't calculate text widths
      # properly if <br /> is used (in order to determine the width of
      # the left part of the plot, where the labels are located). If <br />
      # is used, then Plotly doesn't treat it as a new line, instead it
      # treats the text as one single line when calculating the width, leading
      # to a large blank area on the far left of the plot, left of the labels.
      new_line <- "\n"
    } else {
      arrow <- "->"
      new_line <- "\n"
    }
    if (
      !is.null(b_value) && stringr::str_length(b_value) > 0 &&
      !is.null(a_value) && stringr::str_length(a_value) > 0
    ) {
      label <- paste0(label, new_line, b_value, arrow, a_value)
    }
    label
  }

  rows <- list()
  row_names <- list()
  row_comparisons <- list()
  predictors <- list()
  a_values <- list()
  b_values <- list()

  # First row is the unmodified A group
  rows[[length(rows) + 1]] <- a_group
  row_names[[length(row_names) + 1]] <- "A"
  predictors[[length(predictors) + 1]] <- ""

  # Second row is the unmodified B group
  rows[[length(rows) + 1]] <- b_group
  row_names[[length(row_names) + 1]] <- "B"
  predictors[[length(predictors) + 1]] <- ""

  for (idx in seq_along(names(a_group))) {
    predictor <- names(a_group)[[idx]]
    predictor_label <- get_variable_label(
      model_data,
      predictor,
      escape_html = TRUE
    )

    # Create the denominator: a_group with predictor set b_group[[predictor]]
    cur_ref_group <- a_group
    cur_ref_group[[predictor]] <- b_group[[predictor]]
    rows[[length(rows) + 1]] <- cur_ref_group
    predictors[[length(predictors) + 1]] <- predictor

    # Create the row name (description of the row)
    if (is_variable_categorical(model_data, predictor)) {
      cur_b_label <- get_variable_label_from_value(
        model_data,
        predictor,
        b_group[[predictor]],
        escape_html = TRUE
      )
      cur_a_label <- get_variable_label_from_value(
        model_data,
        predictor,
        a_group[[predictor]],
        escape_html = TRUE
      )
      row_names[[length(row_names) + 1]] <- get_x_axis_label(
        predictor_label,
        cur_a_label,
        cur_b_label
      )
    } else {
      cur_b_value <- b_group[[predictor]]
      cur_a_value <- a_group[[predictor]]
      row_names[[length(row_names) + 1]] <- get_x_axis_label(
        predictor_label,
        cur_a_value,
        cur_b_value
      )
    }
  }

  df <- do.call(rbind.data.frame, rows)

  # Run the pipeline with the input matrix and calculate the relative risk
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df
  )

  # Calculate relative risk and absolute difference.
  # dat[1, ] is group A
  # dat[2, ] is group B
  rr <- dat[1, ] / dat[2:nrow(dat), ]
  ad <- (dat[1, ] - dat[2:nrow(dat), ]) * 100
  overall_rr <- dat[1, ] / dat[2, ]
  output_df <- data.frame(
    # x = rr[2:length(rr)],
    RR = rr[2:length(rr)],
    AD = ad[2:length(ad)],
    predictor = unlist(predictors[3:length(predictors)]),
    Model = cleanup_string(model_data$title),
    Label = unlist(row_names[3:nrow(dat)])
  )

  list(
    df = output_df,
    overall_rr = overall_rr,
    a_pr = dat[1, ],
    b_pr = dat[2, ],
    model_id = model_data$model_id,
    x_axis_label = "Label",
    y_axis_label = dplyr::case_when(
      display_mode == "rr" ~ "Relative Risk",
      display_mode == "ad" ~ "Absolute Difference",
      TRUE ~ "Unknown"
    ),
    title = dplyr::case_when(
      display_mode == "rr" ~ "Relative Risk",
      display_mode == "ad" ~ "Absolute Difference",
      TRUE ~ "Unknown"
    ),
    x_axis_type = "Categorical",
    aes_args = list(
      x = rlang::sym("Label"),
      y = rlang::sym(dplyr::case_when(
        display_mode == "rr" ~ "RR",
        display_mode == "ad" ~ "AD",
        TRUE ~ "RR"
      )),
      other = rlang::sym(dplyr::case_when(
        display_mode == "rr" ~ "AD",
        display_mode == "ad" ~ "RR",
        TRUE ~ "AD"
      ))
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
  shiny::tagList(
    shiny::div(
      style = glue::glue(
        "width: 100%; overflow-x: visible; overflow-y: scroll; ",
        "height: calc(100vh - {external_height}px)"
      ),
      shiny::br(),
      shiny::uiOutput(shiny::NS(id, "info")),
      shiny::hr(style = "margin-bottom: 17px;"),
      plot_additional_controls_container(
        plot_additional_controls_dropdown(
          id = shiny::NS(id, "display_mode"),
          label = "Show",
          choices = c(
              "Relative Risk" = "rr",
              "Absolute Difference" = "ad"
            ),
          num_columns = 2
        ),
        plot_additional_controls_checkbox(
          id = shiny::NS(id, "logarithmic"),
          label = "Logarithmic",
          value = TRUE,
          num_columns = 2
        )
      ),

      shiny::uiOutput(shiny::NS(id, "plot_container")),
      shiny::div(
        style = "padding: 0 15px",
        shiny::conditionalPanel(
          condition = "input.logarithmic && input.display_mode === 'rr'",
          ns = shiny::NS(id),
          rangeSelectorUI(shiny::NS(id, "x_range_rr_log"), "logarithmic")
        ),
        shiny::conditionalPanel(
          condition = "!input.logarithmic && input.display_mode === 'rr'",
          ns = shiny::NS(id),
          rangeSelectorUI(shiny::NS(id, "x_range_rr_linear"), "linear")
        ),
        shiny::conditionalPanel(
          condition = "input.logarithmic && input.display_mode === 'ad'",
          ns = shiny::NS(id),
          rangeSelectorUI(
            shiny::NS(id, "x_range_ad_log"), "logarithmic",
            label = "X Axis Range (signed log10 scale):",
            min = -2, max = 2, value = c(-2, 2)
          )
        ),
        shiny::conditionalPanel(
          condition = "!input.logarithmic && input.display_mode === 'ad'",
          ns = shiny::NS(id),
          rangeSelectorUI(
            shiny::NS(id, "x_range_ad_linear"), "linear",
            min = -100, max = 100, value = c(-100, 100)
          )
        )
      ),

      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            width = 9,
            plotly::plotlyOutput(
              shiny::NS(id, "sub_plot"),
              height = 300
            )
          ),
          shiny::column(
            width = 3,
            shiny::uiOutput(shiny::NS(id, "sub_plot_instructions"))
          )
        )
      )
    )
  )
}
