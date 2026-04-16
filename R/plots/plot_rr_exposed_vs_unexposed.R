#' Exposed vs Unexposed Curve
#'
#' Functions for computing and rendering Exposed vs Unexposed curves.
NULL

plot_id <- "rr_exposed_vs_unexposed_plot"

# Tags to add to the exposed and unexposed predictor control IDs
exposed_group_extra_tag <- "exposed"
unexposed_group_extra_tag <- "unexposed"

#' Build an Exposed vs Unexposed Plot Module Server
#'
#' Shiny module server that renders an interactive exposed vs unexposed
#' relative risk plot. The module also owns the group controls UI
#' (\code{output$group_controls}) which lets users set the predictor values
#' for the exposed and unexposed groups.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotRRExposedUnexposedUI}}).
#' @param predictor A reactive expression returning the currently selected
#'   predictor variable name (character string).
#' @param interaction_predictor A reactive expression returning the currently
#'   selected interaction predictor variable name.
#' @param logarithmic A reactive expression returning a logical indicating
#'   whether to use a logarithmic y-axis scale.
#' @param selected_models A reactive expression returning the list of model
#'   data objects to plot curves for. This is a subset of
#'   \code{model_definitions()$models}.
#' @param selected_reference_groups A reactive expression returning a named
#'   list of reference group predictor values, keyed by model ID.
#' @param model_definitions A reactive expression (or \code{reactiveVal})
#'   returning the top-level model definitions object, or \code{NULL} if no
#'   algorithm is loaded.
#'
#' @return \code{NULL}, called for side effects.
plotRRExposedUnexposedServer <- function(
  id,
  predictor,
  interaction_predictor,
  logarithmic,
  selected_models,
  selected_reference_groups,
  model_definitions
) {
  shiny::moduleServer(id, function(input, output, session) {
    predictor_controls_env <- initialize_predictor_controls_env()

    # Cached curve data, to avoid unncessary recalculation of curves that have
    # already been calculated
    cached_curves <- initialize_cached_data()

    observe({
      # React whenever new model definitions are loaded
      model_definitions()
      # Clear the cached curve data, since they are no longer valid.
      clear_cached_data(cached_curves)
    }, priority = 10000)

    # Create the exposed/unexposed group controls
    output$group_controls <- renderUI({
      destroy_all_predictor_controls(predictor_controls_env)

      if (!is.null(model_definitions())) {
        # Use the first model's reference group data for the default
        # predictor values
        model_data <- head(model_definitions()$models, 1)
        model_data <- model_data[[names(model_data)[1]]]
        reference_group <- model_data$reference_group

        exposed_predictor_ctrl <- create_predictor_controls(
          predictor_controls_env,
          session,
          model_data,
          initial_predictor_values = reference_group,
          extra_tag = exposed_group_extra_tag,
          model_name = "Exposed Group",
          show_model_color = FALSE
        )
        # Unexposed group controls
        unexposed_predictor_ctrl <- create_predictor_controls(
          predictor_controls_env,
          session,
          model_data,
          extra_tag = unexposed_group_extra_tag,
          model_name = "Unexposed Group",
          show_model_color = FALSE
        )

        return(tagList(exposed_predictor_ctrl$ui, hr(), unexposed_predictor_ctrl$ui))
      }
    })

    # Make the plot
    output$plot <- plotly::renderPlotly({
      if (is.null(model_definitions())) {
        return(make_general_plot(
          NULL,
          model_definitions()
        ))
      }

      tryCatch(
        {
          all_curve_data <- list()

          # Go through all models and calculate the exposed vs unexposed curves
          for (model_data in selected_models()) {
            # Use the first model's reference group data for the default
            # predictor values
            exposed_model_data <- head(model_definitions()$models, 1)
            exposed_model_data <- exposed_model_data[[names(exposed_model_data)[1]]]

            exposed_group <- get_predictor_controls_values(
              predictor_controls_env,
              exposed_model_data,
              extra_tag = exposed_group_extra_tag,
              rv_isolate = FALSE
            )
            unexposed_group <- get_predictor_controls_values(
              predictor_controls_env,
              exposed_model_data,
              extra_tag = unexposed_group_extra_tag,
              rv_isolate = FALSE
            )

            # Check if we can use the cached old data for the current model
            model_params <- list(
              exposed_group = exposed_group,
              unexposed_group = unexposed_group
            )
            cache_key <- list("rr_exposed_vs_unexposed", model_data$model_id)
            if (
              is_reusable_cached_data(
                cached_curves,
                cache_key,
                model_params
              )
            ) {
              # Reuse the old data
              all_curve_data[[length(all_curve_data) + 1]] <-
                get_cached_data(cached_curves, cache_key)
            } else {
              # Get predictor type (Categorical or Continuous)
              tic <- Sys.time()

              # Calculate the RR curve for the model
              curve_data <- .calculate_rr_exposed_vs_unexposed_curve(
                model_data = model_data,
                exposed_group = exposed_group,
                unexposed_group = unexposed_group
              )

              elapsed <- Sys.time() - tic
              message(paste0(
                "Elapsed time for Exposed vs Unexposed curve ",
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
          make_general_plot(
            all_curve_data,
            model_definitions(),
            logarithmic(),
            flip_coords = TRUE,
            theme_args = list(axis.title.y = ggplot2::element_blank()),
            plot_type = "point"
          )
        },
        error = function(e) {
          traceback()
          message(e$message)
          make_message_plot(
            glue::glue("<b>Error</b>: {e$message}"),
            color = "red"
          )
        }
      )
    })

    # Make sure output$group_controls is rendered before output$plot, to ensure that the
    # Exposed vs Unexposed group controls are created before plotting
    outputOptions(output, "group_controls", priority = 1)
    outputOptions(output, "plot", priority = 0)
  })
}

#' Calculate Relative Risk Curve: Exposed vs Unexposed
#'
#' Builds a curve dataset comparing the relative risk of an exposed group
#' against an unexposed reference group across all categorical predictors in
#' the model. For each categorical predictor, a row is added for every possible
#' value of that predictor (with the exposed group otherwise held fixed).
#' Continuous predictors are omitted from the per-predictor rows because they
#' do not vary between the exposed and unexposed groups and would be redundant;
#' their contribution is captured in the overall "Exposed vs Unexposed" row.
#' Relative risks are computed by running the model pipeline on all rows and
#' dividing each row's predicted risk by the unexposed group's predicted risk.
#'
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param exposed_group A named list of predictor values representing the
#'   exposed group profile.
#' @param unexposed_group A named list of predictor values representing the
#'   unexposed (reference) group profile. This group's predicted risk is used
#'   as the denominator for all relative risk calculations.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
.calculate_rr_exposed_vs_unexposed_curve <- function(
  model_data,
  exposed_group,
  unexposed_group
) {
  rows <- list()
  row_names <- list()
  row_comparisons <- list()

  # First row is the unmodified exposed group
  rows[[length(rows) + 1]] <- exposed_group
  row_names[[length(row_names) + 1]] <- "<b>Overall</b>"
  row_comparisons[[length(row_comparisons) + 1]] <- "Exposed vs Unexposed"

  for (idx in seq_along(names(exposed_group))) {
    predictor <- names(exposed_group)[[idx]]
    predictor_label <- get_variable_label(
      model_data,
      predictor,
      escape_html = TRUE
    )
    unexposed_value <- unexposed_group[[predictor]]

    if (is_variable_categorical(model_data, predictor)) {
      # Add a row for each allowable value of the predictor. The exposure will
      # be exposed_group but with each allowable value of the current predictor
      # set in the exposed group (eg. for marital status, we could have one row
      # for "Married", one for "Single", and one for
      # "Widowed/separated/divorced")
      predictor_allowable_values <- get_predictor_allowable_values(model_data, predictor)
      for (cur_exposed_value in predictor_allowable_values) {
        # Add the exposed row, with the predictor set to cur_exposed_value
        cur_group <- exposed_group
        cur_group[[predictor]] <- cur_exposed_value
        rows[[length(rows) + 1]] <- cur_group

        # Calculate the name (eg. "Marital status (Married)") of the new row and
        # the comparison label (eg. "Marital status (Married vs Single)")
        cur_exposed_label <- get_variable_label_from_value(
          model_data,
          predictor,
          cur_exposed_value,
          escape_html = TRUE
        )
        exposed_label <- get_variable_label_from_value(
          model_data,
          predictor,
          exposed_group[[predictor]],
          escape_html = TRUE
        )
        row_name <-
          as.character(glue::glue("{predictor_label} ({cur_exposed_label})"))
        row_names[[length(row_names) + 1]] <- row_name
        row_comparison <- as.character(glue::glue(
          "{predictor_label} ({cur_exposed_label} vs {exposed_label})"
        ))
        row_comparisons[[length(row_comparisons) + 1]] <- row_comparison
      }
    } else {
      # All continuous variables will have the same exposed values, and therefore the same
      # relative risk. To avoid the redundant information, the continuous variables are not
      # added to the curve and instead are combined into the first "Overall" row that
      # was added above.
      # cur_group <- exposed_group
      # if (predictor %in% names(exposed_group)) {
      #   exposed_value <- exposed_group[[predictor]]
      # }
      # rows[[length(rows) + 1]] <- cur_group
      # row_names[[length(row_names) + 1]] <- predictor_label
      # row_comparison <- as.character(glue::glue("{predictor_label} ({exposed_value} vs {unexposed_value})"))
      # row_comparisons[[length(row_comparisons) + 1]] <- "Exposed vs Unexposed" #row_comparison
    }
  }

  # The unexposed group is the last row. All risks calculated from previous
  # rows are compared to this one.
  rows[[length(rows) + 1]] <- unexposed_group

  df <- do.call(rbind.data.frame, rows)

  # Run the pipeline with the input matrix and calculate the relative risk
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df
  )

  # Calculate the relative risk. The risks are relative to the
  # risk in the last row.
  rr <- dat[seq_len(nrow(dat) - 1), ] / dat[nrow(dat), ]

  output_df <- data.frame(
    x = rr,
    RR = rr,
    Model = cleanup_string(model_data$title),
    Label = unlist(row_names[seq_len(nrow(dat) - 1)]),
    Comparison = unlist(row_comparisons[seq_len(nrow(dat) - 1)])
  )

  list(
    df = output_df,
    x_axis_label = "Label",
    y_axis_label = "Relative Risk",
    title = "Relative Risk",
    x_axis_type = "Categorical",
    # @TODO: Fix this! We need to figure out a good way to specify
    # the limits. We may want to add a UI control for it.
    ylim_logarithmic = c(0.001, 100),
    aes_args = list(
      x = dplyr::sym("Label"),
      y = dplyr::sym("RR"),
      Comparison = dplyr::sym("Comparison")
    )
  )
}

#' Build the Exposed vs Unexposed Plot UI
#'
#' Returns the UI elements to insert into the Exposed vs Unexposed tab panel.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotRRExposedUnexposedServer}}).
#' @param plot_height Character or numeric specifying the height of the plot
#'   area. Passed to \code{\link[plotly]{plotlyOutput}} as the \code{height}
#'   argument.
#' @param model_definitions A reactive expression (or \code{reactiveVal})
#'   returning the top-level model definitions object.
#'
#' @return A \code{\link[shiny]{tagList}} containing the panel UI elements.
plotRRExposedUnexposedUI <- function(
  id,
  plot_height,
  model_definitions
) {
  tagList(
    br(),
    fluidRow(
      style = "width: 100%",
      column(
        9,
        plotly::plotlyOutput(shiny::NS(id, "plot"), height = plot_height),
        style = "z-index: 20"
      ),
      column(
        3,
        div(
          style = paste(
            "height: calc(100vh - 160px);",
            "margin-bottom: 20px;",
            "overflow-y: scroll;"
          ),
          uiOutput(shiny::NS(id, "group_controls"))
        )
      )
    )
  )
}
