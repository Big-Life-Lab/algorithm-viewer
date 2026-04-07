#' Exposed vs Unexposed Curve
#'
#' Functions for computing and rendering Exposed vs Unexposed curves.
NULL

local({
plot_id <- "rr_exposed_vs_unexposed_plot"

# Tags to add to the exposed and unexposed predictor control IDs
exposed_group_extra_tag <- "exposed"
unexposed_group_extra_tag <- "unexposed"

#' Build a Exposed vs Unexposed Plot
#'
#' @param session The Shiny \code{session} object.
#' @param models A list of model data objects to plot curves for. This is a
#'   subset of model_definitions$models.
#' @param model_definitions The top-level model definitions object.
#' @param predictor_controls_env An environment holding the all predictor
#'   controls used to specify predictor values. This includes the controls
#'   for specifying reference groups and other predictor values used for
#'   plotting. Used the predictor controls manager utility functions.
#' @param cached_curve_env An environment used to cache curve data between
#'   renders so that unchanged models do not trigger redundant pipeline runs.
#'   Used by the cached curve data utility functions.
#'
#' @return A \code{ggplot} object (or a message plot on error / missing data),
#'   as returned by \code{\link{make_general_plot}} or
#'   \code{\link{make_message_plot}}.
make_rr_exposed_vs_unexposed_plot <- function(
  session,
  models,
  model_definitions,
  predictor_controls_env,
  cached_curve_env
) {
  req(session$userData$predictor)

  if (is.null(model_definitions)) {
    return(make_general_plot(
      NULL,
      model_definitions
    ))
  }

  tryCatch(
    {
      all_curve_data <- list()
      predictor <- session$userData$predictor

      # Go through all models and calculate the OR curves
      # We concatenate them (with bind_rows) to show one curve per model
      for (model_data in models) {
        # Use the first model's reference group data for the default
        # predictor values
        exposed_model_data <- head(model_definitions$models, 1)
        exposed_model_data <- exposed_model_data[[names(exposed_model_data)[1]]]

        exposed_group <- get_predictor_controls_values(
          predictor_controls_env,
          exposed_model_data,
          extra_tag = exposed_group_extra_tag
        )
        unexposed_group <- get_predictor_controls_values(
          predictor_controls_env,
          exposed_model_data,
          extra_tag = unexposed_group_extra_tag
        )

        # Check if we can use the cached old data for the current model
        model_params <- list(
          exposed_group = exposed_group,
          unexposed_group = unexposed_group
        )
        if (
          is_reusable_cached_curve_data(
            cached_curve_env,
            "rr_exposed_vs_unexposed",
            model_data$model_id,
            model_params
          )
        ) {
          # Reuse the old data
          all_curve_data[[length(all_curve_data) + 1]] <-
            get_cached_curve_data(
              cached_curve_env,
              "rr_exposed_vs_unexposed",
              model_data$model_id
            )
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
          set_cached_curve_data(
            cached_curve_env,
            "rr_exposed_vs_unexposed",
            model_data$model_id,
            model_params,
            curve_data
          )
        }
      }

      make_general_plot(
        all_curve_data,
        model_definitions,
        session$userData$logarithmic,
        flip_coords = TRUE,
        theme_args = list(axis.title.y = ggplot2::element_blank()),
        plot_type = "point"
      )
    },
    error = function(e) {
      make_message_plot(
        glue::glue("<b>Error</b>: {e$message}"),
        color = "red"
      )
    }
  )
}

#' Create the UI for the Exposed vs Unexposed Plot
#'
#' Clears and rebuilds the predictor controls for the exposed and unexposed
#' groups inside the \code{#rr_plot_exposed_vs_unexposed_group} container.
#' Controls are created from the first model's reference group data and
#' inserted into the DOM immediately via \code{\link[shiny]{insertUI}}.
#'
#' @param model_definitions The top-level model definitions object.
#' @param predictor_controls_env An environment holding the all predictor
#'   controls used to specify predictor values. Used by the predictor controls
#'   manager utility functions. The predictor controls are added to this
#'   environment.
#' @param redraw_trigger A reactive value passed as the \code{change_trigger}
#'   to \code{\link{create_predictor_controls}}; any change to a predictor
#'   control will invalidate this trigger. Callers should respond to this
#'   trigger to redraw the plots.
#'
#' @return Called for its side effects (UI insertion); returns \code{NULL}
#'   invisibly.
create_rr_exposed_vs_unexposed_ui <- function(
  model_definitions,
  predictor_controls_env,
  redraw_trigger
) {
  # Ceate exposed and unexposed group predictor controls
  exposed_container_id <- "#rr_plot_exposed_vs_unexposed_group"
  shiny::removeUI(
    selector = paste0(exposed_container_id, " > *"),
    immediate = TRUE,
    multiple = TRUE
  )
  # Use the first model's reference group data for the default
  # predictor values
  model_data <- head(model_definitions$models, 1)
  model_data <- model_data[[names(model_data)[1]]]

  # Exposed group controls
  exposed_predictor_ctrl <- create_predictor_controls(
    predictor_controls_env,
    model_data,
    extra_tag = exposed_group_extra_tag,
    change_trigger = redraw_trigger,
    model_name = "Exposed Group",
    show_model_color = FALSE
  )
  # Unexposed group controls
  unexposed_predictor_ctrl <- create_predictor_controls(
    predictor_controls_env,
    model_data,
    extra_tag = unexposed_group_extra_tag,
    change_trigger = redraw_trigger,
    model_name = "Unexposed Group",
    show_model_color = FALSE
  )

  # Insert exposed/unexposed groups
  shiny::insertUI(
    selector = exposed_container_id,
    where = "afterBegin",
    ui = tagList(exposed_predictor_ctrl$ui, unexposed_predictor_ctrl$ui)
  )
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
      # Add a row for each value in the predictor range. The exposure will be
      # exposed_group but with each possible value of the current predictor set
      # in the exposed group (eg. for marital status, we could have one row
      # for "Married", one for "Single", and one for
      # "Widowed/separated/divorced")
      predictor_range <- get_predictor_range(model_data, predictor)
      for (cur_exposed_value in predictor_range) {
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

  # Run the piupeline with the input matrix and calculate the relative risk
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df
  )

  # Calcualte the relative risk. The risks are relative to the
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

#' Get the UI Elements to Insert Into the Plot's tabPanel
#'
#' @param plot_height Character or numeric specifying the size of a full-height
#'   plot. This can be passed to \code{\link[plotly]{plotlyOutput}} as the
#'   \code{height} argument.
#'
#' @return A \code{\link[shiny]{tagList}} containing the panel UI elements.
panel_ui <- function(plot_height) {
  tagList(
    br(),
    fluidRow(
      style = "width: 100%",
      column(
        9,
        plotly::plotlyOutput(plot_id, height = plot_height),
        style = "z-index: 20"
      ),
      column(3,
        div(
          id = "rr_plot_exposed_vs_unexposed_group",
          style = paste(
            "height: calc(100vh - 160px);",
            "margin-bottom: 20px;",
            "overflow-y: scroll;"
          )
        )
      )
    )
  )
}

#' Register the Plot with the Plot Manager
#'
#' This function calls \code{\link{plot_man_add_plot}} with the plot's
#' infomation.
#'
#' @param .env The plot manager environment to register this plot into.
#'
#' @return Called for its side effect of registering the plot.
plot_register <- function(.env) {
  plot_man_add_plot(
    .env,
    plot_id = plot_id,
    title = "Exposed vs Unexposed",
    make_plot_fn = make_rr_exposed_vs_unexposed_plot,
    panel_ui_fn = panel_ui,
    model_ui_fn = create_rr_exposed_vs_unexposed_ui
  )
}

# Always have the registration function returned as the last line
plot_register
})
