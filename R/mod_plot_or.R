#' Odds Ratio Curve
#'
#' Functions for computing and rendering odds ratio (OR) curves.
#'
#' @name mod_plot_or
#' @noRd
#' @keywords internal
NULL

#' Build an Odds Ratio Plot Module Server
#'
#' Shiny module server that renders an interactive odds ratio (OR) curve for
#' the currently selected predictor, across all selected models.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotORUI}}).
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
#'
#' @noRd
#' @keywords internal
plotORServer <- function(
  id,
  selected_models,
  selected_reference_groups,
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
        # Populate controls
        populate_controls()
      },
      priority = 10000
    )

    populate_controls <- function() {
      # Freeze so dependents don't trigger with the old values in the inputs.
      # If these inputs are attempted to be accessed during the current frame,
      # the access will raise a silent exception (like req(FALSE)) and stop
      # execution momentarily for the current flush cycle (but will then be
      # re-triggered with the new input values).
      shiny::freezeReactiveValue(input, "predictor")
      shiny::freezeReactiveValue(input, "interaction_predictor")
      
      # Populate UI for "predictor" and "interaction_predictor" dropdowns
      populate_dropdown_predictors(
        session,
        id = "predictor",
        models = model_definitions()$models,
        empty = is.null(model_definitions())
      )
      populate_dropdown_interaction_predictors(
        session,
        id = "interaction_predictor",
        models = model_definitions()$models,
        empty = is.null(model_definitions())
      )
    }

    output$plot <- plotly::renderPlotly({
      shiny::req(input$predictor)
      input$interaction_predictor

      if (is.null(model_definitions())) {
        return(make_general_plot(
          NULL,
          model_definitions()
        ))
      }
      plot_render_safely(function() {
        all_curve_data <- list()

        # Go through all selected_models and calculate the OR curves
        # We concatenate them (with bind_rows) to show one curve per model
        for (model_data in selected_models()) {
          predictor_values <-
            selected_reference_groups()[[model_data$model_id]]

          # Check if we can use the cached old data for the current model
          model_params <- list(
            predictor = input$predictor,
            interaction_predictor = input$interaction_predictor,
            reference_group = predictor_values
          )
          cache_key <- list(
            "or",
            model_data$model_id,
            input$predictor,
            input$interaction_predictor
          )
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
            tic <- Sys.time()

            # Calculate the OR curve for the model
            if (
              length(input$interaction_predictor) == 0 ||
              input$interaction_predictor == config_get_empty_selection()
            ) {
              curve_data <- .calculate_or_curve(
                input$predictor,
                model_data,
                reference_group = predictor_values
              )
            } else {
              curve_data <- .calculate_or_curve_interaction(
                input$predictor,
                input$interaction_predictor,
                model_data,
                reference_group = predictor_values
              )
            }

            elapsed <- Sys.time() - tic
            message(paste0(
              "Elapsed time for OR curve ", model_data$model_id, ": ", elapsed
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
          scale = if (input$logarithmic) "log10" else "linear"
        )
      })
    })
  })
}

#' Calculate Odds Ratio Curve for a Predictor
#'
#' Computes odds ratios for a predictor variable across its allowable values,
#' relative to a reference value.
#'
#' @param predictor Character string specifying the variable name.
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param predictor_allowable_values Numeric vector of predictor values to
#'   evaluate. If NULL, uses the allowable values from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
#'
#' @noRd
#' @keywords internal
.calculate_or_curve <- function(
  predictor,
  model_data,
  predictor_allowable_values = NULL,
  reference_group = NULL
) {
  predictor_allowable_values <- predictor_allowable_values %||%
    model_data$predictor_allowable_values[[predictor]]
  reference_group <- reference_group %||% model_data$reference_group

  predictor_label <- get_variable_label_and_units(
    model_data, predictor,
    escape_html = TRUE
  )
  predictor_label_no_units <- get_variable_label(
    model_data, predictor,
    escape_html = TRUE
  )
  predictor_reference_value <- reference_group[[predictor]]
  output_rows <- length(predictor_allowable_values)

  # Create the input matrix (duplicate reference_group for each
  # value in predictor_allowable_values, set the predictor to the
  # predictor_allowable_values, and add an extra unmodified reference group
  # to the end, at index output_rows+1)
  df <- data.frame(reference_group)
  df <- df[rep(1, output_rows + 1), ] # Add extra unmodified reference group
  df[predictor] <- append(predictor_allowable_values, predictor_reference_value)
  rownames(df) <- seq_len(nrow(df))

  # Run the pipeline with the input matrix and calculate the odds ratios.
  # The odds ratio is odds / reference_group_odds, where odds is calculated
  # as predicted_risk / (1 - predicted_risk)
  # Note that the reference group is located at row output_rows + 1
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df
  )

  # Odds ratio is the predicted risk for each row, divided by the predicted
  # risk of the last row (last row is the unmodified reference group)
  predicted_col <- colnames(dat)[[1]]
  or <- (dat[[predicted_col]] / (1 - dat[[predicted_col]])) /
    (dat[[predicted_col]][output_rows + 1] /
    (1 - dat[[predicted_col]][output_rows + 1]))

  labels <- convert_df_variable_to_label(
    df, model_data, predictor, predictor
  )[[predictor]][1:output_rows]

  if (is_variable_categorical(model_data, predictor)) {
    ref_label <- get_variable_label_from_value(
      model_data, predictor, predictor_reference_value,
      escape_html = TRUE
    )
  } else {
    ref_label <- predictor_reference_value
  }

  # Create the DataFrame of odds ratios
  output_df <- data.frame(
    x = predictor_allowable_values[1:output_rows],
    OR = or[1:output_rows],
    Model = cleanup_string(model_data$title),
    Comparison = glue::glue(
      "{predictor_label_no_units} ({labels} vs {ref_label})"
    )
  )
  names(output_df)[1] <- predictor_label

  output_df <- convert_df_variable_to_label(
    output_df, model_data, predictor, predictor_label,
    escape_html = TRUE
  )

  list(
    df = output_df,
    x_axis_label = predictor_label,
    y_axis_label = "Odds Ratio",
    title = predictor_label,
    x_axis_type = ifelse(
      is_variable_categorical(model_data, predictor),
      "Categorical",
      "Continuous"
    ),
    aes_args = list(
      x = rlang::sym(predictor_label),
      y = rlang::sym("OR"),
      label = rlang::sym("Comparison")
    )
  )
}

#' Calculate Odds Ratio Curve with Interaction
#'
#' Computes odds ratios showing the effect of a one-unit change in
#' interaction_predictor across the allowable values of another predictor.
#'
#' @param predictor Character string specifying the primary variable name
#'   for the x-axis.
#' @param interaction_predictor Character string specifying the variable whose
#'   effect (one-unit change) is being measured.
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param predictor_allowable_values Numeric vector of predictor values to
#'   evaluate. If NULL, uses the allowable values from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
#'
#' @noRd
#' @keywords internal
.calculate_or_curve_interaction <- function(
  predictor,
  interaction_predictor,
  model_data,
  predictor_allowable_values = NULL,
  reference_group = NULL
) {
  predictor_allowable_values <- predictor_allowable_values %||%
    model_data$predictor_allowable_values[[predictor]]
  reference_group <- reference_group %||% model_data$reference_group

  predictor_label <- get_variable_label_and_units(
    model_data, predictor,
    escape_html = TRUE
  )
  interaction_predictor_label <- get_variable_label(
    model_data, interaction_predictor,
    escape_html = TRUE
  )

  output_rows <- length(predictor_allowable_values)

  # df2 is the reference group (spanning all values of
  # predictor_allowable_values for the predictor), while df1 is the target
  # group in the numerator of the RR calculation. df1 is the same as df2, but
  # with the interaction predictor increased by one.
  df2 <- data.frame(reference_group)
  df2 <- df2[rep(1, output_rows), ]
  df2[[predictor]] <- predictor_allowable_values
  rownames(df2) <- seq_len(nrow(df2))
  df1 <- data.frame(df2)

  # df1 is the same as df2 but with interaction_predictor increased by 1
  # (relative to the reference_group)
  if (is_variable_categorical(model_data, interaction_predictor)) {
    cat_val <- reference_group[[interaction_predictor]]

    # Advance cat_val to the next category
    allowable_values <- get_predictor_allowable_values(
      model_data, interaction_predictor
    )
    indices <- unlist(df1[[interaction_predictor]])
    indices <- lapply(indices, function(x) which(x == allowable_values))
    indices <- lapply(
      indices, function(x) (x %% length(allowable_values)) + 1
    )
    indices <- unlist(indices)
    cat_val <- allowable_values[indices]

    df1[[interaction_predictor]] <- cat_val
  } else {
    df1[[interaction_predictor]] <- df1[[interaction_predictor]] + 1
  }

  # Run the pipeline with the input matrix and calculate the odds ratios.
  # The odds ratio is odds / reference_group_odds, where odds is calculated
  # as predicted_risk / (1 - predicted_risk)
  dat1 <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df1
  )
  dat2 <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df2
  )

  predicted_col_1 <- colnames(dat1)[[1]]
  predicted_col_2 <- colnames(dat2)[[1]]
  or <- (dat1[[predicted_col_1]] / (1 - dat1[[predicted_col_1]])) /
    (dat2[[predicted_col_2]] / (1 - dat2[[predicted_col_2]]))

  # Convert the variable IDs (eg. clc_age) to the variable labels
  # (eg. Age)
  labels1 <- convert_df_variable_to_label(
    df1, model_data, interaction_predictor, interaction_predictor,
    escape_html = TRUE
  )[[interaction_predictor]]
  labels2 <- convert_df_variable_to_label(
    df2, model_data, interaction_predictor, interaction_predictor,
    escape_html = TRUE
  )[[interaction_predictor]]

  # Create the DataFrame of odds ratios
  output_df <- data.frame(
    x = predictor_allowable_values,
    OR = or[1:output_rows],
    Model = cleanup_string(model_data$title),
    Comparison = glue::glue(
      "{interaction_predictor_label} ({labels1} vs {labels2})"
    )
  )
  names(output_df)[1] <- predictor_label

  output_df <- convert_df_variable_to_label(
    output_df, model_data, predictor, predictor_label
  )

  interaction_predictor_label <- get_variable_label_and_units(
    model_data, interaction_predictor,
    escape_html = TRUE
  )
  title <- glue::glue(
    "{predictor_label} [interaction = {interaction_predictor_label}]"
  )

  list(
    df = output_df,
    x_axis_label = predictor_label,
    y_axis_label = "Odds Ratio",
    title = title,
    x_axis_type = ifelse(
      is_variable_categorical(model_data, predictor),
      "Categorical",
      "Continuous"
    ),
    aes_args = list(
      x = rlang::sym(predictor_label),
      y = rlang::sym("OR"),
      label = rlang::sym("Comparison")
    )
  )
}

#' Build the Odds Ratio Plot UI
#'
#' Returns the UI elements to insert into the Odds Ratio tab panel.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotORServer}}).
#' @param external_height Height, in pixels, of the area outside of the plot
#'   area (in the main tabs). If a plot wants to fill up the height of the page,
#'   without causing overflow at the bottom (and hence scrolling), then a plot's
#'   height should be "calc(100vh - {external_height}px)".
#'
#' @return A \code{\link[shiny]{tagList}} containing the panel UI elements.
#'
#' @noRd
#' @keywords internal
plotORUI <- function(
  id,
  external_height
) {
  shiny::tagList(
    shiny::br(),
    plot_additional_controls_container(
      plot_additional_controls_dropdown(
        id = shiny::NS(id, "predictor"),
        label = "Predictor",
        choices = c(),
        num_columns = 3
      ),
      plot_additional_controls_dropdown(
        id = shiny::NS(id, "interaction_predictor"),
        label = "Interaction Predictor",
        choices = c(),
        num_columns = 3
      ),
      plot_additional_controls_checkbox(
        id = shiny::NS(id, "logarithmic"),
        label = "Logarithmic",
        value = TRUE,
        num_columns = 3
      )
    ),
    plotly::plotlyOutput(
      shiny::NS(id, "plot"),
      height = glue::glue("calc(100vh - {external_height + plot_additional_controls_height()}px)")
    )
  )
}
