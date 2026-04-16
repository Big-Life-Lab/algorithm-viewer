#' Predicted Risk Curve
#'
#' Functions for computing and rendering predicted risk (PR) curves.
NULL

#' Build a Predicted Risk Plot Module Server
#'
#' Shiny module server that renders an interactive predicted risk (PR) curve
#' for the currently selected predictor, across all selected models.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotPRUI}}).
#' @param predictor A reactive expression returning the currently selected
#'   predictor variable name (character string).
#' @param interaction_predictor A reactive expression returning the currently
#'   selected interaction predictor variable name. Accepted for interface
#'   consistency with the other plot servers but not used in PR calculations.
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
plotPRServer <- function(
  id,
  predictor,
  interaction_predictor,
  logarithmic,
  selected_models,
  selected_reference_groups,
  model_definitions
) {
  shiny::moduleServer(id, function(input, output, session) {
    # Cached curve data, to avoid unncessary recalculation of curves that have
    # already been calculated
    cached_curve_env <- initialize_cached_curve_data_env()

    observe({
      # React whenever new model definitions are loaded
      model_definitions()
      # Clear the cached curve data, since they are no longer valid.
      clear_cached_curve_data(cached_curve_env)
    }, priority = 10000)

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

          # Go through all selected_models and calculate the PR curves
          # We concatenate them (with bind_rows) to show one curve per model
          for (model_data in selected_models()) {
            predictor_values <- selected_reference_groups()[[model_data$model_id]]

            # Check if we can use the cached old data for the current model
            model_params <- list(
              predictor = predictor(),
              reference_group = predictor_values
            )
            cache_key <- list("pr", model_data$model_id, predictor())
            if (
              is_reusable_cached_curve_data(
                cached_curve_env,
                cache_key,
                model_params
              )
            ) {
              # Reuse the old data
              all_curve_data[[length(all_curve_data) + 1]] <-
                get_cached_curve_data(cached_curve_env, cache_key)
            } else {
              tic <- Sys.time()

              # Calculate the PR curve for the model
              curve_data <- .calculate_pr_curve(
                predictor(),
                model_data,
                reference_group = predictor_values
              )

              elapsed <- Sys.time() - tic
              message(paste0(
                "Elapsed time for PR curve ", model_data$model_id, ": ", elapsed
              ))

              all_curve_data[[length(all_curve_data) + 1]] <- curve_data

              # Save the data to our cache
              set_cached_curve_data(
                cached_curve_env,
                cache_key,
                model_params,
                curve_data
              )
            }
          }

          make_general_plot(
            all_curve_data,
            model_definitions(),
            logarithmic()
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
  })
}

#' Calculate Predicted Risk Curve for a Predictor
#'
#' Computes predicted risk values for a predictor variable across its allowable values.
#'
#' @param predictor Character string specifying the variable name.
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param predictor_allowable_values Numeric vector of predictor values to evaluate.
#'   If NULL, uses the allowable values from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
.calculate_pr_curve <- function(
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
  # predictor_allowable_values, then add an extra unmodified reference group
  # to the end)
  df <- data.frame(reference_group)
  df <- df[rep(1, output_rows + 1), ]
  df[predictor] <- append(predictor_allowable_values, predictor_reference_value)
  rownames(df) <- seq_len(nrow(df))

  # Run the pipeline with the input matrix and collect the predicted risks.
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df
  )
  predicted_col <- colnames(dat)[[1]]
  pr <- dat[[predicted_col]]

  labels <- convert_df_variable_to_label(
    df, model_data, predictor, predictor,
    escape_html = TRUE
  )[[predictor]][1:output_rows]

  # Create the DataFrame of predicted risks
  output_df <- data.frame(
    x = predictor_allowable_values[1:output_rows],
    PR = pr[1:output_rows],
    Model = cleanup_string(model_data$title),
    Comparison = glue::glue(
      "{predictor_label_no_units} {labels}"
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
    y_axis_label = "Predicted Risk",
    title = predictor_label,
    x_axis_type = ifelse(
      is_variable_categorical(model_data, predictor),
      "Categorical",
      "Continuous"
    ),
    ylim_linear = c(0, 1),
    aes_args = list(
      x = dplyr::sym(predictor_label),
      y = dplyr::sym("PR")
    )
  )
}

#' Build the Predicted Risk Plot UI
#'
#' Returns the UI elements to insert into the Predicted Risk tab panel.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotPRServer}}).
#' @param plot_height Character or numeric specifying the height of the plot
#'   area. Passed to \code{\link[plotly]{plotlyOutput}} as the \code{height}
#'   argument.
#' @param model_definitions A reactive expression (or \code{reactiveVal})
#'   returning the top-level model definitions object. Currently unused by the
#'   UI but accepted for consistency with the other plot UI functions.
#'
#' @return A \code{\link[shiny]{tagList}} containing the panel UI elements.
plotPRUI <- function(
  id,
  plot_height,
  model_definitions
) {
  tagList(
    br(),
    plotly::plotlyOutput(shiny::NS(id, "plot"), height = plot_height)
  )
}
