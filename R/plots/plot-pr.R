#' Predicted Risk Curve
#'
#' Functions for computing and rendering predicted risk (PR) curves.
#' 
#' The main entry point called by the Shiny server is \code{make_pr_plot}.
NULL

#' Build a Predicted Risk Plot for the Current Predictor
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
make_pr_plot <- function(
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
      predictor <- session$userData$predictor
      all_curve_data <- list()

      # Go through all models and calculate the OR curves
      # We concatenate them (with bind_rows) to show one curve per model
      for (model_data in models) {
        predictor_values <- get_predictor_controls_values(predictor_controls_env, model_data)

        # Check if we can use the cached old data for the current model
        model_params <- list(
          predictor = predictor,
          reference_group = predictor_values
        )
        if (
          is_reusable_cached_curve_data(
            cached_curve_env,
            "pr",
            model_data$model_id, model_params
          )
        ) {
          # Reuse the old data
          all_curve_data[[length(all_curve_data) + 1]] <-
            get_cached_curve_data(cached_curve_env, "pr", model_data$model_id)
        } else {
          # Get predictor type (Categorical or Continuous)
          predictor_type <- model_data$variables |>
            dplyr::filter(variable == predictor) |>
            dplyr::pull(variableType)

          tic <- Sys.time()

          # Calculate the OR curve for the model
          curve_data <- .calculate_pr_curve(
            predictor,
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
            "pr",
            model_data$model_id,
            model_params,
            curve_data
          )
        }
      }

      make_general_plot(
        all_curve_data,
        model_definitions,
        session$userData$logarithmic
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

#' Calculate Predicted Risk Curve for a Predictor
#'
#' Computes predicted risk values for a predictor variable across its range.
#'
#' @param predictor Character string specifying the variable name.
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param predictor_range Numeric vector of predictor values to evaluate.
#'   If NULL, uses the range from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
.calculate_pr_curve <- function(predictor,
                               model_data,
                               predictor_range = NULL,
                               reference_group = NULL) {
  predictor_range <- predictor_range %||%
    model_data$predictor_ranges[[predictor]]
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
  output_rows <- length(predictor_range)

  # Create the input matrix (duplicate reference_group for each
  # value in predictor_range, set the predictor to the predictor_range, then
  # add an extra unmodified reference group to the end)
  df <- data.frame(reference_group)
  df <- df[rep(1, output_rows + 1), ]
  df[predictor] <- append(predictor_range, predictor_reference_value)
  rownames(df) <- seq_len(nrow(df))

  # Run the pipeline with the input matrix and calculate the odds ratios.
  # The odds ratio is predicted_risk / ref_predicted_risk
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    dat = df
  ) |> model.parameters.pipeline::get_pipeline_output()
  predicted_col <- colnames(dat)[[1]]
  pr <- dat[[predicted_col]]

  labels <- convert_df_variable_to_label(
    df, model_data, predictor, predictor,
    escape_html = TRUE
  )[[predictor]][1:output_rows]

  # Create the DataFrame of odds ratios
  output_df <- data.frame(
    x = predictor_range[1:output_rows],
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
