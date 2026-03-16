source("R/model_definitions/model_definitions_utils.R")

#' Calculate Relative Risk Curve for a Predictor
#'
#' Computes relative risk for a predictor variable across its range, relative
#' to a reference value.
#'
#' @param predictor Character string specifying the variable name.
#' @param model_data List containing model data including model parameters,
#'   predictor ranges, and reference group values.
#' @param predictor_range Numeric vector of predictor values to evaluate.
#'   If NULL, uses the range from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A data frame with columns for the predictor values and their
#'   corresponding relative risk (RR).
#'
#' @export
calculate_rr_curve <- function(predictor,
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

  # Create the input matrix (duplicate model_data$reference_group for each
  # value in predictor_range, set the predictor to the predictor_range, then
  # add an extra unmodified reference group to the end)
  df <- data.frame(reference_group)
  df <- df[rep(1, output_rows + 1), ]
  df[predictor] <- append(predictor_range, predictor_reference_value)
  rownames(df) <- seq_len(nrow(df))

  # Run the pipeline with the input matrix and calculate the relative risk
  # The relative risk is predicted_risk / ref_predicted_risk
  # Note that the reference group is located at row output_rows + 1
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    dat = df
  ) |> model.parameters.pipeline::get_pipeline_output()

  predicted_col <- colnames(dat)[[1]]
  rr <- dat[[predicted_col]] / dat[[predicted_col]][output_rows + 1]

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

  # Create the DataFrame of relative risks
  output_df <- data.frame(
    x = predictor_range[1:output_rows],
    RR = rr[1:output_rows],
    Model = cleanup_string(model_data$title),
    Comparison = glue::glue(
      "{predictor_label_no_units} {labels} vs {ref_label}"
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
    y_axis_label = "Relative Risk",
    title = predictor_label,
    x_axis_type = ifelse(
      is_variable_categorical(model_data, predictor),
      "Categorical",
      "Continuous"
    ),
    aes_args = list(
      x = dplyr::sym(predictor_label),
      y = dplyr::sym("RR"),
      label = dplyr::sym("Comparison")
    )
  )
}
