library(dplyr)
library(model.parameters.pipeline)
source("R/model_definitions/model_definitions_utils.R")

#' Calculate Odds Ratio Curve with Interaction
#'
#' Computes odds ratios showing the effect of a one-unit change in
#' interaction_predictor across the range of another predictor.
#'
#' @param predictor Character string specifying the primary variable name
#'   for the x-axis.
#' @param interaction_predictor Character string specifying the variable whose
#'   effect (one-unit change) is being measured.
#' @param model_data List containing model data including model parameters,
#'   predictor ranges, and reference group values.
#' @param predictor_range Numeric vector of predictor values to evaluate.
#'   If NULL, uses the range from model_data.
#' @param interaction_predictor_range Numeric vector of interaction predictor
#'   values. If NULL, uses the range from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A data frame with columns for the predictor values and their
#'   corresponding odds ratios (OR) for the interaction effect.
#'
#' @export
calculate_or_curve_interaction <- function(predictor,
                                           interaction_predictor,
                                           model_data,
                                           predictor_range = NULL,
                                           interaction_predictor_range = NULL,
                                           reference_group = NULL) {
  interaction_predictor_range <- interaction_predictor_range %||%
    model_data$predictor_ranges[[interaction_predictor]]
  predictor_range <- predictor_range %||%
    model_data$predictor_ranges[[predictor]]
  reference_group <- reference_group %||% model_data$reference_group

  predictor_label <- get_variable_label_and_units(
    model_data, predictor,
    escape_html = TRUE
  )
  interaction_predictor_label <- get_variable_label(
    model_data, interaction_predictor,
    escape_html = TRUE
  )

  output_rows <- length(predictor_range)

  # Create the input matrix (duplicate model_data$reference_group for each
  # value in predictor_range, set the predictor to the predictor_range, then
  # add an extra unmodified reference group to the end)
  df1 <- data.frame(reference_group)
  df1 <- df1[rep(1, output_rows), ]
  df1[[predictor]] <- predictor_range
  rownames(df1) <- seq_len(nrow(df1))
  df2 <- data.frame(df1)

  # df2 is the same as df1 but with predictor increased by 1 (relative to the
  # reference_group)
  if (is_variable_categorical(model_data, interaction_predictor)) {
    cat_val <- reference_group[[interaction_predictor]]

    # Advance cat_val to the next category
    allowable_values <- get_predictor_range(
      model_data, interaction_predictor
    )
    indices <- unlist(df2[[interaction_predictor]])
    indices <- lapply(indices, function(x) which(x == allowable_values))
    indices <- lapply(
      indices, function(x) (x %% length(allowable_values)) + 1
    )
    indices <- unlist(indices)
    cat_val <- allowable_values[indices]

    df2[[interaction_predictor]] <- cat_val
  } else {
    df2[[interaction_predictor]] <- df2[[interaction_predictor]] + 1
  }

  # Run the pipeline with the input matrix and calculate the odds ratios.
  # The odds ratio is predicted_risk / ref_predicted_risk
  mod1 <- model_data$model_pipeline
  mod1 <- run_model_pipeline(
    mod1,
    data = df1
  )
  mod2 <- model_data$model_pipeline
  mod2 <- run_model_pipeline(
    mod2,
    data = df2
  )

  predicted_col <- "logistic_1"
  or <- (mod1$df[[predicted_col]] / (1 - mod1$df[[predicted_col]])) /
    (mod2$df[[predicted_col]] / (1 - mod2$df[[predicted_col]]))

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
    x = predictor_range,
    OR = or[1:output_rows],
    Model = cleanup_string(model_data$title),
    Comparison = glue::glue(
      "{interaction_predictor_label} {labels1} vs {labels2}"
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
    model_data = model_data,
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
      x = dplyr::sym(predictor_label),
      y = dplyr::sym("OR"),
      label = dplyr::sym("Comparison")
    )
  )
}
