source("R/model_definitions/model_definitions_utils.R")

#' Calculate Relative Risk Curve with Interaction
#'
#' Computes relative risks showing the effect of a one-unit change in
#' interaction_predictor across the range of another predictor.
#'
#' @param predictor Character string specifying the primary variable name
#'   for the x-axis.
#' @param interaction_predictor Character string specifying the variable whose
#'   effect (one-unit change) is being measured.
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param predictor_range Numeric vector of predictor values to evaluate.
#'   If NULL, uses the range from model_data.
#' @param interaction_predictor_range Numeric vector of interaction predictor
#'   values. If NULL, uses the range from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{df}}{A \code{data.frame} with one row per predictor value. Columns:
#'       the predictor label column (x values), \code{RR} (relative risk for a one-unit
#'       change in \code{interaction_predictor} at each value of \code{predictor}),
#'       \code{Model} (cleaned model title), and \code{Comparison} (description of
#'       the interaction comparison, e.g. \code{"Sex (Male vs Female)"}).}
#'     \item{\code{x_axis_label}}{Character. Name of the predictor column in \code{df},
#'       used as the x-axis label.}
#'     \item{\code{y_axis_label}}{Character. Label for the y axis (\code{"Relative Risk"}).}
#'     \item{\code{title}}{Character. Plot title combining the predictor label and
#'       interaction predictor label.}
#'     \item{\code{x_axis_type}}{Character. Either \code{"Categorical"} or \code{"Continuous"}.}
#'     \item{\code{aes_args}}{A named list of \code{\link[dplyr]{sym}} objects mapping
#'       aesthetic names (\code{x}, \code{y}, \code{label}) to their respective columns
#'       in \code{df}.}
#'   }
#'
#' @export
calculate_rr_curve_interaction <- function(predictor,
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

  # Create the input matrix (duplicate reference_group for each
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

  # Run the pipeline with the input matrix and calculate the relative risks.
  # The relative risk is predicted_risk / ref_predicted_risk
  dat1 <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    dat = df1
  ) |> model.parameters.pipeline::get_pipeline_output()
  dat2 <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    dat = df2
  ) |> model.parameters.pipeline::get_pipeline_output()

  predicted_col_1 <- colnames(dat1)[[1]]
  predicted_col_2 <- colnames(dat2)[[1]]
  rr <- dat1[[predicted_col_1]] / dat2[[predicted_col_2]]

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

  # Create the DataFrame of relative risks
  output_df <- data.frame(
    x = predictor_range,
    RR = rr[1:output_rows],
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
    y_axis_label = "Relative Risk",
    title = title,
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
