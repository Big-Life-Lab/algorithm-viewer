#' Odds Ratio Curve
#'
#' Functions for computing and rendering odds ratio (OR) curves.
#'
#' The main entry point called by the Shiny server is \code{make_or_plot}.
NULL

local({
plot_id <- "or_plot"

#' Build an Odds Ratio Plot for the Current Predictor
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
make_or_plot <- function(
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
      interaction_predictor <- session$userData$interaction_predictor

      # Go through all models and calculate the OR curves
      # We concatenate them (with bind_rows) to show one curve per model
      for (model_data in models) {
        predictor_values <-
          get_predictor_controls_values(predictor_controls_env, model_data)

        # Check if we can use the cached old data for the current model
        model_params <- list(
          predictor = predictor,
          interaction_predictor = interaction_predictor,
          reference_group = predictor_values
        )
        if (
          is_reusable_cached_curve_data(
            cached_curve_env,
            "or",
            model_data$model_id,
            model_params
          )
        ) {
          # Reuse the old data
          all_curve_data[[length(all_curve_data) + 1]] <-
            get_cached_curve_data(cached_curve_env, "or", model_data$model_id)
        } else {
          tic <- Sys.time()

          # Calculate the OR curve for the model
          if (interaction_predictor == config_get_empty_selection()) {
            curve_data <- .calculate_or_curve(
              predictor,
              model_data,
              reference_group = predictor_values
            )
          } else {
            curve_data <- .calculate_or_curve_interaction(
              predictor,
              interaction_predictor,
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
          set_cached_curve_data(
            cached_curve_env,
            "or",
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

#' Calculate Odds Ratio Curve for a Predictor
#'
#' Computes odds ratios for a predictor variable across its range, relative
#' to a reference value.
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
.calculate_or_curve <- function(
  predictor,
  model_data,
  predictor_range = NULL,
  reference_group = NULL
) {
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
  # add an extra unmodified reference group to the end, at index output_rows+1)
  df <- data.frame(reference_group)
  df <- df[rep(1, output_rows + 1), ]
  df[predictor] <- append(predictor_range, predictor_reference_value)
  rownames(df) <- seq_len(nrow(df))

  # Run the pipeline with the input matrix and calculate the odds ratios.
  # The odds ratio is odds / reference_group_odds, where odds is calculated
  # as predicted_risk / (1 - predicted_risk)
  # Note that the reference group is located at row output_rows + 1
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    dat = df
  ) |> model.parameters.pipeline::get_pipeline_output()

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
    x = predictor_range[1:output_rows],
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
      x = dplyr::sym(predictor_label),
      y = dplyr::sym("OR"),
      label = dplyr::sym("Comparison")
    )
  )
}

#' Calculate Odds Ratio Curve with Interaction
#'
#' Computes odds ratios showing the effect of a one-unit change in
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
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
.calculate_or_curve_interaction <- function(
  predictor,
  interaction_predictor,
  model_data,
  predictor_range = NULL,
  interaction_predictor_range = NULL,
  reference_group = NULL
) {
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

  # Run the pipeline with the input matrix and calculate the odds ratios.
  # The odds ratio is odds / reference_group_odds, where odds is calculated
  # as predicted_risk / (1 - predicted_risk)
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
    x = predictor_range,
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
      x = dplyr::sym(predictor_label),
      y = dplyr::sym("OR"),
      label = dplyr::sym("Comparison")
    )
  )
}

panel_ui <- function(plot_height) {
  tagList(
    br(),
    plotly::plotlyOutput(plot_id, height = plot_height)
  )
}

plot_register <- function(.env) {
  plot_man_add_plot(
    .env,
    plot_id = plot_id,
    title = "Odds Ratio",
    make_plot_fn = make_or_plot,
    panel_ui_fn = panel_ui,
    model_ui_fn = NULL
  )
}

plot_register
})