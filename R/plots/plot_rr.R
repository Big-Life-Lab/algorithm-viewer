#' Relative Risk Curve
#'
#' Functions for computing and rendering relative risk (RR) curves.
NULL

#' Build a Relative Risk Plot Module Server
#'
#' Shiny module server that renders an interactive relative risk (RR) curve for
#' the currently selected predictor, across all selected models.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotRRUI}}).
#' @param predictor A reactive expression returning the currently selected
#'   predictor variable name (character string).
#' @param interaction_predictor A reactive expression returning the currently
#'   selected interaction predictor variable name. Returns
#'   \code{config_get_empty_selection()} when no interaction predictor is
#'   selected.
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
#' @param cached_curve_env An environment used to cache curve data between
#'   renders so that unchanged models do not trigger redundant pipeline runs.
#'   Created by \code{\link{initialize_cached_curve_data_env}}.
#'
#' @return \code{NULL}, called for side effects.
plotRRServer <- function(
  id,
  predictor,
  interaction_predictor,
  logarithmic,
  selected_models,
  selected_reference_groups,
  model_definitions,
  cached_curve_env
) {
  shiny::moduleServer(id, function(input, output, session) {
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

          # Go through all selected_models and calculate the RR curves
          # We concatenate them (with bind_rows) to show one curve per model
          for (model_data in selected_models()) {
            predictor_values <- selected_reference_groups()[[model_data$model_id]]

            # Check if we can use the cached old data for the current model
            model_params <- list(
              predictor = predictor(),
              interaction_predictor = interaction_predictor(),
              reference_group = predictor_values
            )
            cache_key <- list("rr", model_data$model_id, predictor(), interaction_predictor())
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

              # Calculate the RR curve for the model
              if (interaction_predictor() == config_get_empty_selection()) {
                curve_data <- .calculate_rr_curve(
                  predictor(),
                  model_data,
                  reference_group = predictor_values
                )
              } else {
                curve_data <- .calculate_rr_curve_interaction(
                  predictor(),
                  interaction_predictor(),
                  model_data,
                  reference_group = predictor_values
                )
              }

              elapsed <- Sys.time() - tic
              message(paste0(
                "Elapsed time for RR curve ", model_data$model_id, ": ", elapsed
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
          make_message_plot(
            glue::glue("<b>Error</b>: {e$message}"),
            color = "red"
          )
        }
      )
    })
  })
}

#' Calculate Relative Risk Curve for a Predictor
#'
#' Computes relative risk for a predictor variable across its allowable values,
#' relative to a reference value.
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
.calculate_rr_curve <- function(
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

  # Run the pipeline with the input matrix and calculate the relative risk
  # The relative risk is predicted_risk / ref_predicted_risk
  # Note that the reference group is located at row output_rows + 1
  dat <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df
  )

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
    x = predictor_allowable_values[1:output_rows],
    RR = rr[1:output_rows],
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

#' Calculate Relative Risk Curve with Interaction
#'
#' Computes relative risks showing the effect of a one-unit change in
#' interaction_predictor across the allowable values of another predictor.
#'
#' @param predictor Character string specifying the primary variable name
#'   for the x-axis.
#' @param interaction_predictor Character string specifying the variable whose
#'   effect (one-unit change) is being measured.
#' @param model_data A model definition named list as returned by the model
#'   definitions utilities.
#' @param predictor_allowable_values Numeric vector of predictor values to evaluate.
#'   If NULL, uses the allowable values from model_data.
#' @param interaction_predictor_allowable_values Numeric vector of interaction predictor
#'   values. If NULL, uses the allowable values from model_data.
#' @param reference_group Named list of reference values for all predictors.
#'   If NULL, uses the reference group from model_data.
#'
#' @return A named list of curve data that can be passed to
#'   \code{\link{make_general_plot}}.
.calculate_rr_curve_interaction <- function(
  predictor,
  interaction_predictor,
  model_data,
  predictor_allowable_values = NULL,
  interaction_predictor_allowable_values = NULL,
  reference_group = NULL
) {
  interaction_predictor_allowable_values <- interaction_predictor_allowable_values %||%
    model_data$predictor_allowable_values[[interaction_predictor]]
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

  # Create the input matrix (duplicate reference_group for each
  # value in predictor_allowable_values, set the predictor to the
  # predictor_allowable_values, then add an extra unmodified reference group
  # to the end)
  df1 <- data.frame(reference_group)
  df1 <- df1[rep(1, output_rows), ]
  df1[[predictor]] <- predictor_allowable_values
  rownames(df1) <- seq_len(nrow(df1))
  df2 <- data.frame(df1)

  # df2 is the same as df1 but with predictor increased by 1 (relative to the
  # reference_group)
  if (is_variable_categorical(model_data, interaction_predictor)) {
    cat_val <- reference_group[[interaction_predictor]]

    # Advance cat_val to the next category
    allowable_values <- get_predictor_allowable_values(
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
    x = df1
  )
  dat2 <- model.parameters.pipeline::run_model_pipeline(
    model_data$model_pipeline,
    x = df2
  )

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
    x = predictor_allowable_values,
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

#' Build the Relative Risk Plot UI
#'
#' Returns the UI elements to insert into the Relative Risk tab panel.
#'
#' @param id Character string. The Shiny module namespace ID (must match the
#'   ID used in \code{\link{plotRRServer}}).
#' @param plot_height Character or numeric specifying the height of the plot
#'   area. Passed to \code{\link[plotly]{plotlyOutput}} as the \code{height}
#'   argument.
#' @param model_definitions A reactive expression (or \code{reactiveVal})
#'   returning the top-level model definitions object.
#'
#' @return A \code{\link[shiny]{tagList}} containing the panel UI elements.
plotRRUI <- function(
  id,
  plot_height,
  model_definitions
) {
  tagList(
    br(),
    plotly::plotlyOutput(shiny::NS(id, "plot"), height = plot_height)
  )
}
