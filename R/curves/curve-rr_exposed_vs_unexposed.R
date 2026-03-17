library(model.parameters.pipeline)
source("R/model_definitions/model_definitions_utils.R")
source("R/model_definitions/model_definitions.R")

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
#' @param exposed_group A named list of predictor values representing the exposed
#'   group profile.
#' @param unexposed_group A named list of predictor values representing the
#'   unexposed (reference) group profile. This group's predicted risk is used
#'   as the denominator for all relative risk calculations.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{df}}{A \code{data.frame} with one row per comparison. Columns:
#'       \code{x} and \code{RR} (relative risk), \code{Model} (cleaned model title),
#'       \code{Label} (display name for the row, e.g. \code{"Marital status (Married)"}),
#'       and \code{Comparison} (description of the comparison, e.g.
#'       \code{"Marital status (Married vs Single)"}).}
#'     \item{\code{x_axis_label}}{Character. Name of the column to use for the x axis (\code{"Label"}).}
#'     \item{\code{y_axis_label}}{Character. Label for the y axis (\code{"Relative Risk"}).}
#'     \item{\code{title}}{Character. Plot title (\code{"Relative Risk"}).}
#'     \item{\code{x_axis_type}}{Character. Axis type (\code{"Categorical"}).}
#'     \item{\code{aes_args}}{A named list of \code{\link[dplyr]{sym}} objects
#'       mapping aesthetic names (\code{x}, \code{y}, \code{Comparison}) to
#'       their respective columns in \code{df}.}
#'   }
calculate_rr_exposed_vs_unexposed_curve <- function(model_data,
                                     exposed_group,
                                     unexposed_group) {
  rows <- list()
  row_names <- list()
  row_comparisons <- list()

  # First row is the unmodified exposed group
  rows[[length(rows) + 1]] <- exposed_group
  row_names[[length(row_names) + 1]] <- "<b>Overall</b>"
  row_comparisons[[length(row_comparisons) + 1]] <- "Exposed vs Unexposed"

  for (idx in seq_along(names(exposed_group))) {
    predictor <- names(exposed_group)[[idx]]
    predictor_label <- get_variable_label(model_data, predictor, escape_html = TRUE)
    unexposed_value <- unexposed_group[[predictor]]

    if (is_variable_categorical(model_data, predictor)) {
      # Add a row for each value in the predictor range. The exposure will be
      # exposed_group but with each possible value of the current predictor set
      # in the exposed group (eg. for marital status, we could have one row
      # for "Married", one for "Single", and one for "Widowed/separated/divorced")
      predictor_range <- get_predictor_range(model_data, predictor)
      for (cur_exposed_value in predictor_range) {
        # Add the exposed row, with the predictor set to cur_exposed_value
        cur_group <- exposed_group
        cur_group[[predictor]] <- cur_exposed_value
        rows[[length(rows) + 1]] <- cur_group

        # Calculate the name (eg. "Marital status (Married)") of the new row and
        # the comparison label (eg. "Marital status (Married vs Single)")
        cur_exposed_label <- get_variable_label_from_value(model_data, predictor, cur_exposed_value, escape_html = TRUE)
        exposed_label <- get_variable_label_from_value(model_data, predictor, exposed_group[[predictor]], escape_html = TRUE)
        row_name <- as.character(glue::glue("{predictor_label} ({cur_exposed_label})"))
        row_names[[length(row_names) + 1]] <- row_name
        row_comparison <- as.character(glue::glue("{predictor_label} ({cur_exposed_label} vs {exposed_label})"))
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
    dat = df
  ) |> model.parameters.pipeline::get_pipeline_output()

  # Calcualte the relative risk. The risks are relative to the
  # risk in the last row.
  rr <- dat[1:nrow(dat) - 1, ] / dat[nrow(dat), ]

  output_df <- data.frame(
    x = rr,
    RR = rr,
    Model = cleanup_string(model_data$title),
    Label = unlist(row_names[1:nrow(dat) - 1]),
    Comparison = unlist(row_comparisons[1:nrow(dat) - 1])
  )

  list(
    df = output_df,
    x_axis_label = "Label",
    y_axis_label = "Relative Risk",
    title = "Relative Risk",
    x_axis_type = "Categorical",
    aes_args = list(
      x = dplyr::sym("Label"),
      y = dplyr::sym("RR"),
      Comparison = dplyr::sym("Comparison")
    )
  )
}
