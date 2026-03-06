#' Cached Curve Data Management
#'
#' Provides functions for caching and reusing previously calculated curve data,
#' stored in the Shiny session's userData. This avoids redundant recalculations
#' when only a subset of models have changed parameters (e.g., a user updates
#' a reference group value for one model but leaves others unchanged).
#'
#' Cache structure in session$userData:
#' \preformatted{
#'   session$userData$cached_curve_data[[curve_type]][[model_id]] <- list(
#'     params = <named list of parameters used to generate the curve>,
#'     data   = <the calculated curve data (data frame)>
#'   )
#' }
#'
#' The curve_type variable identifies what type of curve it is, such as "or"
#' or "pr" (for odds-ratio curve or predicted risk curve).
#'
#' The named list in the params field should contain all the parameters used
#' to calculate the curve. If any of these parameters change then
#' \code{is_reusable_cached_curve_data} will return \code{FALSE}, indicating
#' that the curve should be recalculated. An example for an odds-ratio curve:
#' \preformatted{
#'   params <- list(
#'     predictor = "clc_age",
#'     interaction_predictor = "diabx",
#'     reference_group = list(
#'       clc_age = 20,
#'       diabx = 2,
#'       fmh_15 = 1,
#'       hwmdbmi = 25
#'     )
#'   )
#' }
#'
#' @examples
#' \dontrun{
#' # Store curve data after calculation
#' set_cached_curve_data(
#'   session, "survival", "model_1", model_params, curve_df
#' )
#'
#' # On next update, check if cached data can be reused
#' if (is_reusable_cached_curve_data(
#'   session, "survival", "model_1", model_params
#' )) {
#'   curve_df <- get_cached_curve_data(session, "survival", "model_1")
#' } else {
#'   curve_df <- calculate_curve(...)
#'   set_cached_curve_data(
#'     session, "survival", "model_1", model_params, curve_df
#'   )
#' }
#'
#' # Clear all cached data (e.g., when loading a new model file)
#' clear_cached_curve_data(session)
#' }
#'
#' @name cached_curve_data
NULL

#' Get cached curve data
#'
#' Retrieves the cached curve data for a given curve type and model, or NULL
#' if no cached entry exists.
#'
#' @param session The Shiny session object.
#' @param curve_type Character string identifying the type of curve
#'   (e.g., "survival", "hazard", "or", "pr").
#' @param model_id Character string identifying the model.
#'
#' @return The cached curve data (typically a data frame), or NULL if not found.
get_cached_curve_data <- function(session, curve_type, model_id) {
  .get_cached_curve_entry(session, curve_type, model_id)$data
}

#' Get a cached curve entry
#'
#' Retrieves the full cached entry (both params and data) for a given curve
#' type and model.
#'
#' @param session The Shiny session object.
#' @param curve_type Character string identifying the type of curve.
#' @param model_id Character string identifying the model.
#'
#' @return A list with elements \code{params} and \code{data}, or NULL if no
#'   cached entry exists.
#'
#' @keywords internal
.get_cached_curve_entry <- function(session, curve_type, model_id) {
  session$userData$cached_curve_data[[curve_type]][[model_id]]
}

#' Store curve data in the cache
#'
#' Saves calculated curve data along with the parameters used to generate it,
#' so that subsequent requests with the same parameters can reuse the result.
#'
#' @param session The Shiny session object.
#' @param curve_type Character string identifying the type of curve.
#' @param model_id Character string identifying the model.
#' @param model_params Named list of parameters used to generate the curve data.
#' @param data The calculated curve data to cache (typically a data frame).
set_cached_curve_data <- function(
  session, curve_type, model_id, model_params, data
) {
  entry <- list(
    params = model_params,
    data = data
  )
  session$userData$cached_curve_data[[curve_type]][[model_id]] <- entry
}

#' Check whether cached curve data can be reused
#'
#' Compares the current model parameters against the parameters stored in the
#' cache. Returns TRUE if they are identical (after sorting by name), meaning
#' the cached data is still valid and can be reused without recalculation.
#'
#' @param session The Shiny session object.
#' @param curve_type Character string identifying the type of curve.
#' @param model_id Character string identifying the model.
#' @param model_params Named list of the current model parameters to compare
#'   against the cached parameters.
#'
#' @return Logical. TRUE if cached data exists and was generated with identical
#'   parameters, FALSE otherwise.
is_reusable_cached_curve_data <- function(
  session, curve_type, model_id, model_params
) {
  old_params <- .get_cached_curve_entry(session, curve_type, model_id)$params

  if (is.null(old_params)) {
    return(is.null(model_params))
  }

  old_params <- old_params[order(names(old_params))]
  model_params <- model_params[order(names(model_params))]

  identical(old_params, model_params)
}

#' Clear all cached curve data
#'
#' Removes all cached curve entries from the session. Useful when the
#' underlying models change entirely (e.g., loading a new model file).
#'
#' @param session The Shiny session object.
clear_cached_curve_data <- function(session) {
  session$userData$cached_curve_data <- list()
}
