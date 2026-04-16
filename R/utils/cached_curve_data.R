#' Cached Curve Data Management
#'
#' Provides functions for caching and reusing previously calculated curve data.
#' Each cache is an environment created by [initialize_cached_curve_data_env()]
#' and passed explicitly to the other functions. This avoids redundant
#' recalculations when only a subset of models have changed parameters (e.g., a
#' user updates a reference group value for one model but leaves others
#' unchanged).
#'
#' @section Cache structure:
#' \preformatted{
#'   .env$cached_curve_data[[curve_type]][[model_id]] <- list(
#'     params = <named list of parameters used to generate the curve>,
#'     data   = <the calculated curve data (data frame)>
#'   )
#' }
#'
#' \code{curve_type} identifies the kind of curve, e.g. \code{"or"} (odds
#' ratio) or \code{"pr"} (predicted risk). \code{model_id} identifies which
#' model the entry belongs to.
#'
#' The \code{params} list should contain every parameter that affects the curve
#' output. [is_reusable_cached_curve_data()] returns \code{FALSE} whenever any
#' parameter differs from the cached value, triggering a recalculation.
#'
#' @examples
#' \dontrun{
#' # 1. Create a cache environment at session/module initialisation
#' cache <- initialize_cached_curve_data_env()
#'
#' params <- list(
#'   predictor             = "clc_age",
#'   interaction_predictor = "diabx",
#'   reference_group       = list(clc_age = 20, diabx = 2, hwmdbmi = 25)
#' )
#'
#' # 2. On each update, reuse the cached result when parameters are unchanged
#' if (is_reusable_cached_curve_data(cache, "or", "model_1", params)) {
#'   curve_df <- get_cached_curve_data(cache, "or", "model_1")
#' } else {
#'   curve_df <- calculate_curve(...) # expensive step
#'   set_cached_curve_data(cache, "or", "model_1", params, curve_df)
#' }
#'
#' # 3. Invalidate the entire cache when the underlying model file changes
#' clear_cached_curve_data(cache)
#' }
#'
#' @name cached_curve_data
NULL

#' Initialize the cached curve data environment
#'
#' Creates a new environment used to store cached curve data across calls.
#' Pass the returned environment to other functions in this module (e.g.,
#' [get_cached_curve_data()], [set_cached_curve_data()]).
#'
#' @return An environment with a single element \code{cached_curve_data},
#'   initialized to an empty list.
#'
#' @seealso [get_cached_curve_data()], [set_cached_curve_data()],
#'   [is_reusable_cached_curve_data()], [clear_cached_curve_data()]
initialize_cached_curve_data_env <- function() {
  rlang::env(
    cached_curve_data = list()
  )
}

#' Get cached curve data
#'
#' Retrieves the cached curve data for a given curve type and model, or NULL
#' if no cached entry exists.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param curve_type Character string identifying the type of curve
#'   (e.g., "survival", "hazard", "or", "pr").
#' @param model_id Character string identifying the model.
#'
#' @return The cached curve data (typically a data frame), or NULL if not found.
get_cached_curve_data <- function(.env, curve_type, model_id) {
  .get_cached_curve_entry(.env, curve_type, model_id)$data
}

#' Get a cached curve entry
#'
#' Retrieves the full cached entry (both params and data) for a given curve
#' type and model.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param curve_type Character string identifying the type of curve.
#' @param model_id Character string identifying the model.
#'
#' @return A list with elements \code{params} and \code{data}, or NULL if no
#'   cached entry exists.
#'
#' @keywords internal
.get_cached_curve_entry <- function(.env, curve_type, model_id) {
  .env$cached_curve_data[[curve_type]][[model_id]]
}

#' Store curve data in the cache
#'
#' Saves calculated curve data along with the parameters used to generate it,
#' so that subsequent requests with the same parameters can reuse the result.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param curve_type Character string identifying the type of curve.
#' @param model_id Character string identifying the model.
#' @param model_params Named list of parameters used to generate the curve data.
#' @param data The calculated curve data to cache (typically a data frame).
set_cached_curve_data <- function(
  .env, curve_type, model_id, model_params, data
) {
  entry <- list(
    params = model_params,
    data = data
  )
  .env$cached_curve_data[[curve_type]][[model_id]] <- entry
}

#' Check whether cached curve data can be reused
#'
#' Compares the current model parameters against the parameters stored in the
#' cache. Returns TRUE if they are identical (after sorting by name), meaning
#' the cached data is still valid and can be reused without recalculation.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param curve_type Character string identifying the type of curve.
#' @param model_id Character string identifying the model.
#' @param model_params Named list of the current model parameters to compare
#'   against the cached parameters.
#'
#' @return Logical. TRUE if cached data exists and was generated with identical
#'   parameters, FALSE otherwise.
is_reusable_cached_curve_data <- function(
  .env, curve_type, model_id, model_params
) {
  old_params <- .get_cached_curve_entry(.env, curve_type, model_id)$params

  if (is.null(old_params)) {
    return(is.null(model_params))
  }

  old_params <- old_params[order(names(old_params))]
  model_params <- model_params[order(names(model_params))]

  identical(old_params, model_params)
}

#' Clear all cached curve data
#'
#' Removes all cached curve entries from the environment. Useful when the
#' underlying models change entirely (e.g., loading a new model file).
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
clear_cached_curve_data <- function(.env) {
  .env$cached_curve_data <- list()
}
