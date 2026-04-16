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
#'   .env$cached_curve_data[[key]] <- list(
#'     params = <named list of parameters used to generate the curve>,
#'     data   = <the calculated curve data (data frame)>
#'   )
#' }
#'
#' The \code{params} list should contain every parameter that affects the curve
#' output. [is_reusable_cached_curve_data()] returns \code{FALSE} whenever any
#' parameter differs from the cached value, triggering a recalculation.
#'
#' The \code{key} is passed provided by the caller to the cached curve
#' functions that uniquely identify the cache entry we want to use for
#' the curve data (and is typically a list of values). It will usually
#' contain a curve type (eg. "or" for an odds ratio curve), a model
#' ID, followed by any additional values we want. See the examples below.
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
#' # 2. Create the key that is used to identify the curve data. We will have
#' # a separate cache entry for the curve type ("or"), the model
#' # ID ("model_1"), and the value for the predictor and interaction predictor.
#' # We could add params$reference_group to the cache key, but this will make
#' # a cache entry for every combination of reference group values we
#' # calculate, which might use up a lot of memory.
#' cache_key <- list(
#'    "or",
#'    "model_1",
#'    params$predictor,
#'    params$interaction_predictor
#' )
#'
#' # 2. On each update, reuse the cached result when parameters are unchanged
#' if (is_reusable_cached_curve_data(cache, cache_key, params)) {
#'   curve_df <- get_cached_curve_data(cache, key)
#' } else {
#'   curve_df <- calculate_curve(...) # expensive step
#'   set_cached_curve_data(cache, key, params, curve_df)
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
#' Retrieves the cached curve data for a given curve key, or NULL
#' if no cached entry exists.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param key A value (often a list of values) that identifies the specific cache
#'   entry within the cache. Hashed internally via [.hash_key()].
#'
#' @return The cached curve data (typically a data frame), or NULL if not found.
get_cached_curve_data <- function(.env, key) {
  .get_cached_curve_entry(.env, key)$data
}

#' Get a cached curve entry
#'
#' Retrieves the full cached entry (both params and data) for a given key.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param key A value (often a list of values) that identifies the specific cache
#'   entry within the cache. Hashed internally via [.hash_key()].
#'
#' @return A list with elements \code{params} and \code{data}, or NULL if no
#'   cached entry exists.
#'
#' @keywords internal
.get_cached_curve_entry <- function(.env, key) {
  key <- .hash_key(key)
  .env$cached_curve_data[[key]]
}

#' Store curve data in the cache
#'
#' Saves calculated curve data along with the parameters used to generate it,
#' so that subsequent requests with the same parameters can reuse the result.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param key A value (often a list of values) that identifies the specific cache
#'   entry within the cache. Hashed internally via [.hash_key()].
#' @param model_params Named list of parameters used to generate the curve data.
#' @param data The calculated curve data to cache (typically a data frame).
set_cached_curve_data <- function(
  .env, key, model_params, data
) {
  entry <- list(
    params = model_params,
    data = data
  )
  .env$cached_curve_data[[.hash_key(key)]] <- entry
}

#' Check whether cached curve data can be reused
#'
#' Compares the current model parameters against the parameters stored in the
#' cache. Returns TRUE if they are identical (after sorting by name), meaning
#' the cached data is still valid and can be reused without recalculation.
#'
#' @param .env Environment created by [initialize_cached_curve_env()].
#' @param key A value (often a list of values) that identifies the specific cache
#'   entry within the cache. Hashed internally via [.hash_key()].
#' @param model_params Named list of the current model parameters to compare
#'   against the cached parameters.
#'
#' @return Logical. TRUE if cached data exists and was generated with identical
#'   parameters, FALSE otherwise.
is_reusable_cached_curve_data <- function(
  .env, key, model_params
) {
  old_params <- .get_cached_curve_entry(.env, key)$params

  if (is.null(old_params)) {
    return(is.null(model_params))
  }

  # Process the parameters so they can be compared
  old_params <- old_params[order(names(old_params))]
  model_params <- model_params[order(names(model_params))]

  identical(old_params, model_params)
}

.hash_key <- function(key) {
  digest::digest(key)
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
