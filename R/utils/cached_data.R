#' Cached Data Management
#'
#' Provides functions for caching and reusing data. Each cache is created by
#' [initialize_cached_data()] and passed explicitly to the other functions.
#'
#' Each cache entry has a \code{params} named list associated with it. If any
#' of these values change for a given key, then
#' \code{is_reusable_cached_data()} will return FALSE and the caller should
#' recalculate the cache data and then reset the value in the cache by calling
#' \code{set_cached_data()}. If it returns TRUE then the value does not
#' need to be recalculated and the cached data can be retrieved with
#' \code{get_cached_data()}.
#'
#' @section Cache structure:
#' \preformatted{
#'   cache$cached_data[[key]] <- list(
#'     params = <named list of parameters used to generate the data>,
#'     data   = <the calculated data stored in the cache>
#'   )
#' }
#'
#' The \code{params} list (as described above) contains all values used
#' to calculate the data, while \{data} is the actual data stored in
#' and returned by the cache.
#'
#' The \code{key} into the cache is provided by the caller to the cache
#' functions, and uniquely identify the cache entry. It is typically a
#' list of values. See the examples below.
#'
#' @examples
#' \dontrun{
#' # 1. Create a cache
#' cache <- initialize_cached_data()
#'
#' # 2. Parameters used to calculate our data
#' params <- list(
#'   predictor             = "clc_age",
#'   interaction_predictor = "diabx",
#'   reference_group       = list(clc_age = 20, diabx = 2, hwmdbmi = 25)
#' )
#' 
#' # 3. Create the key that is used to identify the entry.
#' cache_key <- list(
#'    "or",
#'    "model_1",
#'    params$predictor,
#'    params$interaction_predictor
#' )
#'
#' # 4. Reuse the cached result when parameters are unchanged
#' if (is_reusable_cached_data(cache, cache_key, params)) {
#'   curve_df <- get_cached_data(cache, key)
#' } else {
#'   curve_df <- calculate_curve(...) # expensive step (uses params)
#'   set_cached_data(cache, key, params, curve_df)
#' }
#'
#' # 5. Invalidate the entire cache. Should be performed whenever the cache
#' # entries are no longer valid.
#' clear_cached_data(cache)
#' }
#'
#' @name cached_data
NULL

#' Initialize the cached data
#'
#' Creates a new cache used to store cached data across calls. Pass the
#' returned cache to other functions in this module (e.g.,
#' [get_cached_data()], [set_cached_data()]).
#'
#' @return A cache that can be passed to other cached data functions.
#'
#' @seealso [get_cached_data()], [set_cached_data()],
#'   [is_reusable_cached_data()], [clear_cached_data()]
initialize_cached_data <- function() {
  rlang::env(
    cached_data = list()
  )
}

#' Get cached data
#'
#' Retrieves the cached data for a given cache key, or NULL if no cached entry
#' exists.
#'
#' @param cache Cache created by [initialize_cached_data()].
#' @param key A value (often a list of values) that identifies the specific cache
#'   entry within the cache. Hashed internally via [.hash_key()].
#'
#' @return The cached data, or NULL if not found.
get_cached_data <- function(cache, key) {
  .get_cached_data_entry(cache, key)$data
}

#' Get a cached entry
#'
#' Retrieves the full cached entry (both params and data) for a given key.
#'
#' @param cache Cache created by [initialize_cached_data()].
#' @param key A value (often a list of values) that identifies the specific cache
#'   entry within the cache. Hashed internally via [.hash_key()].
#'
#' @return A list with elements \code{params} and \code{data}, or NULL if no
#'   cached entry exists.
#'
#' @keywords internal
.get_cached_data_entry <- function(cache, key) {
  key <- .hash_key(key)
  cache$cached_data[[key]]
}

#' Store data in the cache
#'
#' Saves data along with the parameters used to generate it,
#' so that subsequent requests with the same parameters can reuse the result.
#'
#' @param cache Cache created by [initialize_cached_data()].
#' @param key A value (often a list of values) that identifies the specific
#'   cache entry within the cache. Hashed internally via [.hash_key()].
#' @param params Named list of parameters used to generate the data. It is
#'   used to determine if the data needs to be recalculated (ie. if
#'   \code{is_reusable_cached_data} returns FALSE). If the params change then
#'   the data should be recalculated and \{set_cached_data} should be recalled.
#' @param data The data to cache.
set_cached_data <- function(
  cache, key, params, data
) {
  entry <- list(
    params = params,
    data = data
  )
  cache$cached_data[[.hash_key(key)]] <- entry
}

#' Check whether cached data can be reused
#'
#' Compares the current parameters against the parameters stored in the
#' cache. Returns TRUE if they are identical (after sorting by name), meaning
#' the cached data is still valid and can be reused without recalculation.
#' Call [get_cached_data()] to retrieve the data.
#'
#' @param cache Cache created by [initialize_cached_data()].
#' @param key A value (often a list of values) that identifies the specific cache
#'   entry within the cache. Hashed internally via [.hash_key()].
#' @param params Named list of parameters used to generate the data. It is
#'   used to determine if the data needs to be recalculated (ie. if
#'   \code{is_reusable_cached_data} returns FALSE). If the params change then
#'   the data should be recalculated and \{set_cached_data} should be recalled.
#'
#' @return Logical. TRUE if cached data exists and was generated with identical
#'   parameters, FALSE otherwise.
is_reusable_cached_data <- function(
  cache, key, params
) {
  old_params <- .get_cached_data_entry(cache, key)$params

  if (is.null(old_params)) {
    return(is.null(params))
  }

  # Process the parameters so they can be compared
  old_params <- old_params[order(names(old_params))]
  params <- params[order(names(params))]

  identical(old_params, params)
}

#' Get a hashed cache key that can be used as an index into the cache.
#'
#' Converts an arbitrary R value (typically a list of values) into a string.
#'
#' @param key A value (often a list) to hash. Any R object is accepted.
#'
#' @return A character string that can be used as an index into the cache
#'   (eg. \code{cache$cached_data[[key]]})
#'
#' @keywords internal
.hash_key <- function(key) {
  digest::digest(key)
}

#' Clear all cached data
#'
#' Removes all cached entries from the cache. Should be called
#' whenever the cached data are no longer valid.
#'
#' @param cache Cache created by [initialize_cached_data()].
clear_cached_data <- function(cache) {
  cache$cached_data <- list()
}
