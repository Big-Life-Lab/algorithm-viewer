test_that(
  "initialize_cached_data returns an environment with empty cached_data",
  {
    cache <- algorithm.viewer:::initialize_cached_data()
    expect_true(is.environment(cache))
    expect_equal(cache$cached_data, list())
  }
)

test_that("set_cached_data stores data and get_cached_data retrieves it", {
  cache <- algorithm.viewer:::initialize_cached_data()
  key <- list("curve", "model_1")
  params <- list(predictor = "age", group = "female")
  data <- data.frame(x = 1:3, y = c(0.1, 0.2, 0.3))

  algorithm.viewer:::set_cached_data(cache, key, params, data)
  result <- algorithm.viewer:::get_cached_data(cache, key)

  expect_equal(result, data)
})

test_that("get_cached_data returns NULL for missing key", {
  cache <- algorithm.viewer:::initialize_cached_data()
  result <- algorithm.viewer:::get_cached_data(cache, list("missing"))
  expect_null(result)
})

test_that("is_reusable_cached_data returns FALSE when no entry exists", {
  cache <- algorithm.viewer:::initialize_cached_data()
  params <- list(predictor = "age")
  expect_false(
    algorithm.viewer:::is_reusable_cached_data(cache, list("key"), params)
  )
})

test_that("is_reusable_cached_data returns TRUE when params are identical", {
  cache <- algorithm.viewer:::initialize_cached_data()
  key <- list("curve", "model_1")
  params <- list(predictor = "age", group = "female")

  algorithm.viewer:::set_cached_data(cache, key, params, "data")
  expect_true(algorithm.viewer:::is_reusable_cached_data(cache, key, params))
})

test_that(
  paste(
    "is_reusable_cached_data returns TRUE when params supplied",
    "in different order"
  ),
  {
    cache <- algorithm.viewer:::initialize_cached_data()
    key <- list("curve")
    params_original <- list(predictor = "age", group = "female")
    params_reordered <- list(group = "female", predictor = "age")

    algorithm.viewer:::set_cached_data(cache, key, params_original, "data")
    expect_true(
      algorithm.viewer:::is_reusable_cached_data(cache, key, params_reordered)
    )
  }
)

test_that("is_reusable_cached_data returns FALSE when params differ", {
  cache <- algorithm.viewer:::initialize_cached_data()
  key <- list("curve")
  params_original <- list(predictor = "age")
  params_changed <- list(predictor = "bmi")

  algorithm.viewer:::set_cached_data(cache, key, params_original, "data")
  expect_false(
    algorithm.viewer:::is_reusable_cached_data(cache, key, params_changed)
  )
})

test_that("is_reusable_cached_data returns TRUE when both params are NULL", {
  cache <- algorithm.viewer:::initialize_cached_data()
  key <- list("key")

  algorithm.viewer:::set_cached_data(cache, key, NULL, "data")
  expect_true(algorithm.viewer:::is_reusable_cached_data(cache, key, NULL))
})

test_that(
  paste(
    "is_reusable_cached_data returns FALSE when stored params NULL",
    "but new params not"
  ),
  {
    cache <- algorithm.viewer:::initialize_cached_data()
    key <- list("key")

    algorithm.viewer:::set_cached_data(cache, key, NULL, "data")
    expect_false(
      algorithm.viewer:::is_reusable_cached_data(cache, key, list(x = 1))
    )
  }
)

test_that("set_cached_data overwrites existing entry", {
  cache <- algorithm.viewer:::initialize_cached_data()
  key <- list("key")
  params <- list(x = 1)

  algorithm.viewer:::set_cached_data(cache, key, params, "first")
  algorithm.viewer:::set_cached_data(cache, key, params, "second")

  expect_equal(algorithm.viewer:::get_cached_data(cache, key), "second")
})

test_that("different keys store independent entries", {
  cache <- algorithm.viewer:::initialize_cached_data()
  params <- list(x = 1)

  algorithm.viewer:::set_cached_data(cache, list("key_a"), params, "data_a")
  algorithm.viewer:::set_cached_data(cache, list("key_b"), params, "data_b")

  expect_equal(
    algorithm.viewer:::get_cached_data(cache, list("key_a")),
    "data_a"
  )
  expect_equal(
    algorithm.viewer:::get_cached_data(cache, list("key_b")),
    "data_b"
  )
})

test_that("clear_cached_data removes all entries", {
  cache <- algorithm.viewer:::initialize_cached_data()
  params <- list(x = 1)

  algorithm.viewer:::set_cached_data(cache, list("key_a"), params, "data_a")
  algorithm.viewer:::set_cached_data(cache, list("key_b"), params, "data_b")
  algorithm.viewer:::clear_cached_data(cache)

  expect_null(algorithm.viewer:::get_cached_data(cache, list("key_a")))
  expect_null(algorithm.viewer:::get_cached_data(cache, list("key_b")))
  expect_equal(cache$cached_data, list())
})

test_that("cache is mutable across references (environment semantics)", {
  cache <- algorithm.viewer:::initialize_cached_data()
  cache_ref <- cache

  algorithm.viewer:::set_cached_data(cache, list("key"), list(x = 1), "data")

  expect_equal(
    algorithm.viewer:::get_cached_data(cache_ref, list("key")),
    "data"
  )
})
