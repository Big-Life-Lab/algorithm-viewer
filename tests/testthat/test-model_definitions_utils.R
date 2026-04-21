test_that("is_data_missing returns TRUE for NULL", {
  expect_true(algorithm.viewer:::is_data_missing(NULL))
})

test_that("is_data_missing returns TRUE for empty vector", {
  expect_true(algorithm.viewer:::is_data_missing(character(0)))
  expect_true(algorithm.viewer:::is_data_missing(c()))
})

test_that("is_data_missing returns TRUE for 'n/a' and 'N/A'", {
  expect_true(algorithm.viewer:::is_data_missing("n/a"))
  expect_true(algorithm.viewer:::is_data_missing("N/A"))
  expect_true(algorithm.viewer:::is_data_missing("N/a"))
})

test_that("is_data_missing returns TRUE for NA::b sentinel", {
  expect_true(algorithm.viewer:::is_data_missing("NA::b"))
  expect_true(algorithm.viewer:::is_data_missing("na::b"))
})

test_that("is_data_missing returns FALSE for normal values", {
  expect_false(algorithm.viewer:::is_data_missing("hello"))
  expect_false(algorithm.viewer:::is_data_missing(42))
  expect_false(algorithm.viewer:::is_data_missing(0))
  expect_false(algorithm.viewer:::is_data_missing(FALSE))
})

test_that("cleanup_string escapes HTML special characters", {
  result <- algorithm.viewer:::cleanup_string("<b>bold</b>")
  expect_equal(result, "&lt;b&gt;bold&lt;/b&gt;")
})

test_that("cleanup_string returns non-character values unchanged", {
  expect_equal(algorithm.viewer:::cleanup_string(42L), 42L)
  expect_equal(algorithm.viewer:::cleanup_string(TRUE), TRUE)
  expect_null(algorithm.viewer:::cleanup_string(NULL))
})

test_that("cleanup_string leaves safe strings unchanged", {
  expect_equal(algorithm.viewer:::cleanup_string("hello world"), "hello world")
})
