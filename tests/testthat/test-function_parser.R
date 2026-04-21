# Tests for get_function_and_params

test_that("parses function name and positional numeric params", {
  result <- algorithm.viewer:::get_function_and_params("seq(1, 100, 0.1)")
  expect_equal(result$func, "seq")
  expect_equal(result$params[[1]], 1L)
  expect_equal(result$params[[2]], 100L)
  expect_equal(result$params[[3]], 0.1)
})

test_that("parses named parameters", {
  result <- algorithm.viewer:::get_function_and_params("seq(1, 10, by = 2)")
  expect_equal(result$func, "seq")
  expect_equal(result$params[[1]], 1L)
  expect_equal(result$params[[2]], 10L)
  expect_equal(result$params$by, 2L)
})

test_that("parses string parameters with single quotes", {
  result <- algorithm.viewer:::get_function_and_params("foo('hello', 'world')")
  expect_equal(result$func, "foo")
  expect_equal(result$params[[1]], "hello")
  expect_equal(result$params[[2]], "world")
})

test_that("parses string parameters with double quotes", {
  result <- algorithm.viewer:::get_function_and_params('foo("hello", "world")')
  expect_equal(result$func, "foo")
  expect_equal(result$params[[1]], "hello")
  expect_equal(result$params[[2]], "world")
})

test_that("parses function with no parameters", {
  result <- algorithm.viewer:::get_function_and_params("foo()")
  expect_equal(result$func, "foo")
  expect_equal(result$params, c())
})

test_that("returns NULL for invalid expression", {
  expect_null(algorithm.viewer:::get_function_and_params("not a function call"))
})

test_that("returns NULL for expression with nested parens", {
  expect_null(algorithm.viewer:::get_function_and_params("seq(1, foo(2), 3)"))
})

test_that("returns NULL for invalid param value", {
  expect_null(algorithm.viewer:::get_function_and_params("foo(1*2)"))
})

test_that("parses function names with dots and underscores", {
  result <- algorithm.viewer:::get_function_and_params("my.func_name(1)")
  expect_equal(result$func, "my.func_name")
})

# Tests for .parse_param_value

test_that("parses integer value", {
  result <- algorithm.viewer:::.parse_param_value("42")
  expect_equal(result, 42L)
  expect_type(result, "integer")
})

test_that("parses double value", {
  result <- algorithm.viewer:::.parse_param_value("3.14")
  expect_equal(result, 3.14)
  expect_type(result, "double")
})

test_that("parses single-quoted string", {
  result <- algorithm.viewer:::.parse_param_value("'hello'")
  expect_equal(result, "hello")
})

test_that("parses double-quoted string", {
  result <- algorithm.viewer:::.parse_param_value('"world"')
  expect_equal(result, "world")
})

test_that("stops on invalid numeric value", {
  expect_error(algorithm.viewer:::.parse_param_value("abc"))
})

# Tests for .convert_params_to_named_list

test_that("converts unnamed params", {
  result <- algorithm.viewer:::.convert_params_to_named_list(c("1", "2", "3"))
  expect_equal(result[[1]], 1L)
  expect_equal(result[[2]], 2L)
  expect_equal(result[[3]], 3L)
})

test_that("converts named params", {
  result <- algorithm.viewer:::.convert_params_to_named_list(
    c("x = 5", "y = 10")
  )
  expect_equal(result$x, 5L)
  expect_equal(result$y, 10L)
})

test_that("converts mixed named and unnamed params", {
  result <- algorithm.viewer:::.convert_params_to_named_list(c("1", "by = 0.5"))
  expect_equal(result[[1]], 1L)
  expect_equal(result$by, 0.5)
})

test_that("returns empty list for empty input", {
  result <- algorithm.viewer:::.convert_params_to_named_list(character(0))
  expect_equal(result, list())
})
