test_that("returns a condition object with the correct classes", {
  err <- algorithm.viewer:::make_error(
    "validation_error", "something went wrong"
  )
  expect_s3_class(err, "validation_error")
  expect_s3_class(err, "error")
  expect_s3_class(err, "condition")
})

test_that("class hierarchy puts custom class first", {
  err <- algorithm.viewer:::make_error("my_error", "msg")
  expect_equal(class(err), c("my_error", "error", "condition"))
})

test_that("message is formed by concatenating ... with no separator", {
  err <- algorithm.viewer:::make_error("e", "Value must be ", "positive")
  expect_equal(conditionMessage(err), "Value must be positive")
})

test_that("single message string is preserved as-is", {
  err <- algorithm.viewer:::make_error("e", "simple message")
  expect_equal(conditionMessage(err), "simple message")
})

test_that("error can be raised and caught by custom class with tryCatch", {
  result <- tryCatch(
    stop(algorithm.viewer:::make_error("validation_error", "bad input")),
    validation_error = function(e) conditionMessage(e)
  )
  expect_equal(result, "bad input")
})

test_that("error is not caught by a different class handler", {
  expect_error(
    tryCatch(
      stop(algorithm.viewer:::make_error("type_a_error", "msg")),
      type_b_error = function(e) "should not reach here"
    ),
    class = "type_a_error"
  )
})

test_that("expect_error matches on custom class", {
  expect_error(
    stop(algorithm.viewer:::make_error(
      "validation_error", "Value must be positive"
    )),
    class = "validation_error"
  )
})
