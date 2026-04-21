test_that("unique values are returned unchanged", {
  input <- c("Male", "Female", "Other")
  expect_equal(algorithm.viewer:::make_string_values_unique(input), input)
})

test_that("first duplicate keeps original name, subsequent get numbers", {
  input <- c("Female", "Female", "Male", "Female")
  result <- algorithm.viewer:::make_string_values_unique(input)
  expect_equal(result, c("Female", "Female (2)", "Male", "Female (3)"))
})

test_that("all duplicates disambiguated correctly", {
  input <- c("A", "A", "A")
  result <- algorithm.viewer:::make_string_values_unique(input)
  expect_equal(length(result), 3)
  expect_equal(length(unique(result)), 3)
  expect_equal(result[[1]], "A")
})

test_that("custom template is applied", {
  input <- c("X", "X")
  result <- algorithm.viewer:::make_string_values_unique(
    input, template = "{value}_{num}"
  )
  expect_equal(result, c("X", "X_1"))
})

test_that("empty vector returns empty vector", {
  expect_equal(
    algorithm.viewer:::make_string_values_unique(character(0)), character(0)
  )
})

test_that("single element vector returned unchanged", {
  expect_equal(algorithm.viewer:::make_string_values_unique("only"), "only")
})
