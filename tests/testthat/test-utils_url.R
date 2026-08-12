# Tests for the URL query-string utilities, using a fake session
# (helper-synthetic.R) and a controlled .CONFIG.

test_that("url_has_algorithm_id detects the query parameter", {
  local_config(list(algorithms = list(
    foo = list(title = "Foo", file = "foo.yaml")
  )))
  session <- make_fake_url_session("?algorithm=foo")
  expect_true(algorithm.viewer:::url_has_algorithm_id(session))
})

test_that("url_has_algorithm_id is FALSE when the parameter is absent", {
  local_config(list(algorithms = list(
    foo = list(title = "Foo", file = "foo.yaml")
  )))
  expect_false(
    algorithm.viewer:::url_has_algorithm_id(make_fake_url_session(""))
  )
  expect_false(
    algorithm.viewer:::url_has_algorithm_id(make_fake_url_session("?other=1"))
  )
})

test_that("url_has_algorithm_id is FALSE when URLs are disabled in config", {
  local_config(list(
    allow_algorithm_in_url = FALSE,
    algorithms = list(foo = list(title = "Foo", file = "foo.yaml"))
  ))
  session <- make_fake_url_session("?algorithm=foo")
  expect_false(algorithm.viewer:::url_has_algorithm_id(session))
})

test_that("url_get_algorithm_id returns a configured ID", {
  local_config(list(algorithms = list(
    foo = list(title = "Foo", file = "foo.yaml")
  )))
  session <- make_fake_url_session("?algorithm=foo")
  expect_equal(algorithm.viewer:::url_get_algorithm_id(session), "foo")
})

test_that("url_get_algorithm_id returns NULL for unknown IDs", {
  local_config(list(algorithms = list(
    foo = list(title = "Foo", file = "foo.yaml")
  )))
  session <- make_fake_url_session("?algorithm=unknown")
  expect_null(algorithm.viewer:::url_get_algorithm_id(session))
})

test_that("url_get_algorithm_id returns NULL when no algorithms configured", {
  local_config(list())
  session <- make_fake_url_session("?algorithm=foo")
  expect_null(algorithm.viewer:::url_get_algorithm_id(session))
})

test_that("url_get_algorithm_id returns NULL when URLs are disabled", {
  local_config(list(
    allow_algorithm_in_url = FALSE,
    algorithms = list(foo = list(title = "Foo", file = "foo.yaml"))
  ))
  session <- make_fake_url_session("?algorithm=foo")
  expect_null(algorithm.viewer:::url_get_algorithm_id(session))
})

test_that("url_set_algorithm_id replaces the query string", {
  session <- make_fake_url_session()
  algorithm.viewer:::url_set_algorithm_id("htnport-full", session = session)
  expect_length(session$updates, 1)
  expect_equal(session$updates[[1]]$qs, "?algorithm=htnport-full")
  expect_equal(session$updates[[1]]$mode, "replace")
})

test_that("url_set_algorithm_id URL-encodes reserved characters", {
  session <- make_fake_url_session()
  algorithm.viewer:::url_set_algorithm_id("a b&c", session = session)
  expect_equal(session$updates[[1]]$qs, "?algorithm=a%20b%26c")
})

test_that("url_update_query_string forwards query string and mode", {
  session <- make_fake_url_session()
  algorithm.viewer:::url_update_query_string(
    "?x=1",
    mode = "push", session = session
  )
  expect_equal(session$updates[[1]]$qs, "?x=1")
  expect_equal(session$updates[[1]]$mode, "push")
})
