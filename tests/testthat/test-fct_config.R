# Tests for the global configuration utilities. The shared .CONFIG
# environment is swapped out per-test via local_config() (helper-synthetic.R)
# so tests cannot leak state into each other.

test_that("config_get_empty_selection returns a non-empty string", {
  val <- algorithm.viewer:::config_get_empty_selection()
  expect_type(val, "character")
  expect_gt(nchar(val), 0)
})

test_that("config_get_bool returns the stored logical", {
  local_config(list(somekey = TRUE))
  expect_true(algorithm.viewer:::config_get_bool("somekey", FALSE))
})

test_that("config_get_bool falls back to default for missing keys", {
  local_config(list())
  expect_true(algorithm.viewer:::config_get_bool("missing", TRUE))
  expect_false(algorithm.viewer:::config_get_bool("missing", FALSE))
})

test_that("config_get_bool falls back to default for non-logical values", {
  local_config(list(somekey = "yes"))
  expect_false(algorithm.viewer:::config_get_bool("somekey", FALSE))
})

test_that("config_allow_file_uploads defaults to FALSE", {
  local_config(list())
  expect_false(algorithm.viewer:::config_allow_file_uploads())
})

test_that("config_allow_algorithm_in_url defaults to TRUE", {
  local_config(list())
  expect_true(algorithm.viewer:::config_allow_algorithm_in_url())
  local_config(list(allow_algorithm_in_url = FALSE))
  expect_false(algorithm.viewer:::config_allow_algorithm_in_url())
})

test_that("config_has_algorithms reflects the algorithms key", {
  local_config(list())
  expect_false(algorithm.viewer:::config_has_algorithms())
  local_config(list(algorithms = list(a = list(title = "A", file = "a.yaml"))))
  expect_true(algorithm.viewer:::config_has_algorithms())
})

test_that("config_allow_algorithms_selection needs algorithms AND the flag", {
  # No algorithms: FALSE even though the flag defaults to TRUE
  local_config(list())
  expect_false(algorithm.viewer:::config_allow_algorithms_selection())

  # Algorithms present, flag defaulting to TRUE
  local_config(list(algorithms = list(a = list(title = "A", file = "a.yaml"))))
  expect_true(algorithm.viewer:::config_allow_algorithms_selection())

  # Algorithms present but the flag explicitly FALSE
  local_config(list(
    algorithms = list(a = list(title = "A", file = "a.yaml")),
    allow_algorithms_selection = FALSE
  ))
  expect_false(algorithm.viewer:::config_allow_algorithms_selection())
})

test_that("config_algorithm_id_exists matches only configured IDs", {
  local_config(list(algorithms = list(
    foo = list(title = "Foo", file = "foo.yaml")
  )))
  expect_true(algorithm.viewer:::config_algorithm_id_exists("foo"))
  expect_false(algorithm.viewer:::config_algorithm_id_exists("bar"))
})

test_that("config_get_algorithm_choices maps titles to IDs", {
  local_config(list(algorithms = list(
    foo = list(title = "Foo Model", file = "foo.yaml"),
    bar = list(title = "Bar Model", file = "bar.yaml")
  )))
  choices <- algorithm.viewer:::config_get_algorithm_choices()
  expect_equal(
    choices,
    c("Foo Model" = "foo", "Bar Model" = "bar")
  )
})

test_that("config_get_algorithm_choices disambiguates duplicate titles", {
  local_config(list(algorithms = list(
    foo = list(title = "Model", file = "foo.yaml"),
    bar = list(title = "Model", file = "bar.yaml")
  )))
  choices <- algorithm.viewer:::config_get_algorithm_choices()
  # Every label (name) must stay unique so the dropdown selection is
  # unambiguous, while the values remain the original algorithm IDs.
  expect_equal(length(unique(names(choices))), 2)
  expect_setequal(unname(choices), c("foo", "bar"))
  expect_true("Model" %in% names(choices))
})

test_that("config_get_algorithm_file returns NULL for unknown or NULL IDs", {
  local_config(list(algorithms = list(
    foo = list(title = "Foo", file = "foo.yaml")
  )))
  expect_null(algorithm.viewer:::config_get_algorithm_file("nope"))
  expect_null(algorithm.viewer:::config_get_algorithm_file(NULL))
})

test_that("relative algorithm paths resolve against the config directory", {
  config_dir <- withr::local_tempdir()
  alg_file <- file.path(config_dir, "alg1.yaml")
  writeLines("meta: {}", alg_file)

  local_config(list(
    config_path__ = file.path(config_dir, "config.yaml"),
    algorithms = list(alg1 = list(title = "Alg 1", file = "alg1.yaml"))
  ))

  resolved <- algorithm.viewer:::config_get_algorithm_file("alg1")
  expect_equal(resolved, normalizePath(alg_file, winslash = .Platform$file.sep))
})

test_that("config_get_algorithm_file returns NULL when the file is missing", {
  config_dir <- withr::local_tempdir()
  local_config(list(
    config_path__ = file.path(config_dir, "config.yaml"),
    algorithms = list(alg1 = list(title = "Alg 1", file = "does-not-exist.yaml"))
  ))
  expect_null(algorithm.viewer:::config_get_algorithm_file("alg1"))
})

test_that("config_get_initial_algorithm_file resolves by precedence", {
  config_dir <- withr::local_tempdir()
  file_a <- file.path(config_dir, "a.yaml")
  file_b <- file.path(config_dir, "b.yaml")
  writeLines("meta: {}", file_a)
  writeLines("meta: {}", file_b)
  norm <- function(p) normalizePath(p, winslash = .Platform$file.sep)

  # 1. Explicit initial_algorithm_file wins
  local_config(list(
    config_path__ = file.path(config_dir, "config.yaml"),
    initial_algorithm_file = "a.yaml",
    initial_algorithm_id = "b_alg",
    algorithms = list(b_alg = list(title = "B", file = "b.yaml"))
  ))
  expect_equal(
    algorithm.viewer:::config_get_initial_algorithm_file(), norm(file_a)
  )

  # 2. initial_algorithm_id is used when no explicit file is set
  local_config(list(
    config_path__ = file.path(config_dir, "config.yaml"),
    initial_algorithm_id = "b_alg",
    algorithms = list(
      a_alg = list(title = "A", file = "a.yaml"),
      b_alg = list(title = "B", file = "b.yaml")
    )
  ))
  expect_equal(
    algorithm.viewer:::config_get_initial_algorithm_file(), norm(file_b)
  )

  # 3. Falls back to the first algorithm
  local_config(list(
    config_path__ = file.path(config_dir, "config.yaml"),
    algorithms = list(
      a_alg = list(title = "A", file = "a.yaml"),
      b_alg = list(title = "B", file = "b.yaml")
    )
  ))
  expect_equal(
    algorithm.viewer:::config_get_initial_algorithm_file(), norm(file_a)
  )

  # 4. NULL when nothing is configured
  local_config(list(config_path__ = file.path(config_dir, "config.yaml")))
  expect_null(algorithm.viewer:::config_get_initial_algorithm_file())
})

test_that("config_get_algorithm_id_from_file finds the matching algorithm", {
  config_dir <- withr::local_tempdir()
  alg_file <- file.path(config_dir, "alg1.yaml")
  writeLines("meta: {}", alg_file)

  local_config(list(
    config_path__ = file.path(config_dir, "config.yaml"),
    algorithms = list(alg1 = list(title = "Alg 1", file = "alg1.yaml"))
  ))

  expect_equal(
    algorithm.viewer:::config_get_algorithm_id_from_file(alg_file), "alg1"
  )
  expect_null(
    algorithm.viewer:::config_get_algorithm_id_from_file(
      file.path(config_dir, "unrelated.yaml")
    )
  )
  expect_null(algorithm.viewer:::config_get_algorithm_id_from_file(NULL))
})

test_that("load_config reads and validates a config file", {
  config_env <- algorithm.viewer:::.CONFIG
  old <- as.list(config_env, all.names = TRUE)
  withr::defer({
    rm(list = ls(config_env, all.names = TRUE), envir = config_env)
    list2env(old, config_env)
  })

  config_dir <- withr::local_tempdir()
  config_file <- file.path(config_dir, "config.yaml")
  writeLines(
    c(
      "allow_file_uploads: true",
      "algorithms:",
      "  alg1:",
      "    title: Algorithm One",
      "    file: alg1.yaml"
    ),
    config_file
  )

  algorithm.viewer:::load_config(config_file)
  expect_equal(algorithm.viewer:::config_get_path(), config_file)
  expect_true(algorithm.viewer:::config_allow_file_uploads())
  expect_true(algorithm.viewer:::config_has_algorithms())
})

test_that("load_config rejects an invalid config file", {
  config_env <- algorithm.viewer:::.CONFIG
  old <- as.list(config_env, all.names = TRUE)
  withr::defer({
    rm(list = ls(config_env, all.names = TRUE), envir = config_env)
    list2env(old, config_env)
  })

  config_dir <- withr::local_tempdir()
  config_file <- file.path(config_dir, "config.yaml")
  writeLines("allow_file_uploads: not-a-boolean", config_file)

  expect_error(
    algorithm.viewer:::load_config(config_file),
    "Error with configuration file"
  )
})
