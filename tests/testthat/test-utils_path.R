test_that("expand_and_normalize_path returns NULL for nonexistent path", {
  expect_null(
    algorithm.viewer:::expand_and_normalize_path("/does/not/exist/abc123")
  )
})

test_that(
  "expand_and_normalize_path returns normalized path for existing path",
  {
    tmp <- tempdir()
    result <- algorithm.viewer:::expand_and_normalize_path(tmp)
    expect_false(is.null(result))
    expect_true(dir.exists(result))
  }
)

test_that("expand_and_normalize_path appends trailing slash when requested", {
  tmp <- tempdir()
  result <- algorithm.viewer:::expand_and_normalize_path(
    tmp, add_trailing_slash = TRUE
  )
  expect_true(endsWith(result, .Platform$file.sep))
})

test_that("expand_and_normalize_path does not double trailing slash", {
  tmp <- tempdir()
  result1 <- algorithm.viewer:::expand_and_normalize_path(
    tmp, add_trailing_slash = TRUE
  )
  result2 <- algorithm.viewer:::expand_and_normalize_path(
    result1, add_trailing_slash = TRUE
  )
  expect_equal(result1, result2)
})

test_that("is_file_descendant_of returns TRUE for file inside directory", {
  tmp <- tempdir()
  f <- tempfile(tmpdir = tmp)
  writeLines("x", f)
  on.exit(unlink(f))
  expect_true(algorithm.viewer:::is_file_descendant_of(f, tmp))
})

test_that("is_file_descendant_of returns FALSE for nonexistent file", {
  tmp <- tempdir()
  expect_false(algorithm.viewer:::is_file_descendant_of("/no/such/file", tmp))
})

test_that("is_file_descendant_of returns FALSE for file outside directory", {
  dir1 <- tempfile()
  dir2 <- tempfile()
  dir.create(dir1)
  dir.create(dir2)
  on.exit({
    unlink(dir1, recursive = TRUE)
    unlink(dir2, recursive = TRUE)
  })
  f <- tempfile(tmpdir = dir1)
  writeLines("x", f)
  expect_false(algorithm.viewer:::is_file_descendant_of(f, dir2))
})

test_that("file_relative_to_path returns relative path for descendant file", {
  tmp <- tempdir()
  f <- tempfile(tmpdir = tmp)
  writeLines("x", f)
  on.exit(unlink(f))
  result <- algorithm.viewer:::file_relative_to_path(f, tmp)
  expect_false(startsWith(result, tmp))
  expect_true(nchar(result) < nchar(f))
})

test_that(
  "file_relative_to_path returns basename when file is not a descendant",
  {
    tmp <- tempdir()
    f <- tempfile(tmpdir = tmp)
    writeLines("x", f)
    on.exit(unlink(f))
    other_dir <- tempfile()
    dir.create(other_dir)
    on.exit(unlink(other_dir, recursive = TRUE), add = TRUE)
    result <- algorithm.viewer:::file_relative_to_path(f, other_dir)
    expect_equal(result, basename(f))
  }
)
