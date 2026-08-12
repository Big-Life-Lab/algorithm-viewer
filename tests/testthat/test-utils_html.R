# Tests for the general HTML/CSS utilities (utils_html.R).

# ── stylesheet_link ──────────────────────────────────────────────────────────

test_that("stylesheet_link returns a stylesheet link tag", {
  tag <- algorithm.viewer:::stylesheet_link("www/plot-additional-controls.css")
  expect_s3_class(tag, "shiny.tag")
  expect_equal(tag$name, "link")
  expect_equal(tag$attribs$rel, "stylesheet")
})

test_that("stylesheet_link appends a content-hash cache buster for an existing file", {
  tag <- algorithm.viewer:::stylesheet_link("www/plot-additional-controls.css")
  href <- tag$attribs$href
  # "<path>?<8-hex-char-hash>"
  expect_match(href, "^www/plot-additional-controls\\.css\\?[0-9a-f]{8}$")
})

test_that("stylesheet_link is deterministic for unchanged file contents", {
  href1 <- algorithm.viewer:::stylesheet_link("www/csg.css")$attribs$href
  href2 <- algorithm.viewer:::stylesheet_link("www/csg.css")$attribs$href
  expect_identical(href1, href2)
})

test_that("stylesheet_link gives different hashes for different files", {
  href_a <- algorithm.viewer:::stylesheet_link("www/crt.css")$attribs$href
  href_b <- algorithm.viewer:::stylesheet_link("www/csg.css")$attribs$href
  hash_of <- function(href) sub("^.*\\?", "", href)
  expect_false(identical(hash_of(href_a), hash_of(href_b)))
})

test_that("stylesheet_link leaves the href unchanged when the file does not exist", {
  href <- algorithm.viewer:::stylesheet_link("www/does-not-exist.css")$attribs$href
  expect_equal(href, "www/does-not-exist.css")
})
