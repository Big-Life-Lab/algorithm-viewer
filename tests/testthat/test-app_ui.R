# Tests for app_ui(), the top-level UI builder.
#
# htmltools hoists a page's <head> content out of the body markup, so the
# favicons, title and stylesheets are only visible via renderTags()$head while
# the panels and inputs are in renderTags()$html.
#
# app_ui reads the global config to decide whether the algorithm dropdown and
# the upload control are visible, so those tests swap .CONFIG out via
# local_config() (helper-synthetic.R).

# Render app_ui once, returning the <head> content and the body markup.
render_app_ui <- function() {
  rendered <- htmltools::renderTags(algorithm.viewer:::app_ui(NULL))
  list(head = as.character(rendered$head), body = rendered$html)
}

# Every tag in a UI tree, outermost first.
all_tags <- function(x) {
  if (inherits(x, "shiny.tag")) {
    c(list(x), unlist(lapply(x$children, all_tags), recursive = FALSE))
  } else if (is.list(x)) {
    unlist(lapply(x, all_tags), recursive = FALSE)
  } else {
    list()
  }
}

# The style of the innermost styled <div> around the given input ID. app_ui
# wraps each optional sidebar control in a bare div whose style it sets to
# "display: none" to hide the control, so this is what those tests inspect.
wrapper_style <- function(ui, input_id) {
  wrappers <- Filter(
    function(tag) {
      identical(tag$name, "div") && !is.null(tag$attribs$style) &&
        grepl(sprintf('id="%s"', input_id), as.character(tag), fixed = TRUE)
    },
    all_tags(ui)
  )
  wrappers[[length(wrappers)]]$attribs$style
}

# ── Page structure ────────────────────────────────────────────────────────────

test_that("app_ui builds a complete page", {
  local_config(list())
  ui <- algorithm.viewer:::app_ui(NULL)
  expect_s3_class(ui, "shiny.tag.list")

  rendered <- htmltools::renderTags(ui)
  head <- as.character(rendered$head)
  expect_match(head, "<title>Algorithm Viewer</title>", fixed = TRUE)
  # Both tabsets, and the reactive title in the title panel
  expect_match(rendered$html, 'id="settings_tabs"', fixed = TRUE)
  expect_match(rendered$html, 'id="main_tabs"', fixed = TRUE)
  expect_match(rendered$html, 'id="ui_title"', fixed = TRUE)
})

test_that("app_ui links every favicon size", {
  local_config(list())
  head <- render_app_ui()$head
  for (size in c("32x32", "64x64", "128x128", "180x180")) {
    expect_match(head, sprintf("www/favicon-%s.png", size), fixed = TRUE)
  }
  expect_match(head, 'rel="apple-touch-icon"', fixed = TRUE)
})

test_that("app_ui links every module stylesheet with a cache buster", {
  local_config(list())
  head <- render_app_ui()$head
  for (css in c("crt.css", "csg.css", "plot-additional-controls.css")) {
    # stylesheet_link() appends "?<content hash>"
    expect_match(head, sprintf("www/%s\\?[0-9a-f]{8}", css))
  }
})

# ── Sidebar and main panels ───────────────────────────────────────────────────

test_that("app_ui includes the sidebar's model controls", {
  local_config(list())
  body <- render_app_ui()$body
  expect_match(body, 'id="model_message"', fixed = TRUE)
  expect_match(body, 'id="algorithms"', fixed = TRUE)
  expect_match(body, 'id="upload"', fixed = TRUE)
  expect_match(body, 'id="selected_model_ids"', fixed = TRUE)
})

test_that("app_ui includes both predictor control containers", {
  local_config(list())
  body <- render_app_ui()$body
  expect_match(body, "refgroup", fixed = TRUE)
  expect_match(body, "a_vs_b_groups", fixed = TRUE)
})

test_that("app_ui includes a tab for every plot module", {
  local_config(list())
  body <- render_app_ui()$body
  for (plot_id in c("or_plot", "rr_plot", "pr_plot", "rr_a_vs_b_plot")) {
    expect_match(body, plot_id, fixed = TRUE)
  }
  expect_match(body, 'id="help"', fixed = TRUE)
})

# ── Config-driven visibility ──────────────────────────────────────────────────

test_that("the algorithm dropdown is hidden when no algorithms are configured", {
  local_config(list())
  ui <- algorithm.viewer:::app_ui(NULL)
  expect_equal(wrapper_style(ui, "algorithms"), "display: none")
})

test_that("the algorithm dropdown is shown when algorithms are configured", {
  local_config(list(algorithms = list(a = list(title = "A", file = "a.yaml"))))
  ui <- algorithm.viewer:::app_ui(NULL)
  expect_equal(wrapper_style(ui, "algorithms"), "")
})

test_that("the algorithm dropdown is hidden when selection is disallowed", {
  local_config(list(
    algorithms = list(a = list(title = "A", file = "a.yaml")),
    allow_algorithms_selection = FALSE
  ))
  ui <- algorithm.viewer:::app_ui(NULL)
  expect_equal(wrapper_style(ui, "algorithms"), "display: none")
})

test_that("the upload control is hidden unless uploads are allowed", {
  local_config(list())
  ui <- algorithm.viewer:::app_ui(NULL)
  expect_equal(wrapper_style(ui, "upload"), "display: none")

  local_config(list(allow_file_uploads = TRUE))
  ui <- algorithm.viewer:::app_ui(NULL)
  expect_equal(wrapper_style(ui, "upload"), "")
})
