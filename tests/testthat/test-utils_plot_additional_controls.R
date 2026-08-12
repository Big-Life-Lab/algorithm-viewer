# Tests for the additional-controls helpers (utils_plot_additional_controls.R):
# the pure tag builders and the dropdown-populating functions (which are
# exercised by capturing their shiny::updateSelectInput calls via a mock).

# ── plot_additional_controls_height ──────────────────────────────────────────

test_that("plot_additional_controls_height returns a positive numeric scalar", {
  h <- algorithm.viewer:::plot_additional_controls_height()
  expect_type(h, "double")
  expect_length(h, 1)
  expect_gt(h, 0)
})

# ── plot_additional_controls_container ───────────────────────────────────────

test_that("plot_additional_controls_container wraps cells in a full-width table row", {
  cell <- shiny::tags$td("a cell")
  div <- algorithm.viewer:::plot_additional_controls_container(cell)
  expect_s3_class(div, "shiny.tag")
  expect_equal(div$name, "div")
  expect_equal(div$attribs$class, "plot-additional-controls")
  html <- as.character(div)
  expect_match(html, "<table", fixed = TRUE)
  expect_match(html, "width: 100%", fixed = TRUE)
  expect_match(html, "a cell", fixed = TRUE)
})

test_that("plot_additional_controls_container preserves all supplied cells", {
  div <- algorithm.viewer:::plot_additional_controls_container(
    shiny::tags$td("first"),
    shiny::tags$td("second")
  )
  html <- as.character(div)
  expect_match(html, "first", fixed = TRUE)
  expect_match(html, "second", fixed = TRUE)
})

# ── plot_additional_controls_dropdown ────────────────────────────────────────

test_that("plot_additional_controls_dropdown builds a td with a labelled select input", {
  td <- algorithm.viewer:::plot_additional_controls_dropdown(
    id = "predictor",
    label = "Predictor",
    choices = c("Relative Risk" = "rr", "Absolute Difference" = "ad"),
    num_columns = 2
  )
  expect_s3_class(td, "shiny.tag")
  expect_equal(td$name, "td")
  html <- as.character(td)
  # Label rendered with a trailing colon
  expect_match(html, "Predictor:", fixed = TRUE)
  # selectInput rendered with the given id and choices
  expect_match(html, "id=\"predictor\"", fixed = TRUE)
  expect_match(html, "Relative Risk", fixed = TRUE)
  expect_match(html, "Absolute Difference", fixed = TRUE)
})

test_that("plot_additional_controls_dropdown omits the help icon without a tooltip", {
  td <- algorithm.viewer:::plot_additional_controls_dropdown(
    "p", "P", c(a = "a"), num_columns = 2
  )
  expect_false(grepl("fa-circle-question", as.character(td), fixed = TRUE))
})

test_that("plot_additional_controls_dropdown renders a click/hover tooltip help icon", {
  td <- algorithm.viewer:::plot_additional_controls_dropdown(
    "p", "P", c(a = "a"), num_columns = 2,
    tooltip = "Helpful explanation"
  )
  html <- as.character(td)
  # Help icon present.
  expect_match(html, "fa-circle-question", fixed = TRUE)
  # Focusable wrapper (so a click/tap reveals the tooltip, not just hover).
  expect_match(html, "class=\"apc-help\"", fixed = TRUE)
  expect_match(html, "tabindex=\"0\"", fixed = TRUE)
  # Tooltip text rendered in the revealable element and as the accessible label.
  expect_match(html, "class=\"apc-help-text\"", fixed = TRUE)
  expect_match(html, "Helpful explanation", fixed = TRUE)
  # The label and select input are still rendered.
  expect_match(html, "P:", fixed = TRUE)
  expect_match(html, "id=\"p\"", fixed = TRUE)
})

test_that("interaction_predictor_tooltip describes the categorical wrap-around", {
  txt <- algorithm.viewer:::interaction_predictor_tooltip()
  expect_type(txt, "character")
  expect_match(txt, "next category", fixed = TRUE)
  expect_match(txt, "wraps", fixed = TRUE)
})

test_that("plot_additional_controls_dropdown sizes the cell as an equal fraction", {
  td2 <- algorithm.viewer:::plot_additional_controls_dropdown(
    "p", "P", c(a = "a"), num_columns = 2
  )
  td4 <- algorithm.viewer:::plot_additional_controls_dropdown(
    "p", "P", c(a = "a"), num_columns = 4
  )
  expect_match(td2$attribs$style, "width: 50%", fixed = TRUE)
  expect_match(td4$attribs$style, "width: 25%", fixed = TRUE)
})

# ── plot_additional_controls_checkbox ────────────────────────────────────────

test_that("plot_additional_controls_checkbox builds a td with a checkbox input", {
  td <- algorithm.viewer:::plot_additional_controls_checkbox(
    id = "logarithmic",
    label = "Logarithmic",
    value = TRUE,
    num_columns = 3
  )
  expect_s3_class(td, "shiny.tag")
  expect_equal(td$name, "td")
  html <- as.character(td)
  expect_match(html, "id=\"logarithmic\"", fixed = TRUE)
  expect_match(html, "Logarithmic", fixed = TRUE)
  # value = TRUE renders a checked checkbox
  expect_match(html, "checked", fixed = TRUE)
})

test_that("plot_additional_controls_checkbox respects an unchecked initial value", {
  td <- algorithm.viewer:::plot_additional_controls_checkbox(
    "logarithmic", "Logarithmic", value = FALSE, num_columns = 2
  )
  expect_false(grepl("checked", as.character(td), fixed = TRUE))
})

test_that("plot_additional_controls_checkbox sizes the cell as an equal fraction", {
  td <- algorithm.viewer:::plot_additional_controls_checkbox(
    "c", "C", value = TRUE, num_columns = 4
  )
  expect_match(td$attribs$style, "width: 25%", fixed = TRUE)
})

# ── populate_dropdown_predictors / populate_dropdown_interaction_predictors ───
#
# These call shiny::updateSelectInput, which needs a live session. Rather than
# stand up a full module, we mock updateSelectInput and capture its arguments.

# Capture the most recent updateSelectInput call into `record`, for the
# duration of the calling test.
local_capture_update_select <- function(record, env = parent.frame()) {
  testthat::local_mocked_bindings(
    updateSelectInput = function(session, inputId, label = NULL,
                                 choices = NULL, selected = NULL, ...) {
      record$inputId <- inputId
      record$choices <- choices
      record$selected <- selected
      invisible(NULL)
    },
    .package = "shiny",
    .env = env
  )
}

test_that("populate_dropdown_predictors selects the first predictor by default", {
  record <- new.env()
  local_capture_update_select(record)
  models <- list(make_synthetic_model_data())

  algorithm.viewer:::populate_dropdown_predictors(
    session = NULL, id = "predictor", models = models
  )

  expect_equal(record$inputId, "predictor")
  # Synthetic model has predictors age ("Age") and smoking ("Smoking status")
  expect_equal(unname(record$choices), c("age", "smoking"))
  expect_equal(names(record$choices), c("Age", "Smoking status"))
  # First predictor selected
  expect_equal(record$selected, "age")
})

test_that("populate_dropdown_predictors honours an explicit selection", {
  record <- new.env()
  local_capture_update_select(record)

  algorithm.viewer:::populate_dropdown_predictors(
    session = NULL, id = "predictor",
    models = list(make_synthetic_model_data()), selected = "smoking"
  )

  expect_equal(record$selected, "smoking")
})

test_that("populate_dropdown_predictors clears the input when empty = TRUE", {
  record <- new.env()
  local_capture_update_select(record)

  algorithm.viewer:::populate_dropdown_predictors(
    session = NULL, id = "predictor",
    models = list(make_synthetic_model_data()), empty = TRUE
  )

  expect_length(record$choices, 0)
  expect_length(record$selected, 0)
})

test_that("populate_dropdown_predictors selects nothing when there are no predictors", {
  record <- new.env()
  local_capture_update_select(record)

  algorithm.viewer:::populate_dropdown_predictors(
    session = NULL, id = "predictor", models = list()
  )

  expect_length(record$selected, 0)
})

test_that("populate_dropdown_interaction_predictors prepends and selects the empty option", {
  record <- new.env()
  local_capture_update_select(record)
  empty <- algorithm.viewer:::config_get_empty_selection()

  algorithm.viewer:::populate_dropdown_interaction_predictors(
    session = NULL, id = "interaction_predictor",
    models = list(make_synthetic_model_data())
  )

  expect_equal(record$inputId, "interaction_predictor")
  # Empty option is prepended ahead of the gathered predictors
  expect_equal(record$choices[[1]], empty)
  expect_equal(names(record$choices)[[1]], empty)
  expect_true("age" %in% unlist(record$choices))
  # Default selection is the empty option
  expect_equal(record$selected, empty)
})

test_that("populate_dropdown_interaction_predictors honours an explicit selection", {
  record <- new.env()
  local_capture_update_select(record)

  algorithm.viewer:::populate_dropdown_interaction_predictors(
    session = NULL, id = "interaction_predictor",
    models = list(make_synthetic_model_data()), selected = "age"
  )

  expect_equal(record$selected, "age")
})

test_that("populate_dropdown_interaction_predictors clears the input when empty = TRUE", {
  record <- new.env()
  local_capture_update_select(record)

  algorithm.viewer:::populate_dropdown_interaction_predictors(
    session = NULL, id = "interaction_predictor",
    models = list(make_synthetic_model_data()), empty = TRUE
  )

  expect_length(record$choices, 0)
  expect_length(record$selected, 0)
})

test_that("populate_dropdown_interaction_predictors selects nothing with no predictors", {
  record <- new.env()
  local_capture_update_select(record)

  algorithm.viewer:::populate_dropdown_interaction_predictors(
    session = NULL, id = "interaction_predictor", models = list()
  )

  expect_length(record$selected, 0)
})
