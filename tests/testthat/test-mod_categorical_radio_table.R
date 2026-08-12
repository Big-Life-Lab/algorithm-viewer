# Tests for the categorical radio table module: ID helper, UI structure, and
# server logic via shiny::testServer.

test_that(".crt_group_input_id composes IDs", {
  expect_equal(
    algorithm.viewer:::.crt_group_input_id("smoking", "me"),
    "smoking__me"
  )
})

# ── UI ─────────────────────────────────────────────────────────────────────────

test_that("categoricalRadioTableUI renders one radio group per column", {
  md <- make_synthetic_model_data()
  ui <- algorithm.viewer:::categoricalRadioTableUI(
    "ctrl", md,
    variable = "smoking",
    groups = c(me = "Me", ref = "Ref"),
    initial_values = list(me = "2", ref = "1")
  )
  html <- as.character(ui)
  expect_match(html, 'name="ctrl-smoking__me"', fixed = TRUE)
  expect_match(html, 'name="ctrl-smoking__ref"', fixed = TRUE)
  # Level labels are shown, the NA::b level is excluded
  expect_match(html, "Never", fixed = TRUE)
  expect_match(html, "Current", fixed = TRUE)
  expect_false(grepl("Missing", html, fixed = TRUE))
  # The radio matching each group's initial value is pre-checked:
  # me = "2" -> Current, ref = "1" -> Never
  expect_match(html, 'value="Current"[^>]*checked')
})

# ── Server ─────────────────────────────────────────────────────────────────────

test_that("server initialises rv_values from initial_values", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::categoricalRadioTableServer,
    args = list(
      model_data = md,
      variable = "smoking",
      groups = c(me = "Me", ref = "Ref"),
      initial_values = list(me = "2", ref = "1")
    ),
    {
      vals <- session$getReturned()$rv_values()
      expect_equal(vals$me, "2")
      expect_equal(vals$ref, "1")
    }
  )
})

test_that("server falls back to the first allowable value for bad initials", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::categoricalRadioTableServer,
    args = list(
      model_data = md,
      variable = "smoking",
      groups = c(me = "Me", ref = "Ref"),
      # "9" is not an allowable value; ref is missing entirely
      initial_values = list(me = "9")
    ),
    {
      vals <- session$getReturned()$rv_values()
      expect_equal(vals$me, "1")
      expect_equal(vals$ref, "1")
    }
  )
})

test_that("a radio selection (label) is converted to its internal value", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::categoricalRadioTableServer,
    args = list(
      model_data = md,
      variable = "smoking",
      groups = c(me = "Me", ref = "Ref"),
      initial_values = list(me = "1", ref = "1")
    ),
    {
      # Prime: in testServer the ignoreInit observer swallows the first-ever
      # setInputs, so send the initial label once before the real change.
      session$setInputs(smoking__me = "Never")
      # The browser reports the display label; the server stores the value
      session$setInputs(smoking__me = "Current")
      vals <- session$getReturned()$rv_values()
      expect_equal(vals$me, "2")
      expect_equal(vals$ref, "1")
    }
  )
})

test_that("an unknown label leaves rv_values unchanged", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::categoricalRadioTableServer,
    args = list(
      model_data = md,
      variable = "smoking",
      groups = c(me = "Me"),
      initial_values = list(me = "1")
    ),
    {
      session$setInputs(smoking__me = "Never") # prime (swallowed)
      session$setInputs(smoking__me = "No Such Label")
      expect_equal(session$getReturned()$rv_values()$me, "1")
    }
  )
})

test_that("destroy() removes observers so inputs no longer update values", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::categoricalRadioTableServer,
    args = list(
      model_data = md,
      variable = "smoking",
      groups = c(me = "Me"),
      initial_values = list(me = "1")
    ),
    {
      returned <- session$getReturned()
      # Prime, then confirm the observer is live before destroying it
      session$setInputs(smoking__me = "Never")
      session$setInputs(smoking__me = "Current")
      expect_equal(returned$rv_values()$me, "2")

      returned$destroy()
      session$setInputs(smoking__me = "Never")
      expect_equal(returned$rv_values()$me, "2")
    }
  )
})
