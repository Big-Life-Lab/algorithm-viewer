# Tests for the continuous slider group module: the pure .csg_* helpers and
# the server logic via shiny::testServer.

# ── ID helpers ─────────────────────────────────────────────────────────────────

test_that(".csg_group_input_id and .csg_group_edit_id compose IDs", {
  expect_equal(
    algorithm.viewer:::.csg_group_input_id("age", "me"), "age__me"
  )
  expect_equal(
    algorithm.viewer:::.csg_group_edit_id("age", "me"), "age__me__edit"
  )
})

# ── .csg_step ──────────────────────────────────────────────────────────────────

test_that(".csg_step uses step 1 for consecutive integer ranges", {
  expect_identical(algorithm.viewer:::.csg_step(1:100), 1L)
  expect_identical(algorithm.viewer:::.csg_step(20:25), 1L)
  # Order should not matter
  expect_identical(algorithm.viewer:::.csg_step(rev(20:25)), 1L)
})

test_that(".csg_step uses the minimum gap for non-integer sequences", {
  expect_equal(algorithm.viewer:::.csg_step(seq(0, 1, by = 0.1)), 0.1)
  expect_equal(algorithm.viewer:::.csg_step(c(13, 13.01, 13.02)), 0.01)
})

test_that(".csg_step uses the minimum gap for non-uniform values", {
  expect_equal(algorithm.viewer:::.csg_step(c(1, 2, 10)), 1)
  expect_equal(algorithm.viewer:::.csg_step(c(0, 0.5, 10)), 0.5)
})

test_that(".csg_step falls back to 1 for a single value", {
  expect_identical(algorithm.viewer:::.csg_step(5), 1L)
})

# ── UI ─────────────────────────────────────────────────────────────────────────

test_that("continuousSliderGroupUI renders one slider per group", {
  md <- make_synthetic_model_data()
  ui <- algorithm.viewer:::continuousSliderGroupUI(
    "ctrl", md,
    variable = "age",
    groups = c(me = "Me", ref = "Ref"),
    initial_values = list(me = 45, ref = 60)
  )
  html <- as.character(ui)
  expect_match(html, "ctrl-age__me", fixed = TRUE)
  expect_match(html, "ctrl-age__ref", fixed = TRUE)
  # Variable label with units shown by default
  expect_match(html, "Age (years)", fixed = TRUE)
})

# ── Server ─────────────────────────────────────────────────────────────────────
#
# Note on "priming": the module's observers use ignoreInit = TRUE. In
# testServer there is no startup flush, so an observer's first-ever run
# happens at the first setInputs() — and ignoreInit swallows exactly that
# run. Tests therefore prime each input once with its initial value before
# exercising real behaviour.

test_that("server initialises rv_values from initial_values, clamped", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::continuousSliderGroupServer,
    args = list(
      model_data = md,
      variable = "age",
      groups = c(me = "Me", ref = "Ref"),
      # 200 is above the allowable maximum of 100 and must be clamped
      initial_values = list(me = 45, ref = 200)
    ),
    {
      returned <- session$getReturned()
      vals <- returned$rv_values()
      expect_equal(vals$me, 45)
      expect_equal(vals$ref, 100)
    }
  )
})

test_that("server defaults missing initial values to the minimum", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::continuousSliderGroupServer,
    args = list(
      model_data = md,
      variable = "age",
      groups = c(me = "Me", ref = "Ref"),
      initial_values = list(me = 45)
    ),
    {
      vals <- session$getReturned()$rv_values()
      expect_equal(vals$ref, 20)
    }
  )
})

test_that("slider input updates rv_values as a double", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::continuousSliderGroupServer,
    args = list(
      model_data = md,
      variable = "age",
      groups = c(me = "Me", ref = "Ref"),
      initial_values = list(me = 45, ref = 60)
    ),
    {
      session$setInputs(age__me = 45) # prime (swallowed by ignoreInit)
      session$setInputs(age__me = 50L)
      vals <- session$getReturned()$rv_values()
      expect_identical(vals$me, 50)
      expect_identical(vals$ref, 60)
    }
  )
})

test_that("update_values clamps and updates rv_values immediately", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::continuousSliderGroupServer,
    args = list(
      model_data = md,
      variable = "age",
      groups = c(me = "Me", ref = "Ref"),
      initial_values = list(me = 45, ref = 60)
    ),
    {
      returned <- session$getReturned()
      returned$update_values(list(me = 5, ref = 70)) # 5 below min of 20
      vals <- returned$rv_values()
      expect_equal(vals$me, 20)
      expect_equal(vals$ref, 70)
    }
  )
})

test_that("update_values ignores unknown groups and non-numeric values", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::continuousSliderGroupServer,
    args = list(
      model_data = md,
      variable = "age",
      groups = c(me = "Me"),
      initial_values = list(me = 45)
    ),
    {
      returned <- session$getReturned()
      returned$update_values(list(other = 50, me = "abc"))
      vals <- returned$rv_values()
      expect_equal(vals$me, 45)
      expect_false("other" %in% names(vals))
    }
  )
})

test_that("a stale slider round-trip after update_values is absorbed", {
  # update_values() registers one pending programmatic update; the next
  # slider input event (the stale round-trip from updateSliderInput) must be
  # ignored, and only a subsequent user event applied.
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::continuousSliderGroupServer,
    args = list(
      model_data = md,
      variable = "age",
      groups = c(me = "Me"),
      initial_values = list(me = 45)
    ),
    {
      returned <- session$getReturned()
      session$setInputs(age__me = 45) # prime (swallowed by ignoreInit)

      returned$update_values(list(me = 30))
      expect_equal(returned$rv_values()$me, 30)

      # Stale round-trip carrying the pre-update value: absorbed
      session$setInputs(age__me = 45)
      expect_equal(returned$rv_values()$me, 30)

      # A genuine user change afterwards is applied
      session$setInputs(age__me = 55)
      expect_equal(returned$rv_values()$me, 55)
    }
  )
})

test_that("destroy() removes observers so inputs no longer update values", {
  md <- make_synthetic_model_data()
  shiny::testServer(
    algorithm.viewer:::continuousSliderGroupServer,
    args = list(
      model_data = md,
      variable = "age",
      groups = c(me = "Me"),
      initial_values = list(me = 45)
    ),
    {
      returned <- session$getReturned()
      # Prime, then confirm the observer is live before destroying it
      session$setInputs(age__me = 45)
      session$setInputs(age__me = 50)
      expect_equal(returned$rv_values()$me, 50)

      returned$destroy()
      session$setInputs(age__me = 60)
      expect_equal(returned$rv_values()$me, 50)
    }
  )
})
