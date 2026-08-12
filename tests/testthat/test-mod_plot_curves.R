# Tests for the curve-calculation functions in mod_plot_pr.R, mod_plot_rr.R,
# mod_plot_or.R, and mod_plot_rr_a_vs_b.R.
#
# These run the real model pipeline on the htnport-reduced fixture (.test_md,
# loaded by helper-fixtures.R) and check mathematical invariants that must
# hold for any model:
#   - the relative risk / odds ratio at the reference value is exactly 1
#   - RR and OR are consistent transformations of the predicted risks
#   - comparing a group against itself yields RR == 1 everywhere

# Pick the fixture model and small predictor value sets once for all tests.
.curve_env <- new.env()
if (!is.null(.test_md)) {
  .curve_env$md <- .test_md$models[[1]]
  .curve_env$ref <- .curve_env$md$reference_group
  .curve_env$cont <- fixture_predictor(.curve_env$md, "Continuous")
  .curve_env$cat <- fixture_predictor(.curve_env$md, "Categorical")

  # A small set of continuous values beginning with the reference value, so
  # row 1 of each curve corresponds to the reference itself.
  cont_allowable <- algorithm.viewer:::get_predictor_allowable_values(
    .curve_env$md, .curve_env$cont
  )
  cont_ref <- .curve_env$ref[[.curve_env$cont]]
  .curve_env$cont_values <- c(
    cont_ref,
    utils::head(setdiff(cont_allowable, cont_ref), 2)
  )

  cat_allowable <- algorithm.viewer:::get_predictor_allowable_values(
    .curve_env$md, .curve_env$cat
  )
  cat_ref <- .curve_env$ref[[.curve_env$cat]]
  .curve_env$cat_values <- c(cat_ref, setdiff(cat_allowable, cat_ref))
}

skip_if_no_fixture <- function() {
  skip_if(is.null(.test_md), "Test fixtures not available")
  skip_if(
    is.null(.curve_env$cont) || is.null(.curve_env$cat),
    "Fixture lacks a continuous or categorical predictor"
  )
}

# ── .calculate_pr_curve ────────────────────────────────────────────────────────

test_that("PR curve returns one valid risk per predictor value", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_pr_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  expect_equal(nrow(curve$df), length(.curve_env$cont_values))
  expect_true(all(is.finite(curve$df$PR)))
  expect_true(all(curve$df$PR > 0 & curve$df$PR < 1))
  expect_equal(curve$y_axis_label, "Predicted Risk")
  expect_equal(curve$x_axis_type, "Continuous")
  expect_equal(curve$ylim_linear, c(0, 1))
})

# ── .calculate_rr_curve ────────────────────────────────────────────────────────

test_that("RR is exactly 1 at the reference value (continuous predictor)", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  # Row 1 is the reference value itself
  expect_equal(curve$df$RR[[1]], 1, tolerance = 1e-12)
  expect_true(all(is.finite(curve$df$RR) & curve$df$RR > 0))
  expect_equal(nrow(curve$df), length(.curve_env$cont_values))
})

test_that("RR is exactly 1 at the reference value (categorical predictor)", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_curve(
    .curve_env$cat, .curve_env$md,
    predictor_allowable_values = .curve_env$cat_values
  )
  expect_equal(curve$df$RR[[1]], 1, tolerance = 1e-12)
  expect_equal(curve$x_axis_type, "Categorical")
  expect_equal(nrow(curve$df), length(.curve_env$cat_values))
})

test_that("RR equals predicted risk divided by reference predicted risk", {
  skip_if_no_fixture()
  pr <- algorithm.viewer:::.calculate_pr_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  rr <- algorithm.viewer:::.calculate_rr_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  # Value 1 is the reference, so PR[1] is the reference risk
  expect_equal(rr$df$RR, pr$df$PR / pr$df$PR[[1]], tolerance = 1e-8)
})

# ── .calculate_or_curve ────────────────────────────────────────────────────────

test_that("OR is exactly 1 at the reference value", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_or_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  expect_equal(curve$df$OR[[1]], 1, tolerance = 1e-12)
  expect_true(all(is.finite(curve$df$OR) & curve$df$OR > 0))
  expect_equal(curve$y_axis_label, "Odds Ratio")
})

test_that("OR is the odds transform of the predicted risks", {
  skip_if_no_fixture()
  pr <- algorithm.viewer:::.calculate_pr_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  or <- algorithm.viewer:::.calculate_or_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  odds <- pr$df$PR / (1 - pr$df$PR)
  expect_equal(or$df$OR, odds / odds[[1]], tolerance = 1e-8)
})

# ── Interaction curves ─────────────────────────────────────────────────────────

test_that("RR interaction curve has one row per predictor value", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_curve_interaction(
    .curve_env$cont, .curve_env$cat, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  expect_equal(nrow(curve$df), length(.curve_env$cont_values))
  expect_true(all(is.finite(curve$df$RR) & curve$df$RR > 0))
  # The title is the x-axis predictor's label (the interaction predictor now
  # lives in the subtitle; see the dedicated subtitle tests below).
  pred_label <- algorithm.viewer:::get_variable_label_and_units(
    .curve_env$md, .curve_env$cont,
    escape_html = TRUE
  )
  expect_equal(as.character(curve$title), as.character(pred_label))
})

test_that("OR interaction curve has one row per predictor value", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_or_curve_interaction(
    .curve_env$cont, .curve_env$cat, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  expect_equal(nrow(curve$df), length(.curve_env$cont_values))
  expect_true(all(is.finite(curve$df$OR) & curve$df$OR > 0))
})

test_that("interaction RR and OR agree at small risks' direction", {
  skip_if_no_fixture()
  rr <- algorithm.viewer:::.calculate_rr_curve_interaction(
    .curve_env$cont, .curve_env$cat, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  or <- algorithm.viewer:::.calculate_or_curve_interaction(
    .curve_env$cont, .curve_env$cat, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  # OR and RR are always on the same side of 1 for the same comparison
  expect_equal(rr$df$RR > 1, or$df$OR > 1)
})

# ── Interaction curve subtitles ─────────────────────────────────────────────────

test_that("non-interaction curves carry no subtitle", {
  skip_if_no_fixture()
  rr <- algorithm.viewer:::.calculate_rr_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  or <- algorithm.viewer:::.calculate_or_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  pr <- algorithm.viewer:::.calculate_pr_curve(
    .curve_env$cont, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  expect_null(rr$subtitle)
  expect_null(or$subtitle)
  expect_null(pr$subtitle)
})

test_that("RR interaction curve subtitle names the interaction predictor", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_curve_interaction(
    .curve_env$cont, .curve_env$cat, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  int_label <- algorithm.viewer:::get_variable_label_and_units(
    .curve_env$md, .curve_env$cat,
    escape_html = TRUE
  )
  expect_equal(length(curve$subtitle), 1)
  expect_match(curve$subtitle, "Interaction = ", fixed = TRUE)
  expect_true(grepl(as.character(int_label), curve$subtitle, fixed = TRUE))
})

test_that("OR interaction curve subtitle names the interaction predictor", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_or_curve_interaction(
    .curve_env$cont, .curve_env$cat, .curve_env$md,
    predictor_allowable_values = .curve_env$cont_values
  )
  int_label <- algorithm.viewer:::get_variable_label_and_units(
    .curve_env$md, .curve_env$cat,
    escape_html = TRUE
  )
  expect_equal(length(curve$subtitle), 1)
  expect_match(curve$subtitle, "Interaction = ", fixed = TRUE)
  expect_true(grepl(as.character(int_label), curve$subtitle, fixed = TRUE))
})

# ── A vs B curves ──────────────────────────────────────────────────────────────

test_that("A vs B curve is all 1s when both groups are identical", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = .curve_env$ref,
    b_group = .curve_env$ref,
    display_mode = "rr"
  )
  expect_equal(
    curve$df$RR,
    rep(1, length(.curve_env$ref)),
    tolerance = 1e-12
  )
  # One row per predictor in the group
  expect_equal(nrow(curve$df), length(.curve_env$ref))
  expect_equal(curve$overall_rr, 1, tolerance = 1e-12)
})

test_that("A vs B curve detects a changed categorical predictor", {
  skip_if_no_fixture()
  a_group <- .curve_env$ref
  other_value <- setdiff(
    .curve_env$cat_values,
    a_group[[.curve_env$cat]]
  )[[1]]
  a_group[[.curve_env$cat]] <- other_value

  curve <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = a_group,
    b_group = .curve_env$ref,
    display_mode = "rr"
  )
  # Predictors that did not change still have RR == 1; the changed
  # predictor's RR differs from 1.
  rr <- curve$df$RR
  changed_idx <- which(names(a_group) == .curve_env$cat)
  expect_false(isTRUE(all.equal(rr[[changed_idx]], 1)))
  expect_equal(
    rr[-changed_idx],
    rep(1, length(rr) - 1),
    tolerance = 1e-12
  )
})

test_that("A vs B curve omits predictors the model does not use", {
  skip_if_no_fixture()
  # combine_models can hand A/B groups that span predictors from other models.
  # Predictors this model does not use must not appear as rows (the pipeline
  # ignores the column, so such a row would be a meaningless RR of 1).
  a_group <- .curve_env$ref
  b_group <- .curve_env$ref
  a_group[["not_a_model_predictor"]] <- 1
  b_group[["not_a_model_predictor"]] <- 2

  curve <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = a_group,
    b_group = b_group,
    display_mode = "rr"
  )
  expect_false("not_a_model_predictor" %in% curve$df$predictor)
  # One row per real model predictor in the group (the foreign one dropped).
  expect_equal(nrow(curve$df), length(.curve_env$ref))
})

# ── A vs B display_mode parameter ────────────────────────────────────────────

test_that("A vs B curve labels reflect display_mode = 'rr'", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = .curve_env$ref,
    b_group = .curve_env$ref,
    display_mode = "rr"
  )
  expect_equal(curve$y_axis_label, "Relative Risk")
  expect_equal(curve$title, "Relative Risk")
  # The primary y aesthetic is RR, with AD exposed as the "other" measure.
  expect_equal(rlang::as_string(curve$aes_args$y), "RR")
  expect_equal(rlang::as_string(curve$aes_args$other), "AD")
})

test_that("A vs B curve labels reflect display_mode = 'ad'", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = .curve_env$ref,
    b_group = .curve_env$ref,
    display_mode = "ad"
  )
  expect_equal(curve$y_axis_label, "Absolute Difference")
  expect_equal(curve$title, "Absolute Difference")
  # The primary y aesthetic is AD, with RR exposed as the "other" measure.
  expect_equal(rlang::as_string(curve$aes_args$y), "AD")
  expect_equal(rlang::as_string(curve$aes_args$other), "RR")
})

test_that("A vs B curve computes both RR and AD regardless of display_mode", {
  skip_if_no_fixture()
  rr_mode <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = .curve_env$ref,
    b_group = .curve_env$ref,
    display_mode = "rr"
  )
  ad_mode <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = .curve_env$ref,
    b_group = .curve_env$ref,
    display_mode = "ad"
  )
  # display_mode only changes labels/aesthetics, not the underlying RR and AD
  # columns, which are always both present and identical between modes.
  expect_true(all(c("RR", "AD") %in% names(rr_mode$df)))
  expect_true(all(c("RR", "AD") %in% names(ad_mode$df)))
  expect_equal(rr_mode$df$RR, ad_mode$df$RR, tolerance = 1e-12)
  expect_equal(rr_mode$df$AD, ad_mode$df$AD, tolerance = 1e-12)
  # Identical groups: RR is 1 everywhere and AD is 0 everywhere.
  expect_equal(rr_mode$df$AD, rep(0, nrow(rr_mode$df)), tolerance = 1e-12)
})

test_that("A vs B curve falls back to 'Unknown' labels for invalid mode", {
  skip_if_no_fixture()
  curve <- algorithm.viewer:::.calculate_rr_a_vs_b_curve(
    .curve_env$md,
    a_group = .curve_env$ref,
    b_group = .curve_env$ref,
    display_mode = "not_a_mode"
  )
  expect_equal(curve$y_axis_label, "Unknown")
  expect_equal(curve$title, "Unknown")
  # The y aesthetic still defaults to RR for an unrecognized mode.
  expect_equal(rlang::as_string(curve$aes_args$y), "RR")
  expect_equal(rlang::as_string(curve$aes_args$other), "AD")
})
