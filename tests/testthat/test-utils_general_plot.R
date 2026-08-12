# Tests for the general plotting utilities (message plots, safe rendering,
# and make_general_plot). These use synthetic curve data so they never need
# the model pipeline.

make_synthetic_curve_data <- function(
  x = c(20, 30, 40),
  rr = c(1, 1.5, 2),
  model_title = "Model One",
  x_axis_type = "Continuous"
) {
  df <- data.frame(
    Age = x,
    RR = rr,
    Model = model_title,
    Comparison = "Age (x vs ref)",
    stringsAsFactors = FALSE
  )
  list(
    df = df,
    x_axis_label = "Age",
    y_axis_label = "Relative Risk",
    title = "Age",
    x_axis_type = x_axis_type,
    aes_args = list(
      x = rlang::sym("Age"),
      y = rlang::sym("RR"),
      label = rlang::sym("Comparison")
    )
  )
}

make_synthetic_model_definitions <- function(model_title = "Model One") {
  list(
    models = list(
      m1 = list(
        model_id = "m1",
        title = model_title,
        model_color = "#112233"
      )
    )
  )
}

plot_texts <- function(p) {
  built <- plotly::plotly_build(p)
  unlist(lapply(built$x$data, function(tr) tr$text))
}

# ── make_aes ───────────────────────────────────────────────────────────────────

test_that("make_aes combines base args with extra mappings", {
  aes_args <- list(x = rlang::sym("Age"), y = rlang::sym("RR"))
  mapping <- algorithm.viewer:::make_aes(aes_args, fill = rlang::sym("Model"))
  expect_s3_class(mapping, "uneval")
  expect_setequal(names(mapping), c("x", "y", "fill"))
})

# ── make_message_plot ──────────────────────────────────────────────────────────

test_that("make_message_plot returns a plotly widget containing the message", {
  p <- algorithm.viewer:::make_message_plot("hello test message")
  expect_s3_class(p, "plotly")
  expect_true(any(grepl("hello test message", plot_texts(p), fixed = TRUE)))
})

# ── plot_render_safely ─────────────────────────────────────────────────────────

test_that("plot_render_safely returns the function's value on success", {
  expect_equal(algorithm.viewer:::plot_render_safely(function() 42), 42)
})

test_that("plot_render_safely converts an error into a message plot", {
  p <- suppressMessages(
    algorithm.viewer:::plot_render_safely(function() stop("boom"))
  )
  expect_s3_class(p, "plotly")
  expect_true(any(grepl("boom", plot_texts(p), fixed = TRUE)))
})

test_that("plot_render_safely HTML-escapes the error message", {
  p <- suppressMessages(
    algorithm.viewer:::plot_render_safely(function() stop("<script>"))
  )
  texts <- plot_texts(p)
  expect_false(any(grepl("<script>", texts, fixed = TRUE)))
  expect_true(any(grepl("&lt;script&gt;", texts, fixed = TRUE)))
})

# ── make_general_plot ──────────────────────────────────────────────────────────

test_that("make_general_plot prompts for an upload when no algorithm is loaded", {
  p <- algorithm.viewer:::make_general_plot(NULL, NULL)
  expect_s3_class(p, "plotly")
  expect_true(any(grepl("upload an algorithm", plot_texts(p), fixed = TRUE)))
})

test_that("make_general_plot prompts for model selection with empty curve data", {
  defs <- make_synthetic_model_definitions()
  for (curves in list(NULL, list())) {
    p <- algorithm.viewer:::make_general_plot(curves, defs)
    expect_s3_class(p, "plotly")
    expect_true(any(grepl(
      "select at least one model", plot_texts(p),
      fixed = TRUE
    )))
  }
})

test_that("make_general_plot renders a continuous (line) plot", {
  p <- algorithm.viewer:::make_general_plot(
    list(make_synthetic_curve_data()),
    make_synthetic_model_definitions(),
    scale = "linear"
  )
  expect_s3_class(p, "plotly")
})

test_that("make_general_plot renders a categorical (bar) plot", {
  curve <- make_synthetic_curve_data(
    x = c("Never", "Current"),
    rr = c(1, 2.5),
    x_axis_type = "Categorical"
  )
  p <- algorithm.viewer:::make_general_plot(
    list(curve),
    make_synthetic_model_definitions(),
    scale = "linear"
  )
  expect_s3_class(p, "plotly")
})

test_that("make_general_plot renders a flipped point plot with ylim override", {
  curve <- make_synthetic_curve_data(
    x = c("A", "B"),
    rr = c(0.8, 1.6),
    x_axis_type = "Categorical"
  )
  p <- algorithm.viewer:::make_general_plot(
    list(curve),
    make_synthetic_model_definitions(),
    scale = "log10",
    flip_coords = TRUE,
    plot_type = "point",
    ylim_override = c(0.1, 10)
  )
  expect_s3_class(p, "plotly")
})

test_that("make_general_plot combines curves from multiple models", {
  defs <- list(
    models = list(
      m1 = list(model_id = "m1", title = "Model One", model_color = "#112233"),
      m2 = list(model_id = "m2", title = "Model Two", model_color = "#445566")
    )
  )
  curves <- list(
    make_synthetic_curve_data(model_title = "Model One"),
    make_synthetic_curve_data(rr = c(1, 2, 3), model_title = "Model Two")
  )
  p <- algorithm.viewer:::make_general_plot(curves, defs, scale = "linear")
  expect_s3_class(p, "plotly")
})

test_that("make_general_plot logarithmic flag changes the y-axis label", {
  curve <- make_synthetic_curve_data()
  defs <- make_synthetic_model_definitions()
  p_log <- algorithm.viewer:::make_general_plot(list(curve), defs, "log10")
  p_lin <- algorithm.viewer:::make_general_plot(list(curve), defs, "linear")
  title_of <- function(p) {
    plotly::plotly_build(p)$x$layout$yaxis$title$text
  }
  expect_match(title_of(p_log), "Logarithmic", fixed = TRUE)
  expect_false(grepl("Logarithmic", title_of(p_lin), fixed = TRUE))
})

test_that("make_general_plot embeds a single subtitle in the plot title", {
  curve <- make_synthetic_curve_data()
  curve$subtitle <- "Interaction = Diabetes"
  p <- algorithm.viewer:::make_general_plot(
    list(curve),
    make_synthetic_model_definitions(),
    scale = "linear"
  )
  title_text <- plotly::plotly_build(p)$x$layout$title$text
  expect_true(grepl("Age", title_text, fixed = TRUE))
  expect_true(grepl("Interaction = Diabetes", title_text, fixed = TRUE))
})

test_that("make_general_plot embeds every line of a multi-line subtitle", {
  curve <- make_synthetic_curve_data()
  curve$subtitle <- list("Your value = 45", "Reference value = 60")
  p <- algorithm.viewer:::make_general_plot(
    list(curve),
    make_synthetic_model_definitions(),
    scale = "linear"
  )
  title_text <- plotly::plotly_build(p)$x$layout$title$text
  expect_true(grepl("Your value = 45", title_text, fixed = TRUE))
  expect_true(grepl("Reference value = 60", title_text, fixed = TRUE))
})

test_that("make_general_plot omits the subtitle markup when none is given", {
  curve <- make_synthetic_curve_data()
  p <- algorithm.viewer:::make_general_plot(
    list(curve),
    make_synthetic_model_definitions(),
    scale = "linear"
  )
  title_text <- plotly::plotly_build(p)$x$layout$title$text
  expect_true(grepl("Age", title_text, fixed = TRUE))
  # With no subtitle there is only the single title span (no <br> separator).
  expect_false(grepl("<br>", title_text, fixed = TRUE))
})

test_that("make_general_plot returns an error plot instead of throwing", {
  # Curve data with aes referring to a column that does not exist
  curve <- make_synthetic_curve_data()
  curve$aes_args$y <- rlang::sym("NoSuchColumn")
  p <- suppressMessages(algorithm.viewer:::make_general_plot(
    list(curve),
    make_synthetic_model_definitions(),
    scale = "linear"
  ))
  expect_s3_class(p, "plotly")
  expect_true(any(grepl("Error making plot", plot_texts(p), fixed = TRUE)))
})
