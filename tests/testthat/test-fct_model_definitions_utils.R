test_that("is_data_missing returns TRUE for NULL", {
  expect_true(algorithm.viewer:::is_data_missing(NULL))
})

test_that("is_data_missing returns TRUE for empty vector", {
  expect_true(algorithm.viewer:::is_data_missing(character(0)))
  expect_true(algorithm.viewer:::is_data_missing(c()))
})

test_that("is_data_missing returns TRUE for 'n/a' and 'N/A'", {
  expect_true(algorithm.viewer:::is_data_missing("n/a"))
  expect_true(algorithm.viewer:::is_data_missing("N/A"))
  expect_true(algorithm.viewer:::is_data_missing("N/a"))
})

test_that("is_data_missing returns TRUE for NA::b sentinel", {
  expect_true(algorithm.viewer:::is_data_missing("NA::b"))
  expect_true(algorithm.viewer:::is_data_missing("na::b"))
})

test_that("is_data_missing returns FALSE for normal values", {
  expect_false(algorithm.viewer:::is_data_missing("hello"))
  expect_false(algorithm.viewer:::is_data_missing(42))
  expect_false(algorithm.viewer:::is_data_missing(0))
  expect_false(algorithm.viewer:::is_data_missing(FALSE))
})

test_that("is_data_missing handles length > 1 character vectors", {
  # Any element matching makes the whole value missing
  expect_true(algorithm.viewer:::is_data_missing(c("hello", "n/a")))
  expect_true(algorithm.viewer:::is_data_missing(c("NA::b", "world")))
  # No element matching returns FALSE (not just the first element)
  expect_false(algorithm.viewer:::is_data_missing(c("hello", "world")))
  # First element non-missing, second missing — old code would return FALSE
  expect_true(algorithm.viewer:::is_data_missing(c("hello", "N/A")))
})

test_that("cleanup_string escapes HTML special characters", {
  result <- algorithm.viewer:::cleanup_string("<b>bold</b>")
  expect_equal(result, "&lt;b&gt;bold&lt;/b&gt;")
})

test_that("cleanup_string returns non-character values unchanged", {
  expect_equal(algorithm.viewer:::cleanup_string(42L), 42L)
  expect_equal(algorithm.viewer:::cleanup_string(TRUE), TRUE)
  expect_null(algorithm.viewer:::cleanup_string(NULL))
})

test_that("cleanup_string leaves safe strings unchanged", {
  expect_equal(algorithm.viewer:::cleanup_string("hello world"), "hello world")
})

# ── Variable metadata helpers (synthetic model, see helper-synthetic.R) ────────

test_that("get_variable_info retrieves a metadata column", {
  md <- make_synthetic_model_data()
  expect_equal(algorithm.viewer:::get_variable_info(md, "age", "label"), "Age")
  expect_equal(algorithm.viewer:::get_variable_info(md, "age", "units"), "years")
})

test_that("get_variable_info returns NULL for missing data", {
  md <- make_synthetic_model_data()
  # units for smoking is the "n/a" missing sentinel
  expect_null(algorithm.viewer:::get_variable_info(md, "smoking", "units"))
  # unknown variable matches no rows
  expect_null(algorithm.viewer:::get_variable_info(md, "nope", "label"))
})

test_that("get_variable_info escapes HTML when requested", {
  md <- make_synthetic_model_data()
  md$variables$label[1] <- "<b>Age</b>"
  expect_equal(
    algorithm.viewer:::get_variable_info(md, "age", "label", escape_html = TRUE),
    "&lt;b&gt;Age&lt;/b&gt;"
  )
})

test_that("get_variable_label_and_units appends units when present", {
  md <- make_synthetic_model_data()
  expect_equal(
    as.character(algorithm.viewer:::get_variable_label_and_units(md, "age")),
    "Age (years)"
  )
  expect_equal(
    as.character(
      algorithm.viewer:::get_variable_label_and_units(md, "smoking")
    ),
    "Smoking status"
  )
})

test_that("is_variable_categorical / is_variable_continuous check types", {
  md <- make_synthetic_model_data()
  expect_true(algorithm.viewer:::is_variable_categorical(md, "smoking"))
  expect_false(algorithm.viewer:::is_variable_categorical(md, "age"))
  expect_true(algorithm.viewer:::is_variable_continuous(md, "age"))
  expect_false(algorithm.viewer:::is_variable_continuous(md, "smoking"))
  expect_false(algorithm.viewer:::is_variable_continuous(md, "nope"))
})

test_that("get_variable_labels_to_values maps labels and drops NA::b rows", {
  md <- make_synthetic_model_data()
  mapping <- algorithm.viewer:::get_variable_labels_to_values(md, "smoking")
  expect_equal(mapping, list(Never = "1", Current = "2"))
})

test_that("get_variable_values_to_labels is the inverse mapping", {
  md <- make_synthetic_model_data()
  mapping <- algorithm.viewer:::get_variable_values_to_labels(md, "smoking")
  expect_equal(mapping, list("1" = "Never", "2" = "Current"))
})

test_that("get_variable_value_from_label converts single and multiple labels", {
  md <- make_synthetic_model_data()
  expect_equal(
    algorithm.viewer:::get_variable_value_from_label(md, "smoking", "Current"),
    "2"
  )
  expect_equal(
    algorithm.viewer:::get_variable_value_from_label(
      md, "smoking", c("Current", "Never")
    ),
    c("2", "1")
  )
})

test_that("get_variable_label_from_value converts single and multiple values", {
  md <- make_synthetic_model_data()
  expect_equal(
    algorithm.viewer:::get_variable_label_from_value(md, "smoking", "1"),
    "Never"
  )
  expect_equal(
    algorithm.viewer:::get_variable_label_from_value(
      md, "smoking", c("2", "1")
    ),
    c("Current", "Never")
  )
})

test_that("get_predictor_allowable_values returns the stored values", {
  md <- make_synthetic_model_data()
  expect_equal(
    algorithm.viewer:::get_predictor_allowable_values(md, "smoking"),
    c("1", "2")
  )
  expect_null(algorithm.viewer:::get_predictor_allowable_values(md, "nope"))
})

test_that("convert_df_variable_to_label converts categorical columns", {
  md <- make_synthetic_model_data()
  df <- data.frame(smoking = c("2", "1", "2"), stringsAsFactors = FALSE)
  result <- algorithm.viewer:::convert_df_variable_to_label(
    df, md, "smoking", "smoking"
  )
  expect_equal(result$smoking, c("Current", "Never", "Current"))
})

test_that("convert_df_variable_to_label leaves continuous columns unchanged", {
  md <- make_synthetic_model_data()
  df <- data.frame(age = c(20, 30))
  result <- algorithm.viewer:::convert_df_variable_to_label(
    df, md, "age", "age"
  )
  expect_equal(result$age, c(20, 30))
})

# ── Model-list helpers ─────────────────────────────────────────────────────────

make_two_models <- function() {
  m1 <- make_synthetic_model_data()
  m2 <- make_synthetic_model_data()
  m2$model_id <- "m2"
  m2$title <- "Model Two"
  m2$model_color <- "#445566"
  list(m1 = m1, m2 = m2)
}

test_that("get_all_models_field extracts the field from every model", {
  models <- make_two_models()
  expect_equal(
    algorithm.viewer:::get_all_models_field(models, "title"),
    c("Model One", "Model Two")
  )
})

test_that("get_all_models_field returns NA for models missing the field", {
  # Regression test: append(fields, NULL) was a no-op that silently shrank
  # the result, misaligning it with the models list.
  models <- make_two_models()
  models$m1$model_color <- NULL
  fields <- algorithm.viewer:::get_all_models_field(models, "model_color")
  expect_length(fields, 2)
  expect_true(is.na(fields[[1]]))
  expect_equal(fields[[2]], "#445566")
})

test_that("get_all_models_field escapes HTML when requested", {
  models <- make_two_models()
  models$m1$title <- "<i>One</i>"
  fields <- algorithm.viewer:::get_all_models_field(
    models, "title",
    escape_html = TRUE
  )
  expect_equal(fields[[1]], "&lt;i&gt;One&lt;/i&gt;")
})

test_that("get_model_colors returns colors named by title", {
  colors <- algorithm.viewer:::get_model_colors(make_two_models())
  expect_equal(
    colors,
    c("Model One" = "#112233", "Model Two" = "#445566")
  )
})

test_that("get_model_colors falls back to transparent for missing colors", {
  models <- make_two_models()
  models$m1$model_color <- NULL
  colors <- algorithm.viewer:::get_model_colors(models)
  expect_equal(unname(colors[1]), "#ffffff00")
  expect_equal(unname(colors[2]), "#445566")
})

test_that("get_model_colors supports unnamed output", {
  colors <- algorithm.viewer:::get_model_colors(
    make_two_models(),
    names_field = NULL
  )
  expect_null(names(colors))
})

test_that("get_model_ids returns the list names", {
  expect_equal(
    algorithm.viewer:::get_model_ids(make_two_models()),
    c("m1", "m2")
  )
})

test_that("get_model_choices maps titles to model IDs", {
  choices <- algorithm.viewer:::get_model_choices(make_two_models())
  expect_equal(
    choices,
    c("Model One" = "m1", "Model Two" = "m2")
  )
})

test_that("get_model_titles returns plain titles by default", {
  titles <- algorithm.viewer:::get_model_titles(make_two_models())
  expect_equal(titles, list("Model One", "Model Two"))
})

test_that("get_model_titles escapes HTML and adds color boxes on request", {
  models <- make_two_models()
  models$m1$title <- "<i>One</i>"
  titles <- algorithm.viewer:::get_model_titles(
    models,
    include_model_colors = TRUE, escape_html = TRUE
  )
  expect_s3_class(titles[[1]], "html")
  expect_match(as.character(titles[[1]]), "&lt;i&gt;One&lt;/i&gt;", fixed = TRUE)
  expect_match(as.character(titles[[1]]), "background-color: #112233", fixed = TRUE)
})

test_that("add_model_color places the color box before or after the label", {
  md <- make_synthetic_model_data()
  after <- algorithm.viewer:::add_model_color(md, "Label", "12px", "12px")
  before <- algorithm.viewer:::add_model_color(
    md, "Label", "12px", "12px",
    after = FALSE
  )
  expect_match(after, "^Label <span")
  expect_match(before, "</span> Label$")
  expect_match(after, "background-color: #112233", fixed = TRUE)
})

test_that("gather_predictor_choices collects unique predictors across models", {
  models <- make_two_models()
  choices <- algorithm.viewer:::gather_predictor_choices(models)
  expect_equal(
    choices,
    c("Age" = "age", "Smoking status" = "smoking")
  )
})

test_that("gather_predictor_choices keeps the first model's label", {
  models <- make_two_models()
  models$m2$variables$label[models$m2$variables$variable == "age"] <-
    "Age at baseline"
  choices <- algorithm.viewer:::gather_predictor_choices(models)
  expect_equal(names(choices)[choices == "age"], "Age")
})

# ── combine_models ───────────────────────────────────────────────────────────

# Two models sharing age + smoking, where the second model additionally has a
# "bmi" predictor that the first model lacks, and disagrees on the shared
# predictors' reference values / allowable values.
make_heterogeneous_models <- function() {
  m1 <- make_synthetic_model_data()

  m2 <- make_synthetic_model_data()
  m2$model_id <- "m2"
  m2$title <- "Model Two"
  m2$variables <- rbind(
    m2$variables,
    data.frame(
      variable = "bmi", label = "BMI", units = "kg/m^2",
      variableType = "Continuous", role = "Predictor",
      stringsAsFactors = FALSE
    )
  )
  m2$variable_details <- rbind(
    m2$variable_details,
    data.frame(
      variable = "bmi", recStart = "[15, 40]", recEnd = "copy",
      catLabel = "", stringsAsFactors = FALSE
    )
  )
  m2$predictor_allowable_values$bmi <- seq(15, 40)
  m2$predictor_allowable_values$age <- seq(30, 90) # disagrees with m1
  m2$reference_group <- list(age = 50, smoking = "2", bmi = 22)

  list(m1 = m1, m2 = m2)
}

test_that("combine_models returns NULL for an empty model list", {
  expect_null(algorithm.viewer:::combine_models(list()))
})

test_that("combine_models is a no-op for a single model", {
  m1 <- make_synthetic_model_data()
  combined <- algorithm.viewer:::combine_models(list(m1))
  expect_equal(combined$reference_group, m1$reference_group)
  expect_equal(
    combined$predictor_allowable_values, m1$predictor_allowable_values
  )
})

test_that("combine_models unions predictors across models", {
  combined <- algorithm.viewer:::combine_models(make_heterogeneous_models())
  # bmi (unique to the second model) must now be present, in first-seen order
  # after the first model's predictors.
  expect_equal(names(combined$reference_group), c("age", "smoking", "bmi"))
  expect_equal(combined$reference_group$bmi, 22)
  expect_equal(combined$predictor_allowable_values$bmi, seq(15, 40))
  # bmi is rendered correctly because its metadata came along too.
  expect_true(algorithm.viewer:::is_variable_continuous(combined, "bmi"))
})

test_that("combine_models takes shared predictors from the first model", {
  combined <- algorithm.viewer:::combine_models(make_heterogeneous_models())
  # The first model wins for predictors present in more than one model.
  expect_equal(combined$reference_group$age, 40)
  expect_equal(combined$reference_group$smoking, "1")
  expect_equal(combined$predictor_allowable_values$age, seq(20, 100))
})
