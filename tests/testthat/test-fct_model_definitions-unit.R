# Unit tests for the private helpers in fct_model_definitions.R, using
# small synthetic inputs (no model pipeline required). Integration tests
# against the real fixture live in test-fct_model_definitions.R.

# ── .create_predictor_allowable_values ─────────────────────────────────────────

test_that("seq spec with from/to/by builds the sequence", {
  spec <- list(seq = list(from = 1, to = 2, by = 0.5))
  expect_equal(
    algorithm.viewer:::.create_predictor_allowable_values(spec),
    c(1, 1.5, 2)
  )
})

test_that("seq spec with length.out builds the sequence", {
  spec <- list(seq = list(from = 0, to = 1, length.out = 5))
  expect_equal(
    algorithm.viewer:::.create_predictor_allowable_values(spec),
    seq(0, 1, length.out = 5)
  )
})

test_that("an unnamed vector is returned unchanged", {
  expect_equal(
    algorithm.viewer:::.create_predictor_allowable_values(c(1, 2, 10)),
    c(1, 2, 10)
  )
})

test_that("an unnamed list of values is collapsed to a vector", {
  # A list of values, rather than a vector of them, is what an explicit list of
  # allowable values whose values are not all of one type is read as. The values
  # are used together from there on, so they have to come back as one vector.
  expect_equal(
    algorithm.viewer:::.create_predictor_allowable_values(list(18L, 20.5, 23L)),
    c(18, 20.5, 23)
  )
  expect_equal(
    algorithm.viewer:::.create_predictor_allowable_values(list(1L, "2")),
    c("1", "2")
  )
  expect_equal(
    algorithm.viewer:::.create_predictor_allowable_values(list(1L, 2L)),
    c(1L, 2L)
  )
})

test_that("a named non-seq list returns NULL", {
  expect_null(
    algorithm.viewer:::.create_predictor_allowable_values(
      list(other = list(a = 1))
    )
  )
})

# ── Notes annotation and _all_ merging ─────────────────────────────────────────

# Every value in an algorithm definition may be annotated with free-text notes,
# so .add_notes_and_values rewrites the whole definition into a uniform tree of
# nodes, each holding the notes for one value under the notes key and the value
# itself under the value key. The _all_ merge runs on that annotated tree, and
# .split_off_notes finally separates it back into plain values plus a parallel
# notes tree.
#
# The two keys are taken from the package rather than written out here, so these
# tests go on matching the definitions if either key is ever renamed.
notes_key <- algorithm.viewer:::.notes_key
value_key <- algorithm.viewer:::.value_key

# Build a value with its notes beside it, which is both how a definitions file
# may annotate a value and the shape .add_notes_and_values gives every node.
# Passing notes = NULL builds a wrapped value with no notes key at all.
ann <- function(value, notes = NA) {
  node <- list()
  # Assigning NULL to a list element is a no-op, so notes = NULL simply leaves
  # the notes key out
  node[[notes_key]] <- notes
  node[[value_key]] <- value
  node
}

# Build a value with its notes within it, as a notes key beside the value's own
# entries, which is the other way a definitions file may annotate a value
ann_within <- function(value, notes) {
  node <- list()
  node[[notes_key]] <- notes
  c(node, value)
}

test_that(".add_notes_and_values wraps every node and keeps existing notes", {
  raw <- list(
    meta = list(algorithm = "A"),
    models = list(m1 = list(
      reference_group = list(bmi = ann(20, "bmi note"))
    ))
  )
  result <- algorithm.viewer:::.add_notes_and_values(raw)

  # Unannotated values get a value node with an NA note
  expect_equal(result$meta[[value_key]]$algorithm[[value_key]], "A")
  expect_true(is.na(result$meta[[value_key]]$algorithm[[notes_key]]))

  # Values already annotated in the YAML keep their notes
  m1 <- result$models[[value_key]]$m1[[value_key]]
  bmi <- m1$reference_group[[value_key]]$bmi
  expect_equal(bmi[[value_key]], 20)
  expect_equal(bmi[[notes_key]], "bmi note")
})

test_that(".extract_notes_and_value accepts notes written in either form", {
  extract <- algorithm.viewer:::.extract_notes_and_value

  # Notes beside the value
  beside <- extract(ann(list(title = "M1"), "note"))
  expect_equal(beside$notes, "note")
  expect_equal(beside$value, list(title = "M1"))

  # Notes within the value: they annotate the node, so they are removed from
  # the values it holds
  within <- extract(ann_within(list(title = "M1"), "note"))
  expect_equal(within$notes, "note")
  expect_equal(within$value, list(title = "M1"))

  # Notes within a wrapped value
  wrapped <- extract(ann(ann_within(list(title = "M1"), "note"), notes = NULL))
  expect_equal(wrapped$notes, "note")
  expect_equal(wrapped$value, list(title = "M1"))

  # Both forms at once: the outer notes win, neither is left in the values
  both <- extract(ann(ann_within(list(title = "M1"), "inner"), "outer"))
  expect_equal(both$notes, "outer")
  expect_equal(both$value, list(title = "M1"))

  # No notes at all
  none <- extract(list(title = "M1"))
  expect_true(is.na(none$notes))
  expect_equal(none$value, list(title = "M1"))
})

test_that("a model-level _notes_ key annotates the model, not its values", {
  # Regression test: '_notes_' written beside a model's other keys used to be
  # treated as one of the model's own values, leaving a stray '_notes_' field
  # in the model alongside the correctly recorded note.
  raw <- list(models = list(m1 = ann_within(
    list(title = "M1", reference_group = list(bmi = 20)),
    "Model note"
  )))
  result <- algorithm.viewer:::.split_off_notes(
    algorithm.viewer:::.add_notes_and_values(raw)
  )

  expect_equal(names(result$models$m1), c("title", "reference_group"))
  expect_equal(result$models$m1$title, "M1")
  expect_equal(
    result$notes$models[[value_key]]$m1[[notes_key]],
    "Model note"
  )
})

test_that(
  ".recurse_copy_from_all_model_with_notes fills missing keys without overwriting",
  {
    info <- ann(list(a = ann(1), nested = ann(list(x = ann(1)))))
    all_info <- ann(list(
      a = ann(99),
      b = ann(2),
      nested = ann(list(x = ann(99), y = ann(3, "y note")))
    ))
    result <- algorithm.viewer:::.recurse_copy_from_all_model_with_notes(
      info,
      all_info
    )
    values <- result[[value_key]]
    expect_equal(values$a[[value_key]], 1) # not overwritten
    expect_equal(values$b[[value_key]], 2) # copied
    expect_equal(values$nested[[value_key]]$x[[value_key]], 1) # nested kept
    expect_equal(values$nested[[value_key]]$y[[value_key]], 3) # nested copied
    # A copied value brings its notes along with it
    expect_equal(values$nested[[value_key]]$y[[notes_key]], "y note")
  }
)

test_that(".copy_from_all_model_with_notes applies _all_ and removes it", {
  info <- list(models = ann(list(
    m1 = ann(list(title = ann("M1"))),
    `_all_` = ann(list(shared = ann("value"), title = ann("ignored")))
  )))
  result <- algorithm.viewer:::.copy_from_all_model_with_notes(info)
  models <- result$models[[value_key]]
  expect_false("_all_" %in% names(models))
  expect_equal(models$m1[[value_key]]$shared[[value_key]], "value")
  expect_equal(models$m1[[value_key]]$title[[value_key]], "M1") # untouched
})

test_that(".split_off_notes separates values from a parallel notes tree", {
  annotated <- algorithm.viewer:::.add_notes_and_values(list(
    models = list(m1 = list(
      title = "M1",
      reference_group = list(bmi = ann(20, "bmi note"))
    ))
  ))
  result <- algorithm.viewer:::.split_off_notes(annotated)

  # Values come back plain, with no annotation nodes left in them
  expect_equal(result$models$m1$title, "M1")
  expect_equal(result$models$m1$reference_group$bmi, 20)

  # Notes live in a tree keyed the same way as the values, so the notes for a
  # value are reached by the keys that lead to it
  notes <- result$notes$models[[value_key]]$m1
  expect_equal(
    notes[[value_key]]$reference_group[[value_key]]$bmi[[notes_key]],
    "bmi note"
  )
  expect_true(is.na(notes[[value_key]]$title[[notes_key]]))

  # ... which is what get_notes walks
  expect_equal(
    algorithm.viewer:::get_notes(
      result,
      list("models", "m1", "reference_group", "bmi")
    ),
    "bmi note"
  )
})

test_that(".split_off_notes collapses an annotated sequence back to a vector", {
  # A sequence with no notes is read as a vector, and one whose entries carry
  # notes as a list of the nodes those notes are written as. Both describe the
  # same allowable values, so both must come back as the same vector.
  annotated <- algorithm.viewer:::.add_notes_and_values(list(
    models = list(m1 = list(
      predictor_allowable_values = list(
        plain = c(1, 2),
        annotated = list(ann(1, "no diabetes"), ann(2, "diabetes")),
        part_annotated = list(1, ann(2, "diabetes")),
        mixed_types = list(ann("a", "a note"), 2)
      )
    ))
  ))
  result <- algorithm.viewer:::.split_off_notes(annotated)
  values <- result$models$m1$predictor_allowable_values

  expect_equal(values$annotated, values$plain)
  expect_equal(values$annotated, c(1, 2))
  expect_equal(values$part_annotated, c(1, 2))

  # Entries of more than one type are left as a list, which is also how
  # yaml::read_yaml reads such a sequence
  expect_equal(values$mixed_types, list("a", 2))

  # The notes survive the collapse, still reached by the position of the entry
  # they annotate
  keys <- list("models", "m1", "predictor_allowable_values", "annotated")
  expect_equal(algorithm.viewer:::get_notes(result, c(keys, 2)), "diabetes")
})

test_that("get_notes looks up the notes for a value by its keys", {
  info <- algorithm.viewer:::.split_off_notes(
    algorithm.viewer:::.add_notes_and_values(list(
      meta = list(algorithm = "A", version = "1.0.0"),
      models = list(m1 = ann_within(
        list(
          title = "M1",
          reference_group = list(bmi = ann(20, "bmi note"))
        ),
        "Model note"
      ))
    ))
  )
  get_notes <- algorithm.viewer:::get_notes

  # Values with notes of their own, at each level of the definitions
  expect_equal(get_notes(info, list("models", "m1")), "Model note")
  expect_equal(
    get_notes(info, list("models", "m1", "reference_group", "bmi")),
    "bmi note"
  )
  # Keys may also be given as a character vector
  expect_equal(get_notes(info, c("models", "m1")), "Model note")

  # A value with no notes of its own
  expect_true(is.null(get_notes(info, list("meta", "algorithm"))))
  expect_true(is.null(get_notes(info, list("models", "m1", "title"))))

  # Paths that do not lead to a value
  expect_null(get_notes(info, list("models", "no_such_model")))
  expect_null(get_notes(info, list("no_such_key")))
  expect_null(get_notes(info, list("models", "m1", "title", "deeper")))
})

test_that("get_notes looks up the notes for a value by its position", {
  info <- algorithm.viewer:::.split_off_notes(
    algorithm.viewer:::.add_notes_and_values(list(
      models = list(m1 = ann_within(
        list(predictor_allowable_values = list(
          diabx = list(ann(1, "no diabetes"), ann(2, "diabetes"))
        )),
        "Model note"
      ))
    ))
  )
  get_notes <- algorithm.viewer:::get_notes
  keys <- list("models", "m1", "predictor_allowable_values", "diabx")

  # The entries of a sequence have no names, so a position is the only way to
  # name the one whose notes are wanted
  expect_equal(get_notes(info, c(keys, 1)), "no diabetes")
  expect_equal(get_notes(info, c(keys, 2)), "diabetes")

  # Values that do have names can be reached by position too
  expect_equal(get_notes(info, list("models", 1)), "Model note")

  # Positions outside the value
  expect_null(get_notes(info, c(keys, 3)))
  expect_null(get_notes(info, c(keys, 0)))
})

# ── Title / index / root-dir assignment ────────────────────────────────────────

test_that(".make_model_titles_unique disambiguates duplicate titles", {
  info <- list(models = list(
    a = list(title = "Female"),
    b = list(title = "Female"),
    c = list(title = "Male")
  ))
  result <- algorithm.viewer:::.make_model_titles_unique(info)
  titles <- sapply(result$models, `[[`, "title")
  expect_equal(unname(titles), c("Female", "Female (2)", "Male"))
})

test_that(".add_model_indices_and_ids assigns sequential indices and IDs", {
  info <- list(models = list(x = list(), y = list()))
  result <- algorithm.viewer:::.add_model_indices_and_ids(info)
  expect_equal(result$models$x$model_index, 1)
  expect_equal(result$models$y$model_index, 2)
  expect_equal(result$models$x$model_id, "x")
  expect_equal(result$models$y$model_id, "y")
})

test_that(".assign_root_dir sets root_dir on every model", {
  info <- list(models = list(a = list(), b = list()))
  result <- algorithm.viewer:::.assign_root_dir(info, "/some/dir")
  expect_equal(result$models$a$root_dir, "/some/dir")
  expect_equal(result$models$b$root_dir, "/some/dir")
})

# ── .add_model_colors ──────────────────────────────────────────────────────────

test_that(".add_model_colors keeps valid user-specified colors", {
  info <- list(models = list(
    a = list(model_color = "#FF0000"),
    b = list(model_color = "red"),
    c = list(model_color = "#440154FF")
  ))
  result <- algorithm.viewer:::.add_model_colors(info)
  expect_equal(result$models$a$model_color, "#FF0000")
  expect_equal(result$models$b$model_color, "red")
  expect_equal(result$models$c$model_color, "#440154FF")
})

test_that(".add_model_colors auto-assigns distinct colors when unspecified", {
  info <- list(models = list(a = list(), b = list(), c = list()))
  result <- algorithm.viewer:::.add_model_colors(info)
  colors <- sapply(result$models, `[[`, "model_color")
  expect_length(unique(colors), 3)
})

test_that(".add_model_colors rejects invalid color strings", {
  for (bad in list("#12", "#GGGGGG", "not a color!", "#1234567")) {
    info <- list(models = list(a = list(model_color = bad)))
    expect_error(
      algorithm.viewer:::.add_model_colors(info),
      "Invalid model_color"
    )
  }
})

# ── .cleanup_reference_groups ──────────────────────────────────────────────────

test_that(".cleanup_reference_groups coerces values by variable type", {
  md <- make_synthetic_model_data()
  md$reference_group <- list(age = "40", smoking = 1)
  info <- list(models = list(m1 = md))
  result <- algorithm.viewer:::.cleanup_reference_groups(info)
  ref <- result$models$m1$reference_group
  expect_identical(ref$age, 40) # continuous -> double
  expect_identical(ref$smoking, "1") # categorical -> character
})

# ── Validation helpers ─────────────────────────────────────────────────────────

test_that(".validate_reference_groups passes for a consistent model", {
  info <- list(models = list(m1 = make_synthetic_model_data()))
  expect_no_error(algorithm.viewer:::.validate_reference_groups(info))
})

test_that(".validate_reference_groups errors for an unknown variable", {
  md <- make_synthetic_model_data()
  md$reference_group$mystery <- 1
  info <- list(models = list(m1 = md))
  expect_error(
    algorithm.viewer:::.validate_reference_groups(info),
    "not found in the variables file"
  )
})

test_that(".validate_reference_groups errors for an unrecognised type", {
  md <- make_synthetic_model_data()
  md$variables$variableType[md$variables$variable == "age"] <- "Continuos"
  info <- list(models = list(m1 = md))
  expect_error(
    algorithm.viewer:::.validate_reference_groups(info),
    "unrecognised variableType"
  )
})

test_that(".validate_predictor_consistency passes for matching models", {
  info <- list(models = list(
    m1 = make_synthetic_model_data(),
    m2 = make_synthetic_model_data()
  ))
  expect_no_error(algorithm.viewer:::.validate_predictor_consistency(info))
})

test_that(".validate_predictor_consistency errors on a variableType conflict", {
  m1 <- make_synthetic_model_data()
  m2 <- make_synthetic_model_data()
  m2$variables$variableType[m2$variables$variable == "age"] <- "Categorical"
  info <- list(models = list(m1 = m1, m2 = m2))
  expect_error(
    algorithm.viewer:::.validate_predictor_consistency(info),
    "conflicting variableType"
  )
})

test_that(".validate_predictor_consistency warns on a label conflict", {
  m1 <- make_synthetic_model_data()
  m2 <- make_synthetic_model_data()
  m2$variables$label[m2$variables$variable == "age"] <- "Age at baseline"
  info <- list(models = list(m1 = m1, m2 = m2))
  expect_warning(
    algorithm.viewer:::.validate_predictor_consistency(info),
    "different labels across models"
  )
})

test_that(".validate_predictor_consistency errors when variables are missing", {
  md <- make_synthetic_model_data()
  md$variables <- NULL
  info <- list(models = list(m1 = md))
  expect_error(
    algorithm.viewer:::.validate_predictor_consistency(info),
    "no variables loaded"
  )
})

test_that(".warn_nonuniform_continuous_predictors warns on uneven spacing", {
  md <- make_synthetic_model_data()
  md$predictor_allowable_values$age <- c(1, 2, 10)
  info <- list(models = list(m1 = md))
  expect_warning(
    algorithm.viewer:::.warn_nonuniform_continuous_predictors(info),
    "non-uniform allowable values"
  )
})

test_that(".warn_nonuniform_continuous_predictors is silent for uniform values", {
  info <- list(models = list(m1 = make_synthetic_model_data()))
  expect_no_warning(
    algorithm.viewer:::.warn_nonuniform_continuous_predictors(info)
  )
})

# ── .get_model_predictors ──────────────────────────────────────────────────────

test_that(".get_model_predictors returns only Predictor-role variables", {
  md <- make_synthetic_model_data()
  expect_setequal(
    algorithm.viewer:::.get_model_predictors(md),
    c("age", "smoking")
  )
})
