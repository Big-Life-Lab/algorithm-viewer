# Validation tests for inst/extdata/schema/algorithm.schema.json, driven through
# read_and_validate_yaml() so they exercise the same path as
# read_model_definitions().

algorithm_schema <- function() {
  system.file("extdata/schema/algorithm.schema.json", package = "algorithm.viewer")
}

# Validate YAML text against the algorithm schema, returning TRUE when it
# passes and the (plain-text) error message when it does not.
validate_algorithm_yaml <- function(yaml_text) {
  yaml_file <- withr::local_tempfile(fileext = ".yaml", .local_envir = parent.frame())
  writeLines(yaml_text, yaml_file)
  result <- tryCatch(
    {
      algorithm.viewer:::read_and_validate_yaml(
        yaml_file,
        algorithm_schema(),
        error_html = FALSE
      )
      TRUE
    },
    yaml_validation_error = function(e) conditionMessage(e)
  )
  result
}

expect_algorithm_yaml_valid <- function(yaml_text) {
  result <- validate_algorithm_yaml(yaml_text)
  expect_true(isTRUE(result), info = if (!isTRUE(result)) result)
}

expect_algorithm_yaml_invalid <- function(yaml_text) {
  expect_false(isTRUE(validate_algorithm_yaml(yaml_text)))
}

# ── The shipped algorithm definitions ──────────────────────────────────────────

test_that("the bundled algorithm definitions validate against the schema", {
  skip_if(algorithm_schema() == "", "Algorithm schema not available")

  files <- list.files(
    system.file("extdata/models", package = "algorithm.viewer"),
    pattern = "\\.yaml$",
    recursive = TRUE,
    full.names = TRUE
  )
  expect_gt(length(files), 0)

  for (file in files) {
    expect_no_error(
      algorithm.viewer:::read_and_validate_yaml(
        file,
        algorithm_schema(),
        error_html = FALSE
      )
    )
  }
})

# ── Minimal definitions ────────────────────────────────────────────────────────

test_that("a minimal definition validates", {
  expect_algorithm_yaml_valid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv}
')
})

test_that("meta and models are required", {
  expect_algorithm_yaml_invalid('meta: {algorithm: HTNPoRT, version: "1.0.0"}')
  expect_algorithm_yaml_invalid('
models:
  female: {title: Female, model_export: ./f.csv}
')
})

test_that("unknown keys are rejected", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv}
modelz: 1
')
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv, refrence_group: {bmi: 1}}
')
})

test_that("a model requires a title and a model export", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {model_export: ./f.csv}
')
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female}
')
})

test_that("at least one model is required besides _all_ and notes", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models: {}
')
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  _all_: {reference_group: {bmi: 1}}
')
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  _notes_: Notes about the models
  _all_: {reference_group: {bmi: 1}}
')
})

test_that("_all_ only accepts shared configuration fields", {
  expect_algorithm_yaml_valid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv}
  _all_:
    model_color: red
    reference_group: {clc_age: 20}
    predictor_allowable_values: {clc_age: {seq: {from: 20, to: 80, by: 1}}}
')
  # title/model_export are per-model, not shared
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv}
  _all_: {title: Shared title}
')
})

# ── Notes written inside the value (a "_notes_" key beside the other keys) ─────

test_that("_notes_ is allowed at the model level", {
  expect_algorithm_yaml_valid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  male:
    _notes_: Male notes
    title: Male
    model_export: ./m.csv
    reference_group: {hwmdbmi: 13.8}
')
})

test_that("_notes_ is allowed on every object in the definition", {
  expect_algorithm_yaml_valid('
_notes_: Notes about the whole file
meta:
  _notes_: Notes about the meta block
  algorithm: HTNPoRT
  version: "1.0.0"
models:
  _notes_: Notes about the models
  _all_:
    _notes_: Notes about the shared configuration
    reference_group: {clc_age: 20}
  female:
    _notes_: Notes about the female model
    title: Female
    model_export: ./f.csv
    reference_group:
      _notes_: Notes about the reference group
      hwmdbmi: 14.9
    predictor_allowable_values:
      _notes_: Notes about the allowable values
      hwmdbmi:
        _notes_: Notes about the BMI values
        seq:
          _notes_: Notes about the sequence
          from: 14.9
          to: 49
          by: 0.1
')
})

test_that("notes must be free text, not a structure", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female:
    _notes_: {unexpected: structure}
    title: Female
    model_export: ./f.csv
')
})

test_that("_value_ may not be used as a model or predictor name", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv}
  _value_: {title: Nope, model_export: ./n.csv}
')
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female:
    title: Female
    model_export: ./f.csv
    reference_group: {_value_: 1, bmi: 2}
')
})

# ── Notes written beside the value (a "_value_"/"_notes_" pair) ────────────────

test_that("any value may be replaced by a _value_/_notes_ pair", {
  expect_algorithm_yaml_valid('
_notes_: Notes about the whole file
_value_:
  meta:
    _notes_: Notes about the meta block
    _value_:
      algorithm: {_notes_: Notes about the name, _value_: HTNPoRT}
      version: {_value_: "1.0.0"}
  models:
    _notes_: Notes about the models
    _value_:
      male:
        _notes_: Male notes
        _value_:
          title: {_notes_: Notes about the title, _value_: Male}
          model_export: {_value_: ./m.csv}
          model_color: {_notes_: Notes about the color, _value_: "#440154FF"}
          reference_group:
            _notes_: Notes about the reference group
            _value_:
              hwmdbmi: {_notes_: Notes about BMI, _value_: 13.8}
          predictor_allowable_values:
            _value_:
              hwmdbmi:
                _notes_: Notes about the BMI values
                _value_:
                  seq:
                    _notes_: Notes about the sequence
                    _value_:
                      from: {_notes_: Notes about the start, _value_: 13.8}
                      to: 49
                      by: {_value_: 0.1}
')
})

test_that("a _value_/_notes_ pair still validates the value it wraps", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  male:
    _notes_: Male notes
    _value_: {model_export: ./m.csv}
')
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: {_value_: {unexpected: structure}}, model_export: ./f.csv}
')
})

test_that("no other keys may sit beside a _value_/_notes_ pair", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female:
    title: Female
    model_export: ./f.csv
    reference_group:
      bmi: {_value_: 1, _notes_: Notes about BMI, unexpected: 2}
')
})

# ── Predictor allowable values ────────────────────────────────────────────────

test_that("allowable values accept seq specifications and explicit lists", {
  expect_algorithm_yaml_valid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female:
    title: Female
    model_export: ./f.csv
    predictor_allowable_values:
      clc_age: {seq: {from: 20, to: 80, length.out: 13}}
      hwmdbmi: {seq: {from: 14.9, to: 49, by: 0.1}}
      smoke: [1, 2, "3"]
      annotated_list: {_notes_: Notes, _value_: [1, 2]}
      # A one-element list is unboxed to a scalar before validation
      single: [5]
')
})

test_that("a seq specification requires from and to", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female:
    title: Female
    model_export: ./f.csv
    predictor_allowable_values: {hwmdbmi: {seq: {from: 1, by: 0.1}}}
')
})

test_that("a seq step of zero is rejected", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female:
    title: Female
    model_export: ./f.csv
    predictor_allowable_values: {hwmdbmi: {seq: {from: 1, to: 2, by: 0}}}
')
})

test_that("unknown seq parameters are rejected", {
  expect_algorithm_yaml_invalid('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female:
    title: Female
    model_export: ./f.csv
    predictor_allowable_values: {hwmdbmi: {seq: {from: 1, to: 2, step: 0.1}}}
')
})

# ── Model colors ──────────────────────────────────────────────────────────────

test_that("model_color accepts hex and named colors", {
  for (color in c("#440154FF", "#21908C", "#abcd", "#fff", "red", "steelblue")) {
    expect_algorithm_yaml_valid(sprintf('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv, model_color: "%s"}
', color))
  }
})

test_that("model_color rejects malformed colors", {
  for (color in c("#12345", "#GGGGGG", "rgb(1,2,3)", "light blue")) {
    expect_algorithm_yaml_invalid(sprintf('
meta: {algorithm: HTNPoRT, version: "1.0.0"}
models:
  female: {title: Female, model_export: ./f.csv, model_color: "%s"}
', color))
  }
})
