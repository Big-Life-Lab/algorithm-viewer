# Shared test fixtures loaded once per test session.
#
# .test_md is NULL when model loading fails (e.g., missing package data or
# unavailable pipeline). Tests that depend on these use
# skip_if(is.null(.test_md), ...) to skip gracefully.

.test_reduced_yaml <- system.file(
  "extdata/models/htnport-reduced/htnport-reduced.yaml",
  package = "algorithm.viewer"
)

.test_md <- tryCatch(
  algorithm.viewer:::read_model_definitions(.test_reduced_yaml),
  error = function(e) NULL
)
