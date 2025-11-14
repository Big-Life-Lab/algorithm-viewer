# Test script for OR calculation functions

# Load required libraries
library(dplyr)

# Load OR calculation functions
source("or_calculations.R")

# Load model data
load_model_data <- function(sex) {
  base_path <- file.path("logistic-model-export", sex)
  model_name <- paste0("HTNPoRT-", sex)

  list(
    variables = read.csv(file.path(base_path, paste0(model_name, "-variables.csv")),
                        stringsAsFactors = FALSE),
    variable_details = read.csv(file.path(base_path, paste0(model_name, "-variable-details.csv")),
                                stringsAsFactors = FALSE),
    logistic = read.csv(file.path(base_path, paste0(model_name, "-logistic.csv")),
                       stringsAsFactors = FALSE),
    rcs = read.csv(file.path(base_path, paste0(model_name, "-rcs.csv")),
                  stringsAsFactors = FALSE),
    center = read.csv(file.path(base_path, paste0(model_name, "-center.csv")),
                     stringsAsFactors = FALSE),
    interactions = read.csv(file.path(base_path, paste0(model_name, "-interactions.csv")),
                           stringsAsFactors = FALSE)
  )
}

cat("=== Testing OR Calculation Functions ===\n\n")

# Load male model
cat("Loading male model data...\n")
male_data <- load_model_data("male")
cat("✓ Male model loaded\n\n")

# Test 1: Calculate RCS for age
cat("Test 1: Calculate RCS for age = 40\n")
age_knots <- c(24, 40, 57, 74)
age_rcs <- calculate_rcs(40, age_knots)
cat("Age RCS terms:", age_rcs, "\n\n")

# Test 2: Calculate OR curve for diabetes (categorical predictor)
cat("Test 2: Calculate OR curve for diabetes\n")
tryCatch({
  diabx_or <- get_categorical_or_curves("diabx", male_data, age_range = c(30, 40, 50, 60, 70))
  cat("✓ Diabetes OR calculated successfully\n")
  print(diabx_or)
  cat("\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n\n")
})

# Test 3: Calculate OR curve for BMI (continuous predictor with RCS)
cat("Test 3: Calculate OR curve for BMI (1-unit increase)\n")
tryCatch({
  bmi_or <- calculate_or_curve("hwmdbmi", 1, male_data, age_range = c(30, 40, 50, 60, 70))
  cat("✓ BMI OR calculated successfully\n")
  print(bmi_or)
  cat("\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n\n")
})

# Test 4: Calculate OR curve for family history (categorical)
cat("Test 4: Calculate OR curve for family history\n")
tryCatch({
  fmh_or <- get_categorical_or_curves("fmh_15", male_data, age_range = c(30, 40, 50, 60, 70))
  cat("✓ Family history OR calculated successfully\n")
  print(fmh_or)
  cat("\n")
}, error = function(e) {
  cat("✗ Error:", e$message, "\n\n")
})

cat("=== Testing Complete ===\n")
