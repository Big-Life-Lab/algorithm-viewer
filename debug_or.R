# Debug script to understand OR calculation issue

library(dplyr)
source("or_calculations.R")

# Load model data
load_model_data <- function(sex) {
  base_path <- file.path("logistic-model-export", sex)
  model_name <- paste0("HTNPoRT-", sex)
  list(
    variables = read.csv(file.path(base_path, paste0(model_name, "-variables.csv")), stringsAsFactors = FALSE),
    variable_details = read.csv(file.path(base_path, paste0(model_name, "-variable-details.csv")), stringsAsFactors = FALSE),
    logistic = read.csv(file.path(base_path, paste0(model_name, "-logistic.csv")), stringsAsFactors = FALSE),
    rcs = read.csv(file.path(base_path, paste0(model_name, "-rcs.csv")), stringsAsFactors = FALSE),
    center = read.csv(file.path(base_path, paste0(model_name, "-center.csv")), stringsAsFactors = FALSE),
    interactions = read.csv(file.path(base_path, paste0(model_name, "-interactions.csv")), stringsAsFactors = FALSE)
  )
}

male_data <- load_model_data("male")

# Test single age for diabetes
cat("=== Debug OR Calculation for Diabetes at Age 40 ===\n\n")

# Get coefficients
coefficients <- setNames(male_data$logistic$coefficient, male_data$logistic$variable)
cat("Coefficients:\n")
print(coefficients[1:10])

# Calculate age RCS for age = 40
age_knots <- c(24, 40, 57, 74)
age_rcs <- calculate_rcs(40, age_knots)[1, ]
cat("\nAge RCS for 40:", age_rcs, "\n")

# Get centering values for age
age_rcs_vars <- c("clc_age_rcs_1", "clc_age_rcs_2", "clc_age_rcs_3")
age_center_vals <- male_data$center[male_data$center$origVariable %in% age_rcs_vars, ]
cat("\nAge centering values:\n")
print(age_center_vals[, c("origVariable", "centerValue")])

# Center age RCS
age_center_vals <- setNames(age_center_vals$centerValue, age_center_vals$origVariable)
age_rcs_list <- list()
for (j in seq_along(age_rcs_vars)) {
  centered_value <- age_rcs[j] - age_center_vals[[age_rcs_vars[j]]]
  age_rcs_list[[paste0(age_rcs_vars[j], "_C")]] <- centered_value
}

cat("\nCentered age RCS list:\n")
print(str(age_rcs_list))

# Now add diabetes predictor
pred_var_centered <- "diabx_1_C"
pred_center <- male_data$center[male_data$center$centeredVariable == pred_var_centered, "centerValue"]
cat("\nDiabetes centering value:", pred_center, "\n")

exposed_value <- 1 - pred_center
cat("Exposed value (1 - center):", exposed_value, "\n")

pred_list_exposed <- c(age_rcs_list, setNames(list(exposed_value), pred_var_centered))

cat("\nExposed predictor list:\n")
print(str(pred_list_exposed))

# Check for matching coefficients
cat("\nChecking coefficient matches:\n")
for (var_name in names(pred_list_exposed)) {
  has_coeff <- var_name %in% names(coefficients)
  cat(sprintf("%s: %s (value = %f)\n", var_name, ifelse(has_coeff, "YES", "NO"), pred_list_exposed[[var_name]]))
}

# Try to calculate log-odds
cat("\n=== Calculating log-odds ===\n")
log_odds <- coefficients["Intercept"]
cat("Starting with intercept:", log_odds, "\n")

for (var_name in names(pred_list_exposed)) {
  if (var_name %in% names(coefficients)) {
    contribution <- coefficients[var_name] * pred_list_exposed[[var_name]]
    log_odds <- log_odds + contribution
    cat(sprintf("  + %s: coef=%.4f * value=%.4f = %.4f (log_odds now = %.4f)\n",
                var_name, coefficients[var_name], pred_list_exposed[[var_name]],
                contribution, log_odds))
  }
}

cat("\nFinal log-odds exposed:", log_odds, "\n")

# Now try unexposed
pred_list_unexposed <- age_rcs_list
log_odds_unexp <- calculate_log_odds(coefficients, pred_list_unexposed)
cat("Log-odds unexposed:", log_odds_unexp, "\n")

cat("\nOR = exp(", log_odds, " - ", log_odds_unexp, ") = ", exp(log_odds - log_odds_unexp), "\n")
