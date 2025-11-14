# Test script to verify data loading works correctly

# Load required libraries
required_packages <- c("shiny", "ggplot2", "plotly", "dplyr", "tidyr")

cat("Checking for required packages...\n")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Package '%s' is not installed. Installing...\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  } else {
    cat(sprintf("✓ %s is installed\n", pkg))
  }
}

cat("\n=== Testing Data Loading ===\n")

# Test loading male model data
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

# Test male model
cat("\nLoading male model data...\n")
male_data <- load_model_data("male")
cat(sprintf("✓ Male model loaded: %d predictors, %d coefficients\n",
            nrow(male_data$variables), nrow(male_data$logistic)))

# Test female model
cat("Loading female model data...\n")
female_data <- load_model_data("female")
cat(sprintf("✓ Female model loaded: %d predictors, %d coefficients\n",
            nrow(female_data$variables), nrow(female_data$logistic)))

cat("\n=== Data Loading Test Complete ===\n")
cat("All data files loaded successfully!\n")
