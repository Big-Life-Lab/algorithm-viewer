# Algorithm Viewer

An R Shiny application for visualizing health risk prediction algorithms, displaying odds ratio and predicted risk curves.

## About

This project is part of [Project Big Life](https://www.projectbiglife.ca/) at
The Ottawa Hospital. It provides an interactive interface for exploring and
understanding risk prediction models, allowing researchers and clinicians to
visualize how different predictors affect health outcomes.

The Algorithm Viewer enables users to examine the relationship between
predictor variables and risk outcomes through interactive visualizations,
making complex statistical models more accessible and interpretable.

## Features

### Odds Ratio Visualization

- Interactive plots showing how odds ratios change across predictor values
- Support for both continuous and categorical predictors
- Visualization of predictor interactions to understand combined effects

### Predicted Risk Curves

- View predicted risk as a function of predictor variables
- Compare risk curves across multiple models simultaneously

### Model Comparison

- Load and compare multiple models side-by-side
- Each model is displayed with a distinct color for easy differentiation
- Support for sex-stratified models (e.g., separate male and female models) or
  models stratified by other variables

### Reference Group Configuration

- Customize reference group values for each model
- Adjust baseline predictor values using interactive sliders

## Project Structure

```
algorithm-viewer/
├── app.R                 # Main application entry point
├── global.R              # Global configuration and model loading
├── ui.R                  # User interface definition
├── server.R              # Server-side logic and reactive elements
├── R/                    # R source files
│   ├── curves/           # Curve calculation functions
│   └── model_definitions/# Model definitions/config file parser
└── data/
    └── models/           # Prepackaged model config files (YAML + CSV exports)
```

## Configuration

Models are defined using YAML configuration files that specify:

- Algorithm metadata (name, version)
- Model definitions with titles and data file paths
- Reference group default values
- Predictor ranges for visualization

Example configuration structure:

```yaml
meta:
  algorithm: AlgorithmName
  version: 1.0.0

models:
  model_id:
    title: Model Title
    model_export: ./path-to-model-export.csv
    reference_group:
      predictor1: default_value
      predictor2: default_value
```

See the [Algorithm Viewer Configuration Specification](CONFIG_SPECIFICATION.md)
for details on configuration files.

## Running the App

### Locally

```r
# From the project directory
shiny::runApp()
```

### In RStudio

Open `app.R` and click the "Run App" button.

## Requirements

### R Version

- R >= 4.0

### Required Packages

- shiny
- shinyWidgets
- bslib
- plotly
- ggplot2
- dplyr
- rlang
- glue
- cli
- stringr

Install all dependencies:

```r
install.packages(c("shiny", "shinyWidgets", "bslib", "plotly", "ggplot2",
                   "dplyr", "rlang", "glue", "cli", "stringr"))

install.packages("devtools")
devtools::install_github("Big-Life-Lab/model-parameters-pipeline")
```

## License

This project is developed by Project Big Life at The Ottawa Hospital.
