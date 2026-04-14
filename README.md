# Algorithm Viewer

An R Shiny application for visualizing health risk prediction algorithms,
displaying odds ratio and predicted risk curves.

## About

This project is part of [Project Big Life](https://www.projectbiglife.ca/) at
The Ottawa Hospital. It provides an interactive interface for exploring and
understanding risk prediction models, allowing researchers and clinicians to
visualize how different predictors affect health outcomes.

The Algorithm Viewer enables users to examine the relationship between
predictor variables and risk outcomes through interactive visualizations,
making complex statistical models more accessible and interpretable.

## Algorithm vs Model

In the context of the Algorithm Viewer, an algorithm is a family of models that
each perform a similar prediction, using the same set of inputs. A model is an
instance of the algorithm that can be evaluated. For example, the Hypertension
Population Risk Tool (HTNPoRT) is an algorithm for predicting risk of
hypertension. Within HTNPoRT there are two models: one model to perform
predictions for female individuals, and one for performing predictions for male
individuals.

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
│   ├── plots/            # Curve calculation functions
│   ├── model_definitions/# Model definitions/config file parser
│   ├── modules/          # R Shiny modules
│   └── utils/            # Utility functions
└── data/
    └── models/           # Prepackaged model config files (YAML + CSV exports)
```

## Configuration

Models are defined using YAML configuration files that specify:

- Algorithm metadata (name, version)
- Model definitions with titles and data file paths
- Reference group default values
- Predictor allowable values (eg. ranges for continuous variables, categories
  for categorical variables)

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

See the [Algorithm Viewer Configuration
Specification](docs/specs/CONFIG_SPECIFICATION.md) for details on configuration
files.

## Running the App

### Locally

```r
# From the project directory
shiny::runApp()
```

### In RStudio or VS Code

Open `app.R` and click the "Run App"/"Run Shiny App" button.

### With Docker

Build and run using Docker Compose:

```bash
docker compose up --build
```

Or build and run manually:

```bash
docker build --platform linux/amd64 -t algorithm-viewer .
docker run -p 3838:3838 -v ./data:/srv/shiny-server/algorithm-viewer/data algorithm-viewer
```

Then open `http://localhost:3838` in your browser.

The volume mount (`-v`) allows the container to use algorithm files from your
local `data/` directory. To use different algorithm files, place them in
`data/models/` and update `data/config.yaml` accordingly. Rerun the `docker run`
command to use the new algorithm files.

## Deployment

The [Deployment Specification](docs/specs/DEPLOYMENT.md) is a planning document
that discusses various deployment options we may want to implement in the
future, including a public web application, a hosted algorithm showcase for
sharing models via URL, and a local development tool for scientists building
algorithms.

## Requirements

### R Version

- R >= 4.0

### Required Packages

See the [DESCRIPTION](DESCRIPTION) file for a list of required packages.

Install all dependencies:

```r
install.packages("remotes")
remotes::install_deps(".")
```

## License

This project is developed by Project Big Life at The Ottawa Hospital.
