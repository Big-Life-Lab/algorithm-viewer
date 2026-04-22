# Algorithm Viewer

<!-- badges: start -->
[![R-CMD-check.yaml](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R Shiny application for visualizing health risk prediction algorithms,
displaying various interactive plots including odds ratio, predicted risk
curves, and relative risk.

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

### Multiple Plot Types

- Interactive plots for odds ratio, relative risk, and predicted risk.
- Support for both continuous and categorical predictors
- Visualization of predictor interactions to understand combined effects

### Model Comparison

- Load and compare multiple models side-by-side
- Each model is displayed with a distinct color for easy differentiation
- Support for sex-stratified models (e.g., separate male and female models) or
  models stratified by other variables

### Reference Group Configuration

- Customize reference group values for each model
- Adjust baseline predictor values using interactive sliders

## Project Structure

```text
algorithm-viewer/
├── R/                              # R source files
│   ├── app.R                       # Main application entry point
│   ├── ui.R                        # User interface definition
│   ├── server.R                    # Server-side logic and reactive elements
│   ├── tab_panels.R                # UI tab panel definitions
│   ├── model_definitions.R         # Config file parser
│   ├── model_definitions_utils.R   # Model definition helper functions
│   ├── predictor_controls.R        # Predictor control UI module
│   ├── predictor_controls_manager.R# Manages multiple predictor control modules
│   ├── plot_or.R                   # Odds ratio plot module
│   ├── plot_pr.R                   # Predicted risk plot module
│   ├── plot_rr.R                   # Relative risk plot module
│   ├── plot_rr_exposed_vs_unexposed.R # Exposed vs unexposed RR plot module
│   ├── general_plot.R              # Shared plot utilities
│   ├── cached_data.R               # General-purpose reactive data cache
│   ├── config.R                    # App configuration loading
│   ├── function_parser.R           # Parses R function expressions from config
│   ├── make_string_values_unique.R # Utility for deduplicating string values
│   ├── path_utils.R                # File path helpers
│   └── url.R                       # URL parsing and construction
└── data/
    └── models/           # Prepackaged model config files (YAML + CSV exports)
```

## Configuration

Algorithms and models are defined using YAML configuration files that specify:

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
    predictor_allowable_values:
      predictor1: allowable_values
      predictor2: allowable_values
```

See the [Algorithm Viewer Configuration
Specification](specs/CONFIG_SPECIFICATION.md) for details on configuration
files.

## Running the App

### Parameters

`run_app()` accepts the following parameters:

| Parameter | Default | Description |
| --------- | ------- | ----------- |
| `config` | `NULL` | Path to a YAML configuration file. When `NULL`, the built-in example HTNPoRT configuration (`inst/extdata/config.yaml`) is used. |
| `port` | `getOption("shiny.port")` | Port number for the Shiny server. |
| `host` | `getOption("shiny.host", "127.0.0.1")` | Host address for the Shiny server. |

Example — loading a custom algorithm configuration:

```r
library(algorithm.viewer)
run_app(config = "path/to/my-algorithm/config.yaml")
```

Example — running in Docker or hosting on your local network:

```r
run_app(host = "0.0.0.0", port = 3838)
```

### Locally

Install the package from GitHub:

```r
remotes::install_github("Big-Life-Lab/algorithm-viewer")
```

Or install from a local copy of the repository:

```r
remotes::install_local("path/to/algorithm-viewer")
```

Then run the app:

```r
library(algorithm.viewer)
run_app()
```

#### Development Mode

To run without installing (e.g. while actively editing source files), make sure
your working directory is at the root of the local repository, then load all
source files and call `run_app()` directly:

```r
devtools::load_all()
run_app()
```

### With Docker

Build and run using Docker Compose:

```bash
docker compose up --build
```

Or build and run manually:

```bash
docker build -t algorithm-viewer .
docker run -p 3838:3838 algorithm-viewer
```

Then open `http://localhost:3838` in your browser.

### With ShinyProxy

ShinyProxy can serve the app as a multi-user deployment. A sample
`application.yml` configuration file is included in the package root.

Ensure the Docker container has been built as described above, then from the
package root where `application.yml` is located, launch ShinyProxy with:

```bash
java -jar shinyproxy-x.y.z.jar
```

Then access the site at `http://localhost:8080`.

This requires a Java runtime environment and Docker to be running. For details
on installing these refer to the [ShinyProxy Getting Started
Guide](https://www.shinyproxy.io/documentation/getting-started/).

## Deployment

The [Deployment Specification](specs/DEPLOYMENT.md) is a planning document that
discusses various deployment options we may want to implement in the future,
including a public web application, a hosted algorithm showcase for sharing
models via URL, and a local development tool for scientists building
algorithms.

## Requirements

### R Version

- R >= 4.1

### Required Packages

See the [DESCRIPTION](DESCRIPTION) file for a list of required packages.

Install all dependencies:

```r
install.packages("remotes")
remotes::install_deps(".")
```

## License

This project is developed by Project Big Life at The Ottawa Hospital.
