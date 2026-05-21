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
- Adjust baseline predictor values using interactive controls

## Project Structure

```text
algorithm-viewer/
├── R/                                       # R source files
│   ├── app_global.R                         # Global Shiny options (plot formatting, upload size)
│   ├── app_server.R                         # Main Shiny server function
│   ├── app_ui.R                             # Main Shiny UI function
│   ├── run_app.R                            # Package entry point (run_app())
│   ├── fct_cached_data.R                    # General-purpose reactive data cache
│   ├── fct_config.R                         # App configuration loading
│   ├── fct_model_definitions.R              # YAML config file parser
│   ├── fct_model_definitions_utils.R        # Model definition helper functions
│   ├── mod_categorical_radio_table.R        # Categorical radio button table module
│   ├── mod_continuous_slider_group.R        # Continuous slider group module
│   ├── mod_plot_or.R                        # Odds ratio plot module
│   ├── mod_plot_pr.R                        # Predicted risk plot module
│   ├── mod_plot_rr.R                        # Relative risk plot module
│   ├── mod_plot_rr_a_vs_b.R                 # A vs B relative risk plot module
│   ├── mod_predictor_controls.R             # Per-predictor control UI module
│   ├── mod_predictor_grouped_controls.R     # Multi-model predictor controls module
│   ├── utils_general_plot.R                 # Shared plot utilities
│   ├── utils_jsonschema.R                   # JSON Schema validation error formatting
│   ├── utils_make_error.R                   # Typed error condition constructor
│   ├── utils_make_string_values_unique.R    # Utility for deduplicating string values
│   ├── utils_path.R                         # File path helpers
│   └── utils_url.R                          # URL parsing and construction
├── inst/extdata/
│   ├── config.yaml                          # Default HTNPoRT app configuration
│   ├── help/
│   │   └── main.md                          # In-app help content
│   ├── models/
│   │   ├── htnport-full/                    # Full HTNPoRT model (YAML + CSV)
│   │   └── htnport-reduced/                 # Reduced HTNPoRT model (YAML + CSV)
│   ├── schema/
│   │   ├── algorithm.schema.json            # JSON Schema for algorithm YAML files
│   │   └── config.schema.json               # JSON Schema for config YAML files
│   └── www/                                 # Static web assets (CSS, favicons)
├── specs/
│   ├── CONFIG_SPECIFICATION.md              # Configuration file specification
│   ├── DEPLOYMENT.md                        # Deployment options planning document
│   └── INDIVIDUAL_VS_REFERENCE_SPEC.md      # Individual vs reference predictor spec
├── tests/
│   └── testthat/                            # Unit tests
├── app.R                                    # Shiny app wrapper (for deployment)
├── application.yml                          # ShinyProxy configuration
├── Dockerfile                               # Docker build instructions
├── docker-compose.yml                       # Docker Compose configuration
└── DESCRIPTION                              # R package metadata and dependencies
```

## Configuration

The app uses two types of YAML files.

### App Configuration

The app configuration file is passed to `run_app(config = ...)`. It declares
which algorithms are available and controls feature flags:

```yaml
# Algorithms available for selection or URL access
algorithms:
  my_algorithm:
    title: My Algorithm
    file: path/to/my-algorithm.yaml

# Algorithm to load on startup (matches a key in algorithms)
initial_algorithm_id: my_algorithm

# Feature flags (all optional)
allow_file_uploads: false       # allow users to upload their own algorithm
allow_algorithms_selection: true # show a dropdown to switch algorithms
allow_algorithm_in_url: true    # allow ?algorithm=<id> in the URL
```

### Algorithm YAML

Each algorithm is defined in its own YAML file, referenced from the app
configuration above. The algorithm YAML specifies model metadata, data file
paths, reference group defaults, and predictor allowable values.

See the [Algorithm Viewer Configuration
Specification](specs/CONFIG_SPECIFICATION.md) for the full algorithm YAML
format.

## Running the App

### Parameters

`run_app()` accepts the following parameters:

| Parameter | Default | Description |
| --------- | ------- | ----------- |
| `config` | `NULL` | Path to a YAML configuration file. When `NULL`, the built-in example HTNPoRT configuration (`inst/extdata/config.yaml`) is used. |
| `port` | `getOption("shiny.port")` | Port number for the Shiny server. |
| `host` | `getOption("shiny.host", "127.0.0.1")` | Host address for the Shiny server. |

Example — loading a custom app configuration:

```r
library(algorithm.viewer)
run_app(config = "path/to/my-config.yaml")
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
