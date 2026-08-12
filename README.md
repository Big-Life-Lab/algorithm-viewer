# Algorithm Viewer

<!-- badges: start -->
[![R-CMD-check.yaml](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/R-CMD-check.yaml)
[![Docker Image](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/docker-publish.yaml/badge.svg)](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/docker-publish.yaml)
[![pkgdown](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

An R Shiny application for visualizing health risk prediction algorithms,
displaying various interactive plots including odds ratio, predicted risk
curves, and relative risk. The app plots algorithms that conform to the [Model
Parameters](https://github.com/Big-Life-Lab/model-parameters/) format
developed by Big Life Lab.

## Quick Start

**Requirements:** [R](https://www.r-project.org/) >= 4.1. All R package
dependencies are installed automatically in the steps below.

Install the package from GitHub and run the app:

```r
install.packages("remotes")
remotes::install_github("Big-Life-Lab/algorithm-viewer")

library(algorithm.viewer)
run_app()
```

The app opens in your browser, preloaded with the example [Hypertension
Population Risk Tool (HTNPoRT)](https://github.com/Big-Life-Lab/htnport)
algorithm.

That's all you need for a standard installation. The [Running the
App](#running-the-app) section covers everything else: `run_app()` options,
custom configurations, development mode, and running with Docker or
ShinyProxy.

## Documentation

Full documentation lives at
<https://big-life-lab.github.io/algorithm-viewer/>, organized following the
[Divio](https://docs.divio.com/documentation-system/) framework:

- **Tutorials** — [Installing](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-installing.html),
  [Running](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-running.html),
  and [Viewing the built-in HTNPoRT
  algorithm](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-htnport.html)
  (including how to interpret every plot).
- **How-to guides** — [View your own
  algorithms](https://big-life-lab.github.io/algorithm-viewer/articles/howto-view-your-algorithms.html),
  [add Viewer configs to a Model Parameters
  repo](https://big-life-lab.github.io/algorithm-viewer/articles/howto-add-viewer-configs.html),
  run with [Docker](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.html) /
  [ShinyProxy](https://big-life-lab.github.io/algorithm-viewer/articles/howto-shinyproxy.html) /
  [Watchtower](https://big-life-lab.github.io/algorithm-viewer/articles/howto-watchtower.html).
- **Explanation** — [Why the Algorithm
  Viewer](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-why.html),
  [using it in
  publications](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-publications.html),
  and [what Model Parameters
  is](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-model-parameters.html).
- **Reference** —
  [application configuration](https://big-life-lab.github.io/algorithm-viewer/articles/reference-app-configuration.html),
  [algorithm configuration](https://big-life-lab.github.io/algorithm-viewer/articles/reference-algorithm-configuration.html),
  and the
  [R API](https://big-life-lab.github.io/algorithm-viewer/reference/index.html).

## About

This project is part of [Project Big Life](https://www.projectbiglife.ca/) at
The Ottawa Hospital. It provides an interactive interface for exploring and
understanding risk prediction models, allowing researchers and clinicians to
visualize how different predictors affect health outcomes.

The Algorithm Viewer enables users to examine the relationship between
predictor variables and risk outcomes through interactive visualizations,
making complex statistical models more accessible and interpretable.

### Algorithm vs Model

In the context of the Algorithm Viewer, an algorithm is a family of models that
each perform a similar prediction, using the same set of inputs. A model is an
instance of the algorithm that can be evaluated. For example, the [Hypertension
Population Risk Tool (HTNPoRT)](https://github.com/Big-Life-Lab/htnport) is an
algorithm for predicting risk of hypertension. Within HTNPoRT there are two
models: one model to perform predictions for female individuals, and one for
performing predictions for male individuals.

## Features

### Multiple Plot Types

- Interactive plots for odds ratio, relative risk, and predicted risk
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

## Running the App

The [Quick Start](#quick-start) above is the standard way to install and run
the app. This section covers the `run_app()` options and the other ways to
install and run it. However you install the package, the required R package
dependencies (listed in the
[DESCRIPTION](https://github.com/Big-Life-Lab/algorithm-viewer/blob/main/DESCRIPTION)
file) are installed automatically.

### Parameters

`run_app()` accepts three parameters — `config` (the app configuration file),
`port`, and `host`. For the defaults and full descriptions, see the
[`run_app()` reference
page](https://big-life-lab.github.io/algorithm-viewer/reference/run_app.html).

Example — loading a custom app configuration (see
[Configuration](#configuration) for the file format):

```r
library(algorithm.viewer)
run_app(config = "path/to/my-config.yaml")
```

Example — running in Docker or hosting on your local network:

```r
run_app(host = "0.0.0.0", port = 3838)
```

### Installing from a Local Copy

If you have cloned or downloaded this repository, you can install the package
from your local copy instead of from GitHub:

```r
install.packages("remotes")
remotes::install_local("path/to/algorithm-viewer")
```

Then load and run the app as in the [Quick Start](#quick-start).

### Development Mode

To run without installing (e.g. while actively editing source files), clone
this repository and install the dependencies:

```r
install.packages(c("remotes", "devtools"))
remotes::install_deps("path/to/algorithm-viewer", dependencies = TRUE)
```

Then, with your working directory at the root of the repository, load all
source files and start the app:

```r
devtools::load_all()
run_app()
```

To run the unit tests:

```r
devtools::test()
```

### With Docker

From a clone of this repository, build and run using Docker Compose:

```bash
docker compose up --build
```

Or build and run manually:

```bash
docker build -t algorithm-viewer .
docker run -p 3838:3838 algorithm-viewer
```

Then open <http://localhost:3838> in your browser.

### With ShinyProxy

ShinyProxy can serve the app as a multi-user deployment. A sample
`application.yml` configuration file is included in the package root.

This requires a Java runtime environment and Docker to be running. For details
on installing these refer to the [ShinyProxy Getting Started
Guide](https://www.shinyproxy.io/documentation/getting-started/).

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

For a working example, see the built-in configuration at
[inst/extdata/config.yaml](https://github.com/Big-Life-Lab/algorithm-viewer/blob/main/inst/extdata/config.yaml).

### Algorithm YAML

Each algorithm is defined in its own YAML file, referenced from the app
configuration above. The algorithm YAML specifies model metadata, data file
paths, reference group defaults, and predictor allowable values. Any value in
the file may also carry free-text notes under a `_notes_` key (eg. to document
where that value came from). The data files it references are CSV files that
conform to the [Model
Parameters](https://github.com/Big-Life-Lab/model-parameters/) format developed
by Big Life Lab, which specifies how input variables are transformed to obtain
the final output values that are plotted.

See the [Algorithm Viewer Configuration
Specification](https://github.com/Big-Life-Lab/algorithm-viewer/blob/main/specs/CONFIG_SPECIFICATION.md)
for the full algorithm YAML format, and
[inst/extdata/models/](https://github.com/Big-Life-Lab/algorithm-viewer/tree/main/inst/extdata/models)
for the example HTNPoRT algorithm files.

## Project Structure

```text
algorithm-viewer/
├── R/                                       # R source files
│   ├── app_server.R                         # Main Shiny server function
│   ├── app_ui.R                             # Main Shiny UI function
│   ├── run_app.R                            # Package entry point (run_app())
│   ├── fct_cached_data.R                    # General-purpose reactive data cache
│   ├── fct_config.R                         # App configuration loading
│   ├── fct_model_definitions.R              # YAML config file parser
│   ├── fct_model_definitions_utils.R        # Model definition helper functions
│   ├── mod_categorical_radio_table.R        # Categorical radio button table module
│   ├── mod_continuous_slider_group.R        # Continuous slider group module
│   ├── mod_range_selector.R                 # Axis range selector module (min/slider/max, log or linear)
│   ├── mod_plot_or.R                        # Odds ratio plot module
│   ├── mod_plot_pr.R                        # Predicted risk plot module
│   ├── mod_plot_rr.R                        # Relative risk plot module
│   ├── mod_plot_rr_a_vs_b.R                 # A vs B relative risk plot module
│   ├── mod_predictor_controls.R             # Per-predictor control UI module
│   ├── mod_predictor_grouped_controls.R     # Multi-model predictor controls module
│   ├── utils_general_plot.R                 # Shared plot utilities
│   ├── utils_plot_additional_controls.R     # Per-plot control row (predictor dropdowns, log checkbox)
│   ├── utils_html.R                         # HTML/CSS helpers (cache-busting stylesheet links)
│   ├── utils_jsonschema.R                   # JSON Schema validation error formatting
│   ├── utils_make_error.R                   # Typed error condition constructor
│   ├── utils_make_string_values_unique.R    # Utility for deduplicating string values
│   ├── utils_meta.R                         # Package version display UI
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
├── man/                                     # Generated R documentation (roxygen2)
├── specs/
│   ├── CONFIG_SPECIFICATION.md              # Configuration file specification
│   ├── DEPLOYMENT.md                        # Deployment options planning document
│   ├── DOCUMENTATION-PLAN.md                # Initial documentation plan
│   ├── DOCUMENTATION-TOC.md                 # Documentation table-of-contents
│   ├── DOCUMENTATION-WRITING-PLAN.md        # Documentation writing plan
│   └── INDIVIDUAL_VS_REFERENCE_SPEC.md      # Individual vs reference predictor spec
├── tests/
│   └── testthat/                            # Unit tests (+ helper-* fixtures)
├── .github/workflows/                       # CI/CD (R CMD check, Docker publish, shinylive deploy)
├── app.R                                    # Shiny app wrapper (for deployment)
├── application.yml                          # ShinyProxy configuration
├── Dockerfile                               # Docker build instructions
├── docker-compose.yml                       # Docker Compose configuration
├── _pkgdown.yml                             # pkgdown documentation site configuration
├── NAMESPACE                                # Generated package namespace (roxygen2)
└── DESCRIPTION                              # R package metadata and dependencies
```

## Deployment

The [Deployment
Specification](https://github.com/Big-Life-Lab/algorithm-viewer/blob/main/specs/DEPLOYMENT.md)
is a planning document that discusses various deployment options we may want
to implement in the future, including a public web application, a hosted
algorithm showcase for sharing models via URL, and a local development tool
for scientists building algorithms.

## License

This project is developed by Project Big Life at The Ottawa Hospital and is
released under the MIT License. See the
[LICENSE](https://github.com/Big-Life-Lab/algorithm-viewer/blob/main/LICENSE)
file for details.
