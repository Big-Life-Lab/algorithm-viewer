# Running the Algorithm Viewer in your Web Browser

**Audience:** Anyone who has installed the Algorithm Viewer and wants to
open it.

**Prerequisites:** The Algorithm Viewer installed ([Installing the
Algorithm Viewer on your
Computer](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-installing.md)).

**What you will have at the end:** the app running in your browser,
preloaded with the example HTNPoRT algorithm, and a mental map of its
interface.

------------------------------------------------------------------------

## 1. Launch the app

In R (or RStudio), load the package and call
[`run_app()`](https://big-life-lab.github.io/algorithm-viewer/reference/run_app.md):

``` r
library(algorithm.viewer)
run_app()
```

Your default web browser opens the Algorithm Viewer automatically. (In
RStudio, it may open in the Viewer pane instead — click the “Show in new
window” icon to open it in a full browser tab.)

Because you did not pass a configuration file, the app starts with its
built-in example: the [Hypertension Population Risk Tool
(HTNPoRT)](https://github.com/Big-Life-Lab/htnport) algorithm, with two
models (Female and Male) preloaded.

Leave the R session running while you use the app — the app is served by
that R process. To stop the app, return to R and press Esc (or Ctrl+C in
a terminal).

## 2. Get oriented

The interface has two parts.

### The sidebar (left)

The sidebar holds the controls, organized into tabs:

- **Models** — choose which algorithm and which models are shown. Each
  selected model is drawn in its own colour so you can compare them on
  the same axes. This tab also holds the **Logarithmic** scale checkbox
  and, if the installation allows it, an **Upload Algorithm** control
  and a **Preloaded Algorithms** dropdown.
- **Reference** — sets the baseline “reference patient” that every plot
  is calculated against. Continuous variables (such as age or BMI) use
  sliders; categorical variables (such as diabetes status) use radio
  buttons. Click **Reset** to restore a model’s defaults.

### The plot area (right)

The main area shows one plot at a time, chosen with the tabs across the
top:

- **Odds Ratio**
- **Relative Risk**
- **Predicted Risk**
- **Me vs Ref** — compare a personal profile against a reference
  profile.

Above each plot are controls for choosing the **Predictor** on the
x-axis and an optional **Interaction Predictor**.

The next tutorial explains what each plot means and how to read it.

## 3. Optional: `run_app()` options

[`run_app()`](https://big-life-lab.github.io/algorithm-viewer/reference/run_app.md)
accepts three arguments — `config` (the app configuration file), `port`,
and `host`. For the defaults and full descriptions, see the [`run_app()`
reference
page](https://big-life-lab.github.io/algorithm-viewer/reference/run_app.md).

For example, to load your own configuration and serve the app on your
local network:

``` r
run_app(config = "path/to/my-config.yaml", host = "0.0.0.0", port = 3838)
```

Loading a custom configuration is covered in [View your own algorithms
in the Algorithm
Viewer](https://big-life-lab.github.io/algorithm-viewer/articles/howto-view-your-algorithms.md).

## Next steps

- [Viewing the Built-in HTNPoRT
  Algorithm](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-htnport.md)
  — learn how to read and interpret each plot.
