## Algorithm Viewer Help

The Algorithm Viewer visualizes clinical prediction algorithms by plotting
odds ratio and predicted risk curves. You can upload algorithm archives,
compare multiple models side by side, and explore how individual predictors
affect predicted outcomes.

---

### Uploading an Algorithm

1. In the left sidebar under the **Models** tab, click **Browse** next to
   "Upload Algorithm."
2. Select an archive file (`.zip`, `.tar`, or `.gz`) containing your algorithm.
   The archive must include a YAML configuration file (`.yaml` or `.yml`) along
   with the model parameter files it references. See below for instructions on
   how to create your own algorithm archive.
3. If the archive contains more than one YAML file, you will be prompted to
   choose which one to load.
4. Once loaded, the algorithm name and version appear in the title bar and the
   available models are listed as checkboxes.

---

### Selecting Models

After uploading, each model defined in the configuration file appears as a
checkbox under **Models** in the sidebar.

- Check one or more models to include them in the plot. Each model is drawn
  in a distinct color.
- Multiple models can be selected at once so you can compare them on the same
  axes.

---

### Predictors

#### Primary Predictor

The **Predictor** dropdown selects the variable plotted on the x-axis. Only
predictors present in the currently selected models are listed. Changing the
predictor redraws both the Odds Ratio and Predicted Risk plots.

#### Interaction Predictor

The **Interaction Predictor** dropdown lets you examine how a second variable
modifies the effect of the primary predictor.

- When set to `<empty>` (the default), the plot shows the standard odds ratio
  or predicted risk curve for the primary predictor alone.
- When set to another variable, the plot shows the odds ratio associated with
  a **one-unit change** in the interaction variable at each value of the
  primary predictor. For categorical interaction variables, a one-unit change
  means advancing to the next category. In other words, the reference group
  becomes the group with the one-unit increase.
- The **Interaction Predictor** has no effect on the Predicted Risk plot.

---

### Odds Ratio Tab

The **Odds Ratio** tab shows how the odds of the outcome change as the
selected predictor varies, relative to the reference group values.

**How odds ratios are calculated:**

1. A baseline input is constructed using the current reference group values
   (set in the **Reference** tab).
2. For each value of the selected predictor, the model is evaluated with that
   predictor set to the given value (on the x axis) while all other predictors
   remain at their reference values.
3. The predicted risk at each point is converted to odds:
   `odds = risk / (1 - risk)`.
4. The odds ratio is the odds at the given predictor value divided by the odds
   at the reference value: `OR = odds(value) / odds(reference)`.

**Reading the plot:**

- A dashed horizontal line marks an odds ratio of 1.0 (no change relative to
  the reference).
- Values above 1.0 indicate increased risk; values below 1.0 indicate
  decreased risk.
- Continuous predictors are displayed as line charts; categorical predictors
  are displayed as bar charts.
- Hover over the plot to see exact values.

---

### Predicted Risk Tab

The **Predicted Risk** tab displays the absolute predicted probability
(risk) of the outcome as the selected predictor varies. All other predictors
are held at their reference group values.

- The y-axis ranges from 0 to 1 (i.e., 0% to 100% probability).
- This view is useful for understanding the clinical magnitude of a
  predictor's effect, as opposed to the relative comparison provided by odds
  ratios.

---

### Reference Tab

The **Reference** tab in the left sidebar lets you adjust the baseline values
used in all calculations.

- Each loaded model has its own set of controls, identified by a colored
  header matching the model's plot color.
- **Continuous variables** are adjusted with numeric sliders.
- **Categorical variables** are adjusted with radio buttons showing the
  available categories.
- Changing any reference value immediately redraws the plots.
- Click the **Reset** button below a model's controls to restore all of its
  reference values to the defaults defined in the configuration file.

The reference group defines the "baseline patient" against which odds ratios
are computed. By adjusting these values you can see how the curves shift for
different baseline profiles.

---

### Logarithmic Scale

The **Logarithmic** checkbox (in the Models tab) toggles the y-axis scale on
the Odds Ratio plot:

- **Checked (default):** Log&#8321;&#8320; scale. This is useful when odds
  ratios span a wide range (e.g., 0.1 to 10), because equal multiplicative
  changes are shown as equal distances on the axis.
- **Unchecked:** Linear scale. This is useful for viewing proportional
  differences when odds ratios are close to 1.

---

### Creating an Algorithm Archive

An algorithm archive is a `.zip`, `.tar`, or `.gz` file containing all of
the files the Algorithm Viewer needs to load and run a prediction algorithm.

#### Required files

1. **Configuration file** &mdash; A single `.yaml` or `.yml` file that
   defines the algorithm metadata, models, reference groups, and predictor
   ranges. See *Configuration file format* below.
2. **Model parameter files** &mdash; The CSV files referenced by the
   configuration. Their format is defined by the <a
   href="https://big-life-lab.github.io/model-parameters/2-model-parameter-files.html"
   target="_blank">Model Parameters documentation</a>. At minimum, each model
   requires a *model export* CSV that in turn references a `variables.csv`,
   `variable-details.csv`, and `model-steps.csv`.

All file paths in the configuration are relative to the location of the
YAML file inside the archive.

#### Configuration file format

```yaml
meta:
  algorithm: <algorithm name>   # displayed in the title bar
  version: <version string>     # displayed in the title bar

models:
  <model_id>:
    title: <display name>
    model_export: <relative path to model export CSV>
    reference_group:
      <variable>: <default value>
      ...
    model_color: <color>          # optional, e.g. "#ff0000" or "red"
    predictor_ranges:             # optional
      <variable>: <range>
      ...
  _all_:                          # optional shared settings
    predictor_ranges:
      <variable>: <range>
      ...
```

**Key fields:**

- **`meta.algorithm`** &mdash; Algorithm name shown in the UI.
- **`meta.version`** &mdash; Algorithm version shown in the UI.
- **`title`** &mdash; Display name for the model (shown in checkboxes
  and headings).
- **`model_export`** &mdash; Relative path to the model export CSV
  file.
- **`reference_group`** &mdash; Default baseline values for each
  predictor variable. Variable names must match those in the model's
  `variables.csv`.
- **`model_color`** &mdash; Optional color for this model's plot
  lines/bars.
- **`predictor_ranges`** &mdash; Optional predictor value ranges for
  the x-axis and reference sliders. Supported formats: R sequence
  expressions (e.g. `seq(20, 80, by = 1)`), integer ranges
  (e.g. `20:80`), or YAML arrays (e.g. `[1, 2, 3]`). If omitted,
  ranges are derived from `variable-details.csv`.
- **`_all_`** &mdash; A special entry that provides shared settings
  inherited by all models. Model-specific values take precedence over
  `_all_` values.

For a complete specification of all configuration options, see the
`CONFIG_SPECIFICATION.md` file included with the application.

For details on the model parameter files (`variables.csv`,
`variable-details.csv`, `model-steps.csv`, and the model export CSV), refer to
the <a
href="https://big-life-lab.github.io/model-parameters/2-model-parameter-files.html"
target="_blank">Model Parameters documentation</a>.
