## Algorithm Viewer Help

The Algorithm Viewer visualizes clinical prediction algorithms by plotting
odds ratio and predicted risk curves. You can upload one or more algorithm
archives, compare models side by side, and explore how individual predictors
affect predicted outcomes.

For full documentation — including how to create algorithm archives and
configure models — visit the
<a href="https://github.com/Big-Life-Lab/algorithm-viewer" target="_blank">Algorithm Viewer GitHub repository</a>.

---

### Uploading an Algorithm

Uploading algorithms is only available if the current installation permits it.
If the upload control is not visible, this feature has been disabled by the
administrator.

In the left sidebar under the **Models** tab, click **Browse** next to
"Upload Algorithm" and select an archive file (`.zip`, `.tar`, or `.gz`).
Once loaded, the algorithm name and version appear in the title bar and the
available models are listed as checkboxes.

---

### Preloaded Algorithms

Some installations include preloaded algorithms that are available without
uploading a file; if no preloaded algorithms are shown, the feature is not
enabled for this installation.

In the left sidebar under the **Models** tab, select an algorithm from the
**Preloaded Algorithms** dropdown to view that particular algorithm and its
models.

---

### Selecting Models

Check one or more models in the sidebar to include them in the plots. Each
model is drawn in a distinct color, so multiple models can be compared on
the same axes.

---

### Predictors

- **Predictor** &mdash; the variable plotted on the x-axis.
- **Interaction Predictor** &mdash; an optional second variable that shows
  how a one-unit change in that variable modifies the odds ratio of the
  primary predictor at each x-axis value. Has no effect on the Predicted
  Risk plot.

---

### Odds Ratio Tab

Shows how the odds of the outcome change as the selected predictor varies,
relative to the reference group values. A dashed line marks an odds ratio of
1.0. Values above 1.0 indicate increased risk; values below 1.0 indicate
decreased risk. Hover over the plot to see exact values.

---

### Relative Risk Tab

Shows the predicted risk at each x-axis value divided by the predicted risk
at the reference group values. A value of 1.0 means no difference from the
reference; values above 1.0 indicate higher risk and values below 1.0
indicate lower risk. Unlike the Odds Ratio tab, this expresses risk as a
ratio of probabilities rather than a ratio of odds.

---

### Predicted Risk Tab

Shows the absolute predicted probability (0–100%) of the outcome as the
selected predictor varies, with all other predictors held at their reference
values. Useful for understanding the clinical magnitude of a predictor's
effect.

---

### Reference Tab

Adjusts the baseline values ("reference patient") used in all calculations.
Each loaded model has its own set of controls with sliders for continuous
variables and radio buttons for categorical variables. Changes take effect
immediately. Click **Reset** to restore a model's defaults.

---

### Logarithmic Scale

The **Logarithmic** checkbox (in the Models tab) toggles the y-axis of the
Odds Ratio plot between a log&#8321;&#8320; scale (useful when odds ratios
span a wide range) and a linear scale (useful when odds ratios are close
to 1).
