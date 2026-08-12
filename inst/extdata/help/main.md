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

### Me vs Ref Tab (Plot)

Compares a personal risk profile ("Me") against a reference profile ("Ref")
and breaks down which predictors drive the difference between them. Both
profiles are set in the sidebar (see below).

**Summary panel (top).** For each selected model, a line shows:

- **Your estimated risk** &mdash; the predicted risk of the Me profile.
- **Reference risk** &mdash; the predicted risk of the Ref profile.
- **Overall RR** &mdash; the Me risk divided by the Ref risk (e.g. `1.5×`),
  followed by the absolute difference in percentage points (e.g. `+8.0 pts`).

**Show.** The dropdown chooses what the main plot measures for each predictor:

- **Relative Risk** &mdash; ratio of risks (1 means no difference).
- **Absolute Difference** &mdash; difference in risk in percentage points
  (0 means no difference).

**Main plot.** A horizontal chart with one row per predictor. Each row isolates
that single predictor's contribution: it compares the full Me profile against
the Me profile with *only that predictor* swapped to its Ref value, holding
everything else at the Me values. The row label shows the predictor name and
its `Ref → Me` change. When multiple models are selected, each is drawn in its
own colour. A reference line marks no difference (Relative Risk&nbsp;=&nbsp;1,
or Absolute Difference&nbsp;=&nbsp;0). Hover over any point for exact values.

**Logarithmic** toggles the value axis between a log scale (helpful when values
span a wide range) and a linear scale. The **X Axis Range** slider below the
plot constrains the visible value range; separate ranges apply to each
Show / scale combination.

**Drill-down subplot.** Click any row in the main plot to load the subplot at
the bottom. It shows the relative risk of Me versus Ref as the clicked
predictor takes on all of its values, with a dot marking the current Me value.
Click another row to change the predictor shown.

---

### Me vs Ref Tab (Sidebar Controls)

Sets the predictor values for both the "Me" (individual) and "Ref" (reference)
profiles used in the Me vs Ref plot. Controls are built from the first selected
model and shown together in a single compact panel.

- **Continuous variables** &mdash; two sliders sharing the same range, labelled
  "Me" and "Ref", each with an editable number box for typing an exact value.
- **Categorical variables** &mdash; a radio-button table with a "Me" column and
  a "Ref" column. The two columns are independent, so the same level can be
  selected for both.

Both profiles are initialised to the first model's default reference group
values. Click **Reset** to restore all controls to those defaults.

---

### Logarithmic Scale

Each plot has its own **Logarithmic** checkbox in the row of controls above it.
It toggles the plot's value axis between a log&#8321;&#8320; scale (useful when
values span a wide range) and a linear scale (useful when values stay close
to 1).
