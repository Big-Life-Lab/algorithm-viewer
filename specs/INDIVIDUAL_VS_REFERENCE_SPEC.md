# Individual vs Reference Profile Tab — Feature Specification

## Overview

This tab allows a user to compare their own risk profile against a configurable **Reference Profile**, and to explore **"what-if" scenarios** showing how that relative risk would change if specific variables in their profile were different. It answers two questions: *"How does my risk compare to the reference?"* and *"How would that change if a specific variable were different?"*

The main plot is arranged in rows:

- **Row 1 (Overall):** The unmodified relative risk of the Individual Profile versus the Reference Profile.
- **All other rows (What-If):** For each categorical variable in the model, one row per alternative level — showing the relative risk that would result if the Individual Profile's value for that variable were replaced by the row's level, while all other variables remain unchanged.

A vertical dotted line drawn at the Overall relative risk serves as a personal baseline, making it immediately visible whether a particular what-if change would increase or decrease the individual's risk.

---

## 1. Sidebar: Profile Controls

### 1.1 Placement

The merged profile controls replace the current **Reference** tab in the left sidebar `tabsetPanel` when this tab is active. Using `shiny::conditionalPanel` or swapping `renderUI` output based on `input$main_tabs`, the sidebar shows:

- **This tab active:** Merged Individual + Reference profile controls (described below).
- **Any other tab active:** The normal reference group controls.

### 1.2 Controls Layout

A single scrollable panel contains one merged control per predictor variable. The control type depends on whether the variable is continuous or categorical. A single **Reset** button at the bottom restores both profiles to the first model's reference group values.

```
┌──────────────────────────────────────────┐
│  Profile Controls            ← heading   │
│                                          │
│  Age                                     │
│  Me:  ──────▲──────────────── 45         │
│  Ref: ────────────▲────────── 60         │
│                                          │
│  BMI                                     │
│  Me:  ──────▲──────────────── 22         │
│  Ref: ──▲──────────────────── 18         │
│                                          │
│  Smoking Status                          │
│  ┌──────────────┬──────┬──────┐          │
│  │              │  Me  │ Ref  │          │
│  ├──────────────┼──────┼──────┤          │
│  │ Never        │  ○   │  ○   │          │
│  │ Former       │  ●   │  ○   │          │
│  │ Current      │  ○   │  ●   │          │
│  └──────────────┴──────┴──────┘          │
│                                          │
│  [Reset]                 ← sticky footer │
└──────────────────────────────────────────┘
```

#### Continuous Variables — Dual-Thumb Sliders

Each continuous variable is represented by two vertically stacked sliders sharing the same range (`min` / `max` from `predictor_allowable_values`):

- **"Me" slider** — top; controls the Individual Profile value.
- **"Ref" slider** — bottom; controls the Reference Profile value.

Each slider displays its current value above the thumb position, as per Shiny's default `sliderInput` behaviour. The variable label appears above both sliders.

Implementation note: Shiny's built-in `sliderInput` supports a two-handle range slider (`value = c(lo, hi)`) but does not support two independent sliders with distinct semantics on a shared track; the two-handle slider defines the lower and upper ends of a range, where one handle must be greater than or equal to the other. Two separate `sliderInput` widgets with identical `min`/`max`/`step` and a shared visual label are the recommended implementation.

#### Categorical Variables — Three-Column Table

Each categorical variable is represented as a compact table with three columns:

| *(level labels)* | Me | Ref |
|---|---|---|
| Never | ○ | ○ |
| Former | ● | ○ |
| Current | ○ | ● |

- **Column 1** lists all allowable level labels for the variable, in `predictor_allowable_values` order.
- **Column 2 ("Me")** contains one radio button per level, grouped so that exactly one is selected at all times; controls the Individual Profile value.
- **Column 3 ("Ref")** contains one radio button per level, grouped independently from column 2; controls the Reference Profile value.

The two radio groups are independent — the same level may be selected for both Me and Ref.

### 1.3 Initialisation and Reset

- Both the Individual ("Me") and Reference ("Ref") values initialise to the first loaded model's `reference_group`.
- The **Reset** button restores both sets of values to the first model's reference group, for all variables simultaneously.
- The scrollable container fills the full sidebar height (matching the current Reference tab style: `height: calc(100vh - 140px); overflow-y: scroll`).

### 1.4 Model Selection

The profile controls are built from the **first selected model** only. If the user has multiple models selected, the same Individual Profile and Reference Profile values are applied to each model's pipeline when computing relative risks.

---

## 2. Main Plot

### 2.1 Row Structure

Each row represents one comparison of a (possibly modified) Individual Profile against the fixed Reference Profile.

**Row 1 — Overall:**

```
RR = P(outcome | Individual Profile) / P(outcome | Reference Profile)
```

Y-axis label: `"Overall"`

**Subsequent rows — What-If (one row per alternative categorical level):**

For each categorical variable `V` in the model (in model definition order):
  For each level `L` in `predictor_allowable_values(V)`, where `L ≠ Individual Profile's current value for V` (in allowable-values order):

```
RR = P(outcome | Individual Profile with V replaced by L) / P(outcome | Reference Profile)
```

Y-axis label: see §2.4.

Continuous variables do not produce what-if rows. Their contribution is reflected in the Overall row only.

### 2.2 Reference Lines

Two vertical reference lines are drawn on the plot:

| Line | Style | Position | Purpose |
|---|---|---|---|
| Null reference | Solid, grey | RR = 1 | No effect (baseline for any RR plot) |
| Overall reference | Dotted, model-coloured | RR = Overall RR | Personal baseline for what-if comparisons |

A separate dotted Overall reference line is drawn for each model, coloured to match that model's colour. Each line is labelled (or identified via legend/tooltip) by model name to avoid ambiguity.

### 2.3 Plot Appearance

- **Plot type:** Scatter (points), `plot_type = "point"`.
- **Orientation:** Flipped coordinates (`flip_coords = TRUE`): y-axis lists comparison labels, x-axis shows relative risk values.
- **Row order:** Overall row first, then what-if rows grouped by variable, in model definition order. Within each variable group, levels appear in `predictor_allowable_values` order (skipping the Individual Profile's current level).
- **Multiple models:** One set of points per selected model, dodged vertically (`position_dodge`), coloured by model. The Overall reference dotted lines are also per-model.
- **X-axis label:** `"Relative Risk"` (or `"Relative Risk (Logarithmic)"` when log scale is active).
- **Y-axis label:** Omitted (comparison labels on the y-axis are self-explanatory).
- **Logarithmic toggle:** Respects the sidebar's logarithmic checkbox.

### 2.4 Y-Axis Label Format

**Overall row:**

```
Overall
```

**What-if rows:**

```
<Variable label>: <What-if level label>
```

Examples: `"Smoking Status: Former"`, `"Marital Status: Single"`, `"Diabetes Status: Yes"`.

The Individual Profile's current value for the variable is **not** shown on the y-axis label but is included in the hover tooltip (§2.5).

### 2.5 Hover Tooltip Format

**Overall row:**

```
Overall
RR: <value>
```

**What-if rows:**

```
<Variable label>: <What-if level label>
(Individual has: <Individual's current level label>)
RR: <value>
```

Example:

```
Smoking Status: Former
(Individual has: Current Smoker)
RR: 0.72
```

### 2.6 Help Button

#### Trigger

A small **"?"** icon button is placed at the top-left corner of the plot area (above the plot itself, not overlaid on the plot). Clicking it opens a modal popup that explains the tab and how to read the plot.

#### Popup Content

The popup should cover:

**What this tab shows**

> This tab compares your personal risk profile ("Me") against a configurable reference profile. The sidebar controls let you set the values for each variable for both profiles.

**How to read the plot**

> - **Overall (first row):** Your relative risk compared to the reference profile, using the exact values you entered for both profiles.
> - **What-if rows (all other rows):** Each row asks "what if one of my variables were different?" The individual profile is kept the same *except* for the variable shown in the row label, which is replaced with the alternative level listed. The reference profile is always unchanged.
> - **Solid vertical line (RR = 1):** If a point sits on this line, changing to that level would give the same risk as the reference profile.
> - **Dotted vertical line (your baseline):** This marks the Overall relative risk — your actual risk compared to the reference. Points to the **right** of this line represent levels that would increase your risk above your current profile; points to the **left** represent levels that would decrease it.

#### Implementation

Use `shiny::modalDialog()` triggered by `shiny::observeEvent()` on the button click. The button itself is rendered as part of the tab's UI, directly above the plot output. The modal requires no footer action beyond a **Close** button (the default `shiny::modalDialog` footer).

### 3.1 Matrix Construction

For each selected model, construct one input matrix covering all comparisons plus the denominator row:

```
rows:
  1. Individual Profile (unmodified)                          → Overall RR
  2…N. Individual Profile with V replaced by L, for each
       categorical variable V and each alternative level L    → What-If RR
  N+1. Reference Profile                                      → denominator
```

Pseudocode:

```r
rows <- list(individual_profile)  # Row 1: Overall

for each variable V in model definition order:
  if is_variable_categorical(model_data, V):
    individual_value_V <- individual_profile[[V]]
    for each level L in get_predictor_allowable_values(model_data, V):
      if L != individual_value_V:
        modified <- individual_profile
        modified[[V]] <- L
        rows <- c(rows, list(modified))

rows <- c(rows, list(reference_profile))  # final row: denominator

df      <- do.call(rbind, lapply(rows, as.data.frame))
risks   <- run_model_pipeline(model_data$model_pipeline, x = df)
n_rows  <- nrow(df) - 1L
rr      <- risks[seq_len(n_rows)] / risks[nrow(df)]
```

### 3.2 Return Structure

```r
list(
  df = data.frame(
    x          = rr,
    RR         = rr,
    Model      = model_title,
    Label      = c("Overall",
                   "Smoking Status: Former",
                   "Smoking Status: Never",
                   "Marital Status: Single",
                   ...),
    Comparison = c("Individual vs Reference (Overall)",
                   "Smoking Status: Former vs Current",
                   "Smoking Status: Never vs Current",
                   "Marital Status: Single vs Married",
                   ...),
    predictor        = c("",       "smoking_status",      "smoking_status",      ...),
    individual_value = c("",       "Current Smoker",      "Current Smoker",      ...),
    whatif_value     = c("",       "Former",              "Never",               ...)
  ),
  overall_rr    = rr[1],
  x_axis_label  = "Relative Risk",
  y_axis_label  = "",
  x_axis_type   = "Categorical",
  aes_args      = list(x = sym("Label"), y = sym("RR"),
                       Comparison = sym("Comparison"))
)
```

### 3.3 Overall Reference Line

After `make_general_plot()` renders the base plot, an additional `geom_vline` layer is added for each model at its `overall_rr`, using `linetype = "dotted"` and the model's colour. This is analogous to the existing implementation that adds dotted lines to the plot via `extra_plot`.

---

## 4. Interaction with Existing Controls

| Control | Effect on this tab |
|---|---|
| **Predictor** dropdown | Not used. Hidden or ignored when this tab is active. |
| **Interaction Predictor** dropdown | Not used. Hidden or ignored when this tab is active. |
| **Logarithmic** checkbox | Applies to the main plot. |
| **Models** checkbox group | Selects which models contribute points. Multiple models produce dodged points and separate Overall reference lines. |
| **Individual Profile controls** | Changing any control value invalidates the plot and redraws all rows. |
| **Reference Profile controls** | Changing any control value invalidates the plot (denominator changes, so all RRs change). |

---

## 5. Caching

Use the existing `initialize_cached_data` / `is_reusable_cached_data` / `set_cached_data` / `get_cached_data` infrastructure.

**Cache key:** `list("rr_individual_vs_reference", model_id)`

**Cache params:**

```r
list(
  individual_profile = individual_profile,
  reference_profile  = reference_profile
)
```

Cache is cleared whenever new model definitions are loaded (priority 10000 observer, same pattern as existing modules).

---

## 6. Open Questions

1. **Variable grouping on the y-axis:** Should the what-if rows for a single categorical variable be visually grouped — for example, with the variable label as a bold separator row, or with a gap between variable groups — or is the `"Variable: Level"` label format on every row sufficient to convey the grouping?

2. **Continuous predictor what-if rows:** Currently out of scope. A future iteration could add what-if rows for continuous variables at specified alternative values (e.g., ±1 SD, min/max), but this adds significant UI complexity for selecting those alternative values.

3. **Axis limits and scale:** When the individual adjusts the profile controls, some what-if RRs may shift far from the others, causing the axis to rescale and making changes hard to follow. A fixed-axis-range control may be valuable here.
