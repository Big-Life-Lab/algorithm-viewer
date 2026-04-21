# Algorithm Viewer Configuration Specification

This document describes how to populate the YAML configuration file for the
Algorithm Viewer application.

## Overview

The YAML configuration file is the central configuration that defines:

- Algorithm metadata displayed in the UI
- Available models and their display names
- Data files to load for each model
- Default reference group values for odds ratio calculations
- Predictor allowable values for visualization

## File Structure

```yaml
meta:
  algorithm: <string>
  version: <string>

models:
  <model_id>:
    title: <string>
    model_export: <path>
    reference_group:
      <variable>: <value>
      ...
    predictor_allowable_values:
      <variable>: <value_expression>
      ...
  _all_:
    predictor_allowable_values:
      <variable>: <value_expression>
      ...
```

---

## Sections

### 1. `meta` Section

Contains algorithm metadata displayed in the application title.

| Field       | Type   | Required | Description                                     |
|-------------|--------|----------|-------------------------------------------------|
| `algorithm` | string | Yes      | Name of the algorithm (e.g., "HTNPoRT")         |
| `version`   | string | Yes      | Version number of the algorithm (e.g., "1.0.0") |

**Example:**

```yaml
meta:
  algorithm: HTNPoRT
  version: 1.0.0
```

---

### 2. `models` Section

Defines individual models available in the application. Each key under `models`
is a model identifier. As many models as necessary can be specified.

#### Model Definition

| Field                        | Type   | Required | Description                                                                                                                 |
|------------------------------|--------|----------|-----------------------------------------------------------------------------------------------------------------------------|
| `title`                      | string | Yes      | Display name shown in UI (radio buttons, headings)                                                                          |
| `model_export`               | path   | Yes      | Relative path to the model export CSV file                                                                                  |
| `reference_group`            | object | Yes      | Default baseline values for odds ratio calculations                                                                         |
| `model_color`                | string | No       | Color to use for plots representing this model (eg. "#ff0000" or "red"). If not specified then one is chosen automatically. |
| `predictor_allowable_values` | object | No       | Override/specify allowable values for specific predictors                                                                   |

**Example:**

```yaml
models:
  male:
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
    model_color: "#ff0000"
    reference_group:
      clc_age: 20
      fmh_15: 2
      hwmdbmi: 13.83
      diabx: 2
```

#### `model_export`

The `model_export` field references a CSV file that catalogs all required data
files for the model. This file, its format, and the format of its dependencies
are specified in the [Model Parameters
documentation](https://big-life-lab.github.io/model-parameters/2-model-parameter-files.html).
It's main purpose is to specify how input variables are transformed to obtain
the final output values that are plotted.

#### `reference_group`

Specifies default baseline values used as the reference point for odds ratio
calculations. Each key is a variable name from the model, and the value is the
default baseline. The reference group can be modified in the UI of the
Algorithm Viewer; the values specified in the configuration file are the
default values.

- **Purpose:** Sets initial slider values in the "Reference" tab
- **Variable names:** Must match variable names in the model's variables CSV file
- **Values:**
  - For continuous variables: numeric value within the allowable values
  - For categorical variables: numeric code or string representing the
    category. These should correspond to the untransformed inputs to the
    pipeline, so if an integer is used to represent a category in the input,
    then an integer should be used, whereas if the input is a string then the
    string category should be used.

**Example:**

```yaml
reference_group:
  clc_age: 20        # Age 20 as baseline
  fmh_15: 2          # Family history of hypertension category 2
  hwmdbmi: 13.83     # BMI of 13.83 as baseline
  diabx: 2           # Diabetes category 2
```

#### `predictor_allowable_values`

Defines the allowable values displayed on the x-axis of plots, as well as the
values displayed in UI sliders/radio buttons to allow the user to modify the
reference groups. If not specified, allowable values are derived from the
`variable-details.csv` file. If the allowable values can be calculated based on
`variable-details.csv` then the values under `predictor_allowable_values` will
take precedence. If the allowable values for a predictor cannot be determined
based on `variable-details.csv`, then they must be specified in this section.
The allowable values for categorical variables should be of the same type (eg.
a string or integer) as what is expected as the untransformed input to the
pipeline.

**Supported formats:**

1. **R sequence expression:**

   ```yaml
   hwmdbmi: seq(13, 49, by = 0.01)
   clc_age: seq(20, 79, length.out = 10)
   ```

2. **Integer range:**

   ```yaml
   clc_age: 20:80
   ```

3. **YAML Array:**

   ```yaml
   diabx: [1, 2]
   ```

---

### 3. `_all_` Special Model

The `_all_` key defines shared configuration applied to all models that don't
override these settings.

**Example:**

```yaml
models:
  male:
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
    predictor_allowable_values:
      hwmdbmi: seq(13, 49, by = 0.01)
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    predictor_allowable_values:
      hwmdbmi: seq(13, 49, by = 0.01)
      clc_age: 15:85
  _all_:
    predictor_allowable_values:
      clc_age: 20:80
```

In this example, `_all_` defines `clc_age: 20:80` as a shared default under the
`predictor_allowable_values` key. The `male` model does not have `clc_age`
defined under `predictor_allowable_values`, so inherits the values from
`_all_`. The `female` model already has `clc_age` defined, so it does not
inherit the values from `_all_`. The above example is equivalent to:

```yaml
models:
  male:
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
    predictor_allowable_values:
      hwmdbmi: seq(13, 49, by = 0.01)
      clc_age: 20:80
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    predictor_allowable_values:
      hwmdbmi: seq(13, 49, by = 0.01)
      clc_age: 15:85
```

This inheritance pattern applies to all keys and values specified under
`_all_`, not just those specified under `predictor_allowable_values`. The
`_all_` key does not correspond to an actual model displayed in the Algorithm
Viewer; it exists solely to supply default configuration values to the other
models.

**Inheritance behavior:**

- Settings in `_all_` are copied to each model
- Model-specific settings take precedence over `_all_` settings

---

## Complete Example

```yaml
meta:
  algorithm: HTNPoRT
  version: 1.0.0

models:
  male:
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
    reference_group:
      clc_age: 20
      fmh_15: 2
      hwmdbmi: 13.83
      diabx: 2
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    reference_group:
      clc_age: 20
      fmh_15: 2
      hwmdbmi: 14.9
      diabx: 2
  _all_:
    predictor_allowable_values:
      hwmdbmi: seq(13, 49, by = 0.01)
```
