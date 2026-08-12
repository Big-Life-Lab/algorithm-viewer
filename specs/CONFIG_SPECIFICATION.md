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
- Optional free-text notes describing any of the above

## File Structure

```yaml
meta:
  algorithm: <string>
  version: <string>

models:
  <model_id>:
    _notes_: <string>
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

Any value in the file may carry free-text notes under a `_notes_` key; see
[Notes (`_notes_` and `_value_`)](#4-notes-_notes_-and-_value_).

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
| `_notes_`                    | string | No       | Free-text notes about this model (see [Notes](#4-notes-_notes_-and-_value_)). Every field above may also be annotated.      |

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

1. **YAML sequence definition:**

   ```yaml
   hwmdbmi:
     # Generates the sequence seq(from = 13, to = 49, by = 0.01)
     # or: [13.00, 13.01, 13.02, ..., 48.99, 49.00]
     seq:
       from: 13
       to: 49
       by: 0.01
   clc_age:
     # Generates the sequence seq(from = 20, to = 79, length.out = 5)
     # or: [20.00, 34.75, 49.50, 64.25, 79.00]
     seq:
       from: 20
       to: 79
       length.out: 5
   fmh_15: [1, 2]
   ```

2. **YAML Array:**

   ```yaml
   diabx: [1, 2]
   ```

   The values of an array are used together, as one set, so they are read as a
   single type: an array mixing whole and decimal numbers (`[18, 20.5, 23]`) is
   read as numbers, and one containing any string (`[1, "2"]`) is read entirely
   as strings. Values given in the type the pipeline expects are left in that
   type.

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
      hwmdbmi:
        seq:
          from: 13
          to: 49
          by: 0.01
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    predictor_allowable_values:
      hwmdbmi:
        seq:
          from: 13
          to: 49
          by: 0.01
      clc_age:
        seq:
          from: 15
          to: 85
  _all_:
    predictor_allowable_values:
      clc_age:
        seq:
          from: 20
          to: 80
```

In this example, `_all_` defines a `clc_age` sequence as a shared default under
the `predictor_allowable_values` key. The `male` model does not have `clc_age`
defined under `predictor_allowable_values`, so inherits the values from
`_all_`. The `female` model already has `clc_age` defined, so it does not
inherit the values from `_all_`. The above example is equivalent to:

```yaml
models:
  male:
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
    predictor_allowable_values:
      hwmdbmi:
        seq:
          from: 13
          to: 49
          by: 0.01
      clc_age:
        seq:
          from: 20
          to: 80
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    predictor_allowable_values:
      hwmdbmi:
        seq:
          from: 13
          to: 49
          by: 0.01
      clc_age:
        seq:
          from: 15
          to: 85
```

This inheritance pattern applies to all keys and values specified under
`_all_`, not just those specified under `predictor_allowable_values`. The
`_all_` key does not correspond to an actual model displayed in the Algorithm
Viewer; it exists solely to supply default configuration values to the other
models.

**Inheritance behavior:**

- Settings in `_all_` are copied to each model
- Model-specific settings take precedence over `_all_` settings
- Notes are inherited along with the values they annotate (see
  [Notes](#4-notes-_notes_-and-_value_))

---

### 4. Notes (`_notes_` and `_value_`)

Any value in the configuration file may carry free-text notes under a `_notes_`
key. Notes record where a value came from or why it was chosen — the study a
reference group is based on, the reason a range stops where it does — inside the
configuration file, next to the value they describe.

Notes are documentation only. They are stripped out of the configuration before
it is used, so adding them never changes how an algorithm is loaded, computed,
or plotted. The Algorithm Viewer keeps them in a separate structure so they can
be displayed in the UI beside the corresponding control. Displaying them is not
yet implemented: notes are currently read and stored, but nothing in the
interface shows them.

#### Supported formats

Notes may be written in either of two equivalent forms.

1. **Beside the value** — replace the value with a two-key object holding the
   notes under `_notes_` and the original value under `_value_`:

   ```yaml
   reference_group:
     hwmdbmi:
       _notes_: Median BMI of the CCHS 2015 cohort.
       _value_: 14.9
   ```

   This form works for every value, and is the only form available for values
   that are not objects (strings, numbers, and lists).

2. **Inside the value** — where the value is itself an object (a model, `meta`,
   a `reference_group`, a `seq` specification, and so on), add a `_notes_` key
   alongside its other keys:

   ```yaml
   models:
     male:
       _notes_: Fitted on the male subsample only.
       title: Male
       model_export: ./HTNPoRT-male-model-export.csv
   ```

   The `_notes_` key annotates the object it appears in and is never one of that
   object's own entries — it is not a model, not a predictor name, and not a
   `seq` parameter.

If both forms are used on the same value, the notes written beside the value
(form 1) take precedence.

#### Where notes can be attached

| Location                                                    | Example                                                           |
|-------------------------------------------------------------|-------------------------------------------------------------------|
| `meta` and its fields                                       | `version: {_notes_: Bumped for the 2026 release, _value_: 1.0.0}` |
| The `models` map                                            | a `_notes_` key directly under `models:`                          |
| A model, and each of its fields                             | `title`, `model_export`, `model_color`                            |
| A `reference_group`, and each variable in it                | `clc_age: {_notes_: Youngest age in the cohort, _value_: 20}`     |
| Each value of an explicit list of allowable values          | see "Annotating individual allowable values" below                |
| A `predictor_allowable_values` map, and each variable in it | see the example below                                             |
| A `seq` specification, and each of its parameters           | `by: {_notes_: 0.1 keeps the slider responsive, _value_: 0.1}`    |
| The `_all_` block, and anything inside it                   | see "Notes and `_all_`" below                                     |

**Example** annotating several levels at once:

```yaml
models:
  female:
    _notes_: Female model from Table 2 of the derivation paper.
    _value_:
      title: Female
      model_export: ./HTNPoRT-female-model-export.csv
      reference_group:
        _notes_: Reference patient agreed on with the clinical team.
        hwmdbmi:
          _notes_: Median BMI of the female cohort.
          _value_: 14.9
      predictor_allowable_values:
        hwmdbmi:
          _notes_: Range observed in the derivation cohort.
          _value_:
            seq:
              from: 14.9
              to: 49
              by: 0.1
```

#### Annotating individual allowable values

Where a predictor's allowable values are given as an explicit list, notes may be
attached to the list as a whole, describing the set of values:

```yaml
predictor_allowable_values:
  diabx:
    _notes_: Diabetes status, as coded in the CCHS.
    _value_: [1, 2]
```

or to the individual values, describing one value each, which is useful for
recording what a category code means:

```yaml
predictor_allowable_values:
  diabx:
    - _notes_: No diabetes.
      _value_: 1
    - _notes_: Diabetes.
      _value_: 2
```

Values may be annotated individually, as a set, or both, and any value in the
list may be left unannotated. The allowable values are unaffected either way:
both examples above define the same values as `diabx: [1, 2]`.

#### Notes and `_all_`

Notes travel with the values they annotate: a value copied out of `_all_` into a
model brings its notes with it. A model that defines its own value at that path
keeps its own value *and* its own notes, and inherits neither.

```yaml
models:
  male:
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    reference_group:
      clc_age:
        _notes_: Female-specific baseline age.
        _value_: 25
  _all_:
    reference_group:
      clc_age:
        _notes_: Youngest age in the derivation cohort.
        _value_: 20
```

The `male` model inherits `clc_age: 20` together with the note "Youngest age in
the derivation cohort". The `female` model keeps `clc_age: 25` and its own note.

The same applies to `_all_`'s own `_notes_` key: a `_notes_` written directly in
`_all_` becomes the note of every model that does not carry a note of its own.
To describe the shared block itself without that happening, attach the note to a
value inside `_all_` rather than to `_all_`.

#### Restrictions

- `_value_` is a reserved key: it may not be used as a model identifier or as a
  predictor variable name. `_notes_` is likewise reserved wherever notes are
  accepted.
- Notes must be strings. A single line or a multi-line YAML block scalar
  (`_notes_: |`) is accepted; structured values are rejected by the schema.
- A `_notes_` key at the very top of the file (a sibling of `meta` and `models`)
  is accepted but discarded. Attach file-level notes to `meta` instead.

#### How notes are processed

`read_model_definitions()` (in `R/fct_model_definitions.R`) normalizes both
forms into a uniform tree of `_notes_`/`_value_` nodes, merges the `_all_` block
on that tree so notes stay attached to their values, and then splits the notes
back out. The definitions it returns therefore hold plain values, with all the
notes gathered under a `$notes` element whose structure mirrors the definitions.
A sequence whose values carry notes is read as a list of the mappings those
notes are written as, so it is collapsed back to a vector as the notes are split
off, leaving it in the same form as the sequence written without notes.

Look a note up with the same sequence of keys that leads to the value:

```r
info <- read_model_definitions("path/to/algorithm.yaml")

info$models$male$reference_group$clc_age
#> 20

get_notes(info, list("models", "male", "reference_group", "clc_age"))
#> "Youngest age in the derivation cohort."
```

A key may also be a number, addressing the value at that position rather than by
name. This is the only way to reach the entries of an unnamed list, which have
no names of their own — the notes on the second allowable value of `diabx` are:

```r
get_notes(
  info,
  list("models", "male", "predictor_allowable_values", "diabx", 2)
)
```

Positions are 1-based and work for named values too, so `list("models", 1)`
reaches the notes on the first model. A path that mixes names and positions must
be a `list()`: `c()` would coerce the numbers to strings, and they would then be
looked up as names.

`get_notes()` returns `NULL` when the key path does not exist — including a
position outside the node it is applied to — or when the value it names has no
notes.

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
      hwmdbmi:
        _notes_: Median BMI of the male derivation cohort.
        _value_: 13.83
      diabx: 2
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    reference_group:
      clc_age: 20
      fmh_15: 2
      hwmdbmi:
        _notes_: Median BMI of the female derivation cohort.
        _value_: 14.9
      diabx: 2
  _all_:
    predictor_allowable_values:
      hwmdbmi:
        _notes_: BMI range observed across the derivation cohort.
        _value_:
          seq:
            from: 13
            to: 49
            by: 0.01
```
