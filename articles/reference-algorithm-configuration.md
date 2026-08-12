# Algorithm configuration

**Audience:** Anyone writing the YAML file that defines an algorithm and
its models.

This page documents the **algorithm configuration** file — the YAML that
defines one algorithm, its models, their data files, reference-group
defaults, and predictor allowable values. It is the file referenced by
`file:` in the [application
configuration](https://big-life-lab.github.io/algorithm-viewer/articles/reference-app-configuration.md),
or uploaded through the app.

Each algorithm file is validated against a JSON Schema
(`inst/extdata/schema/algorithm.schema.json`); unknown fields are
rejected.

------------------------------------------------------------------------

## File structure

``` yaml
meta:
  algorithm: <string>
  version: <string>

models:
  <model_id>:
    _notes_: <string>               # optional notes about this model
    title: <string>
    model_export: <path>
    model_color: <color>            # optional
    reference_group:
      <variable>: <value>
      ...
    predictor_allowable_values:     # optional
      <variable>: <value_expression>
      ...
  _all_:                            # optional shared defaults
    ...
```

The two required top-level keys are `meta` and `models`. Any value in
the file may additionally carry free-text notes — see
[Notes](#notes-_notes_-and-_value_).

## `meta`

Algorithm metadata, displayed in the application title bar.

| Field       | Type   | Required | Description                                         |
|-------------|--------|----------|-----------------------------------------------------|
| `algorithm` | string | Yes      | Name of the algorithm (e.g. `HTNPoRT Full`).        |
| `version`   | string | Yes      | Version of the algorithm definition (e.g. `1.0.0`). |

``` yaml
meta:
  algorithm: HTNPoRT Full
  version: 1.0.0
```

## `models`

A map of model entries. Each key is an arbitrary **model identifier**;
define as many models as you need. The special key `_all_` is not a
model — it supplies shared defaults (see
[`_all_`](#the-_all_-shared-block) below).

### Fields of a model entry

| Field                        | Type   | Required | Description                                                                                             |
|------------------------------|--------|----------|---------------------------------------------------------------------------------------------------------|
| `title`                      | string | Yes      | Display name shown in radio buttons and headings.                                                       |
| `model_export`               | path   | Yes      | Path (relative to this YAML file’s directory) to the model export CSV.                                  |
| `reference_group`            | map    | Yes\*    | Default baseline values for each variable.                                                              |
| `model_color`                | string | No       | Colour for this model’s plots. If omitted, a colour is assigned automatically from the viridis palette. |
| `predictor_allowable_values` | map    | No       | Allowable values for specific predictors (x-axis values and control ranges).                            |

\* `reference_group` is required for a usable model; it may be supplied
on the model itself or inherited from `_all_`.

``` yaml
models:
  female:
    title: Female
    model_export: ./female-full/HTNPoRT-full-female-model-export.csv
    reference_group:
      hwmdbmi: 14.9
    predictor_allowable_values:
      hwmdbmi:
        seq:
          from: 14.9
          to: 49
          by: 0.1
```

### `model_export`

A path — relative to the algorithm YAML file’s own directory — to the
model export CSV. This CSV catalogs all the data files the model needs
and specifies how input variables are transformed into the plotted
output values. Its format and the format of the files it references are
part of the [Model
Parameters](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-model-parameters.md)
specification; see the [Model Parameters file
documentation](https://big-life-lab.github.io/model-parameters/2-model-parameter-files.html).

### `model_color`

An optional colour for the model’s curves. Accepts a CSS hex colour (3,
4, 6, or 8 hex digits, e.g. `#21908C` or `#440154FF`) or a CSS named
colour (e.g. `red`, `steelblue`). If omitted, the viewer assigns a
colour automatically from the viridis palette.

### `reference_group`

Default baseline (“reference patient”) values used as the reference
point for odds ratio and relative risk calculations, and as the initial
values of the sidebar controls. Each key is a variable name; each value
is that variable’s baseline.

- **Variable names** must match the variable names in the model’s
  variables file.
- **Continuous variables** take a numeric value within the allowable
  range.
- **Categorical variables** take the value that represents the category
  as the *untransformed input* to the pipeline — an integer if the input
  uses integer codes, a string if the input uses string categories.

The values here are only defaults; users can change the reference group
in the UI.

``` yaml
reference_group:
  clc_age: 20        # age 20 as baseline
  fmh_15: 2          # family-history category 2
  hwmdbmi: 13.83     # BMI 13.83 as baseline
  diabx: 2           # diabetes category 2
```

To record where a baseline value came from, attach `_notes_` to it — see
[Notes](#notes-_notes_-and-_value_).

### `predictor_allowable_values`

Defines the values shown on a plot’s x-axis and offered in the sidebar
controls for a predictor. If omitted for a predictor, allowable values
are derived from the model’s `variable-details.csv`. **Values specified
here take precedence** over the CSV-derived ones. If the allowable
values cannot be determined from `variable-details.csv`, they must be
specified here. For categorical variables, use the same type (integer or
string) expected as the untransformed pipeline input.

Two forms are accepted:

**1. A `seq` specification** — generates a numeric sequence, equivalent
to R’s [`seq()`](https://rdrr.io/r/base/seq.html). Requires `from` and
`to`; use *either* `by` (step size) *or* `length.out` (number of
values), not both.

``` yaml
hwmdbmi:
  # seq(from = 13, to = 49, by = 0.01) → 13.00, 13.01, ..., 49.00
  seq:
    from: 13
    to: 49
    by: 0.01
clc_age:
  # seq(from = 20, to = 79, length.out = 5) → 20.00, 34.75, 49.50, 64.25, 79.00
  seq:
    from: 20
    to: 79
    length.out: 5
```

**2. An explicit array** — an inline list of allowable values (numbers
for continuous, numbers or strings for categorical).

``` yaml
diabx: [1, 2]
fmh_15: [1, 2]
```

The values of an array are used together, as one set, so they are read
as a single type: an array mixing whole and decimal numbers
(`[18, 20.5, 23]`) is read as numbers, and one containing any string
(`[1, "2"]`) is read entirely as strings. Give a categorical predictor’s
values in the type its pipeline expects, and they will be left in that
type.

Either form may be annotated with `_notes_`, and an explicit array may
be annotated value by value — see [Notes](#notes-_notes_-and-_value_)
and [Annotating individual allowable
values](#annotating-individual-allowable-values).

## The `_all_` shared block

`_all_` is a special key under `models` that defines configuration
**shared** by every model. Its values are merged into each named model,
but a value already set on a model takes precedence over the one in
`_all_`. `_all_` does not itself appear as a model in the UI.

Merging applies to every key `_all_` defines — not only
`predictor_allowable_values`. This lets you write shared reference-group
values or allowable-value sequences once.

``` yaml
models:
  male:
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
    predictor_allowable_values:
      hwmdbmi:
        seq: { from: 13, to: 49, by: 0.01 }
  female:
    title: Female
    model_export: ./HTNPoRT-female-model-export.csv
    predictor_allowable_values:
      hwmdbmi:
        seq: { from: 13, to: 49, by: 0.01 }
      clc_age:
        seq: { from: 15, to: 85 }
  _all_:
    predictor_allowable_values:
      clc_age:
        seq: { from: 20, to: 80 }
```

Here `male` has no `clc_age` under `predictor_allowable_values`, so it
inherits `clc_age: seq(20, 80)` from `_all_`. `female` already defines
its own `clc_age`, so it keeps `seq(15, 85)` and does **not** inherit
from `_all_`.

**Inheritance rules:**

- Settings in `_all_` are copied into each model.
- Model-specific settings override `_all_` settings.
- Notes are inherited the same way as the values they annotate (see
  [Notes](#notes-_notes_-and-_value_)).

## Notes (`_notes_` and `_value_`)

**Any** value in an algorithm configuration file may carry free-text
notes, written under the key `_notes_`. Notes let you record where a
value came from or why it was chosen — for example the study a reference
group is based on, or the reason a BMI range stops at 49 — inside the
configuration file itself, next to the value they describe.

Notes are documentation only. They are stripped out of the configuration
before it is used, so adding them never changes how an algorithm is
loaded, computed, or plotted. The viewer keeps them separately so that
they can be displayed in the UI beside the corresponding control (not
yet implemented — at present notes are read and stored, but nothing in
the interface shows them).

### The two forms

Notes can be written in either of two equivalent forms.

**1. Beside the value** — replace the value with a two-key object
holding the notes under `_notes_` and the original value under
`_value_`:

``` yaml
reference_group:
  hwmdbmi:
    _notes_: Median BMI of the CCHS 2015 cohort.
    _value_: 14.9
```

This form works for **every** value, and is the only form available for
values that are not objects — strings, numbers, and lists.

**2. Inside the value** — where the value is itself an object (a model,
`meta`, a `reference_group`, a `seq` specification, …), add a `_notes_`
key alongside its other keys:

``` yaml
models:
  male:
    _notes_: Fitted on the male subsample only.
    title: Male
    model_export: ./HTNPoRT-male-model-export.csv
```

The `_notes_` key annotates the object it appears in; it is never
treated as one of that object’s own entries (it is not a model, not a
predictor name, and not a `seq` parameter). The same model could equally
be written in form 1:

``` yaml
models:
  male:
    _notes_: Fitted on the male subsample only.
    _value_:
      title: Male
      model_export: ./HTNPoRT-male-model-export.csv
```

If a value is given notes in both forms at once, the ones written beside
the value (form 1) win.

### Where notes can be attached

Every level of the file accepts notes, including:

| Location                                                    | Example                                                                               |
|-------------------------------------------------------------|---------------------------------------------------------------------------------------|
| `meta` and its fields                                       | `version: {_notes_: Bumped for the 2026 release, _value_: 1.0.0}`                     |
| The `models` map                                            | a `_notes_` key directly under `models:`                                              |
| A single model                                              | see form 2 above                                                                      |
| Any model field                                             | `title`, `model_export`, `model_color`                                                |
| A `reference_group`, and each variable in it                | `clc_age: {_notes_: Youngest age in the cohort, _value_: 20}`                         |
| A `predictor_allowable_values` map, and each variable in it | see the example below                                                                 |
| Each value of an explicit list of allowable values          | see [Annotating individual allowable values](#annotating-individual-allowable-values) |
| A `seq` specification, and each of its parameters           | `by: {_notes_: 0.1 keeps the slider responsive, _value_: 0.1}`                        |
| The `_all_` shared block, and anything inside it            | see [Notes and `_all_`](#notes-and-_all_)                                             |

A fuller example, annotating several levels at once:

``` yaml
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

### Annotating individual allowable values

Where a predictor’s allowable values are given as an explicit list, the
whole list and each value in it can be annotated. Notes on the list as a
whole describe the set of values:

``` yaml
predictor_allowable_values:
  diabx:
    _notes_: Diabetes status, as coded in the CCHS.
    _value_: [1, 2]
```

Notes on the individual values describe one value each, which is useful
for recording what a category code means:

``` yaml
predictor_allowable_values:
  diabx:
    - _notes_: No diabetes.
      _value_: 1
    - _notes_: Diabetes.
      _value_: 2
```

Values may be annotated individually, as a set, or both, and any value
in the list may be left unannotated. The allowable values themselves are
unaffected either way: the two examples above define exactly the same
values as `diabx: [1, 2]`.

### Notes and `_all_`

Notes travel with the values they annotate: when a value is copied out
of `_all_` into a model, its notes are copied with it. A model that
defines its own value at that path keeps its own value **and** its own
notes, and inherits neither.

``` yaml
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

Here `male` inherits `clc_age: 20` together with the note “Youngest age
in the derivation cohort”, while `female` keeps `clc_age: 25` and its
own note.

This applies to `_all_`’s own `_notes_` key as well: a `_notes_` written
directly in `_all_` is inherited as the note of every model that does
not carry a note of its own. If you want a note that describes only the
shared block, put it on a value inside `_all_` rather than on `_all_`
itself.

### Restrictions

- **`_value_` is a reserved key.** It may not be used as a model
  identifier or as a predictor variable name. `_notes_` is likewise
  reserved wherever notes are accepted.
- **Notes must be strings.** A single line, or a multi-line YAML block
  scalar (`_notes_: |`), is fine; structured values are rejected.
- **File-level notes are not retained.** A `_notes_` key at the very top
  of the file (a sibling of `meta` and `models`) is accepted but
  discarded; attach notes to `meta` instead.

### Reading notes from R

Notes are separated from the values they annotate when the file is
loaded, so the loaded model definitions hold plain values and gather
every note under a `$notes` element whose structure mirrors the
definitions. Developers extending the viewer look a note up with the
same sequence of keys that leads to the value:

``` r
info <- read_model_definitions("path/to/algorithm.yaml")

info$models$male$reference_group$clc_age
#> 20

get_notes(info, list("models", "male", "reference_group", "clc_age"))
#> "Youngest age in the derivation cohort."
```

A key may also be a **number**, addressing the value at that position
instead of by name. This is how the entries of a list are reached, since
they have no names of their own — the notes on the second allowable
value of `diabx` are:

``` r
get_notes(
  info,
  list("models", "male", "predictor_allowable_values", "diabx", 2)
)
```

Positions are 1-based, and work for named values too
(`list("models", 1)` is the first model). Because
[`c()`](https://rdrr.io/r/base/c.html) would turn a number given
alongside names into a string, a path that mixes names and positions
must be a [`list()`](https://rdrr.io/r/base/list.html).

See [Add your own tab and
plots](https://big-life-lab.github.io/algorithm-viewer/articles/howto-add-tab-and-plots.md)
for reading notes from inside a plot module.

## Complete example

``` yaml
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

## Related

- [Application configuration
  reference](https://big-life-lab.github.io/algorithm-viewer/articles/reference-app-configuration.md)
  — the file that references algorithm files.
- [What is Model
  Parameters?](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-model-parameters.md)
  — the format the `model_export` files conform to.
- [Add Algorithm Viewer configurations to your own Model Parameters
  repository](https://big-life-lab.github.io/algorithm-viewer/articles/howto-add-viewer-configs.md).
