# What is Model Parameters?

**Audience:** Anyone who wants to understand the format the Algorithm
Viewer depends on, and why it depends on it.

------------------------------------------------------------------------

## The short version

[Model Parameters](https://github.com/Big-Life-Lab/model-parameters/) is
a standardized, file-based format — developed by Big Life Lab — for
describing a statistical prediction model completely enough that
software can *evaluate* it. Instead of shipping a model as bespoke code,
you ship a set of CSV files that declare the model’s variables, how each
input is transformed, and the coefficients that combine them into a
prediction.

The Algorithm Viewer can only visualize algorithms expressed in this
format. That is the foundational dependency behind everything else in
this documentation.

## Why a format like this is needed

A fitted model — say a logistic regression predicting hypertension — is
more than its coefficients. To turn a person’s raw inputs into a
prediction you also need to know:

- Which variables are inputs, and whether each is continuous or
  categorical.
- How each raw input is transformed before it enters the equation
  (recoding categories, centring, splines, interaction terms, and so
  on).
- What the coefficients are and how they combine.
- How the linear predictor is mapped to a final output (for example, a
  logistic link mapping to a probability).

If all of that lives inside a script, only that script can run the
model. Model Parameters instead captures it as *data* in a defined
layout, so that a single, general-purpose engine can read any conforming
model and evaluate it. That engine is the [Model Parameters
Pipeline](https://github.com/Big-Life-Lab/model-parameters-pipeline),
which the Algorithm Viewer uses under the hood to compute every point on
every plot.

## How it relates to an “algorithm”

In the Algorithm Viewer’s terminology:

- An **algorithm** is a family of models that make a similar prediction
  from the same set of inputs.
- A **model** is one evaluatable instance of that algorithm.

Model Parameters is the format each **model** is expressed in. The
[algorithm YAML
file](https://big-life-lab.github.io/algorithm-viewer/articles/reference-algorithm-configuration.md)
that the viewer reads is a thin layer *on top of* Model Parameters: it
names the algorithm, lists its models, and — through each model’s
`model_export` field — points at the Model Parameters files that
actually define that model.

    Algorithm (e.g. HTNPoRT)
    ├── algorithm YAML  ── viewer-specific: titles, reference groups, allowable values
    │    └── model_export ──▶ Model Parameters files (CSV) ── the actual model
    ├── model: Female  ──▶ its own Model Parameters files
    └── model: Male    ──▶ its own Model Parameters files

So the algorithm YAML is *how the viewer finds and presents* the models,
and Model Parameters is *what the models are*.

## The model export file

Each model in an algorithm YAML has a `model_export` field pointing at a
**model export CSV**. This is the entry point into that model’s Model
Parameters files: it catalogs the data files the model needs and,
critically, specifies how input variables are transformed to produce the
output values that the viewer plots. Its format and the format of the
files it references are documented in the [Model Parameters
documentation](https://big-life-lab.github.io/model-parameters/2-model-parameter-files.html).

Two files worth knowing about by name:

- **`variable-details.csv`** — describes each variable, including its
  allowable values. The viewer derives the x-axis values and the
  reference-group controls from this file when they are not overridden
  in the algorithm YAML’s `predictor_allowable_values`.
- **the model export CSV** — the catalog described above, referenced
  directly by `model_export`.

## Why the viewer depends on it

Because Model Parameters is a general format with a general evaluation
engine, the Algorithm Viewer does not need to know anything about *your
specific model*. It does not contain HTNPoRT’s equations, or anyone
else’s. It knows only how to:

1.  read an algorithm YAML,
2.  hand each model’s Model Parameters files to the pipeline, and
3.  plot what the pipeline returns.

This is what lets the same application visualize any conforming
algorithm — the one thing every viewable algorithm has in common is that
it speaks Model Parameters.

## Learn more

- [Model Parameters repository and
  documentation](https://github.com/Big-Life-Lab/model-parameters/)
- [Model Parameters file
  formats](https://big-life-lab.github.io/model-parameters/2-model-parameter-files.html)
- [Model Parameters
  Pipeline](https://github.com/Big-Life-Lab/model-parameters-pipeline)

## Next steps

- [Add Algorithm Viewer configurations to your own Model Parameters
  repository](https://big-life-lab.github.io/algorithm-viewer/articles/howto-add-viewer-configs.md)
  — put this into practice.
- [Algorithm configuration
  reference](https://big-life-lab.github.io/algorithm-viewer/articles/reference-algorithm-configuration.md)
  — the YAML layer that sits on top of Model Parameters.
