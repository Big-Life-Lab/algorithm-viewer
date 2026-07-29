# Why did we build the Algorithm Viewer?

**Audience:** Anyone deciding whether the Algorithm Viewer is the right
tool, or wanting to understand its purpose and scope.

------------------------------------------------------------------------

## The problem

Health risk prediction models are usually shared as equations,
coefficient tables, or code. That form is precise but hard to
*understand*: it is difficult to see, at a glance, how a predictor such
as age or BMI actually shifts the predicted outcome, how two models
differ, or whether a large relative effect corresponds to a large
absolute one. Reviewers, collaborators, clinicians, and the researchers
building the models all need a way to *see* a model’s behaviour, not
just read its parameters.

The Algorithm Viewer exists to make that behaviour visible and
interactive.

## What it is

The Algorithm Viewer is an R Shiny application, developed by [Project
Big Life](https://www.projectbiglife.ca/) at The Ottawa Hospital, for
visualizing health risk prediction algorithms. It plots a model’s
behaviour as interactive curves — odds ratio, predicted risk, and
relative risk — and lets you adjust a baseline “reference patient” and
compare models side by side. It works with any algorithm expressed in
the [Model
Parameters](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-model-parameters.md)
format developed by Big Life Lab.

The goal is to turn a static model specification into something a
researcher or clinician can explore directly: change a predictor, watch
the curve move, compare two models, and build intuition about what the
model does.

## Who it is for

- **Researchers and statisticians** building risk-prediction models, who
  want to sanity-check behaviour during development — verifying that
  curves move in the expected direction and spotting unexpected
  patterns.
- **Clinicians** reviewing a model, who want to understand its
  implications without reading its mathematics.
- **Readers of a publication**, who can be given a link to explore the
  exact algorithm a paper describes (see [Including the Algorithm Viewer
  in
  publications](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-publications.md)).
- **Students** learning how statistical prediction models behave.

## What it is good for

- Seeing how a single predictor moves the outcome across its whole
  range.
- Distinguishing *relative* effects (odds ratio, relative risk) from
  *absolute* effects (predicted risk) — a distinction that matters
  clinically and is easy to lose in a coefficient table.
- Comparing related models (for example sex-stratified models) on the
  same axes.
- Exploring “what-if” and personal-profile comparisons through the Me vs
  Ref tab.
- Sharing a model interactively without asking the audience to install
  anything (when deployed as a web app).

## What it is *not* for

- **It is not a modelling or fitting tool.** The viewer visualizes
  models that already exist; it does not estimate, fit, or validate
  them. The statistics come from the underlying Model Parameters
  pipeline, not from the viewer.
- **It is not a clinical decision-making tool.** The plots are for
  exploration and understanding. Individual risk estimates should not be
  used to make care decisions unless the underlying algorithm has been
  validated and approved for that purpose.
- **It does not read algorithms directly from the internet.** The files
  it visualizes must be available on the local filesystem (see [View
  your own
  algorithms](https://big-life-lab.github.io/algorithm-viewer/articles/howto-view-your-algorithms.md)).
- **It only understands the Model Parameters format.** An algorithm must
  be expressed in that format to be viewable.

## Features at a glance

- **Multiple plot types** — odds ratio, relative risk, and predicted
  risk, for both continuous and categorical predictors, plus
  visualization of predictor interactions.
- **Model comparison** — load and compare multiple models side by side,
  each in a distinct colour, including sex-stratified or otherwise
  stratified models.
- **Reference group configuration** — adjust the baseline predictor
  values interactively to re-centre every plot on a reference patient of
  your choosing.
- **Personal-profile comparison** — the Me vs Ref tab compares a
  personal profile against a reference and breaks down which predictors
  drive the difference.
- **Flexible deployment** — run it locally in R, as a Docker container,
  via ShinyProxy for multi-user access, or as a browser-only Shinylive
  build.

## Where it is heading

The Algorithm Viewer’s deployment roadmap envisions three complementary
modes, rolled out in phases:

1.  **Local development tool** — the installable R package and Docker
    image (available today), letting scientists view their models with a
    single command.
2.  **Public web application** — a hosted deployment where users can
    upload and explore their own algorithms without installing anything.
3.  **Hosted algorithm showcase** — a mode where uploads are disabled
    and specific algorithms are shared for viewing, linkable by URL —
    designed for publications and dissemination.

These options are not mutually exclusive; they target different
audiences and can run in parallel. The design details and open questions
are discussed in the project’s deployment specification.

## Next steps

- [What is Model
  Parameters?](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-model-parameters.md)
  — the format the viewer depends on.
- [Viewing the Built-in HTNPoRT
  Algorithm](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-htnport.md)
  — see the viewer in action.
