# Viewing the Built-in HTNPoRT Algorithm

**Audience:** Anyone who wants to understand what the Algorithm Viewer’s
plots mean and how to read them. No statistical background is assumed.

**Prerequisites:** The app running in your browser ([Running the
Algorithm Viewer in your Web
Browser](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-running.md)).
No configuration or setup is needed — this tutorial uses the example
that loads by default.

**What you will have at the end:** the ability to interpret every plot
in the viewer, using HTNPoRT as a worked example.

------------------------------------------------------------------------

## About the example algorithm

When you run
[`run_app()`](https://big-life-lab.github.io/algorithm-viewer/reference/run_app.md)
with no arguments, the viewer loads the [Hypertension Population Risk
Tool (HTNPoRT)](https://github.com/Big-Life-Lab/htnport) — an algorithm
that predicts a person’s risk of developing hypertension. HTNPoRT ships
as two models:

- **Female**
- **Male**

Both are selected in the **Models** tab by default, so every plot shows
one curve per model, each in its own colour.

HTNPoRT uses predictors such as **age**, **body mass index (BMI)**,
**family history of hypertension**, and **diabetes status**. The full
model adds many more (marital status, education, physical activity,
smoking, sleep, and others); the reduced model uses a smaller subset.
You do not need to memorize the variable codes — the viewer labels the
controls for you.

## The idea behind most plots: the reference patient

The **Odds Ratio**, **Predicted Risk**, and **Relative Risk** tabs all
answer the same underlying question:

> *If I take a baseline “reference patient” and change **one** predictor
> across its whole range, what happens to their predicted outcome?*

(The **Me vs Ref** tab works differently — it compares two fully
specified profiles instead of varying a single predictor, and it defines
its own “Ref” profile rather than using the reference patient described
here. See [The Me vs Ref tab](#the-me-vs-ref-tab) below.)

The **reference patient** is defined in the **Reference** tab in the
sidebar. Each model has its own set of controls: sliders for continuous
variables (age, BMI) and radio buttons for categorical variables
(diabetes status, family history). The values that load by default come
from the algorithm’s configuration — for HTNPoRT, a young reference
patient (age 20) with a low BMI and no diabetes.

The **Predictor** dropdown above the plot chooses which single variable
is varied along the x-axis. Every *other* predictor stays fixed at its
reference value. This is why the reference patient matters: it is the
“everything else” the plot holds constant.

Try this now: set **Predictor** to **Age** and watch the Odds Ratio,
Predicted Risk, or Relative Risk plot redraw as a curve over age.

## The Odds Ratio tab

**What it shows.** How the *odds* of developing hypertension change as
the selected predictor varies, expressed *relative to the reference
patient*.

**How to read it.**

- A dashed horizontal line marks an odds ratio of **1.0**. At the
  reference value, the curve sits on this line — the reference patient
  is being compared to itself, so the ratio is 1.
- Points **above 1.0** mean higher odds of the outcome than the
  reference patient. An odds ratio of 2.0 means roughly twice the odds.
- Points **below 1.0** mean lower odds than the reference patient. An
  odds ratio of 0.5 means roughly half the odds.
- Hover over the curve to read the exact odds ratio at any x-axis value.

**Worked example.** With **Predictor** set to **Age**, the odds ratio
curve climbs as age increases: an older person has higher odds of
hypertension than the young reference patient, all else being equal.

**The Logarithmic checkbox.** Odds ratios are multiplicative — an odds
ratio of 0.5 (half) and 2.0 (double) are “equal but opposite” changes,
yet on a linear axis 2.0 looks four times further from 1.0 than 0.5
does. The **Logarithmic** checkbox (above the plot) switches the y-axis
to a log₁₀ scale, which places 0.5 and 2.0 an equal distance from 1.0
and makes wide-ranging curves easier to read. Use the linear scale when
the odds ratios stay close to 1.

## The Interaction Predictor

The **Interaction Predictor** dropdown (next to **Predictor**) is
optional and affects the **Odds Ratio** and **Relative Risk** plots
only.

When you choose an interaction predictor, the plot shows how a
**one-unit increase** in that second variable *modifies* the ratio (the
odds ratio or the relative risk, depending on the tab) of the primary
predictor at each x-axis value. In other words, it visualizes whether
two predictors amplify or dampen each other’s effect, rather than acting
independently.

Leave it set to `<empty>` if you only want to see a single predictor’s
effect.

## The Predicted Risk tab

**What it shows.** The *absolute* predicted probability of the outcome —
from 0% to 100% — as the selected predictor varies, with everything else
held at the reference values.

**How to read it.**

- The y-axis is a real probability. If the curve reads 8% at age 50, the
  model predicts an 8% chance of the outcome for a reference patient who
  is 50.

**Why it matters.** Odds ratios tell you about *relative* change but
hide the *magnitude*. A predictor can double the odds (odds ratio 2.0)
while moving the absolute risk from only 1% to 2% — a doubling that may
not be clinically important. The Predicted Risk tab is where you judge
whether a large relative effect corresponds to a large *absolute*
effect.

**Worked example.** Compare the Female and Male curves at the same age.
The vertical gap between them is the difference in absolute predicted
risk between the two models at that age.

## The Relative Risk tab

**What it shows.** The predicted risk at each x-axis value **divided
by** the predicted risk at the reference values.

**How to read it.**

- A value of **1.0** means no difference from the reference patient.
- Above **1.0** means higher risk; below **1.0** means lower risk.
- Like the Odds Ratio tab, this is a ratio — but a ratio of
  *probabilities* (risks), not of *odds*.

**Odds ratio vs. relative risk.** They answer subtly different questions
and are only close to each other when the outcome is rare. When risks
are high, an odds ratio and a relative risk for the same comparison can
differ noticeably. If you care about “how many times more likely,”
relative risk is the more direct reading; odds ratios are reported
because they are the natural output of the logistic models underneath.

## The Me vs Ref tab

**What it shows.** A comparison of a **personal profile (“Me”)** against
a **reference profile (“Ref”)**, broken down to show which predictors
drive the difference between them. Unlike the other tabs — which vary
one predictor across its full range — this tab compares two fully
specified people. It also does not use the **Predictor** dropdown or the
reference patient from the **Reference** tab: both profiles are set from
this tab’s own controls.

**Setting the two profiles.** When this tab is active, the sidebar
changes to a single panel with paired controls: a “Me” and a “Ref” value
for every predictor. Continuous variables get two sliders (each with a
number box for typing an exact value); categorical variables get a table
with a “Me” column and a “Ref” column. The two columns are independent,
so you can set them to the same or different levels. Both profiles start
at the first model’s default reference group; click **Reset** to return
them to those defaults.

**The summary panel (top).** For each selected model it reports:

- **Your estimated risk** — the predicted risk of the Me profile.
- **Reference risk** — the predicted risk of the Ref profile.
- **Overall RR** — the Me risk divided by the Ref risk (e.g. `1.5×`),
  followed by the absolute difference in percentage points
  (e.g. `+8.0 pts`).

**The main plot.** A horizontal chart with one row per predictor. Each
row isolates a single predictor’s contribution: it compares the full Me
profile against the Me profile with *only that one predictor* swapped to
its Ref value, holding everything else at the Me values. The row label
shows the predictor and its `Ref → Me` change. A reference line marks
“no difference” (Relative Risk = 1, or Absolute Difference = 0). Reading
the rows tells you which individual predictors contribute most to the
gap between Me and Ref.

**The Show dropdown** switches what the rows measure:

- **Relative Risk** — the ratio of risks (1 means no difference).
- **Absolute Difference** — the difference in risk in percentage points
  (0 means no difference).

**The drill-down subplot.** Click any row in the main plot to load a
subplot at the bottom. It shows the relative risk of Me versus Ref as
that clicked predictor takes on all of its values, with a dot marking
the current Me value. Click a different row to change which predictor is
shown.

## Comparing the two models

Because Female and Male are both selected, every plot draws both curves
at once, each in its own colour. This makes sex-based differences
immediate: at a given age and BMI, you can see directly how the two
models’ predicted risks and odds ratios differ. Uncheck one model in the
**Models** tab to focus on a single model.

## Putting it together: a suggested tour

1.  On the **Predicted Risk** tab, set **Predictor** to **Age**. Note
    how absolute risk rises with age, and how Female and Male differ.
2.  Switch to **Odds Ratio** for the same predictor. Notice the curve
    now expresses the *relative* change against the reference patient,
    crossing 1.0 at the reference age.
3.  Toggle **Logarithmic** on and off to see how the scale changes the
    shape.
4.  In the **Reference** tab, raise the reference patient’s BMI, then
    return to the plots — every curve re-centres on the new reference.
5.  Open **Me vs Ref**, set a “Me” profile that differs from “Ref” in a
    few variables, and read off which predictors drive the difference.

## Next steps

- Ready to load your own algorithm? [View your own algorithms in the
  Algorithm
  Viewer](https://big-life-lab.github.io/algorithm-viewer/articles/howto-view-your-algorithms.md).
- Curious what “Model Parameters format” means? [What is Model
  Parameters?](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-model-parameters.md).
