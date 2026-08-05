# Algorithm Viewer Documentation

The Algorithm Viewer is an R Shiny application for visualizing health
risk prediction algorithms. It plots odds ratio, predicted risk, and
relative risk curves for algorithms that conform to the [Model
Parameters](https://github.com/Big-Life-Lab/model-parameters/) format
developed by Big Life Lab, and lets you compare models side by side and
adjust the reference group interactively.

This documentation is organized following the [Divio documentation
system](https://docs.divio.com/documentation-system/) into four kinds of
material. Start with whichever matches what you are trying to do.

## Tutorials

Learning-oriented, start-to-finish walkthroughs. If you are new to the
Algorithm Viewer, work through these in order.

- [Installing the Algorithm Viewer on your
  Computer](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-installing.md)
- [Running the Algorithm Viewer in your Web
  Browser](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-running.md)
- [Viewing the Built-in HTNPoRT
  Algorithm](https://big-life-lab.github.io/algorithm-viewer/articles/tutorial-htnport.md)
  — the highest-value tutorial: how to read and interpret every plot.

## How-to guides

Task-oriented recipes for people who already know the basics.

- [View your own algorithms in the Algorithm
  Viewer](https://big-life-lab.github.io/algorithm-viewer/articles/howto-view-your-algorithms.md)
- [Add Algorithm Viewer configurations to your own Model Parameters
  repository](https://big-life-lab.github.io/algorithm-viewer/articles/howto-add-viewer-configs.md)
- [Add your own tab and
  plots](https://big-life-lab.github.io/algorithm-viewer/articles/howto-add-tab-and-plots.md)
  — for developers extending the viewer with a new plot type.
- [Run the Algorithm Viewer as a Docker
  container](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.md)
- [Run the Algorithm Viewer with
  ShinyProxy](https://big-life-lab.github.io/algorithm-viewer/articles/howto-shinyproxy.md)
- [Automatically update a deployed web app with
  Watchtower](https://big-life-lab.github.io/algorithm-viewer/articles/howto-watchtower.md)

## Explanation

Understanding-oriented discussion of the “why” behind the project.

- [Why did we build the Algorithm
  Viewer?](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-why.md)
- [Including the Algorithm Viewer in publications: best
  practices](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-publications.md)
- [What is Model
  Parameters?](https://big-life-lab.github.io/algorithm-viewer/articles/explanation-model-parameters.md)

## Reference

Information-oriented, precise descriptions of the configuration formats
and the R API.

- [Application
  configuration](https://big-life-lab.github.io/algorithm-viewer/articles/reference-app-configuration.md)
  — the format of the `config.yaml` file passed to
  [`run_app()`](https://big-life-lab.github.io/algorithm-viewer/reference/run_app.md).
- [Algorithm
  configuration](https://big-life-lab.github.io/algorithm-viewer/articles/reference-algorithm-configuration.md)
  — the format of an algorithm’s YAML definition file.
- [R API
  reference](https://big-life-lab.github.io/algorithm-viewer/reference/index.md)
  — the exported R functions, generated from the package source.

## A note on terminology: algorithm vs. model

Throughout this documentation, an **algorithm** is a family of models
that each perform a similar prediction using the same set of inputs. A
**model** is a single instance of the algorithm that can be evaluated.
For example, HTNPoRT is an algorithm for predicting the risk of
hypertension; within it there are two models — one for female
individuals and one for male individuals.
