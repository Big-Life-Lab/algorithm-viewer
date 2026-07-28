# Documentation Writing Plan — Algorithm Viewer

A chronological plan for writing the full Algorithm Viewer documentation, based
on [DOCUMENTATION-PLAN.md](DOCUMENTATION-PLAN.md) (priority + framing) and
[DOCUMENTATION-TOC.md](DOCUMENTATION-TOC.md) (proposed table of contents).

**Owner:** Martin Wellman\
**Framework:** [Divio](https://docs.divio.com/documentation-system/) — Tutorials,
How-To Guides, Explanation (Discussions), Reference.

## Guiding principles

- **Write in priority order.** User docs first (the layer LLMs write worst),
  then intent/scope, then architecture (deprioritized — DeepWiki etc. cover it).
- **One deliverable at a time.** Each page is drafted, self-reviewed, and
  committed before starting the next. Don't batch.
- **Reuse existing material.** The `specs/` folder already holds source content:
  [CONFIG_SPECIFICATION.md](CONFIG_SPECIFICATION.md),
  [DEPLOYMENT.md](DEPLOYMENT.md), and
  [INDIVIDUAL_VS_REFERENCE_SPEC.md](INDIVIDUAL_VS_REFERENCE_SPEC.md) feed the
  reference and how-to sections. `README.md` feeds the intent/scope pages.
- **Don't overcomplicate.** Ship a first complete draft of each page; polish in a
  later pass rather than perfecting each page before moving on.

## Where docs live

Long-form docs are authored as **pkgdown vignettes** (`vignettes/*.Rmd`), which
render into the existing site (`docs/`, `_pkgdown.yml`). Keep `README.md` as
the concise entry point that links into the vignettes. Reference material for
the R API comes from roxygen docblocks (already in `man/`).

## Phase 0 — Setup (do once, first)

1. Scaffold the vignette setup: add `usethis::use_vignette()` (or equivalent)
   plumbing, ensure `knitr`/`rmarkdown` are in `DESCRIPTION` Suggests, and add
   `vignettes/` to `.Rbuildignore` handling as needed.
2. Create the `vignettes/` `.Rmd` files (one per deliverable) and wire them into
   `_pkgdown.yml` under an articles structure grouped by Divio category.
3. Add a top-level documentation index / landing article that links the four
   Divio sections.
4. Agree on filename/slug conventions and a page template (title, audience,
   prerequisites, body, next steps).

## Phase 1 — User documentation (TOP PRIORITY)

Write the Tutorials and the core How-To guides — the hands-on "using and
interpreting the viewer" layer.

### 1a. Tutorials (chronological, easiest first)

1. **Installing the Algorithm Viewer on your Computer** — `remotes::install_github`,
   R >= 4.1 prerequisites. (Source: README Quick Start.)
2. **Running the Algorithm Viewer in your Web Browser** — `run_app()`, what opens,
   basic orientation of the UI.
3. **Viewing the Built-in HTNPoRT Algorithm** — a guided walkthrough of the
   preloaded example: predictors, the plots (odds ratio, predicted risk, relative
   risk), and **how to interpret them**. This is the highest-value page — the
   interpretation guidance is what LLMs cannot generate well.

### 1b. How-To Guides (task-oriented)

4. **How to view your own algorithms in the Algorithm Viewer** — MUST cover the
   repo-to-viewer integration: wiring a repo (e.g. HTNPoRT) via a config file
   requires **cloning the repo locally first** — the viewer cannot read a repo
   remotely from GitHub. Call this constraint out explicitly and early.
   (Source: CONFIG_SPECIFICATION.md, config.yaml.)
5. **How to add Algorithm Viewer configurations to your own Model
   Parameters-based repository** — for maintainers of a Model Parameters repo:
   add the config file(s) the viewer needs so the repo can be loaded in the
   viewer. Complements #4 (loading an algorithm) and the config-file references
   (#11, #12). (Source: CONFIG_SPECIFICATION.md.)
6. **How to run the Algorithm Viewer as a Docker Container** — (Source: Dockerfile,
   docker-compose.yml, DEPLOYMENT.md.)
7. **How to run the Algorithm Viewer with ShinyProxy** — (Source: application.yml,
   DEPLOYMENT.md.)

## Phase 2 — Intent & scope (Discussions / Explanation)

README-level "why" content. Draft after user docs exist so it can link into them.

8. **Why did we build the Algorithm Viewer?** — purpose, use cases,
   appropriate vs. not-appropriate uses, features, roadmap. (Source: README About.)
9. **Including the Algorithm Viewer in Publications: Best Practices** — citation,
   reproducibility, how to reference a specific algorithm/config.
10. **What is Model Parameters?** — explain the [Model
    Parameters](https://github.com/Big-Life-Lab/model-parameters/) format that
    algorithms must conform to: what it is, why the viewer depends on it, and how
    it relates to an "algorithm." Foundational conceptual context that the HTNPoRT
    tutorial (#3) and the Algorithm configuration reference (#12) both lean on —
    write it early in Phase 2 and link to it from those pages. (Source: README,
    model-parameters repo.)

## Phase 3 — Reference

Drier, complete, generated-where-possible material. Written last in the user-doc
priority tier but kept accurate as Phases 1–2 reveal gaps.

11. **Application configuration** — the format of the
    `inst/extdata/config.yaml` file (preloaded algorithms, initial algorithm,
    upload/selection/URL flags).
12. **Algorithm configuration** — the format of the algorithm config files,
    e.g. `inst/extdata/models/htnport-full/htnport-full.yaml` and
    `inst/extdata/models/htnport-reduced/htnport-reduced.yaml`. (Source:
    CONFIG_SPECIFICATION.md)
13. **R API** — ensure roxygen docs on exported functions (`run_app()` etc.)
    are complete; let pkgdown generate the reference. Fill gaps in docblocks
    rather than hand-writing.

## Phase 4 — Architecture / code exploration (DEPRIORITIZED)

Only if time allows. AI tools (DeepWiki etc.) largely cover this. If written,
keep it a short orientation pointing at those tools plus a module map.

## Execution checklist (repeat per page)

For each numbered deliverable, in order:

1. Draft from the cited source material.
2. Run any code snippets / commands in the page to confirm they work.
3. Self-review for accuracy and audience fit (Divio category discipline: don't
   mix tutorial and reference in one page).
4. Wire into `_pkgdown.yml` navigation and build the site locally to check
   rendering.
5. Commit the single page.

## Deliverables summary

| # | Deliverable | Divio type | Phase |
|---|-------------|-----------|-------|
| 1 | Installing on your Computer | Tutorial | 1 |
| 2 | Running in your Web Browser | Tutorial | 1 |
| 3 | Viewing the Built-in HTNPoRT Algorithm | Tutorial | 1 |
| 4 | View your own algorithms (incl. local-clone integration) | How-To | 1 |
| 5 | Add Viewer configs to your own Model Parameters repo | How-To | 1 |
| 6 | Run as a Docker Container | How-To | 1 |
| 7 | Run with ShinyProxy | How-To | 1 |
| 8 | Why did we build the Algorithm Viewer? | Explanation | 2 |
| 9 | Including in Publications: Best Practices | Explanation | 2 |
| 10 | What is Model Parameters? | Explanation | 2 |
| 11 | Application configuration | Reference | 3 |
| 12 | Algorithm configuration | Reference | 3 |
| 13 | R API | Reference | 3 |
| — | Architecture / code exploration | Explanation | 4 (optional) |
