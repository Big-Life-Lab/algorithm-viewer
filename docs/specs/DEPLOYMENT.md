# Algorithm Viewer Deployment Specification

## 1. Overview

This document describes the planned deployment strategies for the Algorithm
Viewer, an R Shiny application for visualizing and exploring health risk
prediction algorithms. The Algorithm Viewer allows researchers, clinicians, and
other stakeholders to interactively examine how predictor variables influence
model outcomes through odds ratio plots, predicted risk curves, and interaction
visualizations.

Three primary deployment options are proposed, each targeting a different use
case and audience. These options are not mutually exclusive and can be deployed
in parallel to serve the full range of users.

| Option | Use Case | Upload | Audience |
|--------|----------|--------|----------|
| [Public Web Application](#2-option-1-public-web-application) | General-purpose exploration | Yes | Any user |
| [Hosted Algorithm Showcase](#3-option-2-hosted-algorithm-showcase) | Sharing and publication | No | Viewers of a specific algorithm |
| [Local Development Tool](#4-option-3-local-development-tool) | Algorithm development | Yes (local files) | Scientists building models |

---

## 2. Option 1: Public Web Application

### 2.1 Description

A publicly accessible website where users can upload their own Algorithm
Definition files (YAML configuration with associated model data, packaged as
ZIP, TAR, or GZ archives) and interactively explore the resulting
visualizations. This is the most general deployment option, offering the full
feature set of the Algorithm Viewer to any user.

### 2.2 Target Audience

- Researchers exploring algorithms developed by others
- Clinicians reviewing risk prediction models
- Students learning about statistical modelling
- Any user with an Algorithm Definition file

### 2.3 Functional Requirements

1. Users can upload Algorithm Definition archives (ZIP, TAR, GZ).
2. All current visualization features are available: odds ratio plots,
   predicted risk curves, interaction visualizations, reference group
   adjustment, and model comparison.
3. No user account or authentication is required.
4. Uploaded data is not persisted on the server beyond the user's session.
5. A help page is available to guide users through the interface and the
   Algorithm Definition file format.
6. An example algorithm may optionally be pre-loaded so that new users can
   explore the interface immediately without needing to upload a file.
7. The configuration file should have a flag to specify that users can upload
   their own Algorithm Definitions. An additional configuration option can
   specify which pre-loaded Algorithm should be displayed by default (before a
   user uploads their own algorithm).

### 2.5 Deployment Methods

Candidate hosting platforms include:

- **Shinylive (static hosting)** -- The application can be deployed as a
  Shinylive app on any static file host (GitHub Pages, Netlify, institutional
  web hosting, or any CDN). Shinylive runs R entirely in the browser via WebR,
  requiring no server-side R process. File uploads and processing are handled
  within the browser. This is the simplest deployment option, as it requires no
  server infrastructure.

Additional server-based hosting platforms include:

- **Posit Connect (formerly RStudio Connect)** -- A commercial platform for
  deploying Shiny applications with built-in scaling, authentication, and
  monitoring.
- **Shiny Server (Open Source or Pro)** -- Self-hosted R Shiny server. The
  open-source edition provides basic hosting; the Pro edition adds
  authentication, SSL, and load balancing.
- **Cloud VM or container** -- Deploy on a cloud provider (AWS, GCP, Azure,
  DigitalOcean) using Docker (see [Section 5.1](#51-docker-image)) or a bare R
  installation with Shiny Server.
- **Big Life Lab server** -- Hosted on existing Big Life Lab infrastructure.

### 2.6 Security Considerations

These security considerations apply to when the app is running on a server,
rather than locally on the user's own computer/browser.

- Uploaded files should be processed in an isolated temporary directory and
  cleaned up after the session ends.
- Maximum upload size should be enforced (currently 30 MB via
  `shiny.maxRequestSize`).
- Uploaded archives should be validated: only YAML and CSV files should be
  processed, and path traversal in archive entries should be rejected.
- Rate limiting or session limits may be appropriate to prevent abuse on a
  public-facing deployment.

### 2.7 Scalability

These scalability considerations apply to when the app is running on a server,
rather than locally on the user's own computer/browser.

For moderate traffic, a single Shiny Server instance is sufficient. If usage
grows, consider:

- **Shiny Server Pro** or **Posit Connect** for multi-process load balancing.
- **ShinyProxy** (open-source) for container-based scaling, where each user
  session launches a dedicated Docker container.

---

## 3. Option 2: Hosted Algorithm Showcase

### 3.1 Description

A deployment mode in which users **cannot** upload their own data. Instead, one
or more predefined algorithms are bundled with the application and made
available for viewing. This mode is designed for sharing algorithms with others
-- for example, so that a scientist can include a URL in a publication or share
it with collaborators, allowing anyone to explore the algorithm interactively
without needing to install software or obtain the Algorithm Definition files.

### 3.2 Target Audience

- Readers of a scientific publication who want to explore the algorithm
  described in the paper.
- Collaborators or peer reviewers examining a shared model.
- Conference attendees or workshop participants directed to a live demo.
- The general public, for algorithms intended for broad dissemination.

### 3.3 Functional Requirements

1. File uploads are disabled.
2. One or more predefined algorithms are bundled with the deployment.
3. All visualization features (odds ratio plots, predicted risk curves,
   interaction visualizations, reference group adjustment, model comparison)
   remain fully functional.
4. If multiple algorithms are available, the user can select which algorithm to
   view.
5. A specific algorithm can be pre-selected via a URL parameter (e.g.,
   `?algorithm=htnport-full`), allowing direct linking to a particular
   algorithm.
6. The interface may optionally include metadata about the algorithm (e.g.,
   citation information, a link to the associated publication, or author
   contact details).
7. The configuration file should set a flag to indicate that users cannot
   upload their own algorithms. There should also be configuration options to
   specify which algorithms the users can view.

### 3.5 URL-Based Algorithm Selection

To support linking to specific algorithms at a shared base URL, the application
should accept a GET query parameter specifying which algorithm to display.

**Proposed URL format:**

```
https://viewer.biglifelab.ca/?algorithm=htnport-full
```

**Implementation approach:**

The application reads the `algorithm` query parameter at startup using
`shiny::parseQueryString()`. If the parameter is present and maps to a known
algorithm identifier, that algorithm is loaded. If the parameter is absent or
invalid, a default algorithm or a selection menu is shown.

A registry of available algorithms is maintained in `config.yaml`.

### 3.6 Sub-Option A: Centralized Hosting on Big Life Lab Server

A single deployment on Big Life Lab infrastructure hosts algorithms for
multiple scientists. Scientists submit their Algorithm Definition files to be
added to the registry, and each algorithm is accessible at a unique URL via the
`?algorithm=` parameter.

**Advantages:**

- Scientists do not need to manage their own hosting infrastructure.
- A single, well-known domain (e.g., `viewer.biglifelab.ca`) provides a
  recognizable and trustworthy URL for publications.
- Centralized maintenance and updates to the Algorithm Viewer software.

**Considerations:**

- A submission and review process is needed for adding new algorithms.
- Storage and computing costs scale with the number of hosted algorithms.
- Clear policies are needed for how long algorithms remain hosted and whether
  they can be updated or removed.

### 3.7 Sub-Option B: Self-Hosted by Scientists

Scientists deploy their own instance of the Algorithm Viewer, bundled with
their specific algorithm(s), on their own hosting resources. This gives
scientists full control over the deployment and does not depend on Big Life Lab
infrastructure.

**Deployment methods for self-hosting:**

1. **Shinylive (static hosting)** -- The application can be deployed as a
   static site on GitHub Pages, Netlify, institutional web hosting, or any
   static file server. Shinylive runs R entirely in the browser via WebR,
   requiring no server-side R process. This is the lowest-maintenance
   self-hosting option, as no server infrastructure is needed.

2. **Docker container** -- The scientist downloads a pre-built Docker image
   (see [Section 5.1](#51-docker-image)), adds their Algorithm Definition
   files, and deploys the container on any Docker-capable host (cloud VM,
   institutional server, or a container platform such as AWS ECS, Google Cloud
   Run, or Azure Container Apps).

3. **Shiny Server** -- The scientist installs Shiny Server on their own server
   and deploys the Algorithm Viewer as a standard Shiny application.

**Template repository approach:**

To simplify self-hosting, a template GitHub repository could be provided that
scientists can fork or clone. The template would include:

- The Algorithm Viewer application code.
- A `data/models/` directory where the scientist places their Algorithm
  Definition files.
- A pre-configured `config.yaml` with `allow_file_uploads: FALSE`.
- A GitHub Actions workflow for automated deployment to GitHub Pages (via
  Shinylive) or to a container registry.
- Documentation guiding the scientist through the setup process.

---

## 4. Option 3: Local Development Tool

### 4.1 Description

A deployment mode targeted at scientists who are actively developing and
refining their prediction algorithms. The goal is to provide a fast,
low-friction way for scientists to visualize their models during development so
that they can better understand relationships between variables, identify
unexpected patterns, and verify that their models behave as expected.

This option prioritizes ease of use: the scientist should be able to view their
algorithm with minimal or no coding effort.

### 4.2 Target Audience

- Scientists and statisticians building risk prediction models.
- Research teams iterating on model specifications.
- Data scientists performing exploratory analysis of model behavior.

### 4.3 Functional Requirements

1. The scientist can view their algorithm locally on their own computer.
2. Setup requires minimal or no R coding beyond a single function call or
   command.
3. Changes to the Algorithm Definition files are reflected quickly (ideally by
   refreshing the browser or re-running a single command).
4. File uploads are enabled so that the scientist can load different Algorithm
   Definition files during a session.
5. The tool works without an internet connection once installed.

### 4.4 Deployment Methods

#### 4.4.1 R Function Call (Simplest)

The most direct approach for scientists who already have R installed. A wrapper
function in the `algorithm-viewer` package (or a standalone script) launches
the application with a single call. Either an Algorithm Viewer configuration
file or an Algorithm Definition file can be specified as a parameter.

**Advantages:**

- Minimal setup for users already working in R/RStudio.
- Integrates naturally into an R-based data science workflow.
- No containerization or web server knowledge required.
- The viewer opens in the user's default web browser or in the RStudio Viewer
  pane.

**Requirements:**

- R (>= 4.0) installed.
- Package dependencies are automatically installed.

#### 4.4.2 Docker Container

A pre-built Docker image provides a fully self-contained environment that does
not require the scientist to install R or manage package dependencies. The
Docker image can be run, allowing the user to view the Algorithm Viewer in a
web browser.

**Advantages:**

- No R installation required.
- Reproducible environment; no dependency version conflicts.
- Works on any platform with Docker support (Windows, macOS, Linux).

**Considerations:**

- Docker must be installed, which may be unfamiliar to some scientists.
- Container image size may be large due to R and package dependencies.

#### 4.4.3 Shinylive (Browser-Only)

Shinylive allows scientists to run the Algorithm Viewer entirely in their
browser without any server-side R process. A scientist could download a
Shinylive export of the application, open the `index.html` file in their
browser, and use the file upload feature to load their algorithm.

**Advantages:**

- No installation of any kind required beyond a web browser.
- Fully offline-capable after the initial download.

**Considerations:**

- Performance may be slower than a native R process for large models.

#### 4.4.4 RStudio Add-in (Future Consideration)

An RStudio add-in could integrate the Algorithm Viewer directly into the
RStudio IDE, allowing scientists to launch the viewer from a menu or keyboard
shortcut while working on their algorithm code.

**Advantages:**

- Tight integration with the scientist's existing development environment.
- Could automatically detect Algorithm Definition files in the current project.

**Considerations:**

- Additional development effort to create and maintain the add-in.
- Only available to RStudio users.

---

## 5. Shared Infrastructure

### 5.1 Docker Image

A Docker image should be created and published that can be used by both the
public web application (Option 1) and the local development tool (Option 3).
The Hosted Algorithm Showcase (Option 2) can also use this image when not using
Shinylive.

The image should be published to a container registry (e.g., Docker Hub, GitHub
Container Registry) and tagged with version numbers corresponding to Algorithm
Viewer releases.

A working [Dockerfile](Dockerfile) is already available in the Algorithm Viewer
repository.

### 5.2 CI/CD Pipeline

The existing GitHub Actions workflow (`.github/workflows/deploy-app.yaml`)
handles Shinylive deployment to GitHub Pages. Additional workflows should be
added for:

1. **Docker image build and publish** -- Triggered on tagged releases. Builds
   the Docker image and pushes it to a container registry.
2. **Automated testing** -- Runs on pull requests and pushes to validate that
   the application starts correctly and core functionality works.

---

## 6. Comparison of Deployment Methods

| Method | Server Required | R Required | Internet Required | Offline Capable | File Upload | Scalability |
|--------|:-:|:-:|:-:|:-:|:-:|:-:|
| Posit Connect | Yes | On server | Yes | No | Yes | High |
| Shiny Server | Yes | On server | Yes | No | Yes | Medium |
| Docker | Yes | In container | For pull only | Yes (after pull) | Yes | High |
| ShinyProxy | Yes | In containers | Yes | No | Yes | High |
| Shinylive (GitHub Pages) | No | No (WebR) | Yes | Partial | Yes | Static |
| Shinylive (local) | No | No (WebR) | No | Yes | Yes | N/A |
| R function call | No | On machine | No | Yes | Yes | N/A |
| Cloud VM | Yes | On VM | Yes | No | Yes | High |

---

## 7. Recommended Approach

A phased rollout is suggested:

### Phase 1: Local Development Tool (Option 3)

Package the Algorithm Viewer as an installable R package with a `run()`
function, allowing scientists to launch the viewer with a single command. This
delivers immediate value to the primary user base (scientists developing
algorithms) with minimal infrastructure investment. Additionally, create and
publish a Docker image for scientists who prefer a container-based workflow or
do not have R installed.

### Phase 2: Public Web Application (Option 1)

Deploy the Algorithm Viewer to a Big Life Lab server or a cloud platform. This
makes the tool accessible to a broader audience without requiring any local
installation. Use the Docker image from Phase 1 to simplify deployment.

### Phase 3: Hosted Algorithm Showcase (Option 2)

Implement URL-based algorithm selection and the algorithm registry in
`config.yaml`. Deploy a centralized instance on Big Life Lab infrastructure.
Provide a template repository for scientists who prefer self-hosting.

---

## 8. Open Questions

1. **Domain and URL structure** -- What domain should the public deployment
   use? Should the showcase (Option 2) be at the same domain as the public
   application (Option 1), at a subdomain, or at a separate domain?
2. **Algorithm submission process** -- For the centralized showcase (Option
   2A), what is the process for scientists to submit algorithms? Is manual
   review required?
3. **Algorithm retention policy** -- How long should algorithms remain hosted
   on the centralized showcase? Should there be a process for removal or
   archival?
4. **Custom branding** -- Should the Hosted Algorithm Showcase (Option 2)
   support custom branding or theming for individual scientists or
   institutions?
5. **Usage analytics** -- Should any deployment option collect anonymized usage
   analytics (e.g., number of sessions, algorithms viewed)?
6. **Access control** -- Should the showcase support private algorithms (e.g.,
   password-protected or restricted to specific users) for pre-publication
   sharing?
