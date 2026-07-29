# Run the Algorithm Viewer with ShinyProxy

**Audience:** Deployers who want to serve the Algorithm Viewer to
multiple users, with each user session running in its own isolated
container.

**Prerequisites:**

- A local clone of the [algorithm-viewer
  repository](https://github.com/Big-Life-Lab/algorithm-viewer) (it
  includes a sample `application.yml`).
- [Docker](https://docs.docker.com/get-docker/) installed and running.
- A Java runtime environment (required by ShinyProxy).
- The Algorithm Viewer Docker image built locally (see [Run the
  Algorithm Viewer as a Docker
  container](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.md)).

For installing Java and Docker, see the [ShinyProxy Getting Started
Guide](https://www.shinyproxy.io/documentation/getting-started/).

------------------------------------------------------------------------

## What ShinyProxy gives you

[ShinyProxy](https://www.shinyproxy.io/) is an open-source server for
hosting Shiny apps as a multi-user service. When a user opens the app,
ShinyProxy launches a dedicated Docker container for that session and
tears it down when the session ends. This provides per-user isolation
and container-based scaling — useful for a shared or public deployment.

## Step 1 — Build the Algorithm Viewer image

ShinyProxy launches the app from a Docker image. Build it first, tagged
`algorithm-viewer:latest` (the tag the sample config expects):

``` bash
docker build -t algorithm-viewer:latest .
```

See the [Docker
how-to](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.md)
for details.

## Step 2 — Review the sample `application.yml`

The repository root contains a sample
[`application.yml`](https://github.com/Big-Life-Lab/algorithm-viewer/blob/main/application.yml)
— a ShinyProxy configuration that already defines an `algorithm-viewer`
app spec pointing at the `algorithm-viewer:latest` image:

``` yaml
proxy:
  title: Big Life Lab Shiny Apps
  port: 8080
  authentication: none
  docker:
    port-range-start: 20000
  specs:
    - id: algorithm-viewer
      display-name: Algorithm Viewer
      description: An application for visualizing health risk prediction algorithms, displaying various curves and comparisons.
      container-image: algorithm-viewer:latest
```

By default it runs with `authentication: none` and serves on port 8080.

## Step 3 — Enable authentication (recommended for shared deployments)

To require a login, set `authentication: simple` and define users.
**Never hard-code real credentials in `application.yml`.** ShinyProxy is
a Spring Boot application, so it resolves `${ENV_VAR}` placeholders from
environment variables with an optional `${ENV_VAR:default}` fallback.
The sample config is already set up for this:

``` yaml
proxy:
  authentication: simple
  users:
    - name: ${SHINYPROXY_USERNAME:biglifelab}
      password: ${SHINYPROXY_PASSWORD}
      groups: scientist
```

Set a strong password via an environment variable before starting
ShinyProxy:

``` bash
export SHINYPROXY_USERNAME=biglifelab
export SHINYPROXY_PASSWORD='a-strong-secret'
```

When running ShinyProxy in Docker, either forward values already
exported in your shell (the bare `-e NAME` form copies the value from
the host shell):

``` bash
docker run -e SHINYPROXY_USERNAME -e SHINYPROXY_PASSWORD ... shinyproxy
```

or set them inline:

``` bash
docker run -e SHINYPROXY_USERNAME=biglifelab \
           -e SHINYPROXY_PASSWORD='a-strong-secret' ... shinyproxy
```

The values are substituted at startup and never committed in plain text.

## Step 4 — Start ShinyProxy

Run ShinyProxy with the `application.yml` in the working directory,
following the [ShinyProxy
documentation](https://www.shinyproxy.io/documentation/getting-started/)
for your chosen method (the standalone JAR or the official ShinyProxy
Docker image). ShinyProxy reads `application.yml`, and when a user opens
the Algorithm Viewer it launches a container from the
`algorithm-viewer:latest` image on a port from the `port-range-start`
range.

Open <http://localhost:8080> to reach the ShinyProxy landing page and
launch the Algorithm Viewer.

## Customization notes

- **Logo and theme.** The sample config has commented-out `logo-url` and
  `template-path` options for custom branding.
- **Logging.** ShinyProxy writes to `shinyproxy.log` as configured under
  the `logging:` key.
- **Which algorithm is served.** The container serves whatever its image
  is configured to run — by default the built-in HTNPoRT configuration.
  To serve a different algorithm, build an image that bundles your
  algorithm and config (see the [Docker
  how-to](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.md))
  and reference that image in `container-image`.

## Next steps

- [Automatically update a deployed web app with
  Watchtower](https://big-life-lab.github.io/algorithm-viewer/articles/howto-watchtower.md).
- [Run the Algorithm Viewer as a Docker
  container](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.md)
  — the single-container alternative.
