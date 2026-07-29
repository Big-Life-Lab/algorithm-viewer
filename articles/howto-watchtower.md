# Automatically update a deployed web app with Watchtower

**Audience:** Deployers running the Algorithm Viewer as a Docker
container who want a live web app to update itself whenever a new image
is published.

**Prerequisites:** The Algorithm Viewer already running as a Docker
container ([Run the Algorithm Viewer as a Docker
container](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.md));
the container started from an image in a registry (so there is a
published image to watch).

**What you will have at the end:**
[Watchtower](https://containrrr.dev/watchtower/) monitoring your running
container and automatically pulling and restarting it whenever a newer
image is pushed.

------------------------------------------------------------------------

## Why Watchtower

When the Algorithm Viewer image is rebuilt and pushed to a registry —
for example by the project’s [docker-publish GitHub Actions
workflow](https://github.com/Big-Life-Lab/algorithm-viewer/actions/workflows/docker-publish.yaml)
— a running container keeps using the *old* image until someone manually
pulls the new one and restarts.
[Watchtower](https://containrrr.dev/watchtower/) closes that gap: it
periodically checks the registry for a newer version of the image your
container was started from, and when it finds one it gracefully stops
the old container, pulls the new image, and starts a replacement with
the same configuration. The deployed web app stays current with no
manual redeploys.

> Watchtower can only update containers started from a **registry
> image** (one that was `docker pull`-ed). A container built locally
> from source (for example `docker compose up --build`) has no upstream
> image to watch — publish and pull the image first.

## Step 1 — Run the Algorithm Viewer from a published image

Start the viewer from the registry image rather than a local build, so
Watchtower has something to track:

``` bash
docker run -d --name algorithm-viewer -p 3838:3838 \
  ghcr.io/big-life-lab/algorithm-viewer:latest
```

Replace the image reference with the one your deployment uses. The `-d`
flag runs it in the background and `--name` gives it a stable name.

## Step 2 — Start Watchtower

Run Watchtower as its own container. It needs access to the host’s
Docker socket so it can manage other containers:

``` bash
docker run -d --name watchtower \
  -v /var/run/docker.sock:/var/run/docker.sock \
  containrrr/watchtower \
  algorithm-viewer
```

Naming `algorithm-viewer` at the end tells Watchtower to watch **only**
that container. Omit it to watch every container on the host.

By default Watchtower checks for updates on a periodic interval. To set
an explicit interval — for example every 5 minutes (300 seconds) — add
`--interval`:

``` bash
docker run -d --name watchtower \
  -v /var/run/docker.sock:/var/run/docker.sock \
  containrrr/watchtower \
  --interval 300 \
  algorithm-viewer
```

## Step 3 (optional) — Add Watchtower to Docker Compose

If you deploy with Docker Compose, add Watchtower as a service alongside
the viewer so both come up together:

``` yaml
services:
  algorithm-viewer:
    image: ghcr.io/big-life-lab/algorithm-viewer:latest
    ports:
      - "3838:3838"
    restart: unless-stopped

  watchtower:
    image: containrrr/watchtower
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock
    command: --interval 300 algorithm-viewer
    restart: unless-stopped
```

Note this uses `image:` (a published image) rather than the `build:`
directive from the basic Compose setup — Watchtower updates pulled
images, not locally built ones. Start it with:

``` bash
docker compose up -d
```

## How an update plays out

1.  A new Algorithm Viewer image is pushed to the registry.
2.  On its next check, Watchtower notices the `:latest` tag now points
    at a newer image and pulls it.
3.  Watchtower stops the running `algorithm-viewer` container and starts
    a new one from the new image, reusing the same ports, volumes, and
    environment.
4.  Users reaching the app now get the updated version. In-flight
    sessions are interrupted by the restart, so schedule or expect a
    brief blip when an update lands.

## Notes and cautions

- **Pin a tag deliberately.** Watching `:latest` means you get every
  published change automatically. If you want more control, publish and
  watch a specific version tag and move it when you are ready to roll
  forward.
- **Private registries.** If your image is private, give Watchtower
  registry credentials (for example by mounting a Docker config with a
  stored login). See the [Watchtower
  documentation](https://containrrr.dev/watchtower/) for the current
  options.
- **Cleanup.** Watchtower can remove the old image after updating;
  consult its documentation for the relevant flag if disk usage is a
  concern.

## Next steps

- [Run the Algorithm Viewer as a Docker
  container](https://big-life-lab.github.io/algorithm-viewer/articles/howto-docker.md)
  — the base deployment this guide builds on.
- [Run the Algorithm Viewer with
  ShinyProxy](https://big-life-lab.github.io/algorithm-viewer/articles/howto-shinyproxy.md).
