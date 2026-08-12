# Launch the Algorithm Viewer Shiny Application

Main entry point for the Algorithm Viewer app. Initializes configuration
and starts the Shiny server.

## Usage

``` r
run_app(
  config = NULL,
  port = getOption("shiny.port"),
  host = getOption("shiny.host", "127.0.0.1")
)
```

## Arguments

- config:

  Path to a YAML configuration file, or `NULL` to use the built-in
  example HTNPoRT configuration file (located at
  inst/extdata/config.yaml).

- port:

  Port number for the Shiny server. Defaults to
  `getOption("shiny.port")`. If `NULL` (the default when the
  `shiny.port` option is not set), a random available port is chosen.

- host:

  Host address for the Shiny server. Defaults to
  `getOption("shiny.host", "127.0.0.1")`.

## Value

A Shiny app object (invisibly), as returned by
[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html).
