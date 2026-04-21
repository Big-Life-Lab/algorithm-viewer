#' Main R Shiny UI Function
#'
#' The main ui function for the Algorithm Viewer R Shiny App.
#'
#' @name app_ui
#' @noRd
#' @keywords internal
NULL

app_ui <- function(request) {
  # Allow browser access to files in the www directory (eg. images, favicons)
  shiny::addResourcePath(
    "www",
    directoryPath = system.file("extdata/www", package = utils::packageName())
  )

  shiny::fluidPage(
    title = "Algorithm Viewer",
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(
        rel = "icon", type = "image/png",
        sizes = "128x128", href = "/www/favicon-128x128.png"
      ),
      shiny::tags$link(
        rel = "icon", type = "image/png",
        sizes = "64x64", href = "/www/favicon-64x64.png"
      ),
      shiny::tags$link(
        rel = "icon", type = "image/png",
        sizes = "32x32", href = "/www/favicon-32x32.png"
      ),
      shiny::tags$link(
        rel = "icon", type = "image/png",
        sizes = "180x180", href = "/www/favicon-180x180.png"
      ),
      shiny::tags$link(
        rel = "apple-touch-icon",
        sizes = "180x180", href = "/www/favicon-180x180.png"
      )
    ),

    # Removes width of the controls in the sidebar so that they take up the full
    # width (instead of the default 300px).
    shiny::tags$style(shiny::HTML("
      .shiny-input-container:not(.shiny-input-container-inline) {
        width: unset;
      }
    ")),

    # Application title
    shiny::titlePanel(
      shiny::htmlOutput("ui_title")
    ),

    # Sidebar layout
    shiny::sidebarLayout(
      # Sidebar panel for inputs
      # Using mainPanel instead of the usual sidebarPanel because it looks
      # better
      shiny::mainPanel(
        width = 3,
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Models",
            icon = shiny::icon("atom"),
            shiny::br(),
            shiny::div(
              # Model selection and message
              shiny::htmlOutput("model_message"),
              shiny::div(
                style = ifelse(
                  config_allow_algorithms_selection(), "", "display: none"
                ),
                shiny::selectInput(
                  inputId = "algorithms",
                  label = "Preloaded Algorithms",
                  choices = NULL
                )
              ),
              shiny::div(
                style = ifelse(
                  config_allow_file_uploads(), "", "display: none"
                ),
                shiny::fileInput(
                  "upload",
                  "Upload Algorithm:",
                  accept = c(".zip", ".tar", ".gz")
                ),
                shiny::hr(),
                shiny::checkboxGroupInput(
                  inputId = "selected_model_ids",
                  label = "Models:"
                ),
              ),
              shiny::hr(
                style = ifelse(
                  config_allow_algorithms_selection() ||
                    config_allow_file_uploads(),
                  "",
                  "display: none"
                )
              ),

              # Predictor selection (will be populated dynamically)
              shiny::selectInput(
                inputId = "predictor",
                label = "Predictor:",
                choices = NULL
              ),

              # Interaction predictor selection (will be populated dynamically)
              shiny::selectInput(
                inputId = "interaction_predictor",
                label = "Interaction Predictor:",
                choices = NULL
              ),
              shiny::hr(),

              # Log or non-log scale
              shiny::checkboxInput("logarithmic", "Logarithmic", value = TRUE),
              shiny::hr(),

              # Algorithm Viewer version number
              shiny::div(
                style = "color: #999",
                paste0("Algorithm Viewer v", packageVersion(packageName()))
              )
            )
          ),

          # Predictor controls get added as children to #refgroup_controls
          shiny::tabPanel(
            "Reference",
            icon = icon("cog"),
            shiny::div(
              style = paste(
                "height: calc(100vh - 140px); margin-bottom: 20px;",
                "overflow-y: scroll"
              ),
              shiny::uiOutput("refgroup_controls")
            )
          )
        )
      ),

      # Main panel for displaying outputs
      shiny::mainPanel(
        width = 9,

        # Tabs for different views
        shiny::tabsetPanel(
          type = "tabs",
          id = "main_tabs",
          shiny::tabPanel(
            "Odds Ratio",
            icon = shiny::icon("chart-line"),
            shiny::uiOutput("or_plot")
          ),
          shiny::tabPanel(
            "Relative Risk",
            icon = shiny::icon("chart-line"),
            shiny::uiOutput("rr_plot")
          ),
          shiny::tabPanel(
            "Predicted Risk",
            icon = shiny::icon("chart-line"),
            shiny::uiOutput("pr_plot")
          ),
          shiny::tabPanel(
            "Exposed vs Unexposed",
            icon = shiny::icon("chart-line"),
            shiny::uiOutput("rr_exposed_vs_unexposed_plot")
          ),
          shiny::tabPanel(
            "Help",
            icon = shiny::icon("circle-question"),
            style = "max-width: 800px",
            shiny::br(),
            shiny::htmlOutput("help")
          )
        ),
        shiny::br()
      )
    )
  )
}

app_ui
