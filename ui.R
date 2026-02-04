library(dplyr)
library(shiny)
library(shinyWidgets)
library(bslib)
library(plotly)

# Standard height of the plots
plot_height <- "calc(100vh - 170px)"

ui <- fluidPage(
  title = "Algorithm Viewer",
  tags$head(
    tags$link(
      rel = "icon", type = "image/png",
      sizes = "128x128", href = "/favicon-128x128.png"
    ),
    tags$link(
      rel = "icon", type = "image/png",
      sizes = "64x64", href = "/favicon-64x64.png"
    ),
    tags$link(
      rel = "icon", type = "image/png",
      sizes = "32x32", href = "/favicon-32x32.png"
    ),
    tags$link(
      rel = "icon", type = "image/png",
      sizes = "180x180", href = "/favicon-180x180.png"
    )
  ),
  tags$style(HTML("
    .shiny-input-container:not(.shiny-input-container-inline) {
      width: unset;
    }
  ")),

  # Application title
  titlePanel(
    htmlOutput("ui_title")
  ),

  # This style gets rid of the 300px width of the controls within
  # the panels, to make them wider.
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    mainPanel(
      width = 3,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Models",
          icon = icon("atom"),
          br(),
          div(
            div(
              style = ifelse(allow_file_uploads, "", "display: none"),
              htmlOutput("model_message"),
              fileInput(
                "upload", "Upload Algorithm:",
                accept = c(".zip", ".tar", ".gz")
              ),
              hr(),
              checkboxGroupInput(
                inputId = "model_id",
                label = "Models:"
              ),
              hr()
            ),

            # Predictor selection (will be populated dynamically)
            selectInput(
              inputId = "predictor",
              label = "Predictor:",
              choices = NULL
            ),

            # Interaction predictor selection (will be populated dynamically)
            selectInput(
              inputId = "interaction_predictor",
              label = "Interaction Predictor:",
              choices = NULL
            ),
            hr(),

            # Log or non-log scale
            checkboxInput("logarithmic", "Logarithmic", value = TRUE),
          )
        ),
        # Controls for the reference groups get added as children to #refgroups
        tabPanel(
          "Reference",
          icon = icon("angle-double-down"),
          div(
            style = "height: calc(100vh - 150px); overflow-y: scroll",
            div(id = "refgroups")
          )
        )
      )
    ),

    # Main panel for displaying outputs
    mainPanel(
      width = 9,

      # Tabs for different views
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Odds Ratio",
          icon = icon("chart-line"),
          br(),
          plotlyOutput("or_plot", height = plot_height)
        ),
        tabPanel(
          "Predicted Risk",
          icon = icon("chart-line"),
          br(),
          plotlyOutput("pr_plot", height = plot_height)
        ),
        tabPanel(
          "Help",
          icon = icon("circle-question"),
          style = "max-width: 800px",
          br(),
          htmlOutput("help")
        )
      ),
      br()
    )
  )
)

ui
