source("R/utils/config.R")

# Standard height of the plots
plot_height <- "calc(100vh - 170px)"

ui <- fluidPage(
  title = "Algorithm Viewer",
  shinyjs::useShinyjs(),
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

  # Removes width of the controls in the sidebar so that they take up the full
  # width (instead of the default 300px).
  tags$style(HTML("
    .shiny-input-container:not(.shiny-input-container-inline) {
      width: unset;
    }
  ")),

  # Application title
  titlePanel(
    htmlOutput("ui_title")
  ),

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
            # Model selection and message
            htmlOutput("model_message"),
            div(
              style = ifelse(config_allow_algorithms_selection(), "", "display: none"),
              selectInput(
                inputId = "algorithms",
                label = "Preloaded Algorithms",
                choices = NULL
              )
            ),
            div(
              style = ifelse(config_allow_file_uploads(), "", "display: none"),
              fileInput(
                "upload",
                "Upload Algorithm:",
                accept = c(".zip", ".tar", ".gz")
              ),
              hr(),
              checkboxGroupInput(
                inputId = "model_id",
                label = "Models:"
              ),
            ),
            hr(
              style = ifelse(
                config_has_algorithms() || config_allow_file_uploads(),
                "",
                "display: none"
              )
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

        # Predictor controls get added as children to #refgroup_controls
        tabPanel(
          "Reference",
          icon = icon("angle-double-down"),
          div(
            style = paste(
              "height: calc(100vh - 140px); margin-bottom: 20px;",
              "overflow-y: scroll"
            ),
            div(id = "refgroup_controls")
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
          plotly::plotlyOutput("or_plot", height = plot_height)
        ),
        tabPanel(
          "Predicted Risk",
          icon = icon("chart-line"),
          br(),
          plotly::plotlyOutput("pr_plot", height = plot_height)
        ),
        tabPanel(
          "Relative Risk",
          icon = icon("chart-line"),
          br(),
          plotly::plotlyOutput("rr_plot", height = plot_height)
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
