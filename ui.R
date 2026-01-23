library(dplyr)
library(shiny)
library(shinyWidgets)
library(bslib)
library(plotly)

plot_height <- "700px"

ui <- fluidPage(
  # fillPage(),
  # Application title
  titlePanel(
    glue::glue(
      "{model_definitions$meta$algorithm} v{model_definitions$meta$version} ",
      "Algorithm Viewer"
    )
  ),

  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Models",
          div(
            br(),
            # Model selection
            if(length(model_definitions$models) > 0)
              checkboxGroupInput(
                inputId = "model_id",
                label = "Models:",
                selected = unname(get_model_choices(model_definitions$models)),
                choiceNames = get_model_titles(model_definitions$models, include_model_colors = TRUE),
                choiceValues = get_model_ids(model_definitions$models)
              )
            else
              h4("No models defined in configuration file")
            ,

            hr(),

            # Predictor selection (will be populated dynamically)
            selectInput(
              inputId = "predictor",
              label = "Predictor:",
              choices = NULL
            ),

            selectInput(
              inputId = "interaction_predictor",
              label = "Interaction Predictor:",
              choices = NULL
            ),

            hr(),

            # Log or non-log scale
            checkboxInput("logarithmic", "Logarithmic", value = TRUE)
          )
        ),
        tabPanel(
          "Reference",
          div(
            htmlOutput("refgroups")
          )
        )
      ),

      hr(),

      # Help button
      actionButton(
        inputId = "help",
        label = "Help",
        icon = icon("question-circle")
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
          br(),
          plotlyOutput("or_plot", height = plot_height)
        ),
        tabPanel(
          "Predicted Risk",
          br(),
          plotlyOutput("pr_plot", height = plot_height)
        )
      )
    )
  )
)

ui