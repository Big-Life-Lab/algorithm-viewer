# Algorithm Viewer v2.0.0
# HTNPoRT Odds Ratio Visualization

# Load required libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

# Load OR calculation functions
source("or_calculations.R")

# ============================================================================
# DATA LOADING FUNCTIONS
# ============================================================================

#' Load model data for a specific sex
#' @param sex Either "male" or "female"
#' @return List containing all model data files
load_model_data <- function(sex) {
  base_path <- file.path("logistic-model-export", sex)
  model_name <- paste0("HTNPoRT-", sex)

  list(
    variables = read.csv(file.path(base_path, paste0(model_name, "-variables.csv")),
                        stringsAsFactors = FALSE),
    variable_details = read.csv(file.path(base_path, paste0(model_name, "-variable-details.csv")),
                                stringsAsFactors = FALSE),
    logistic = read.csv(file.path(base_path, paste0(model_name, "-logistic.csv")),
                       stringsAsFactors = FALSE),
    rcs = read.csv(file.path(base_path, paste0(model_name, "-rcs.csv")),
                  stringsAsFactors = FALSE),
    center = read.csv(file.path(base_path, paste0(model_name, "-center.csv")),
                     stringsAsFactors = FALSE),
    interactions = read.csv(file.path(base_path, paste0(model_name, "-interactions.csv")),
                           stringsAsFactors = FALSE)
  )
}

# Load both models at startup
male_data <- load_model_data("male")
female_data <- load_model_data("female")

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(

  # Application title
  titlePanel("HTNPoRT Algorithm Viewer"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,

      # Sex selection
      radioButtons(
        inputId = "sex",
        label = "Model:",
        choices = c("Male" = "male", "Female" = "female"),
        selected = "male"
      ),

      hr(),

      # Predictor selection (will be populated dynamically)
      selectInput(
        inputId = "predictor",
        label = "Select Predictor:",
        choices = NULL
      ),

      hr(),

      # Help button
      actionButton(
        inputId = "help",
        label = "Help",
        icon = icon("question-circle")
      ),

      br(), br(),

      # Model information display
      h5("Model Information"),
      textOutput("model_info")
    ),

    # Main panel for displaying outputs
    mainPanel(
      width = 9,

      # Tabs for different views
      tabsetPanel(
        type = "tabs",

        tabPanel(
          "Odds Ratio Curves",
          br(),
          plotlyOutput("or_plot", height = "600px"),
          br(),
          htmlOutput("plot_description")
        ),

        tabPanel(
          "Data Summary",
          br(),
          h4("Loaded Model Data"),
          verbatimTextOutput("data_summary")
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {

  # Reactive expression to get current model data based on sex selection
  current_model <- reactive({
    if (input$sex == "male") {
      male_data
    } else {
      female_data
    }
  })

  # Update predictor choices when sex changes
  observe({
    model <- current_model()

    # Get predictors from variables table (role == "Predictor")
    predictors <- model$variables %>%
      filter(role == "Predictor") %>%
      select(variable, label)

    # Create named vector for selectInput
    predictor_choices <- setNames(predictors$variable, predictors$label)

    updateSelectInput(
      session,
      "predictor",
      choices = predictor_choices,
      selected = predictors$variable[1]
    )
  })

  # Display model information
  output$model_info <- renderText({
    model <- current_model()
    n_predictors <- nrow(model$variables %>% filter(role == "Predictor"))
    n_coefficients <- nrow(model$logistic)

    paste0(
      "Predictors: ", n_predictors, "\n",
      "Coefficients: ", n_coefficients
    )
  })

  # Calculate and plot OR curves
  output$or_plot <- renderPlotly({
    req(input$predictor)

    model <- current_model()
    predictor_label <- model$variables %>%
      filter(variable == input$predictor) %>%
      pull(label)

    predictor_type <- model$variables %>%
      filter(variable == input$predictor) %>%
      pull(variableType)

    # Calculate OR curves
    age_range <- 20:79

    tryCatch({
      if (predictor_type == "Categorical") {
        # Get OR curves for all categories
        or_data <- get_categorical_or_curves(input$predictor, model, age_range)

        # Create plot with multiple lines
        p <- ggplot(or_data, aes(x = age, y = OR, color = category_label, group = category_label)) +
          geom_line(size = 1.2) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
          labs(
            title = paste("Odds Ratio Curves:", predictor_label),
            subtitle = paste("Model:", ifelse(input$sex == "male", "Male", "Female")),
            x = "Age (years)",
            y = "Odds Ratio",
            color = "Category"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12),
            legend.position = "right"
          ) +
          scale_color_brewer(palette = "Set1")

      } else {
        # Continuous predictor - show effect of one-unit increase
        or_data <- calculate_or_curve(input$predictor, 1, model, age_range)

        # Create plot
        p <- ggplot(or_data, aes(x = age, y = OR)) +
          geom_line(size = 1.2, color = "#377EB8") +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
          labs(
            title = paste("Odds Ratio Curve:", predictor_label),
            subtitle = paste("Model:", ifelse(input$sex == "male", "Male", "Female"), "| One-unit increase"),
            x = "Age (years)",
            y = "Odds Ratio"
          ) +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12)
          )
      }

      ggplotly(p) %>%
        layout(hovermode = "x unified")

    }, error = function(e) {
      # Display error message if calculation fails
      plot_ly() %>%
        add_text(x = 0.5, y = 0.5, text = paste("Error calculating OR:", e$message),
                textfont = list(size = 14, color = "red")) %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          showlegend = FALSE
        )
    })
  })

  # Plot description
  output$plot_description <- renderUI({
    req(input$predictor)

    model <- current_model()
    predictor_info <- model$variables %>%
      filter(variable == input$predictor)

    # Check if predictor has RCS transformation
    has_rcs <- input$predictor %in% model$rcs$variable

    # Check for age interactions
    has_interactions <- any(grepl(input$predictor, model$interactions$interactingVariables))

    interpretation <- if (predictor_info$variableType == "Categorical") {
      "Each curve shows the odds ratio comparing that category to the reference category across different ages."
    } else {
      paste0("The curve shows the odds ratio for a one-unit increase in ", predictor_info$labelLong, " across different ages.")
    }

    transformation_note <- if (has_rcs) {
      "This predictor uses restricted cubic spline (RCS) transformation."
    } else {
      ""
    }

    interaction_note <- if (has_interactions) {
      "This predictor has interaction terms with age, which is why the odds ratio changes across the age range."
    } else {
      ""
    }

    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 10px;'>",
      "<h5 style='margin-top: 0;'>Interpretation</h5>",
      "<p>", interpretation, "</p>",
      if (transformation_note != "") paste0("<p><em>", transformation_note, "</em></p>") else "",
      if (interaction_note != "") paste0("<p><em>", interaction_note, "</em></p>") else "",
      "<hr style='margin: 10px 0;'>",
      "<p style='margin-bottom: 0;'>",
      "<strong>Predictor:</strong> ", predictor_info$labelLong, "<br>",
      "<strong>Type:</strong> ", predictor_info$variableType,
      if (predictor_info$units != "N/A") paste0(" (", predictor_info$units, ")") else "",
      "</p>",
      "</div>"
    ))
  })

  # Data summary for verification
  output$data_summary <- renderPrint({
    model <- current_model()

    cat("=== VARIABLES ===\n")
    print(model$variables)
    cat("\n=== LOGISTIC COEFFICIENTS ===\n")
    print(model$logistic)
    cat("\n=== RCS KNOTS ===\n")
    print(model$rcs)
    cat("\n=== CENTERING VALUES ===\n")
    print(head(model$center, 10))
    cat("\n=== INTERACTIONS ===\n")
    print(model$interactions)
  })

  # Help modal (Phase 5 will expand this)
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Algorithm Viewer Help",
      HTML("
        <h4>What is the Algorithm Viewer?</h4>
        <p>This application helps you understand how predictors affect hypertension risk
        across different ages in the HTNPoRT model.</p>

        <h4>How to use:</h4>
        <ol>
          <li>Select a model (Male or Female)</li>
          <li>Choose a predictor to visualize</li>
          <li>View the odds ratio curve showing how the effect changes with age</li>
        </ol>

        <p><em>More detailed help will be added in Phase 5.</em></p>
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
