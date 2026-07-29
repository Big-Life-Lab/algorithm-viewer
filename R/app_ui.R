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
        sizes = "128x128", href = "www/favicon-128x128.png"
      ),
      shiny::tags$link(
        rel = "icon", type = "image/png",
        sizes = "64x64", href = "www/favicon-64x64.png"
      ),
      shiny::tags$link(
        rel = "icon", type = "image/png",
        sizes = "32x32", href = "www/favicon-32x32.png"
      ),
      shiny::tags$link(
        rel = "icon", type = "image/png",
        sizes = "180x180", href = "www/favicon-180x180.png"
      ),
      shiny::tags$link(
        rel = "apple-touch-icon",
        sizes = "180x180", href = "www/favicon-180x180.png"
      )
    ),
    shiny::tags$head(
      # Styles for mod_categorical_radio_table.R
      stylesheet_link("www/crt.css"),
      # Styles for mod_continuous_slider_group.R
      stylesheet_link("www/csg.css"),
      # Styles for utils_plot_additional_controls.R
      stylesheet_link("www/plot-additional-controls.css")
    ),
    shiny::tags$style(shiny::HTML("
      /* Removes width of the controls in the sidebar so that they take up the
         full width (instead of the default 300px). */
      .shiny-input-container:not(.shiny-input-container-inline) {
        width: unset;
      }
    ")),

    # Application title
    shiny::titlePanel(
      shiny::textOutput("ui_title", inline = TRUE)
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
          id = "settings_tabs",
          shiny::tabPanel(
            "Models",
            style = paste(
              "max-height: calc(100vh - 140px); margin-bottom: 30px;",
              "overflow-y: scroll"
            ),
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
                )
              ),
              shiny::div(
                shiny::hr(
                  style = ifelse(
                    config_allow_algorithms_selection() ||
                      config_allow_file_uploads(),
                    "",
                    "display: none"
                  )
                ),
                shiny::checkboxGroupInput(
                  inputId = "selected_model_ids",
                  label = "Models:"
                )
              ),

              shiny::hr(),

              # Algorithm Viewer version number
              package_versions_ui(algorithm_viewer_only = TRUE),

              shiny::br()
            )
          ),

          # Predictor controls get added as children to #refgroup_controls
          shiny::tabPanel(
            "Reference",
            value = "reference_groups",
            icon = shiny::icon("cog"),
            predictorGroupedControlsUI("refgroup")
          ),

          shiny::tabPanel(
            "Me vs Ref",
            value = "a_vs_b",
            icon = shiny::icon("cog"),
            predictorGroupedControlsUI("a_vs_b_groups")
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
            value = "or",
            icon = shiny::icon("chart-line"),
            plotORUI("or_plot", .external_height)
          ),
          shiny::tabPanel(
            "Relative Risk",
            value = "rr",
            icon = shiny::icon("chart-line"),
            plotRRUI("rr_plot", .external_height)
          ),
          shiny::tabPanel(
            "Predicted Risk",
            value = "pr",
            icon = shiny::icon("chart-line"),
            plotPRUI("pr_plot", .external_height)
          ),
          shiny::tabPanel(
            "Me vs Ref",
            value = "a_vs_b",
            icon = shiny::icon("chart-line"),
            plotRRAvsBUI("rr_a_vs_b_plot", .external_height)
          ),
          shiny::tabPanel(
            "Help",
            value = "help",
            icon = shiny::icon("circle-question"),
            style = "max-width: 800px",
            shiny::br(),
            shiny::htmlOutput("help"),
            shiny::hr(),

            # Version numbers
            package_versions_ui()
          )
        ),
        shiny::br()
      )
    )
  )
}
