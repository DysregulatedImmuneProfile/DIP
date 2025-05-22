library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(DIP)
library(readxl)

# 🧑‍🎨 Design theme inspired by modern SaaS apps
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#005f73",
  secondary = "#0a9396",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter Tight")
)

ui <- fluidPage(
  theme = custom_theme,

  tags$head(
    tags$style(HTML("
      .title-bar {
        background-color: #ffffff;
        padding: 1.5rem 2rem;
        border-bottom: 1px solid #eaeaea;
        box-shadow: 0 2px 4px rgba(0,0,0,0.03);
        margin-bottom: 2rem;
      }
      .title-bar h1 {
        margin: 0;
        font-size: 1.9rem;
        font-weight: 600;
        color: #005f73;
      }
      .disclaimer {
        font-size: 0.9rem;
        color: #6c757d;
        margin-top: 0.25rem;
        margin-bottom: 1rem;
      }
      .section-title {
        margin-top: 2.5rem;
        font-size: 1.2rem;
        font-weight: 600;
        color: #005f73;
      }
      .shiny-download-link {
        margin-top: 0.5rem;
      }
    "))
  ),

  div(class = "title-bar",
      h1("Immune Dysregulation Predictor"),
      div(class = "disclaimer",
          "Note: If patients have measurements from multiple days, ",
          "make sure the day is included in the ID column (e.g., 'Patient_1_Day1')."
      )
  ),

  sidebarLayout(
    sidebarPanel(
      tags$h5("Upload Patient Data", class = "mb-2 mt-2"),
      fileInput("file", NULL, accept = c(".csv", ".xlsx", ".xls")),

      tags$small("Accepted formats: CSV or Excel. Columns required: ID, TREM_1, IL_6, Procalcitonin."),

      downloadButton("downloadExample", "Download Example File", class = "btn-outline-primary mt-2 mb-3"),

      radioButtons("mode", "Prediction Type",
                   choices = c("DIP stage (categorical)" = "dip",
                               "cDIP (continuous)" = "cdip")),

      actionButton("run", "Run Prediction", class = "btn-primary mt-3"),
      br(), br(),
      downloadButton("downloadData", "Download Full Results", class = "btn-outline-success")
    ),

    mainPanel(
      conditionalPanel("output.prediction_ready == true",
                       h4("Preview (first 5 rows)", class = "section-title"),
                       tableOutput("preview")
      ),

      conditionalPanel("output.prediction_ready == true && output.show_removed == true",
                       tags$div(class = "text-warning mb-3",
                                tags$strong("Patients removed due to missing classifier data:"),
                                verbatimTextOutput("removed")
                       )
      ),

      conditionalPanel("input.mode == 'dip' && output.prediction_ready == true",
                       h4("DIP Prediction Summary", class = "section-title"),
                       plotlyOutput("pie", height = "380px"),
                       h4("3D Probability Visualization", class = "section-title"),
                       plotlyOutput("scatter", height = "480px")
      ),

      conditionalPanel("input.mode == 'cdip' && output.prediction_ready == true",
                       h4("cDIP Beeswarm Plot", class = "section-title"),
                       plotOutput("cdip_plot", height = "500px")
      )
    )
  )
)

server <- function(input, output, session) {
  full_results <- reactiveVal(NULL)
  removed_ids <- reactiveVal(NULL)
  pie_data <- reactiveVal(NULL)
  scatter_data <- reactiveVal(NULL)
  cdip_plot <- reactiveVal(NULL)
  prediction_made <- reactiveVal(FALSE)

  observeEvent(input$run, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)

    if (ext == "csv") {
      data <- read.csv(input$file$datapath)
    } else if (ext %in% c("xlsx", "xls")) {
      data <- readxl::read_excel(input$file$datapath)
      data <- as.data.frame(data)
    } else {
      showModal(modalDialog(
        title = "Unsupported File Type",
        "Please upload a .csv or .xlsx file.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }

    if (input$mode == "dip") {
      res <- run_DIP_stage_for_shiny(data)
      full_results(res$results)
      removed_ids(res$removed)
      pie_data(res$piechart)
      scatter_data(res$scatter3d)
      cdip_plot(NULL)
    } else {
      res <- run_cDIP_for_shiny(data)
      full_results(res$results)
      removed_ids(res$removed)
      cdip_plot(res$plot)
      pie_data(NULL)
      scatter_data(NULL)
    }

    prediction_made(TRUE)
  })

  output$preview <- renderTable({
    req(full_results())
    head(full_results(), 5)
  })

  output$removed <- renderPrint({
    ids <- removed_ids()
    if (!is.null(ids) && length(ids) > 0) {
      paste(ids, collapse = ", ")
    } else {
      "None"
    }
  })

  output$show_removed <- reactive({
    !is.null(removed_ids()) && length(removed_ids()) > 0
  })
  outputOptions(output, "show_removed", suspendWhenHidden = FALSE)

  output$prediction_ready <- reactive({
    prediction_made()
  })
  outputOptions(output, "prediction_ready", suspendWhenHidden = FALSE)

  output$pie <- renderPlotly({
    req(pie_data())
    pie_data()
  })

  output$scatter <- renderPlotly({
    req(scatter_data())
    scatter_data()
  })

  output$cdip_plot <- renderPlot({
    req(cdip_plot())
    cdip_plot()
  })

  output$downloadExample <- downloadHandler(
    filename = function() {
      "example_patient_data.xlsx"
    },
    content = function(file) {
      file.copy(system.file("extdata", "example_patient_data.xlsx", package = "DIP"), file)
    }
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("DIP_predictions_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(full_results(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
