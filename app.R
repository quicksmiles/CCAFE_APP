#
# This is the CCAFE Shiny web application. You can run the application by clicking
# the 'Run App' button in R Studio or by visiting [link].
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(callr)
library(CCAFE)
library(data.table)
library(DT)
library(future)
library(shinybusy)
library(shinyjs)
library(stats)
library(tidyr)
library(tools)
library(vcfR)
library(dotenv)

load_dot_env()

# Source modules
source("modules/file_upload.R")
source("modules/operation_selection.R")
#source("modules/email_results.R")


  
  ui <- fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    useShinyjs(), # Use shinyjs for wizard functionality
    
    
    navbarPage(
      id = "CCAFE",
      title = tags$div("",
        tags$img(src = "https://raw.githubusercontent.com/wolffha/wolffha/refs/heads/main/images/CCAFE-hex.png",
                 width = 75,
                 height = 75
                 )
        ),
      tabPanel("Home", value = "Home",
               h3("Welcome to the CCAFE App"),
               p("This tool estimates case and control allele frequencies using GWAS summary data. 
                 Please assure your data is lifted over on the current human reference genome, GRCh38. 
                 The current version of this app only performs computations on data with reference genome build GRCh38.
                 
                 "),
               actionButton("go_to_upload", "Go to Upload Page", class = "btn-primary")
      ),
      tabPanel(title = "Step 1: Upload File", value = "Step1", fileUploadUI("file_upload")),
      tabPanel(title = "Step 2: Select Operation", value = "Step2", operationSelectionUI("operation_selection")),
    # tabPanel(title = "Step 3: Provide Email", value = "Step3",
    #            emailResultsUI("email_results"))
    )
  )
  
  server <- function(input, output, session) {
    # When "Go to Upload Page" is clicked...
    observeEvent(input$go_to_upload, {
      # Move to step 1 (next) page
      updateNavbarPage(session = getDefaultReactiveDomain(), inputId = "CCAFE", selected = "Step1")
    })
    
    # File upload module
    uploaded_data <- fileUploadServer("file_upload", session)
    
    # Reactive for column names
    column_names <- reactive({
      req(uploaded_data())
      colnames(uploaded_data())
    })
    
    # Operation selection module
    results <- operationSelectionServer("operation_selection", uploaded_data, column_names, session)
    
  }
  
  shinyApp(ui, server)  
