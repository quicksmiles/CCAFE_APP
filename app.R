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
library(future)
library(shinybusy)
library(shinyjs)
library(vcfR)
library(data.table)
library(tools)


# Source modules
source("modules/file_upload.R")
source("modules/operation_selection.R")
#source("modules/email_results.R")


  
  ui <- fluidPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "minty"),
    useShinyjs(), # Use shinyjs for wizard functionality
    use_busy_spinner(),
    
    navbarPage(
      id = "CCAFE",
      title = "CCAFE App",
      tabPanel("Home", value = "Home",
               h3("Welcome to the CCAFE App"),
               p("This application helps you estimate case and control allele frequencies using your data."),
               actionButton("go_to_upload", "Go to Upload Page", class = "btn-primary")
      ),
      tabPanel(title = "Step 1: Upload File", value = "Step1", fileUploadUI("file_upload")),
      tabPanel(title = "Step 2: Select Operation", value = "Step2", operationSelectionUI("operation_selection"))#,
    #   tabPanel(title = "Step 3: Provide Email", value = "Step3",
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
    results <- operationSelectionServer("operation_selection", uploaded_data, column_names)
    
  }
  
  shinyApp(ui, server)  
