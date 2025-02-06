source("upload.R")

fileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload your GWAS data (gzipped text or gzipped vcf files only)", accept = c(".bgz", ".gz")),
    actionButton(ns("process_file"), "Process File"),
    tableOutput(ns("file_preview")),
    tags$div(
      actionButton(ns("go_to_page2"), "Continue", class = "btn-primary"),
      style = "position: fixed; bottom: 20px; right: 20px;"
    )
  )
}

fileUploadServer <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Handle user file upload, create a reactive variable for use in other functions
    uploaded_data <- reactiveVal()
    
    # When "Process File" button is clicked...
    observeEvent(input$process_file, {
      req(input$file)
      
      # Try processing the uploaded file
      data <- tryCatch({
        upload_file(input$file, input$file$name, input$file$datapath)
      }, error = function(e) {
        showNotification(paste("Error processing file:", e$message), type = "error")
        NULL
      })
      
      # When the files data is correctly processed
      if (!is.null(data)) {
        # Pass the uploaded file data to reactive value
        uploaded_data(data)
        
        # Render preview of uploaded data
        output$file_preview <- renderTable({
          req(uploaded_data())
          head(uploaded_data(), 10)
        })
      } else{
        # In the case where there is no file to process, display error msg to user
        showModal(modalDialog(
          title = "Error",
          "Please upload a file before clicking 'Process File'.",
          footer = modalButton("OK"),
          easyClose = TRUE
        ))
        return() # Exit the error msg
      }
    })
    
    # When "Save File" button is clicked...
    observeEvent(input$go_to_page2, {
      # Ensure the reactive variable for the processed file data values are available 
      req(uploaded_data())
      # Try to save user file data to disk space
      saved_file <- tryCatch(
        save_file(uploaded_data()),
        # Throw an error if user file data is not saved successfully
        error = function(e) {
          showNotification(e$message, type = "error")
          return(NULL)
        }
      )
      # Move to operation selection page
      updateNavbarPage(main_session, inputId = "CCAFE", selected = "Step2")
    })
    
    # Limit the file size a user can upload to 512MB
    options(shiny.maxRequestSize = (1024^3)/2)
    
    return(uploaded_data)
  
  })
}
