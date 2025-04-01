box::use(
  shiny[...],
  DT[...],
)

box::use(
  app/logic/upload[...],
)

fileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload GWAS summary statistics (compressed text or VCF file)", accept = c(".bgz", ".gz")),
    actionButton(ns("process_file"), "Process Data", class = "btn btn-default btn-round"),
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
        
        # Try to save user file data to disk space
        saved_file <- tryCatch(
          save_file(uploaded_data()),
          # Throw an error if user file data is not saved successfully
          error = function(e) {
            showNotification(e$message, type = "error")
            return(NULL)
          }
        )
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
    
    
    # Limit the file size a user can upload to 512MB
    options(shiny.maxRequestSize = (1024^3)/2)
    
    return(uploaded_data)
  
  })
}
