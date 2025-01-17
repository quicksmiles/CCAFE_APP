#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(future)
# Use multiple sessions for parallel processing
plan(multisession)
# Link upload.R functions to server.R file
source("upload.R", local = TRUE)
# Define server logic required to draw a histogram
upload_server <- function(input, output, session) {
    output$file_info <- renderText({
        req(input$file)
        paste("File Name:", input$file$name,
              "| Size:", format(input$file$size / 1024^2, digits = 2), "MB")
    })
    
    # Upload user file
    file_data <- reactive({
        req(input$file)
        upload_file(input$file, input$file$name, input$file$path)
    })
    
    
    # Output status messages for user feedback
    output$status <- renderText({
        if (is.null(file_data())) {
            "Waiting for VCF file..."
        } else {
            "VCF file loaded successfully!"
        }
    })

    # Display a preview of the VCF data
    output$vcf_preview <- renderTable({
        req(file_data())
        head(file_data(), 10)  # Display the first 10 rows as a preview
    })

   
    # Temporarily save user uploaded file into disk space
    observeEvent(file_data(), {
        req(file_data())
        saved_file <- tryCatch(
            save_file(file_data()),
            error = function(e) {
                showNotification(e$message, type = "error")
                return(NULL)
            }
        )
        if (!is.null(saved_file)) {
            output$file_info <- renderText({
                paste("File succesfully saved at:", saved_file)
            })
        }
    })
    
    # Limit the file size a user can upload
    options(shiny.maxRequestSize = (1024^3)/2)
}