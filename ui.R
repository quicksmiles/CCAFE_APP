#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
upload_ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose a VCF File (max 1GB)",
                      accept = c(".gz", ".bgz")),
            textOutput("file_info"),
            textOutput("status"),
            actionButton("process_btn", "Process File"),
            uiOutput("progress")
        ),
        mainPanel(
            tableOutput("vcf_preview")
        )
    )
)
