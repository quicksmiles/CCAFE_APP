box::use(
  argonR[...],
  argonDash[...],
  CCAFE[...],
  reactR[...],
  shiny[...],
  shinyjs[...],
  DT[DTOutput, renderDT],
)

box::use(
  app/view/file_upload[...],
)


operationSelectionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    argonRow(
      
      # Left Column with Three Cards
      argonColumn(
        width = 4,
        
        # Data Selection Card
        argonCard(
          title = "Data Selection",
          width = 12,
          radioButtons(
            ns("data_source"),
            label = "Choose data source:",
            choices = c("Upload File" = "upload", "Use Sample Data" = "sample"),
            selected = "upload",
            inline = TRUE
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
            fileUploadUI(ns("file_upload"))
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'sample'", ns("data_source")),
            actionButton(ns("process_file"), "Process Data", class = "btn btn-default btn-round")
          )
        ),
        
        # Inputs Card
        argonCard(
          title = "Inputs",
          width = 12,
          
          # Conditional Panel: Only display inputs when data is available
          conditionalPanel(
            condition = sprintf("input['%s'] != null || input['%s'] > 0", ns("file_upload"), ns("process_file")),
            
            # Operation Selection (Radio Buttons)
            radioButtons(
              ns("operation"), "Select Operation",
              choices = c("CaseControl_AF" = "AF", "CaseControl_SE" = "SE"),
              inline = TRUE
            ),
            
            # AF Section
            conditionalPanel(
              condition = sprintf("input['%s'] == 'AF'", ns("operation")),
              numericInput(ns("N_case"), "Number of cases:", value = 16550),
              numericInput(ns("N_control"), "Number of controls:", value = 403923),
              selectInput(ns("OR_colname"), "Odds Ratio Column", choices = NULL),
              selectInput(ns("AF_total_colname"), "Total AF Column:", choices = NULL),
              actionButton(ns("run_casecontrolaf"), "Run CaseControl_AF", class = "btn-primary")
            ),
            
            # SE Section
            conditionalPanel(
              condition = sprintf("input['%s'] == 'SE'", ns("operation")),
              selectInput(ns("population"), "Select Population for Bias Correction",
                          choices = c("total", "afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "remaining", "sas")),
              numericInput(ns("N_case_se"), "Number of cases:", value = NULL),
              numericInput(ns("N_control_se"), "Number of controls:", value = NULL),
              selectInput(ns("OR_colname_se"), "Odds Ratio Column", choices = NULL),
              selectInput(ns("SE_colname_se"), "SE Column:", choices = NULL),
              selectInput(ns("chromosome_colname"), "Chromosome Column:", choices = NULL),
              selectInput(ns("position_colname"), "Position Column:", choices = NULL),
              textInput(ns("user_email"), "Email", placeholder = "Enter your email"),
              actionButton(ns("run_casecontrolse"), "Run CaseControl_SE", class = "btn-primary")
            )
          )
        ),
        
        # Effect Estimate Calculation Card
        argonCard(
          title = tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            
            # Title text on the left
            tags$span("Effect Estimate Calculation"),
            
            # Toggle button on the right
            tags$label(
              class = "custom-toggle",
              style = "margin-left: 100px;",  # Pushes the toggle to the right
              tags$input(
                type = "checkbox",
                id = ns("toggle_button")
              ),
              tags$span(
                style = "display: inline-block; margin-left: auto;", 
                class = "custom-toggle-slider rounded-circle"
              )
            )
          ),
          
          width = 12,
          
          # Input Fields shown only when toggle is ON
          conditionalPanel(
            condition = sprintf("input['%s'] == true", ns("toggle_button")),
            selectInput(ns("BETA_colname"), "Beta Column", choices = NULL)
          )
        )
      ),
      
      # Right Column with Full-Height DTOutput
      argonColumn(
        width = 8,
        argonCard(
          width = 12,
          # style = "min-height: 100%; height: calc(100vh - 100px);",  # Full height of the three cards
          tags$div(class = "table-responsive py-4", DTOutput("sample_preview")),
          DTOutput(ns("upload_preview")),
          DTOutput(ns("results_preview")),
          downloadButton(ns("download_results"), "Download Results")
        )
      )
    )
  )
}


  # 
  # tagList(
  #   h3("Choose Operation"),
  #   radioButtons(ns("operation"), "Select Operation",
  #                choices = c("CaseControl_AF" = "AF", "CaseControl_SE" = "SE")),
  #   selectInput(ns("BETA_colname"), "Beta Column", choices = NULL),
  #   checkboxInput(inputId = ns("calculate_OR"), label = "Calculate OR column?", value = FALSE),
    # conditionalPanel(
    #   condition = paste0("input['", ns("operation"), "'] == 'AF'"),
    #   numericInput(ns("N_case"), "Number of cases:", value = NULL),
    #   numericInput(ns("N_control"), "Number of controls:", value = NULL),
    #   selectInput(ns("OR_colname"), "Odds Ratio Column", choices = NULL),
    #   selectInput(ns("AF_total_colname"), "Total AF Column:", choices = NULL),
    #   actionButton(ns("run_casecontrolaf"), "Run CaseControl_AF", class = "btn-primary"),
    #   downloadButton(ns("download_results"))
    # ),
    # conditionalPanel(
    #   condition = paste0("input['", ns("operation"), "'] == 'SE'"),
    #   selectInput(ns("super_population"), "Select Super-Population for Bias Correction",
    #               choices = c("total", "afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "remaining", "sas")),
    #   numericInput(ns("N_case_se"), "Number of cases:", value = NULL),
    #   numericInput(ns("N_control_se"), "Number of controls:", value = NULL),
    #   selectInput(ns("OR_colname_se"), "Odds Ratio Column", choices = NULL),
    #   selectInput(ns("SE_colname_se"), "SE Column:", choices = NULL),
    #   selectInput(ns("chromosome_colname"), "Chromosome Column:", choices = NULL),
    #   selectInput(ns("position_colname"), "Position Column:", choices = NULL),
    #   textInput(ns("user_email"), "Email", placeholder = "Enter your email"),
    #   actionButton(ns("run_casecontrolse"), "Run CaseControl_SE", class = "btn-primary")
    # ),
  #   
  #   DTOutput(ns("results_preview"))
  # )

operationSelectionServer <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveVal()
    
    # Reactive function to load sample data
    getSampleData <- reactive({
      sampleDat <- utils::read.table("app/static/sampledata.txt", header = T)
      sampleDat
    })
    
    # Render preview of uploaded data
    output$sample_preview <- renderDT({
      req(getSampleData())
      
      # Properly render DataTable with class and ID inside the datatable function
      datatable(
        getSampleData(),
        
        # DataTable options
        options = list(
          dom = 'Bfrtip',                       # Include buttons
          buttons = c('download'),  # Export options
          paging = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        
        # Add extensions and styling
        extensions = c('Buttons', 'Select'),
        class = "table table-flush",
        id = "datatable-basic", # Use Argon table styling
        rownames = FALSE                        # Hide row names
      )
    })
    
    uploaded_data <- fileUploadServer("file_upload", main_session)
    
    # Choose between uploaded data or sample data
    current_data <- reactive({
      if (input$data_source == "sample") {
        getSampleData()
      } else {
        req(uploaded_data())
        uploaded_data()
      }
    })
    
    column_names <- reactive({
      req(current_data())
      colnames(current_data())
    })
      
    selected_population <- reactive({
      input$population  # Automatically updates when user selects a value
    })
    
    observeEvent(column_names(), {
      req(column_names())
      req(current_data())
      
      updateSelectInput(session, "BETA_colname", choices = column_names(), selected = "BETA")
      updateNumericInput(session, "N_case", value = input$N_case)
      updateNumericInput(session, "N_control", value = input$N_control)
      updateNumericInput(session, "N_case_se", value = input$N_case_se)
      updateNumericInput(session, "N_control_se", value = input$N_control_se)
      updateSelectInput(session, "OR_colname", choices = column_names(), selected = "OR")
      updateSelectInput(session, "OR_colname_se", choices = column_names(), selected = "OR")
      updateSelectInput(session, "AF_total_colname", choices = column_names(), selected = "true_maf_pop")
      updateSelectInput(session, "SE_colname_se", choices = column_names(), selected = "SE")
      updateSelectInput(session, "chromosome_colname", choices = column_names(), selected = "chrom")
      updateSelectInput(session, "position_colname", choices = column_names(), selected = "pos")
      # updateSelectInput(session, "population", selected = "nfe")
    })
    # observe if the calculate OR button is toggled
    # updated_data <- reactiveVal()
    observe({
      req(uploaded_data())  # Ensure data is uploaded first
      
      # Only proceed if toggle is ON
      if (input$toggle_button) {
        
        # Get the dataframe
        df <- uploaded_data()
        
        # Apply calculation if the Beta column is selected
        if (!is.null(input$BETA_colname) && input$BETA_colname %in% colnames(df)) {
          df$OR <- exp(df[[input$BETA_colname]])
          uploaded_data(df)
          
          # Update the OR column selection
          updateSelectInput(session, "OR_colname", choices = colnames(df), selected = "OR")
        }
        
      } else {
        # Optionally reset or clear values when toggle is OFF
        updateSelectInput(session, "BETA_colname", choices = NULL)
        updateSelectInput(session, "OR_colname", choices = NULL)
      }
    })
    
    
    observeEvent(input$run_casecontrolaf, {
      req(current_data())
      
      results_af <- CaseControl_AF(data = current_data(),
                                   N_case = input$N_case,
                                   N_control = input$N_control,
                                   OR_colname = input$OR_colname,
                                   AF_total_colname = input$AF_total_colname)
      # Move to email input page
      # updateNavbarPage(session, "CCAFE App", selected = "Step3")
      results(results_af)
    })
    
    observeEvent(input$run_casecontrolse, {
      # Move to email input page
      # updateNavbarPage(session, "CCAFE", selected = "Step3")
      req(uploaded_data())  # Ensure the user has uploaded data
      req(selected_population())
      req(input$user_email)
      
      uploaded_data <- uploaded_data()
      selected_population <- selected_population()
      
      user_email <- input$user_email
      N_case_se <- input$N_case_se
      N_control_se <- input$N_control_se
      OR_colname_se <- input$OR_colname_se
      SE_colname_se <- input$SE_colname_se
      chromosome_colname <- input$chromosome_colname
      position_colname <- input$position_colname
      
      # Show a modal spinner while the process runs
      #show_modal_spinner(spin = "self-building-square", text = "Processing query in the background...")
      serialized_handle_se <- serialize(object = handle_se, NULL)
      # Run merge.R in the background
      handle_se_process <- callr::r_bg(
        function(handle_se, selected_population, user_email, uploaded_data, N_case_se, N_control_se, 
                 OR_colname_se, SE_colname_se, chromosome_colname, position_colname) {
          # FIXME: move this to app.R, and if that doesn't work, then move it back
          # Source the merge.R script
          source("../CCAFE/app/utils/handle_se.R")
          handle_se(selected_population, user_email, uploaded_data, N_case_se, N_control_se,
                    OR_colname_se, SE_colname_se, chromosome_colname, position_colname) # Execute function
        }, 
        args = list(handle_se, selected_population, user_email, uploaded_data, N_case_se, N_control_se, 
                    OR_colname_se, SE_colname_se, chromosome_colname, position_colname), 
        supervise = TRUE
      )
      print(processx::poll(list(handle_se_process), 1000))
      # Poll for completion of merge process
      observe({
        if (handle_se_process$poll_io(0)["process"] != "ready") {
          # If process is still running, don't do anything yet
          print("still querying...")
          print(processx::poll(list(handle_se_process), 60000))
          print(handle_se_process$get_exit_status())
          print(handle_se_process$read_error())
          print(handle_se_process$read_output())
          showNotification("Still querying...", type = "message")
          invalidateLater(60000, session) # Poll every minute
          # Navigate to the email input page
        } else {
          print("completed query succesful...")
          print(handle_se_process$get_exit_status())
          # Process completed
          if (handle_se_process$get_exit_status() == 0) {
            print("Completed querying data and running CaseControl_SE()")
            results_se <- handle_se_process$get_result()
            print(str(results_se))
            results(results_se)
            print(str(results()))
            # Notify the user and allow navigation to the next step
            print("ControlCase_SE method was executed successfully!")
            # showNotification("Process completed successfully!", type = "message")
            
          } else {
            # Handle errors
            print("Error occurred after querying and merging was finished, did not go into CC_SE")
            # showNotification("Error occurred during processing.", type = "error")
          }
        }
      })  
    })
    
    # Display the first 10 rows of the results dataframe
    output$results_preview <- renderDT({
      req(results())
      req(column_names())
      
      results_formatted <- format(x = results(), digits = 4, scientific = TRUE)
      print(results())
      print(str(results_formatted))
      print(str(results_formatted[ , !(colnames(results()) %in% column_names())]))
      print(colnames(results_formatted[ , !(colnames(results()) %in% column_names())]))
      print(c(colnames(results_formatted[ , !(colnames(results()) %in% column_names())])))
      # Display the first 10 rows as a preview and highlight the newly generated columns
      datatable(head(results_formatted, 10), options = list(dom = 't')) %>%
        formatStyle(
          columns = c(colnames(results_formatted[ , !(colnames(results()) %in% column_names())])),
          color = "green"
        )
    })
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("case_control_AF_estimates", ".text.gz")
      },
      content = function(file) {
        fwrite(results(), file, sep = "\t", compress = "gzip")
      }
    )
    
    return(results)
  })
}