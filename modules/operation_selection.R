operationSelectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Choose Operation"),
    radioButtons(ns("operation"), "Select Operation",
                 choices = c("CaseControl_AF" = "AF", "CaseControl_SE" = "SE")),
    conditionalPanel(
      condition = paste0("input['", ns("operation"), "'] == 'AF'"),
      numericInput(ns("N_case"), "Number of cases:", value = NULL),
      numericInput(ns("N_control"), "Number of controls:", value = NULL),
      selectInput(ns("OR_colname"), "Odds Ratio or Beta Column:", choices = NULL),
      selectInput(ns("AF_total_colname"), "Total AF Column:", choices = NULL),
      actionButton(ns("run_casecontrolaf"), "Run CaseControl_AF", class = "btn-primary"),
      downloadButton(ns("download_results"))
    ),
    conditionalPanel(
      condition = paste0("input['", ns("operation"), "'] == 'SE'"),
      selectInput(ns("super_population"), "Select Super-Population for Bias Correction", 
                  choices = c("total", "afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "remaining", "sas")),
      numericInput(ns("N_case_se"), "Number of cases:", value = NULL),
      numericInput(ns("N_control_se"), "Number of controls:", value = NULL),
      selectInput(ns("OR_colname_se"), "Odds Ratio or Beta Column:", choices = NULL),
      selectInput(ns("SE_colname_se"), "SE Column:", choices = NULL),
      selectInput(ns("chromosome_colname"), "Chromosome Column:", choices = NULL),
      selectInput(ns("position_colname"), "Position Column:", choices = NULL),
      textInput(ns("user_email"), "Email", placeholder = "Enter your email"),
      actionButton(ns("run_casecontrolse"), "Run CaseControl_SE", class = "btn-primary")
    ),
    
    DTOutput(ns("results_preview"))
  )
}

operationSelectionServer <- function(id, uploaded_data, column_names, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveVal()
    
    selected_super_population <- reactive({
      input$super_population  # Automatically updates when user selects a value
    })
    
    observeEvent(column_names(), {
      req(column_names())
      req(uploaded_data())
      
      oddRatioBeta <- if("OR" %in% column_names() == TRUE){
        "OR"
      } else if("BETA" %in% column_names() == TRUE){
        "BETA"
      } else{
        oddRationBeta = NULL
      }
      
      updateNumericInput(session, "N_case", value = input$N_case)
      updateNumericInput(session, "N_control", value = input$N_control)
      updateNumericInput(session, "N_case_se", value = input$N_case_se)
      updateNumericInput(session, "N_control_se", value = input$N_control_se)
      updateSelectInput(session, "OR_colname", choices = column_names(), selected = oddRatioBeta)
      updateSelectInput(session, "OR_colname_se", choices = column_names(), selected = oddRatioBeta)
      updateSelectInput(session, "AF_total_colname", choices = column_names(), selected = "AF_total")
      updateSelectInput(session, "SE_colname_se", choices = column_names(), selected = "SE")
      updateSelectInput(session, "chromosome_colname", choices = column_names(), selected = "chrom")
      updateSelectInput(session, "position_colname", choices = column_names(), selected = "pos")
    })
    
    observeEvent(input$run_casecontrolaf, {
      req(uploaded_data())
      results_af <- CaseControl_AF(data = uploaded_data(),
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
      req(selected_super_population())
      req(input$user_email)
      
      uploaded_data <- uploaded_data()
      selected_population <- selected_super_population()
      
      user_email <- input$user_email
      N_case_se <- input$N_case_se
      N_control_se <- input$N_control_se
      OR_colname_se <- input$OR_colname_se
      SE_colname_se <- input$SE_colname_se
      chromosome_colname <- input$chromosome_colname
      position_colname <- input$position_colname
      
      # Show a modal spinner while the process runs
      #show_modal_spinner(spin = "self-building-square", text = "Processing query in the background...")

      # Run merge.R in the background
      handle_se_process <- callr::r_bg(
        function(selected_population, user_email, uploaded_data, N_case_se, N_control_se, 
                 OR_colname_se, SE_colname_se, chromosome_colname, position_colname) {
          # FIXME: move this to app.R, and if that doesn't work, then move it back
          source("../CCAFE/handle_casecontrolse.R") # Source the merge.R script
          handle_cc_se(selected_population, user_email, uploaded_data, N_case_se, N_control_se,
                       OR_colname_se, SE_colname_se, chromosome_colname, position_colname)# Execute function
        }, 
        args = list(selected_population, user_email, uploaded_data, N_case_se, N_control_se, 
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
            results(results_se)
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