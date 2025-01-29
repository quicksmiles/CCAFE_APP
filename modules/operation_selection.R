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
      actionButton(ns("run_casecontrolaf"), "Run CaseControl_AF", class = "btn-primary")
    ),
    conditionalPanel(
      condition = paste0("input['", ns("operation"), "'] == 'SE'"),
      selectInput(ns("super_population"), "Select Super-Population for Bias Correction", 
                  choices = c("total", "afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "remaining", "sas")),
      numericInput(ns("N_case_se"), "Number of cases:", value = NULL),
      numericInput(ns("N_control_se"), "Number of controls:", value = NULL),
      selectInput(ns("OR_colname_se"), "Odds Ratio or Beta Column:", choices = NULL),
      selectInput(ns("SE_colname"), "SE Column:", choices = NULL),
      selectInput(ns("chromosome_colname"), "Chromosome Column:", choices = NULL),
      selectInput(ns("position_colname"), "Position Column:", choices = NULL),
      actionButton(ns("run_casecontrolse"), "Run CaseControl_SE", class = "btn-primary")
    ),
    tableOutput(ns("results_preview"))
  )
}

operationSelectionServer <- function(id, uploaded_data, column_names) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveVal()
    
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
      
      updateNumericInput(session, "N_case", value = uploaded_data()$n_cases[1])
      updateNumericInput(session, "N_control", value = uploaded_data()$n_controls[1])
      updateNumericInput(session, "N_case_se", value = uploaded_data()$n_cases[1])
      updateNumericInput(session, "N_control_se", value = uploaded_data()$n_controls[1])
      updateSelectInput(session, "OR_colname", choices = column_names(), selected = oddRatioBeta)
      updateSelectInput(session, "OR_colname_se", choices = column_names(), selected = oddRatioBeta)
      updateSelectInput(session, "AF_total_colname", choices = column_names(), selected = "true_maf_pop")
      updateSelectInput(session, "SE_colname", choices = column_names(), selected = "SE")
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

    # observeEvent(input$run_casecontrolse, {
    #   req(uploaded_data())
    #   results_se <- CaseControl_SE(data = uploaded_data(),
    #                                N_case = input$N_case_se,
    #                                N_control = input$N_control_se,
    #                                OR_colname = input$OR_colname_se,
    #                                SE_colname = input$SE_colname,
    #                                chromosome_colname = input$chromosome_colname,
    #                                sex_chromosomes = FALSE,
    #                                position_colname = input$position_colname,
    #                                do_correction = FALSE)
    #   CaseControl_SE()
    #   results(results_se)
    # })

    observeEvent(input$run_casecontrolse, {
      # Move to email input page
      # updateNavbarPage(session, "CCAFE", selected = "Step3")
      req(uploaded_data())  # Ensure the user has uploaded data

      # Show a modal spinner while the process runs
      show_modal_spinner(spin = "self-building-square", text = "Processing query in the background...")

      # Run merge.R in the background
      merge_process <- callr::r_bg(function() {
        source("../CCAFE/merge.R") # Source the merge.R script
        maf_population_results # Execute the merge function
      })

      # Poll for completion of merge process
      observe({
        if (merge_process$is_alive()) {
          # If process is still running, don't do anything yet
          invalidateLater(1000, session) # Poll every 1 second
          # Navigate to the email input page
        } else {
          # Process completed
          if (merge_process$get_exit_status() == 0) {
            super_population_maf <- merge_process$get_result() # Get the results
            corr_data <- data.frame(CHR = super_population_maf$populations$chrom,
                                    POS = super_population_maf$populations$pos,
                                    proxy_MAF = super_population_maf$populations$paste0("MAF_", input$super_population)
            )
            # Perform CaseControl_SE operation
            results_se <- CaseControl_SE(data = uploaded_data(),
                                         N_case = input$N_case_se,
                                         N_control = input$N_control_se,
                                         OR_colname = input$OR_colname_se,
                                         SE_colname = input$SE_colname_se,
                                         chromosome_colname = input$chromosome_colname,
                                         position_colname = input$position_colname,
                                         do_correction = TRUE,
                                         correction_data = corr_data,
                                         sex_chromosomes = FALSE
                                         )

            # Store results in session
            session$userData$results_se <- results_se
            results(session$user$results_se)
            # Notify the user and allow navigation to the next step
            remove_modal_spinner()
            showNotification("Process completed successfully!", type = "message")

          } else {
            # Handle errors
            remove_modal_spinner()
            showNotification("Error occurred during processing.", type = "error")
          }
        }
      })
    })
    
    # Display the first 10 rows of the results dataframe
    output$results_preview <- renderTable({
              req(results())
              head(results(), 10)  # Display the first 10 rows as a preview
          })
    
    return(results)
  })
}