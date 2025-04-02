box::use(
  argonR[...],
  argonDash[...],
  CCAFE[...],
  data.table[fwrite],
  dplyr[...],
  reactR[...],
  shiny[...],
  shinyjs[...],
  DT[...],
)

box::use(
  app/view/file_upload[...],
  app/logic/upload[...],
  app/logic/handle_se[...],
)


operationSelectionUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(), # Enable JavaScript control
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
            actionButton(ns("process_file"), "Preview Data", class = "btn btn-default btn-round")
          )
        ),
        
        # Inputs Card
        argonCard(
          title = "Inputs",
          width = 12,
          
          # Conditional Panel: Only display inputs when data is available
          conditionalPanel(
            condition = sprintf("input['%s'] !== null || input['%s'] > 0", ns("file_upload"), ns("process_file")),
            
            # Operation Selection (Radio Buttons)
            radioButtons(
              ns("operation"), "Select Operation",
              choices = c("CaseControl_AF" = "AF", "CaseControl_SE" = "SE"),
              selected = character(0),
              inline = TRUE
            ),
            
            # AF Section
            conditionalPanel(
              condition = sprintf("input['%s'] == 'AF'", ns("operation")),
              numericInput(ns("N_case"), "Number of cases:", value = 16550),
              numericInput(ns("N_control"), "Number of controls:", value = 403923),
              
              radioButtons(
                ns("effect_estimate_type_AF"),
                "Effect Estimate Type:",
                choices = c("Beta" = "beta", "Odds Ratio" = "or"),
                inline = TRUE
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'beta'", ns("effect_estimate_type_AF")),
                selectInput(ns("BETA_colname_AF"), "Beta Column", choices = NULL)
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'or'", ns("effect_estimate_type_AF")),
                selectInput(ns("OR_colname_AF"), "Odds Ratio Column", choices = NULL)
              ),
              
              selectInput(ns("AF_total_colname"), "Total AF Column:", choices = NULL),
              actionButton(ns("run_casecontrolaf"), "Run CaseControl_AF", class = "btn-primary")
            ),
            
            # SE Section
            conditionalPanel(
              condition = sprintf("input['%s'] == 'SE'", ns("operation")),
              selectInput(ns("population"), "Select Population for Bias Correction",
                          choices = c("total", "afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "remaining", "sas"),
                          selected = "nfe"),
              numericInput(ns("N_case_se"), "Number of cases:", value = 16550),
              numericInput(ns("N_control_se"), "Number of controls:", value = 403923),
              
              radioButtons(
                ns("effect_estimate_type_SE"),
                "Effect Estimate Type:",
                choices = c("Beta" = "beta", "Odds Ratio" = "or"),
                inline = TRUE
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'beta'", ns("effect_estimate_type_SE")),
                selectInput(ns("BETA_colname_SE"), "Beta Column", choices = NULL)
              ),
              
              conditionalPanel(
                condition = sprintf("input['%s'] == 'or'", ns("effect_estimate_type_SE")),
                selectInput(ns("OR_colname_SE"), "Odds Ratio Column", choices = NULL)
              ),
              
              selectInput(ns("SE_colname_se"), "SE Column:", choices = NULL),
              selectInput(ns("chromosome_colname"), "Chromosome Column:", choices = NULL, selected = "CHR"),
              selectInput(ns("position_colname"), "Position Column:", choices = NULL, selected = "POS"),
              textInput(ns("user_email"), "Email", placeholder = "Enter your email"),
              actionButton(ns("run_casecontrolse"), "Run CaseControl_SE", class = "btn-primary"),
            )
            
          )
        )
      ),
      
      # Right Column with Full-Height DTOutput
      argonColumn(
        width = 8,
        argonCard(
          width = 12,
          
          # dynamic title
          uiOutput(ns("card_title")),
          
          conditionalPanel(
            condition = sprintf("!%s", ns("show_results")),  # Show data preview when results are not available
            DTOutput(ns("data_preview"))
          ),
          
          conditionalPanel(
            condition = sprintf("%s", ns("show_results")),  # Show results preview when results are available
            DTOutput(ns("results_preview"))
          ),
          
          downloadButton(ns("download_results"), "Download Results")
        )
      )
    )
  )
}

operationSelectionServer <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveVal()
    
    # Reactive function to load sample data
    getSampleData <- reactive({
      sampleDat <- utils::read.table("app/static/sampledata.txt", header = T)
      sampleDat
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
    
    # Render preview of uploaded data or sample data
    observeEvent(input$process_file, {
      output$data_preview <- renderDT({
        req(current_data())
        
        datatable(
          current_data(),
          options = list(
            dom = 'Bfrtip',
            paging = TRUE,
            searching = TRUE,
            ordering = TRUE,
            scrollX = TRUE,
            autoWidth = TRUE
          ),
          rownames = FALSE
        )
      })
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
      
      updateSelectInput(session, "BETA_colname_AF", choices = column_names(), selected = "BETA")
      updateSelectInput(session, "BETA_colname_SE", choices = column_names(), selected = "BETA")
      updateSelectInput(session, "OR_colname_AF", choices = column_names(), selected = "OR")
      updateSelectInput(session, "OR_colname_SE", choices = column_names(), selected = "OR")
      updateSelectInput(session, "AF_total_colname", choices = column_names(), selected = "true_maf_pop")
      updateSelectInput(session, "SE_colname_se", choices = column_names(), selected = "SE")
      updateSelectInput(session, "chromosome_colname", choices = column_names(), selected = "chrom")
      updateSelectInput(session, "position_colname", choices = column_names(), selected = "pos")
    })
    # # observe if the calculate OR button is toggled
    # # updated_data <- reactiveVal()
    # observe({
    #   req(uploaded_data())  # Ensure data is uploaded first
    #   
    #   # Only proceed if toggle is ON
    #   if (input$toggle_button) {
    #     
    #     # Get the dataframe
    #     df <- uploaded_data()
    #     
    #     # Apply calculation if the Beta column is selected
    #     if (!is.null(input$BETA_colname) && input$BETA_colname %in% colnames(df)) {
    #       df$OR <- exp(df[[input$BETA_colname]])
    #       uploaded_data(df)
    #       
    #       # Update the OR column selection
    #       updateSelectInput(session, "OR_colname", choices = colnames(df), selected = "OR")
    #     }
    #     
    #   } else {
    #     # Optionally reset or clear values when toggle is OFF
    #     updateSelectInput(session, "BETA_colname", choices = NULL)
    #     updateSelectInput(session, "OR_colname", choices = NULL)
    #   }
    # })
    
    # function to compute OR from beta
    compute_OR <- reactive({
      req(current_data())  # Ensure data is available
      
      data <- current_data()
      
      # Determine which operation is selected
      operation <- input$operation
      
      # Determine if Beta is selected for OR calculation
      if (operation == "AF" && input$effect_estimate_type_AF == "beta") {
        beta_colname <- input$BETA_colname_AF
      } else if (operation == "SE" && input$effect_estimate_type_SE == "beta") {
        beta_colname <- input$BETA_colname_SE
      } else {
        return(data)  # No Beta column selected, return unmodified data
      }
      
      # Compute OR only when Beta is used
      if (!is.null(beta_colname) && beta_colname %in% colnames(data)) {
        if (operation == "AF") {
          data$OR_AF <- exp(data[[beta_colname]])  # Compute OR only for CaseControl_AF
        } else if (operation == "SE") {
          data$OR_SE <- exp(data[[beta_colname]])  # Compute OR only for CaseControl_SE
        }
      }
      
      return(data)
    })
    
    processed_data <- reactive({
      req(compute_OR())  # Ensure OR is computed if needed
      compute_OR()       # Return the modified dataset with OR values (if applicable)
    })
    
    observeEvent(input$run_casecontrolaf, {
      req(processed_data())
      
      results_af <- CaseControl_AF(
        data = processed_data(),
        N_case = input$N_case,
        N_control = input$N_control,
        OR_colname = ifelse(input$effect_estimate_type_AF == "beta", "OR_AF", input$OR_colname_AF),
        AF_total_colname = input$AF_total_colname
      )
      # Move to email input page
      # updateNavbarPage(session, "CCAFE App", selected = "Step3")
      results(results_af)
      
      shinyjs::hide("data_preview")
      shinyjs::show("results_preview")
    })
    
    observeEvent(input$run_casecontrolse, {
      # Move to email input page
      # updateNavbarPage(session, "CCAFE", selected = "Step3")
      req(processed_data())  # Ensure the user has uploaded data
      req(selected_population())
      req(input$user_email)
      
      uploaded_data <- processed_data()
      selected_population <- selected_population()
      
      user_email <- input$user_email
      N_case_se <- input$N_case_se
      N_control_se <- input$N_control_se
      OR_colname_se <- ifelse(input$effect_estimate_type_SE == "beta", "OR_SE", input$OR_colname_SE)
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
            
            shinyjs::hide("data_preview")
            shinyjs::show("results_preview")
            
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
      req(results())  # Ensure results exist
      
      results_formatted <- format(results(), digits = 4, scientific = TRUE)
      
      new_columns <- setdiff(colnames(results_formatted), colnames(current_data()))
      
      datatable(
        results_formatted,
        options = list(
          dom = 't',
          paging = TRUE,
          searching = TRUE,
          ordering = TRUE,
          scrollX = TRUE,
          autoWidth = TRUE
        )
      ) %>%
        formatStyle(
          columns = new_columns,
          color = "green"
        )
    })
    
    # Dynamic Title Update
    output$card_title <- renderUI({
      if (!is.null(results())) {
        if (input$operation == "AF") {
          h4("Results Preview: CaseControl_AF")
        } else if (input$operation == "SE") {
          h4("Results Preview: CaseControl_SE")
        } else {
          h4("Results Preview")
        }
      } else {
        h4("Data Preview")
      }
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