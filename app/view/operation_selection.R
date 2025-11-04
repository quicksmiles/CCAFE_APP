box::use(
  argonR[...],
  argonDash[...],
  CCAFE[...],
  data.table[fwrite],
  dplyr[...],
  reactR[...],
  shiny[...],
  shinyjs[...],
  stats[...],
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
          background_color = "white",
          shadow = TRUE,
          border_level = 5,
          radioButtons(
            ns("data_source"),
            label = "Choose data source:",
            choices = c("Upload File" = "upload", "Use Sample Data" = "sample"),
            selected = "upload",
            inline = TRUE
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
            fileInput(ns("file"), "Upload GWAS summary statistics (compressed text or VCF file)", accept = c(".bgz", ".gz")),
            # actionButton(ns("process_file"), "Preview Data", class = "btn btn-default btn-round"),
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s'] == 'sample'", ns("data_source")),
            # actionButton(ns("process_file"), "Preview Data", class = "btn btn-default btn-round")
          ),

          actionButton(ns("process_file"), "Preview Data", class = "btn btn-default btn-round")
        ),
        
        # Inputs Card
        argonCard(
          title = "Inputs",
          width = 12,
          background_color = "white",
          shadow = TRUE,
          border_level = 5,
          
          # Conditional Panel: Only display inputs when data is available
          conditionalPanel(
            condition = sprintf("input['%s'] !== null || input['%s'] > 0", ns("file"), ns("process_file")),
            
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
                          choices = c("total", 
                                      "afr", 
                                      "ami", 
                                      "amr", 
                                      "asj", 
                                      "eas", 
                                      "fin", 
                                      "mid", 
                                      "nfe", 
                                      "remaining", 
                                      "sas"),
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
            ),
          ),
          
          actionButton(ns("reset_button"), "Reset All", class = "btn-neutral")
        )
      ),
      
      # Right Column with Full-Height DTOutput
      argonColumn(
        width = 8,
        argonCard(
          width = 12,
          background_color = "white",
          shadow = TRUE,
          
          # dynamic title
          uiOutput(ns("card_title")),
          
          conditionalPanel(
            condition = sprintf("!input['%s']", ns("show_results")),  # Show data preview when results are not available
            DTOutput(ns("data_preview"))
          ),
          
          conditionalPanel(
            condition = sprintf("input['%s']", ns("show_results")),  # Show results preview when results are available
            DTOutput(ns("results_preview"))
          ),
          
          uiOutput(ns("download_results_ui"))
        )
      )
    )
  )
}

operationSelectionServer <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    results <- reactiveVal(NULL)
    current_data <- reactiveVal(NULL)
    show_results <- reactiveVal(FALSE)
    uploaded_data <- reactiveVal(NULL)
    bg_job <- reactiveVal(NULL)
    
    # Reactive function to load sample data
    getSampleData <- reactive({
      sampleDat <- utils::read.table("app/static/sampledata.txt", header = T)
      colnames(sampleDat)[1:4] <- c("chrom", "pos", "ref", "alt")
      sampleDat$chrom <- ifelse(grepl("^chr", sampleDat$chrom), gsub("^chr", "", sampleDat$chrom), sampleDat$chrom)
      sampleDat <- sampleDat[complete.cases(sampleDat), ]
      sampleDat
    })
    
    observeEvent(input$file, {
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
    
    # Render preview of uploaded data or sample data
    observeEvent(input$process_file, {
      
      if (input$data_source == "sample") {
        current_data(getSampleData())
      } else if (input$data_source == "upload") {
        current_data(uploaded_data())
      }
      
      output$data_preview <- renderDT({
        req(!show_results())
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
      
      updateSelectInput(session, "BETA_colname_AF", choices = column_names(), selected = "beta")
      updateSelectInput(session, "BETA_colname_SE", choices = column_names(), selected = "beta")
      updateSelectInput(session, "OR_colname_AF", choices = column_names(), selected = "OR")
      updateSelectInput(session, "OR_colname_SE", choices = column_names(), selected = "OR")
      updateSelectInput(session, "AF_total_colname", choices = column_names(), selected = "true_maf_pop")
      updateSelectInput(session, "SE_colname_se", choices = column_names(), selected = "SE")
      updateSelectInput(session, "chromosome_colname", choices = column_names(), selected = "chrom")
      updateSelectInput(session, "position_colname", choices = column_names(), selected = "pos")
    })
    
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
      show_results(TRUE)
    })
    
    observeEvent(input$run_casecontrolse, {
      # Move to email input page
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

      # save upload_data to a tempfile so we don't have to serialize it to pass to the bg function
      uploaded_data_path <- tempfile(fileext = ".rds")
      saveRDS(processed_data(), uploaded_data_path)
      
      # Show a modal spinner while the process runs
      #show_modal_spinner(spin = "self-building-square", text = "Processing query in the background...")
      # serialized_handle_se <- serialize(object = handle_se, NULL)

      # Run merge.R in the background
      proc <- callr::r_bg(
        function(selected_population, user_email, uploaded_data_path, N_case_se, N_control_se, 
                 OR_colname_se, SE_colname_se, chromosome_colname, position_colname) {
          # FIXME: move this to app.R, and if that doesn't work, then move it back
          # Source the merge.R script
          message("Entering background process...")

          source(file.path(Sys.getenv("APP_ROOT", unset="../CCAFE"), "app/logic/handle_se.R"))
          message("Sourced handle_se.R, now executing handle_se()...")

          # load uploaded_data from the tempfile path
          uploaded_data <- readRDS(uploaded_data_path)
          message("Loading data from ", uploaded_data_path)

          handle_se(selected_population, user_email, uploaded_data, N_case_se, N_control_se,
                    OR_colname_se, SE_colname_se, chromosome_colname, position_colname) # Execute function
        }, 
        args = list(selected_population, user_email, uploaded_data_path, N_case_se, N_control_se, 
                    OR_colname_se, SE_colname_se, chromosome_colname, position_colname), 
        supervise = TRUE
      )

      message("Started background process with PID: ", proc$get_pid())

      # store the handle in a reactive so we can observe it
      bg_job(proc)

      showNotification("Starting process in the background...", type = "message")
    })

    # poll the background process every 500ms for output and completion status
    resultPoll <- reactivePoll(
      intervalMillis = 500,
      session = session,
      checkFunc = function() {
        j <- bg_job()
        if (is.null(j)) return("no-job")

        # show stdout and stderr for debugging
        msg <- j$read_output_lines()
        if (length(msg)) message(paste("bg_task (stdout): ", msg, collapse="\n"))

        err <- j$read_error_lines()
        if (length(err)) message(paste("bg_task (stderr): ", err, collapse="\n"))

        if (j$is_alive()) "running" else paste0("exit:", j$get_exit_status())
      },
      valueFunc = function() {
        j <- bg_job()
        
        if (is.null(j) || j$is_alive()) {
          return(list(done = FALSE))
        }

        # process has finished; decide success vs failure
        exit <- j$get_exit_status()
        if (!is.null(exit) && exit != 0) {
          # non-zero exit — definitely failed
          return(list(
            done = TRUE, ok = FALSE, exit_status = exit,
            error = paste(c(j$read_error_lines(), collapse = "\n"))
          ))
        }
        # exit status is 0, but get_result() could still raise a remote error
        out <- tryCatch(
          list(done = TRUE, ok = TRUE, result = j$get_result()),
          error = function(e) {
            list(done = TRUE, ok = FALSE, exit_status = 1, error = conditionMessage(e))
          }
        )
        # (optional) include captured output for debugging
        out$output <- paste(j$read_output_lines(), collapse = "\n")
        out
      }
    )

    observeEvent(resultPoll(), ignoreInit = TRUE, {
      payload <- resultPoll()
      req(isTRUE(payload$done))  # only run when finished

      if (isTRUE(payload$ok)) {
        result <- payload$result
        showNotification(sprintf("Job done! Result = %s", as.character(result)), type = "message")
      } else {
        msg <- if (!is.null(payload$error) && nzchar(payload$error)) payload$error else "Unknown error"
        showNotification(paste("Job failed:", msg), type = "error", duration = NULL)
        # you can also surface payload$exit_status or payload$output for debugging
      }
    })
    
    # Display the first 10 rows of the results dataframe
    output$results_preview <- renderDT({
      req(results())  # Ensure results exist
      req(show_results())
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
      if (!is.null(results()) & length(input$operation) > 0) {
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
    
    output$download_results_ui <- renderUI({
      req(results()) # ensure results exist before displaying button
      argonCard(
        width = 12,
        downloadButton(ns("download_results"), "Download Results", class = "btn btn-success")
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
    
    # reset button logic
    observeEvent(input$reset_button, {
      # clear the data
      # clear results
      current_data(NULL)
      results(NULL)
      uploaded_data(NULL)
      show_results(FALSE)
      
      reset("file")

      # reset select data to default
      updateRadioButtons(session, "data_source", selected = "upload")
      
      # Reset all inputs to their default values
      updateRadioButtons(session, "operation", selected = character(0))  # Clear the radio buttons
      updateNumericInput(session, "N_case", value = 16550)  # Reset number of cases
      updateNumericInput(session, "N_control", value = 403923)  # Reset number of controls
      updateRadioButtons(session, "effect_estimate_type_AF", selected = "beta")  # Reset effect estimate type
      updateSelectInput(session, "BETA_colname_AF", selected = NULL)  # Reset select input for Beta Column
      updateSelectInput(session, "OR_colname_AF", selected = NULL)  # Reset select input for Odds Ratio Column
      updateSelectInput(session, "AF_total_colname", selected = NULL)  # Reset select input for Total AF Column
      updateSelectInput(session, "population", selected = "nfe")  # Reset population selection
      updateNumericInput(session, "N_case_se", value = 16550)  # Reset number of cases for SE
      updateNumericInput(session, "N_control_se", value = 403923)  # Reset number of controls for SE
      updateRadioButtons(session, "effect_estimate_type_SE", selected = "beta")  # Reset effect estimate type
      updateSelectInput(session, "BETA_colname_SE", selected = NULL)  # Reset select input for Beta Column
      updateSelectInput(session, "OR_colname_SE", selected = NULL)  # Reset select input for Odds Ratio Column
      updateSelectInput(session, "SE_colname_se", selected = NULL)  # Reset SE Column
      updateSelectInput(session, "chromosome_colname", selected = "CHR")  # Reset Chromosome Column
      updateSelectInput(session, "position_colname", selected = "POS")  # Reset Position Column
      updateTextInput(session, "user_email", value = "")  # Reset Email input

      # Hide data and results previews when reset is clicked
     
      # Clear or reset the data preview table 
      shinyjs::hide("results_preview")
    
      output$data_preview <- renderDT(NULL) # reset to null DT
      
      shinyjs::show("data_preview")
      
    })
    
    return(results)
  })
}