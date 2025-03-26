box::use(
  argonR[...],
  argonDash[...],
  reactR[...],
  shiny[...],
  shinyjs[...],
  DT[DTOutput, renderDT])


operationSelectionUI <- function(id) {
  ns <- NS(id)
  
  argonRow(
    argonColumn(
      width = 12,
      br(),
      br(),
      hidden(
        div(id = ns("modules"),
            argonCard(
              title = "Data Selection",
              width = 4,
              radioButtons(
                ns("data_source"),
                label = "Choose data source:",
                choices = c("Upload File" = "upload", "Use Sample Data" = "sample"),
                selected = "upload",
                inline = TRUE
              ),
              conditionalPanel(
                condition = paste0("input['", ns("data_source"), "'] == 'upload'"),
                fileInput(
                  ns("file"),
                  "Upload GWAS summary statistics (compressed text or VCF file)",
                  accept = c(".bgz", ".gz"),
                  buttonLabel = "Upload",
                  width = "100%"
                )
              ),
              actionButton(ns("process_file"), "Process Data", class = "btn btn-default btn-round")
            ),
            
            argonCard(
              title = "Inputs",
              width = 4,
              conditionalPanel(
                condition = paste0("input['", ns("operation"), "'] == 'AF'"),
                numericInput(ns("N_case"), "Number of cases:", value = NULL),
                numericInput(ns("N_control"), "Number of controls:", value = NULL),
                selectInput(ns("OR_colname"), "Odds Ratio Column", choices = NULL),
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
                selectInput(ns("OR_colname_se"), "Odds Ratio Column", choices = NULL),
                selectInput(ns("SE_colname_se"), "SE Column:", choices = NULL),
                selectInput(ns("chromosome_colname"), "Chromosome Column:", choices = NULL),
                selectInput(ns("position_colname"), "Position Column:", choices = NULL),
                textInput(ns("user_email"), "Email", placeholder = "Enter your email"),
                actionButton(ns("run_casecontrolse"), "Run CaseControl_SE", class = "btn-primary")
              )
            )
        ),
        DTOutput("data_table")
      )
    )
  )
}

  
  # argonRow(
  #   argonColumn(
  #     width = 4,
  #     argonCard(
  #       width = "auto",
  #       title = "Upload & Download",
  #       fileInput(ns("file"), "Upload your GWAS data (gzipped text or gzipped vcf files only)", accept = c(".bgz", ".gz")),
  #       actionButton(ns("process_file"), "Process File"),
  #       downloadButton(ns("download_results"), "Download Results")
  #     )
  #   ),
  #   argonColumn(
  #     width = 4,
  #     argonCard(
  #       width = "auto",
  #       title = "Input Selection & Filtering",
  #       # radioButtons(ns("operation"), "Select Operation",
  #       #              choices = c("CaseControl_AF" = "AF", "CaseControl_SE" = "SE")),
  #       argonRow(
  #         div(
  #           class = "btn-group-toggle",
  #           type = "radio",
  #           argonButton(ns("operation"), "Select Operation",
  #                        choices = c("CaseControl_AF" = "AF", "CaseControl_SE" = "SE"))
  #         )
  #       ),
  #       conditionalPanel(
  #         condition = paste0("input['", ns("operation"), "'] == 'AF'"),
  #         numericInput(ns("N_case"), "Number of cases:", value = NULL),
  #         numericInput(ns("N_control"), "Number of controls:", value = NULL),
  #         selectInput(ns("OR_colname"), "Odds Ratio Column", choices = NULL),
  #         selectInput(ns("AF_total_colname"), "Total AF Column:", choices = NULL),
  #         actionButton(ns("run_casecontrolaf"), "Run CaseControl_AF", class = "btn-primary")
  #       ),
  #       conditionalPanel(
  #         condition = paste0("input['", ns("operation"), "'] == 'SE'"),
  #         selectInput(ns("super_population"), "Select Super-Population for Bias Correction", 
  #                     choices = c("total", "afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "remaining", "sas")),
  #         numericInput(ns("N_case_se"), "Number of cases:", value = NULL),
  #         numericInput(ns("N_control_se"), "Number of controls:", value = NULL),
  #         selectInput(ns("OR_colname_se"), "Odds Ratio Column", choices = NULL),
  #         selectInput(ns("SE_colname_se"), "SE Column:", choices = NULL),
  #         selectInput(ns("chromosome_colname"), "Chromosome Column:", choices = NULL),
  #         selectInput(ns("position_colname"), "Position Column:", choices = NULL),
  #         textInput(ns("user_email"), "Email", placeholder = "Enter your email"),
  #         actionButton(ns("run_casecontrolse"), "Run CaseControl_SE", class = "btn-primary")
  #       ),
  #       uiOutput(ns("dynamic_inputs"))
  #     )
  #   ),
  #   argonColumn(
  #     width = 4,
  #     argonCard(
  #       title = "Results",
  #       width = "auto",
  #       tableOutput(ns("file_preview")),
  #       DTOutput(ns("results_table"))
  #     )
  #   )
  # )
  # 
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

operationSelectionServer <- function(id, uploaded_data, column_names, main_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    results <- reactiveVal()
    
    # Reactive function to load sample data
    getSampleData <- reactive({
      sampleDat <- utils::read.table("app/static/sampledata.txt", header = T)
      sampleDat
    })
    
    # Choose between uploaded data or sample data
    current_data <- reactive({
      if (input$data_source == "sample") {
        getSampleData()
      } else {
        req(uploaded_data())
        uploaded_data()
      }
    })
      
    selected_super_population <- reactive({
      input$super_population
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
      updateSelectInput(session, "AF_total_colname", choices = column_names(), selected = "AF_total")
      updateSelectInput(session, "SE_colname_se", choices = column_names(), selected = "SE")
      updateSelectInput(session, "chromosome_colname", choices = column_names(), selected = "chrom")
      updateSelectInput(session, "position_colname", choices = column_names(), selected = "pos")
    })
    
    observe({
      req(input$BETA_colname)
      df <- current_data()
      if (input$calculate_OR == TRUE) {
        df$OR <- exp(df[[input$BETA_colname]])
        uploaded_data(df)
      }
      updateSelectInput(session, "OR_colname", choices = colnames(df), selected = "OR")
    })
    
    observeEvent(input$run_casecontrolaf, {
      req(current_data())
      
      results_af <- CaseControl_AF(
        data = current_data(),
        N_case = input$N_case,
        N_control = input$N_control,
        OR_colname = input$OR_colname,
        AF_total_colname = input$AF_total_colname
      )
      
      results(results_af)
    })
    
    observeEvent(input$run_casecontrolse, {
      req(current_data())
      req(selected_super_population())
      req(input$user_email)
      
      data <- current_data()
      pop <- selected_super_population()
      
      user_email <- input$user_email
      N_case_se <- input$N_case_se
      N_control_se <- input$N_control_se
      OR_colname_se <- input$OR_colname_se
      SE_colname_se <- input$SE_colname_se
      chromosome_colname <- input$chromosome_colname
      position_colname <- input$position_colname
      
      # Simulate processing function
      results_se <- data  # Placeholder for actual CaseControl_SE output
      results(results_se)
    })
    
    # Display the first 10 rows of the results dataframe
    output$results_preview <- renderDT({
      req(results())
      
      datatable(
        head(results(), 10),
        options = list(dom = 't')
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
