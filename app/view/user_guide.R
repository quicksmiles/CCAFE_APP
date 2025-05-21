box::use(
  shiny[...],
  argonR[...],
  argonDash[...],
  reactable[...],
  tibble[...]
)

guideUI <- function(id) {
  ns <- NS(id)
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        argonH1(display = 3, "Quick Start Guide"),
        argonLead(
          tagList(
            "How to use the ", argonBadge("Analysis", status = "primary"),
            "module"
          )
        ),
        reactableOutput(ns("ccafe_description_table"))
      )
    ),
        br(),
        argonTabSet(
          id = ns("analysis_steps"),
          card_wrapper = TRUE,
          horizontal = TRUE,
          size = "sm",
          width = 12,
          #iconList = list(argonIcon("zoom-split-in"), argonIcon("settings-gear-65"), argonIcon("check-bold")),
          
          # step 0
          argonTab(
            tabName = "Step 0: Analysis Module",
            active = TRUE,
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("AnalysisDefault"), height = "auto") 
              ),
              argonColumn(
                width = 6,
                tagList(
                  argonTextColor(
                    tags$p(
                      "Once you click on the ", argonBadge("Analysis"), " tab, the module will load."
                    ),
                    color = "dark"
                  )
                )
              )
            )
          ),
          
          # step 1
          argonTab(
            tabName = "Step 1: Upload Data",
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("DataPreview"), height = "auto")
              ),
              argonColumn(
                width = 6,
                tagList(   # Use tagList() to wrap everything
                  argonTextColor(
                    tags$p(
                      "Once within the ", argonBadge("Analysis"), " module you will be able to upload your data."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "To do so, select ", strong("Upload File"), 
                      ", which will open your file browser. Select your desired file, which will populate the 
                      selected file name."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "Alternatively, you can select ", strong("Use Sample Data"), 
                      " to use the sample data to preview how the app works."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "Once you have ensured you selected the desired file (or selected sample data), 
                      click the ", argonBadge("Process File"), 
                      " button, which will perform pre-processing of the file and show a preview table. This step removes 
                      rows with missing data and standardizes column headers."
                    ),
                    color = "dark"
                  )
                )
              )
            )
          ),
          
          # step 2
          argonTab(
            tabName = "Step 2: Select Operation",
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("SelectOperation"), height = "auto") 
              ),
              argonColumn(
                width = 6,
                tagList(
                  argonTextColor(
                    tags$p(
                      "After you have uploaded your data, you will select which operation you want to perform."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "Both operations require the number of cases, number of controls, and effect estimate."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "If you have total allele frequency (AF), you should select ", strong("CaseControl_AF"), 
                      " and continue to step 3a."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "If you have SE (of the log(OR)), you should select ", strong("CaseControl_SE"), 
                      " and continue to step 3b."
                    ),
                    color = "dark"
                  )
                )
              )
            )
          ),
          
          # step 3a
          argonTab(
            tabName = "Step 3a: Select Inputs AF",
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("AFInputs_Beta"), height = "auto")
              ),
              argonColumn(
                width = 6,
                tagList(   # Use tagList() to wrap everything
                  argonTextColor(
                    tags$p(
                      "Once the ", strong("CaseControl_AF"), " operation is selected, you will be prompted to enter
                      the required inputs."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "You will need to specifiy whether your effect size estimates are ", strong("Beta"),
                      " or ", strong("Odds Ratios"), 
                      ". You will then be prompted to select the correct columm given your selection."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "If you select ", strong("Beta"), " a column will be added with the calculated odds ratios."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "Once you have entered the required inputs, you can click the ", argonBadge("Run CaseControl_AF"), 
                      " button, which will run the method to estimate case and control AFs using total AF."
                    ),
                    color = "dark"
                  )
                )
              )
            )
          ),
          
          # step 3b
          argonTab(
            tabName = "Step 3b: Select Inputs SE",
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("SEInputs_Beta"), height = "auto")
              ),
              argonColumn(
                width = 6,
                tagList(   # Use tagList() to wrap everything
                  argonTextColor(
                    tags$p(
                      "Once the ", strong("CaseControl_SE"), " operation is selected, you will be prompted to enter
                      the required inputs."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "You will need to specifiy which gnomAD group you would like to use for bias correction from
                      the dropdown menu. For more information about the gnomAD groups see ",
                      tags$a(href = "https://gnomad.broadinstitute.org/news/2023-11-gnomad-v4-0/",
                             target = "_blank", # opens link in a new tab 
                             "here.",
                             style = "color: #007bff; text-decoration: underline;")
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "You will also need to specifiy whether your effect size estimates are ", strong("Beta"),
                      " or ", strong("Odds Ratios"), 
                      ". You will then be prompted to select the correct columm given your selection."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "If you select ", strong("Beta"), " a column will be added with the calculated odds ratios."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "Once you have entered the required inputs, you can click the ", argonBadge("Run CaseControl_SE"), 
                      " button, which will run the method to estimate case and control AFs using total AF."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "The CCAFE App will query your input data against gnomAD v4.0 and merge it with the AF data for
                      the selected group to be used as proxies for bias correction. As this process can be time consuming
                      due to limitations of the gnomAD API, the results will be emailed to the entered email address."
                    ),
                    color = "dark"
                  )
                )
              )
            )
          ),
          
          # step 4
          argonTab(
            tabName = "Step 4: Results",
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("DownloadResults"), height = "auto") 
              ),
              argonColumn(
                width = 6,
                tagList(
                  argonTextColor(
                    tags$p(
                      "After the case and control AFs have been estimated, the results preview
                      with population and replace the data preview. The new columns will be appended
                      to the table."
                    ),
                    color = "dark"
                  ),
                  argonTextColor(
                    tags$p(
                      "You can download the results as a .txt.gz by clicking ", argonBadge("Download Results")
                    ),
                    color = "dark"
                  )
                )
              )
            )
          )
        )
      )
}

# Server Module
guideServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$ccafe_description_table <- renderReactable({
      ccafe_description_table <- tibble(
        tab = c(
          "Analysis: CaseControlAF()",
          "Analysis: CaseControlSE()"
        ),
        input = c(
          "number of cases, number of controls, odds ratio (OR), total allele frequency",
          "position, chromosome, number of cases, number of controls, odds ratio (OR) or beta, standard error (SE), population group, email"
        ),
        output = c(
          "Table 00-0.00 GWAS Summary Statistics: Unadjusted Case and Control Allele Frequencies",
          "Table 00-0.00 GWAS Summary Statistics: Unadjusted and Adjusted Case and Control Allele Frequencies"
        )
      )
      reactable(ccafe_description_table)
    })
    
    output$AnalysisDefault<- renderImage({
      list(
        src = "app/static/AnalysisDefault.png",
        alt = "Filter Screenshot 1",
        width = "100%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$DataPreview <- renderImage({
      list(
        src = "app/static/DataPreview.png",
        alt = "Filter Screenshot 2",
        width = "100%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$SelectOperation <- renderImage({
      list(
        src = "app/static/SelectOperation.png",
        alt = "Filter Screenshot 2",
        width = "60%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$AFInputs_Beta <- renderImage({
      list(
        src = "app/static/AFInputs_Beta.png",
        alt = "Filter Screenshot 2",
        width = "60%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$SEInputs_Beta <- renderImage({
      list(
        src = "app/static/SEInputs_Beta.png",
        alt = "Filter Screenshot 2",
        width = "60%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$DownloadResults <- renderImage({
      list(
        src = "app/static/DownloadResults.png",
        alt = "Filter Screenshot 2",
        width = "100%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$hex <- renderImage({
      list(
        src = "app/static/CCAFE-hex.png",
        alt = "Filter Screenshot 3",
        width = "20%",
        height = "auto"
      )
    }, deleteFile = FALSE)
  })
}