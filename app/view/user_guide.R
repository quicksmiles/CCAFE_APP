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
        argonH1(display = 1, "Application Guide"),
        
        argonLead(
          "The CCAFE Shiny Application contains a distinct interface, 
          displaying the analysis output table as described below."
        ),
        reactableOutput(ns("ccafe_description_table"))
      )
    ),
    br(),
    argonRow(
      argonColumn(
        width = 12,
        argonH1(display = 3, "Inputs & Filters"),
        argonLead(
          tagList(
            "The", argonBadge("Analysis", status = "primary"),
            "module allows for filters to be applied based on variables in the",
            argonBadge("CHR1_PHENO_295.1_EUR", status = "info"), "and",
            argonBadge("CHR1_UKBB", status = "info"), "data sets. 
            Below is an example of performing population specific estimations 
            using beta to calculate odds ratio and gnomAD proxy values to calculate
            adjusted case and control allele frequencies within the module:"
          )
        ),
        br(),
        argonTabSet(
          id = ns("analysis_steps"),
          card_wrapper = TRUE,
          horizontal = TRUE,
          size = "sm",
          width = 12,
          iconList = list(argonIcon("zoom-split-in"), argonIcon("settings-gear-65"), argonIcon("check-bold")),
          argonTab(
            tabName = "Step 1",
            active = TRUE,
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("step1_image"), height = "auto")
              ),
              argonColumn(
                width = 6,
                argonTextColor(
                  tagList(
                    "Within the Inputs widget, click the box with the desired operationg to be performed",
                    strong("Select Operation")),
                    color = "dark"
                )
              )
            )
          ),
          argonTab(
            tabName = "Step 2",
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("step2_image"), height = "auto")
              ),
              argonColumn(
                width = 6,
                argonTextColor(
                  tagList(
                    "Scroll up/down or use the search bar to find the variable for population group. Click the desired variable,",
                    strong("finnish"), "in this example"),
                  color = "dark"
                )
              )
            )
          ),
          argonTab(
            tabName = "Step 3",
            argonRow(
              argonColumn(
                width = 6,
                imageOutput(ns("step3_image"), height = "auto")
              ),
              argonColumn(
                width = 6,
                argonTextColor(
                  tag = tagList(
                    "In the", strong("Effect Estimate Calculattion"),
                    "widget, toggle the button and the selected variable with its available categories or levels will display.",
                    strong("BETA"), "in this example, is displayed if beta is included instead of", strong("OR"), 
                    ". If the selected variable in the previous step contains OR, then there is no need to apply this filter."
                  ),
                  color = "dark"
                ),
                br(),
                br(),
                argonTextColor(
                  tag = tagList(
                    "Girls hit your hallelujah (Woo)
                    Girls hit your hallelujah (Woo)
                    Girls hit your hallelujah (Woo)
                    'Cause uptown funk gon' give it to you (Woo)
                    ('Cause uptown funk gon' give it to you)
                    'Cause uptown funk gon' give it to you
                    Saturday night and we in the spot
                    Don't believe me, just watch, come on
                    Don't believe me, just watch, uh
                    Don't believe me, just watch, uh
                    Don't believe me, just watch
                    Don't believe me, just watch
                    Hey, hey, hey, oh
                    
                    [Verse 2]
                    Stop, wait a minute
                    Fill my cup, put some liquor in it
                    Take a sip, sign the check
                    Julio, get the stretch
                    Ride to Harlem, Hollywood, Jackson, Mississippi
                    If we show up, we gon' show out
                    Smoother than a fresh jar of Skippy"),
                  color = "dark"
                )
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
          "position, chromosome, total allele frequency",
          "position, chromosome, odds ration, population group"
        ),
        output = c(
          "Table 00-0.00 GWAS Summary Statistics: Unadjusted Case and Control Allele Frequencies",
          "Table 00-0.00 GWAS Summary Statistics: Adjusted Case and Control Allele Frequencies"
        )
      )
      reactable(ccafe_description_table)
    })
    
    output$step1_image <- renderImage({
      list(
        src = "app/static/CCAFE-hex.png",
        alt = "Filter Screenshot 1",
        width = "90%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$step2_image <- renderImage({
      list(
        src = "app/static/CCAFE-hex.png",
        alt = "Filter Screenshot 2",
        width = "90%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    output$step3_image <- renderImage({
      list(
        src = "app/static/CCAFE-hex.png",
        alt = "Filter Screenshot 3",
        width = "90%",
        height = "auto"
      )
    }, deleteFile = FALSE)
  })
}