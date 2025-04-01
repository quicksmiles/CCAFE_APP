box::use(
  shiny[...],
  argonR[...],
  argonDash[...],
  htmltools[...]
)

homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Row for Image and About Section (two columns)
    argonRow(
      style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
      argonColumn(
        width = 6,  # Image takes 6 columns
        tags$div(
          style = "display: flex; flex-direction: column; align-items: center;",
          tags$img(
            src = "static/CCAFE-hex.png",
            width = "60%"  # Adjust width if needed
          )
        )
      ),
      argonColumn(
        width = 6,  # About section takes 6 columns
        argonCard(
          width = 12,
          background_color = "transparent",
          argonH1("About CCAFE", display = 4),
          argonLead(
            "The CCAFE Application offers a free, user-friendly web-based interface for the CCAFE R package. It 
            implements methods for case and control allele frequency estimation based on available 
            summary statistics."
          ),
          argonLead(
            "CCAFE's latest allows for estimation using either total (cases and controls aggregated) AF or SE.
            When using SE, the application runs a query of gnomAD V4 to use as proxies to implement bias 
            correction."
          ),
          argonLead(
            "The platform is freely accessible and intended for researchers and bioinformaticians working 
            on genetic association studies."
          ),
          argonLead(
            "For details on the CCAFE methods please see our publication: ",
            tags$a(href = "https://www.biorxiv.org/content/10.1101/2024.10.24.619530v1.full-text",
                   target = "_blank", # opens link in a new tab 
                   "CCAFE: Estimating Case and Control Allele Frequencies from GWAS Summary Statistics",
                   style = "color: #007bff; text-decoration: underline;")
          )
        )
      )
    ),
    
    # Features Section
    argonRow(
      style = "margin-top: 2em;",
      argonColumn(
        width = 12,
        argonCard(
          width = 12,
          background_color = "light",
          argonH1("Key Features", display = 4),
          argonLead("✅ Estimation of case and control AFs using total (aggregated) AF"),
          argonLead("✅ Estimation of case and control AFs using SE"),
          argonLead("✅ Bias correction for SE-based estimates using gnomAD AFs as proxies")
        )
      )
    ),
    
    # Footer Section
    argonRow(
      style = "margin-top: 2em; margin-bottom: 2em;",
      argonColumn(
        width = 12,
        argonCard(
          width = 12,
          background_color = "transparent",
          tags$div(
            style = "text-align: center;",
            argonLead("Developed by: Your Name | Contact: your.email@example.com")
          )
        )
      )
    )
   )
  
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
  # This module only renders static css, no server logic needed
}
