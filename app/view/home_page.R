box::use(
  shiny[...],
  argonR[...],
  argonDash[...],
  htmltools[...]
)

homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    argonRow(
      style = "display: flex; justify-content: center; align-items: center; width: 100%;",
      argonColumn(
        width = 12,
        center = TRUE,
        argonCard(
          width = 12,
          background_color = "transparent",
          tags$div(
            style = "display: flex; flex-direction: column; align-items: center; margin-bottom: 2em;",
            tags$img(
              src = "https://raw.githubusercontent.com/wolffha/wolffha/refs/heads/main/images/CCAFE-hex.png",
              width = "30%",
            ),
            argonH1(
              "Case and Control Allele Frequency Estimation Application",
              display = 1,
              style = "padding: 1em; font-size: 1.8em; letter-spacing: 2px; text-align: center"
            )
        )
      )
    ),
    br(),
    br(),
    br(),
    argonRow(
      argonColumn(
        width = 12,
        argonCard(
          width = 12,
          background_color = "transparent",
          argonH1("About CCAFE", display = 4),
          argonLead(
            "The CCAFE Application is... add more"
          ),
          argonLead(
            "CCAFE's latest version introduces... add more"
          ),
          argonLead(
            "The platform is freely accessible... add more"
          )
        )
      )
    )
  )
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}
