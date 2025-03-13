box::use(
  argonR[...],
  htmltools[...],
  markdown[...],
  shiny[NS]
)

rpackageUI <- function(id) {
  ns <- NS(id)
  argonRow(
    argonColumn(
      width = 12,
      argonCard(
        width = 12,
        tags$iframe(
          src = "https://wolffha.github.io/CCAFE_documentation/index.html",
          width = "100%",
          height = "600px",
          style = "border:none;"
        )
      )
    )
  )
}

rpackageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # This module only renders static Markdown, no server logic needed
  })
}