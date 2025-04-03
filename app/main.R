box::use(
  argonDash[...],
  argonR[...],
  CCAFE[...],
  htmltools[...],
  dotenv[load_dot_env],
  reactR[...],
  shiny[...],
  shinyjs[...]
)

box::use(
  app/logic/upload[...],
  app/logic/email[...],
  app/logic/handle_se[...],
  app/logic/merge[...],
  app/logic/query[...],
  app/view/operation_selection[operationSelectionUI, operationSelectionServer],
  app/view/file_upload[fileUploadUI, fileUploadServer],
  app/view/home_page[...],
  app/view/user_guide[...],
  app/view/r_package[...],
)

load_dot_env()

#' @export
ui <- function(id) {
  ns <- NS(id)
  # creates a shiny dashboard template with all the main UI components for a webpage
  # makes use of argonR and argonDash - two libraries for UI tools in shiny apps
  argonDashPage(
    title = "CCAFE",
    description = "Case and Control Allele Frequency Estimation Shiny App",
    header = argonDashHeader(tags$style(HTML(".header {
                                              position: fixed;
                                              top: 0;
                                              left: 0;
                                              right: 0;
                                              z-index: auto;
                                              width: 100%;}")),
                             color = "primary",
                             separator = FALSE,
                             bottom_padding = 4,
                             top_padding = 5,
                            
                            ),
    sidebar = NULL,
    navbar = argonDashNavbar(headroom = FALSE,
                             style = "display: flex; 
                                      justify-content: space-between; 
                                      align-items: center;
                                      width: 95%;
                                      margin: 0;
                                      position: fixed;
                                      top: 0;
                                      z-index: auto;",
                             
                             argonRow(
                                      style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin: 0;",
                                      
                                      argonColumn(
                                        width = 4,
                                        tags$div(
                                          class = "navbar-brand pt-0 my-0",
                                          style = "display: flex; align-items: center; justify-content: flex-start;",
                                          tags$h1("CCAFE", style = "color: white;"),
                                        )
                                      ),
                                      
                                      argonColumn(
                                        width = 8,
                                        argonNavMenu(side = "right",
                                                     style = "display: flex; justify-content: flex-end; align-items: center; gap: 10px",
                                                     width = 6,
                                                     
                                                     argonSidebarMenu(
                                                       style = "display:-webkit-inline-box;",
                                                       argonSidebarItem("Home", tabName = "home",
                                                                        style = "background-color: transparent; border: none; color: secondary; border-radius: 20px; transition: 0.3s;",
                                                                        tags$style("#home:hover, #home:focus { background-color: #f8f9fa; color: #0056b3; }")
                                                                        ),
                                                       argonSidebarItem("User Guide", tabName = "guide",
                                                                        style = "background-color: transparent; border: none; color: secondary; border-radius: 20px; transition: 0.3s;",
                                                                        tags$style("#guide:hover, #guide:focus { background-color: #f8f9fa; color: #0056b3; }")
                                                                        ),
                                                       argonSidebarItem("Analysis", tabName = "analysis",
                                                                        style = "background-color: transparent; border: none; color: secondary; border-radius: 20px; transition: 0.3s;",
                                                                        tags$style("#analysis:hover, #analysis:focus { background-color: #f8f9fa; color: #0056b3; }")
                                                                        ),
                                                       argonSidebarItem("R Package", tabName = "rpackage",
                                                                        style = "background-color: transparent; border: none; color: secondary; border-radius: 20px; transition: 0.3s;",
                                                                        tags$style("#rpackage:hover, #rpackage:focus { background-color: #f8f9fa; color: #0056b3; }")
                                                                        )
                                                     )
                                        )
                                      )
                          )
            ),
    body = argonDashBody(
      style = "margin-top: 80px;",
      argonTabItems(
        argonTabItem(
          tabName = "home",
          homeUI(ns("home"))
        ),
        argonTabItem(
          tabName = "guide",
          guideUI(ns("guide"))
        ),
        argonTabItem(
          tabName = "analysis",
          operationSelectionUI(ns("operation_selection"))
        ),
        argonTabItem(
          tabName = "rpackage",
          rpackageUI(ns("rpackage"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # welcomeServer("welcome")
    guideServer("guide")
    
    # Operation selection module
    results <- operationSelectionServer("operation_selection", session)
  })
}

