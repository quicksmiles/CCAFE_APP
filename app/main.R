box::use(
  argonDash[argonDashBody, argonDashHeader, argonDashNavbar, argonDashPage, argonSidebarItem, argonSidebarMenu, argonTabItem, argonTabItems],
  argonR[argonColumn, argonNavMenu, argonRow],
  dotenv[load_dot_env],
  shiny[HTML, moduleServer, NS, tags],
)

box::use(
  app/view/home_page,
  app/view/operation_selection,
  app/view/r_package,
  app/view/user_guide,
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
                                                z-index: 200;
                                                width: 100%;
                                              }
                                              .main-content .navbar-top {
                                                z-index: 201;
                                              }")),
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
                                          tags$a(
                                            href = "https://hendrickslab.cu-dbmi.dev/ccafe/",
                                            tags$h1("CCAFE", style = "color: white; margin:0;")
                                          )
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
          home_page$homeUI(ns("home"))
        ),
        argonTabItem(
          tabName = "guide",
          user_guide$guideUI(ns("guide"))
        ),
        argonTabItem(
          tabName = "analysis",
          operation_selection$operationSelectionUI(ns("operation_selection"))
        ),
        argonTabItem(
          tabName = "rpackage",
          r_package$rpackageUI(ns("rpackage"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    user_guide$guideServer("guide")
    
    # Operation selection module
    results <- operation_selection$operationSelectionServer("operation_selection", session)
  })
}

