source("modules/reportsModule.R")

ui <- shinydashboard::dashboardPage(
  skin = "black",
  title = "KU ADRC Dashboard",

  shinydashboard::dashboardHeader(
    title = htmltools::span(
      htmltools::img(
        src = "https://www.kumc.edu/_dev/logo--kumc-main.png",
        height = "50px",
        style = "margin-right: 10px;"
      ),
      "KU ADRC"
    ),
    titleWidth = 350
  ),
  shinydashboard::dashboardSidebar(
    width = 250,
    shinydashboard::sidebarMenu(
      # Add more menu items as needed
      shinydashboard::menuItem(
        "Program Reports",
        tabName = "reports",
        icon = shiny::icon("file-alt")
      )
    )
  ),
  shinydashboard::dashboardBody(
    shiny::includeCSS("www/style.css"),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "reports", reportUI("reports"))
    )
  )
)

server <- function(input, output, session) {
  reportServer("reports")
}

shiny::shinyApp(ui, server, options = options("shiny.devmode" = TRUE))
