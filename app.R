# https://stats.stackexchange.com/questions/69157/why-do-we-need-to-normalize-data-before-principal-component-analysis-pca
logo_blue_gradient <- dashboardthemes::shinyDashboardLogoDIY(
  boldText = "SVD",
  mainText = "Intuition",
  textSize = 16,
  badgeText = "BETA",
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "#40E0D0",
  badgeBorderRadius = 3
)

source("globals.R")

ui <- shinydashboardPlus::dashboardPagePlus(
  header = shinydashboardPlus::dashboardHeaderPlus(
    title = logo_blue_gradient
  ),
  skin = "blue-light",
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "sidebarmenu",

      shinydashboard::menuItem(
        text = "IMAGE SVD",
        tabName = "TAB2",
        icon = shiny::icon("image")
      ),

      shiny::conditionalPanel(
        "input.sidebarmenu === 'TAB2'",
        shiny::uiOutput("TAB2_CONTROLS")
      ),

      shinydashboard::menuItem(
        text = "TEXT SVD",
        tabName = "TAB3",
        icon = icon("file-alt")
      ),

      shiny::conditionalPanel(
        "input.sidebarmenu === 'TAB3'",
        shiny::uiOutput("TAB3_CONTROLS")
      ),

      shinydashboard::menuItem(
        text = "WHY SVD",
        tabName = "TAB1",
        icon = shiny::icon("question")
      ),

      shiny::conditionalPanel(
        "input.sidebarmenu === 'TAB1'",
        shiny::uiOutput("TAB1_CONTROLS")
      )
    )
  ),
  body = shinydashboard::dashboardBody(

    # condense dataTable display
    tags$head(tags$style(HTML("
      table.dataTable thead th, 
      table.dataTable thead td {
        padding: 0px 2px !important;
      }

      table.dataTable tbody th,
      table.dataTable tbody td {
        padding: 0 2px !important;
      }
    "))),

    # format the menus and boxes
    shinyEffects::setShadow(class = "dropdown-menu"),
    shinyEffects::setShadow(class = "box"),

    # All tabs
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "TAB1",
        shinydashboard::box(
          # title = "Why is Singular Value Decomposition so fundamental to Data Science?",
          width = NULL,
          uiOutput("TAB1_BODY")
        )
      ),


      shinydashboard::tabItem(
        tabName = "TAB2",
        shinydashboard::box(
          title = "Image compression with Singular Value Decomposition",
          width = NULL,
          uiOutput("TAB2_BODY")
        )
      ),

      shinydashboard::tabItem(
        tabName = "TAB3",
        shinydashboard::box(
          title = "Latent Semantic Analysis of text with Singular Value Decomposition",
          width = NULL,
          uiOutput("TAB3_BODY")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  TAB1_SERVER(input, output, session)
  TAB2_SERVER(input, output, session)
  TAB3_SERVER(input, output, session)
}

shinyApp(ui, server)
