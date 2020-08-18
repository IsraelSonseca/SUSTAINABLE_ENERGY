library(shinydashboard)
library(shiny)

header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody("ISRAEL WEBAAPP")

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) { }

shinyApp(ui, server)