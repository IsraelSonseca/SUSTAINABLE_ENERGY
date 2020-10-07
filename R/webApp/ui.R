header <- dashboardHeader(
  title = "IsraDataVision"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Datos", tabName = "entry", icon = icon("table")),
    menuItem("SeriesTemporales", icon = icon("chart-area"), tabName = "tempser"),
    menuItem("Diagramas de caja", icon = icon("box"), tabName = "boxplots",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "entry",
            h2("Apartado para introducir los datos que se van a analizar"),
            
            
            fluidRow(             
              box(
                title = "Introducir Series Temporales",solidHeader = TRUE, width = 4, status = "primary",
                uploadUI("datafile", "User data (excell format)")
              ),
              box(
                title = "Resumen de los datos", width = 8, background = "light-blue",
              
                # Output: Data file ----
                verbatimTextOutput("summary")              )
            ),
            
            fluidRow(
              box(
                plotOutput("PRUEBA")
              )
            )

            
            
    ),
    
    tabItem(tabName = "tempser",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "boxplots",
            h2("Widgets tab content"),
            fluidRow(
              box(
                title = "Introducir Series Temporales",solidHeader = TRUE, width = 12, status = "primary",
                  boxplotUI("asd")
                )
            )
    )
  )
)

shinyui <- dashboardPage(skin="blue",header, sidebar, body)