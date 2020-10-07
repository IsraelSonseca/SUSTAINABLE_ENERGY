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
              column(width = 6,
                box(title = "Resumen de los datos",width = 6, background = "light-blue",
                
                  # Output: Data file ----
                  verbatimTextOutput("summary"),
                  selectColumnUI("selecGlob"),
                  downloadButton("downloadGlob", label = "Download The Plot Global")
                )
              ),
              column(width = 6,
                     
                box(
                
                plotOutput("PRUEBA")
                )
              )
            ),
            
            fluidRow(

              box(width = 12,dataTableOutput("tableGlob"))
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