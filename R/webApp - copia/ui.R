header <- dashboardHeader(
  title = "IsraDataVision"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data", tabName = "entry", icon = icon("table")),
    menuItem("Time Series", icon = icon("chart-area"), tabName = "tempser"),
    menuItem("Boxplots", icon = icon("box"), tabName = "boxplots"),
    menuItem("Predictions", icon = icon("arrow-alt-circle-right"), tabName = "predictions",
             badgeLabel = "new", badgeColor = "blue")
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
            h2("Widgets tab content"),
            dygraphOutput("dygraph")
    ),
    tabItem(tabName = "boxplots",
            h2("Widgets tab content"),
            fluidRow(
              box(
                title = "Introducir Series Temporales",solidHeader = TRUE, width = 12, status = "primary",
                  boxplotUI("asd")
                )
            )
    ),
    
    tabItem(tabName = "predictions",width=12,
            h2("Widgets tab content"),
            
            box(width = 3,
              title = "Inputs", background = "black",
              "Box content here", br(), "More box content",
              selectColumnUI("selecPred"),
              sliderInput("slider", "Slider input: (DAY)", 1, 100, 50),
              checkboxInput("week", "Week View:")
            ),
            
            # Output: Tabset w/ plot, summary, and table ----
            tabBox(type = "tabs",width=9,
                        tabPanel("knn_MIMO",
                                 knnUI("knnUI")
                                 ),
                        tabPanel("knn_RECURSIVE"),
                        tabPanel("frbs_DENFIS"),
                        tabPanel("frbs_HYFIS"),
                        tabPanel("auto_ARIMA"),
                        tabPanel("ARNN"),
                        tabPanel("Combination")
                        )
    )
  )
)

shinyui <- dashboardPage(skin="blue",header, sidebar, body)