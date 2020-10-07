boxplotUI <- function(id, label = "boxplots"){
  
  ns <- NS(id)
  
  tagList(
    #column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    column(12,
           # Input: Selector for variable to plot against mpg ----
           selectInput(ns("variable"), "Variable:",
                       c("Hora" = "hora",
                         "Mes" = "mes",
                         "Anio" = "anio",
                         "Periodo" = "periodo_de_la_semana",
                         "Dia"="dia_de_la_semana")),
           

          
           selectInput(ns("columna"), "Columna:",
                       c("PotenciaTrafo2"= "POTENCIA_TRAFO2",
                         "PotenciaTrafo3"= "POTENCIA_TRAFO3",
                         "PotenciaTrafo4"= "POTENCIA_TRAFO4",
                         "PotenciaTrafo5"= "POTENCIA_TRAFO5")),

    
    # Input: Checkbox for whether outliers should be included ----
    checkboxInput(ns("outliers"), "Show outliers", TRUE),
    
  
    # Output: Formatted text for caption ----
    h3(textOutput(ns("caption"))),
    
    # Output: Plot of the requested variable against mpg ----
    plotOutput(ns("mpgPlot")),
    
    downloadButton(outputId = ns("Cat10"), label = "Download The Plot")
    )
  )
}

# Define server logic ----
boxplotServer <- function(id, datos) {
  moduleServer(
    id,
    
    ## Below is the module function
    function(input, output, session) {

      # Compute the formula text ----
      # This is in a reactive expression since it is shared by the
      #output$caption and output$mpgPlot functions
      formulaText <- reactive({
        #paste("POTENCIATRAFO2 ~", input$variable)
        paste(input$columna," ~", input$variable)
      })
      
      # Return the formula text for printing as a caption ----
      output$caption <- renderText({
        formulaText()
      })
      
      # Generate a plot of the requested variable against mpg ----
      # and only exclude outliers if requested
      output$mpgPlot <- renderPlot({
        req(datos)
        boxplot(as.formula(formulaText()),
                data = datos,
                outline = input$outliers,
                col = "#75AADB", pch = 19)
      })
      
      output$Cat10<- downloadHandler(
        
        #Specify The File Name 
        filename = function() {
          paste("plot","png",sep= ".")},
        content = function(file){
          # open the format of file which needs to be downloaded ex: pdf, png etc. 
          png(file)
          
          grafico<-boxplot(as.formula(formulaText()),
                  data = datos,
                  outline = input$outliers,
                  col = "#75AADB", pch = 19)
          
          dev.off()
        }
      )
      return(datos)
    }
  )
}