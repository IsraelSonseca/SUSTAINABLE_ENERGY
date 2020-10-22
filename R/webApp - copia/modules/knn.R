knnUI <- function(id, label = "predictions"){
  
  ns <- NS(id)
  
  tagList(
    column(12,
          plotOutput(ns("knnPlot")),
          # Dynamic valueBoxes
          plotOutput(ns("zoom")),
          valueBoxOutput(ns("meanDay")),
        valueBox(10 * 2, ns("maxDay"), icon = icon("calendar-week")),
        
        # Dynamic valueBoxes
        valueBoxOutput(ns("meanMAXDAY")),
        
        valueBoxOutput(ns("RMSE")),
        plotOutput(ns("knnPlot"))
  ))
}

# Define server logic ----
knnServer <- function(id, datos, dia, semana,fechas) {
  moduleServer(
    id,
    
    
    ## Below is the module function
    function(input, output, session) {
      # Generate a plot of the requested variable against mpg ----
      # and only exclude outliers if requested
      output$knnPlot <- renderPlot({
        #req(datos)
        plot(datos)
      })
      
      
      zoom<-reactive({
        pruebbaaaaaa<-data.frame(datos$prediction,knn_mimo[["marcatemporal"]])
        (((dia-1)*96)+1)
        (dia*96)
        bueno<-pruebbaaaaaa[(((dia-1)*96)+1):(dia*96),]
        colnames(bueno)<-c("prediction","hora","fecha")
        bueno
      })
      
      output$zoom <- renderPlot({
        #req(datos)
        #dia<-2
        
        ggplot(zoom(),aes(hora,prediction))+geom_col()
      })
      
      output$meanDay <- renderValueBox({
        valueBox(
          mean(datos), "Average POWER", icon = icon("tachometer-alt-average"),
          color = "purple"
        )
      })
      
      output$meanMAXDAY <- renderValueBox({
        valueBox(
          mean(datos), "Average POWER", icon = icon("tachometer-alt-average"),
          color = "purple"
        )
      })
      output$RMSE <- renderValueBox({
        valueBox(
          mean(datos), "Average POWER", icon = icon("tachometer-alt-average"),
          color = "purple"
        )
      })
      
      return(1)
    }
  )
}