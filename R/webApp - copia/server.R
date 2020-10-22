server <- function(input, output) { 
  datafile <- uploadServer("datafile", stringsAsFactors = FALSE)
  
  datos<-reactive(tratamientoFechas(potencias_data))
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    summary(potencias_data)
  })
  
  
  
  
  # Generate a summary of the dataset ----
  output$PRUEBA <- renderPlot({
    browse()
    plot(potencias_data_ts[,selectColumnServer("selecGlob")])

  })
  
  output$downloadGlob<- downloadHandler(
    
    #Specify The File Name 
    filename = function() {
      paste("plot",selectColumnServer("selecGlob"),".png",sep= "")},
    content = function(file){
      # open the format of file which needs to be downloaded ex: pdf, png etc. 
      png(file)
      
      grafico<-    plot(potencias_data_ts[,selectColumnServer("selecGlob")])

      
      dev.off()
    }
  )
  
  
  #png("Mi gráfico.png")
  
  #edades <- c(20, 33, 40, 12, 50, 26, 18, 79, 47, 23, 8)
  #plot(edades, main = "Edades", xlab = "Nº de orden", ylab = "Años")
  
  #dev.off()
  
  
  output$tableGlob <- renderDataTable(potencias_data)
  


  datos<- 
    boxplotServer(
      "asd",datos=potencias_data_ts)
  
  
  
  prediccc<-reactive(knn_mimo[[selectColumnServer("selecPred")]])
  
  observeEvent(selectColumnServer("selecPred"),{
    value<-knnServer("knnUI",datos=prediccc(),dia=input$slider,semana=input$week,fechas=knn_mimo[["marcatemporal"]])
  })
  
  observeEvent(input$slider,{
    value<-knnServer("knnUI",datos=prediccc(),dia=input$slider,semana=input$week,fechas=knn_mimo[["marcatemporal"]])
  })

  output$dygraph <- renderDygraph({
    dygraph(potencias_data_ts_xts$POTENCIA_TRAFO2, main = "Predicted Deaths/Month")
  })
  #output$temperatura <- renderDygraph({
  #  dygraph(xts$TEMPERATURA)
  #})
  #output$humedad <- renderDygraph({
  #  dygraph(xts$HUMEDAD)
  #})
  #output$presion <- renderDygraph({
  #  dygraph(xts$PRESION)
  #})

  
  
}