server <- function(input, output) { 
  datafile <- uploadServer("datafile", stringsAsFactors = FALSE)
  
  potencias_data <- reactive({
    potencias_data<-read_excel(input$file1$datapath)
    potencias_data
    })
  
  datos<-reactive(tratamientoFechas(potencias_data))
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    req(input$file1)
    summary(potencias_data())
  })
  
  
  
  
  # Generate a summary of the dataset ----
  output$PRUEBA <- renderPlot({
    req(input$file1)
    potencias_data_ts=as.data.frame(potencias_data())
    plot(potencias_data_ts$POTENCIATRAFO2)
  })

  datos<-boxplotServer(
      "asd",datos=potencias_data_ts)

  
  
}