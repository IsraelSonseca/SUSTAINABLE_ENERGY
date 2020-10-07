selectColumnUI <- function(id, label = "selectColumn"){
  
  ns <- NS(id)
  
  tagList(
    column(12,
           selectInput(ns("columna"), "Columna:",
                       c("PotenciaTrafo2"= "POTENCIA_TRAFO2",
                         "PotenciaTrafo3"= "POTENCIA_TRAFO3",
                         "PotenciaTrafo4"= "POTENCIA_TRAFO4",
                         "PotenciaTrafo5"= "POTENCIA_TRAFO5")),
    )
  )
}

# Define server logic ----
selectColumnServer <- function(id, datos) {
  moduleServer(
    id,
    
    ## Below is the module function
    function(input, output, session) {
      return(input$columna)
    }
  )
}