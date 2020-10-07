library(shiny)

# Define UI for data upload app ----
uploadUI <- function(id, label = "uploadData"){
  
  ns <- NS(id)
  
  tagList(
      
      # Input: Select a file ----
      fileInput("file1", "Choose Excell File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE)
  )
}

# Define server logic to read selected file ----
uploadServer <- function(id, stringsAsFactors) {
  moduleServer(
    id,
    
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      dataframe <- reactive({
        read_excel(userFile()$datapath,
                 header = input$header,
                 stringsAsFactors = stringsAsFactors)
      })
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      return(dataframe)
      
    }
  )
}

