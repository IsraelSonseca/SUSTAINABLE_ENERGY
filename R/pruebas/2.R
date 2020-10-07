runExample("01_hello")      # a histogram
runExample("02_text")       # tables and data frames
runExample("03_reactivity") # a reactive expression
runExample("04_mpg")        # global variables
runExample("05_sliders")    # slider bars
runExample("06_tabsets")    # tabbed panels
runExample("07_widgets")    # help text and submit buttons
runExample("08_html")       # Shiny app built from HTML
runExample("09_upload")     # file upload wizard
runExample("10_download")   # file download wizard
runExample("11_timer")      # an automated timer




## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    checkboxGroupInput("variable", "Variables to show:",
                       c("Cylinders" = "cyl",
                         "Transmission" = "am",
                         "Gears" = "gear")),
    tableOutput("data")
  )
  
  server <- function(input, output, session) {
    output$data <- renderTable({
      mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
  
  shinyApp(ui, server)
  
  ui <- fluidPage(
    checkboxGroupInput("icons", "Choose icons:",
                       choiceNames =
                         list(icon("calendar"), icon("bed"),
                              icon("cog"), icon("bug")),
                       choiceValues =
                         list("calendar", "bed", "cog", "bug")
    ),
    textOutput("txt")
  )
  
  server <- function(input, output, session) {
    output$txt <- renderText({
      icons <- paste(input$icons, collapse = ", ")
      paste("You chose", icons)
    })
  }
  
  shinyApp(ui, server)
}
