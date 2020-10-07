library(shiny)

ui <- fluidPage( 
  titlePanel(title = h4("Business Intelligence & Analytics", align="center")), 
  sidebarLayout(  
    sidebarPanel(("Make Selections Here"),
                 textInput("name","Enter your name",""),
                 textInput("Age","Enter Age",""),
                 radioButtons("Gender","Enter Gender",list("Male","Female")),
                 sliderInput("Slider","Select The Value", min = 0, max = 100, value = 4, animate = T,
                             step = 5),  
                 selectInput("State","Name Of The States",c("California","Arizona",
                                                            "Chicago","Rosemont","Dallas")
                             ,selected = "Dallas",selectize = TRUE, multiple = TRUE ), 
                 selectizeInput("Variable","Select the Var",
                                c("Sepal.Length"=1,"Sepal.Width"=2,"Petal.Width"=3)),
                 sliderInput("Bins","Select The Number Of Bins",min = 5, max=20,value = 2),
                 radioButtons("Colour","Select The Colour",list("Green","Yellow","Red","Black"),
                              selected = "Yellow"),
                 radioButtons("Download Option", "Select the Option", list("png","jpeg","pdf"))),
    
    mainPanel(
      tabsetPanel(type="tab", #adding tab sheets 
                  tabPanel("Summary",verbatimTextOutput("Cat9")),
                  tabPanel("Structure", verbatimTextOutput("Cat8")), #verbatim TextOutput used to show output of render print
                  tabPanel("Data",tableOutput("Cat7")),
                  tabPanel("Plot", plotOutput("Cat6"))
      ),
      textOutput("Cat1"),
      textOutput("Cat2"),
      textOutput("Cat3"), 
      textOutput("Cat4"),
      textOutput("Cat5"), 
      plotOutput("hist"),
      downloadButton(outputId = "Cat10", label = "Download The Plot")
      
    ) 
  )
)

server <- function(input, output) {
  
  colm <- reactive({as.numeric(input$Variable)})
  
  output$Cat1 <- renderText(input$name)
  output$Cat2 <- renderText(input$Age) 
  output$Cat3<- renderText(input$Gender)
  output$Cat4 <- renderText(paste("You Selected The Value:", input$Slider))
  output$Cat5 <- renderText(input$State)
  output$Cat6<- renderPlot({
    hist(iris[,colm()], breaks =seq(0,max(iris[,colm()]),l=input$Bins+1), 
         col = input$Colour, main = "Histogram Of Iris",xlab = 
           names(iris[colm()]))
  })
  
  output$Cat7 = renderTable({
    
    head(iris,4)
    
  }) 
  
  output$Cat8 = renderPrint({
    str(iris3) 
    
  })   
  
  output$Cat9 <- renderPrint({summary(iris3)
  })
  
  output$Cat10<- downloadHandler(
    #Specify The File Name 
    filename = function() {
      paste("iris",input$`Download Option`,sep= ".")},
    content = function(file){
      # open the format of file which needs to be downloaded ex: pdf, png etc. 
      if (input$`Download Option`== "png"){
        png(file)
      } else if (input$`Download Option`== "pdf"){
        pdf(file)
      } else {
        jpeg(file) 
      }
      hist(iris[,colm()], breaks =seq(0,max(iris[,colm()]),l=input$Bins+1), 
           col = input$Colour, main = "Histogram Of Iris",xlab = 
             names(iris[colm()]))
      
      dev.off()
    }
  )
}




selectDataVarUI <- function(id) {
  tagList(
    datasetInput(NS(id, "data"), filter = is.data.frame),
    selectVarInput(NS(id, "var"))
  )
}
selectDataVarServer <- function(id, filter = is.numeric) {
  moduleServer(id, function(input, output, session) {
    data <- datasetServer("data")
    var <- selectVarServer("var", data, filter = filter)
    var
  })
}

selectDataVarApp <- function(filter = is.numeric) {
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(selectDataVarUI("var")),
      mainPanel(verbatimTextOutput("out"))
    )
  )
  server <- function(input, output, session) {
    var <- selectDataVarServer("var", filter)
    output$out <- renderPrint(var(), width = 40)
  }
  shinyApp(ui, server)
}

