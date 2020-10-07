############################# 
##### Lectura de datos
############################# 

f <- file.choose() #elegir el fichero "ptenciageneradora.xlsx"
potencias_data <- read_excel(f)

summary(potencias_data)

potencias_data_ts=as.data.frame(potencias_data)


##-----conversion de datos---------------------
# Se aniaden las siguientes variables por considerarse relevantes a la hora de explicar la demanda
## Periodo de la semana (d???????a entre semana o fin de semana)
potencias_data_ts$periodo_de_la_semana <- unlist(lapply(potencias_data_ts$Fecha_hora, extrae_dia_semana))

potencias_data_ts$periodo_de_la_semana

## Dia de la semana
potencias_data_ts$dia_de_la_semana <-weekdays(potencias_data_ts$Fecha_hora) # extrae el d???????a de la semana, dada una fecha

potencias_data_ts$dia_de_la_semana

## Mes 
potencias_data_ts$hora <- hour(potencias_data_ts$Fecha_hora)
potencias_data_ts$mes <- month(potencias_data_ts$Fecha_hora)
potencias_data_ts$anio <- year(potencias_data_ts$Fecha_hora)
potencias_data_ts$hora
potencias_data_ts$mes
potencias_data_ts$anio

# Comprobacion de todos los valores en las columnas recien creadas 
unique(potencias_data_ts$periodo_de_la_semana)
unique(potencias_data_ts$dia_de_la_semana)
unique(potencias_data_ts$hora)
unique(potencias_data_ts$mes)
unique(potencias_data_ts$anio)

# Establecimiento del tipo de las columnas como "factor" que serviran para mostrar en grafico
potencias_data_ts$dia_de_la_semana <-as.factor(potencias_data_ts$dia_de_la_semana)
potencias_data_ts$periodo_de_la_semana <- as.factor(potencias_data_ts$periodo_de_la_semana)
potencias_data_ts$mes <- as.factor(potencias_data_ts$mes)
potencias_data_ts$hora <- as.factor(potencias_data_ts$hora)
potencias_data_ts$anio <- as.factor(potencias_data_ts$anio)



str(potencias_data_ts)
head(potencias_data_ts)
summary(potencias_data_ts)

library(plotly)
today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm
y <- rnorm(length(x))
fig <- plot_ly(x = ~x, y = ~y, mode = 'lines', text = paste(tm, "days from today"))

fig






opciones <-colnames(potencias_data)

choice<-list()
cont<-0
for(i in 2:length(opciones)) {
  cont<-cont+1
  print (i)
  newChoice<-list(as.character(cont))
  names(newChoice) <- c(opciones[i])
  choice <-append(choice,newChoice)
}

xts_1 <- xts(potencias_data_ts$POTENCIATRAFO2,order.by=potencias_data_ts$Fecha_hora,frequency = 35036)
xts_2 <- xts(potencias_data_ts$POTENCIA_TRAFO3,order.by=potencias_data_ts$Fecha_hora,frequency = 35036)
xts_3 <- xts(potencias_data_ts$POTENCIA_TRAFO4,order.by=potencias_data_ts$Fecha_hora,frequency = 35036)
xts_4 <- xts(potencias_data_ts$POTENCIA_TRAFO5,order.by=potencias_data_ts$Fecha_hora,frequency = 35036)

xtss<-append(xts_1,xts_2,xts_3,xts_4)




library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      checkboxGroupInput("checkGroup", 
                         h3("Checkbox group"), 
                         choices = choice,
                         selected = 1:2)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      #plotlyOutput(outputId = "distPlot")
      
      
      dygraphOutput(outputId = "dygraph")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$dygraph <- renderDygraph({
    #mostrar<-as.xts(mi_entorno[["1"]])
    #i<-"1"
    #grafico<-dygraph(mi_entorno[[i]])
    #dySeriesData(grafico,"w",mi_entorno[["2"]])
    bind<-xts_1
    for ( i in input$checkGroup) {
      #dySeriesData(grafico,"w",mi_entorno[[i]])
      bind<-cbind(bind,mi_entorno[[i]])
    }
    dygraph(bind,xlab = choice) %>% dyRangeSelector()
    
    #potenciasseries <-cbind(seq(xts[list(input$checkGroup)]))
    #dygraph(potenciasseries) %>% dyRangeSelector()
    #x    <- potencias_data_ts$Fecha_hora
    #bins <- potencias_data_ts$POTENCIATRAFO2
    
    #hist(x, breaks = bins, col = "#75AADB", border = "white",
    #     xlab = "Waiting time to next eruption (in mins)",
    #     main = "Histogram of waiting times")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)
datamde <- mdeaths
serietemp1 <-xts(ts(potencias_data_ts$POTENCIATRAFO2, start = 2017, end = 2018, frequency = 35036))
serietemp2 <-ts(potencias_data_ts$POTENCIA_TRAFO3, start = 2017, end = 2018, frequency = 35036)
lungDeaths <- cbind(serietemp1, serietemp2)
dygraph(serietemp1) %>% dyRangeSelector()
plot(lungDeaths)
plot_ly(lungDeaths)


trafo2_xts <- xts(potencias_data_ts$POTENCIATRAFO2,order.by=potencias_data_ts$Fecha_hora,frequency = 35036)
dygraph(trafo2_xts) %>% dyRangeSelector()

choiceeees = list("Choice 1" = 1, 
               "Choice 2" = 2, 
               "Choice 3" = 3)


x <- c(1,2,3,4)
lista<-list()



checkboxGroupInput("checkGroup", 
                   h3("Seleccione las potencias a visualizar"), 
                   choices = list("Choice 1" = 1, 
                                  "Choice 2" = 2, 
                                  "Choice 3" = 3),
                   selected = 1)





trafo2_xts <- xts(potencias_data_ts$POTENCIATRAFO2,order.by=potencias_data_ts$Fecha_hora,frequency = 35036)
trafo3_xts <- xts(potencias_data_ts$POTENCIA_TRAFO3,order.by=potencias_data_ts$Fecha_hora,frequency = 35036)
potenciasseries <-cbind(xts(xtss[1]),xts(xtss[2]))



choice1 <- list("Choice 1" = 1, 
               "Choice 2" = 2, 
               "Choice 3" = 3)
choice2 <-list("asdfa"=4)
choices <- append(choice1,choice2)

opciones <-colnames(potencias_data)

choice<-list()

for(i in opciones) {
  name <- str(i)
  newChoice<-list(name = 1)
  choice <-append(choice,newChoice)
}



mi_entorno = new.env()


mi_entorno[[ "1" ]] = xts_1
mi_entorno[[ "2" ]] = xts_2
mi_entorno[[ "3" ]] = xts_3
mi_entorno[[ "4" ]] = xts_4


mi_entorno = new.env()

for (i in 1:4) {
  mi_entorno[[ as.character(i) ]] = paste(xts_,as.character(i))
}
mi_entorno[[ "1" ]] = xts_1
mi_entorno[[ "2" ]] = xts_2
mi_entorno[[ "3" ]] = xts_3
mi_entorno[[ "4" ]] = xts_4



mi_entorno[[ "1" ]]


aprueba<-mi_entorno[[ "1" ]]



dygraph(mi_entorno[["1"]])

