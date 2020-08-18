library(readxl)
library("lubridate")
library("ggplot2")
library("ggthemes")






############################## 
##### Zona de funciones 
############################## 
# Funcion para mapear cada fecha con su correspondiente periodo de la semana
# Asigna categoria "dia_entre_semana" o "fin_de_semana" a cada fecha
extrae_dia_semana <- function(x) {
  val <- weekdays(x) # Funcion que extrae el d???¡ì??as de la semana, dada una fecha
  if (grepl("s?bado", val, perl=TRUE) | val == "domingo") { 
    val2 = "fin_de_semana"
  }
  else {
    val2= "dia_entre_semana"
  }
  return(val2)
}

############################# 
##### Lectura de datos
############################# 

f <- file.choose() #elegir el fichero "ptenciageneradora.xlsx"
potencias_data <- read_excel(f)

potencias_data_ts=as.data.frame(potencias_data)


##-----conversion de datos---------------------
# Se aniaden las siguientes variables por considerarse relevantes a la hora de explicar la demanda
## Periodo de la semana (d???¡ì??a entre semana o fin de semana)
potencias_data_ts$periodo_de_la_semana <- unlist(lapply(potencias_data_ts$Fecha_hora, extrae_dia_semana))
potencias_data_ts$periodo_de_la_semana

## Dia de la semana
potencias_data_ts$dia_de_la_semana <-weekdays(potencias_data_ts$Fecha_hora) # extrae el d???¡ì??a de la semana, dada una fecha

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

set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), rating = c(rnorm(200),rnorm(200, mean=.8)))

p <- ggplot(dat, aes(x=cond, y=rating)) + geom_boxplot()
ggplotly(p)

fig


fig <- plot_ly(potencias_data_ts, x= ~hora, y=~POTENCIATRAFO2, type = "box")
fig


boxplot(POTENCIATRAFO2~mes,
        data = potencias_data_ts,
        outline = FALSE,
        col = "#75AADB", pch = 19)

boxplot(POTENCIATRAFO2~hora,
        data = potencias_data_ts,
        outline = TRUE,
        col = "#75AADB", pch = 19)

#### Descripcion grafica  ####
# Comparacion de potencias trafico horaria 
#(ver si hay diferencia entre diferentes horas)
ggplot(potencias_data_ts, aes(x=hora, y=POTENCIATRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias horarias del trafico del establecimient')+labs(x="Hora", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))





# Comparacion de potencias trafico diaria
ggplot(potencias_data_ts, aes(Fecha_hora, POTENCIATRAFO2))+geom_line(color="darkblue", size=1)+ggtitle('Potencias diarias del trafico del establecimiento')+labs(x="Fecha", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))


# Comparacion de potencias trafico de los dias semanal
ggplot(potencias_data_ts, aes(x=dia_de_la_semana, y=POTENCIATRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias semanal del trafico del establecimiento')+labs(x="Dia de la semana", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))

# Comparacion de potencia trafico entre dias_jornada y fines_semana
ggplot(potencias_data_ts, aes(x=periodo_de_la_semana, y=POTENCIATRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias entre semanal del trafico del establecimiento')+labs(x="Periodo de la semana", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))


# Comparacion de potencias trafico mensual
ggplot(potencias_data_ts, aes(x=mes, y=POTENCIATRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias mensuales del trafico del establecimiento')+labs(x="Mes", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))



# Comprobacion de existencia de valores faltantes (NA)
anyNA(potencias_data_ts)


# Particion de datos en conjuntos de train, test y validacion.
summary(potencias_data_ts)



library(shiny)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Miles Per Gallon"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Hora" = "hora",
                    "Mes" = "mes",
                    "Anio" = "anio")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot"),
      
      downloadButton(outputId = "Cat10", label = "Download The Plot")
      
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  #output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("POTENCIATRAFO2 ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = potencias_data_ts,
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

      boxplot(as.formula(formulaText()),
              data = potencias_data_ts,
              outline = input$outliers,
              col = "#75AADB", pch = 19)
      
      dev.off()
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)

