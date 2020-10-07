#carga de librerias 
library("caret")
library("e1071")
library("lubridate")
library("dummies")
library("imputeTS")
library("ggplot2")
library("forecast")
library("frbs")
library("tsfknn")
# Carga del paquete "ARNN.R"
libreria_arnn=file.choose()
source(libreria_arnn)
library("Metrics") # para rmse
library(tidyr)
library(dplyr)
library(readxl)
library(tidyverse)


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


plot(potencias_data)
