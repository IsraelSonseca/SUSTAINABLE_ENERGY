tratamientoFechas <- function(potencias_data){
  potencias_data_ts=as.data.frame(potencias_data)
  
  
  ##-----conversion de datos---------------------
  # Se aniaden las siguientes variables por considerarse relevantes a la hora de explicar la demanda
  ## Periodo de la semana (d???????a entre semana o fin de semana)
  potencias_data_ts$periodo_de_la_semana <- unlist(lapply(potencias_data_ts$Fecha_hora, extrae_dia_semana))
  
  ## Dia de la semana
  potencias_data_ts$dia_de_la_semana <-weekdays(potencias_data_ts$Fecha_hora) # extrae el d???????a de la semana, dada una fecha
  
  ## Mes 
  potencias_data_ts$hora <- hour(potencias_data_ts$Fecha_hora)
  potencias_data_ts$mes <- month(potencias_data_ts$Fecha_hora)
  potencias_data_ts$anio <- year(potencias_data_ts$Fecha_hora)
  
  # Establecimiento del tipo de las columnas como "factor" que serviran para mostrar en grafico
  potencias_data_ts$dia_de_la_semana <-as.factor(potencias_data_ts$dia_de_la_semana)
  potencias_data_ts$periodo_de_la_semana <- as.factor(potencias_data_ts$periodo_de_la_semana)
  potencias_data_ts$mes <- as.factor(potencias_data_ts$mes)
  potencias_data_ts$hora <- as.factor(potencias_data_ts$hora)
  potencias_data_ts$anio <- as.factor(potencias_data_ts$anio)
  
  datos<-potencias_data_ts
  potencias_data_ts
}