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

potencias_data$Fecha_hora <-as.POSIXct(potencias_data$Fecha_hora)




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

#### Descripcion grafica  ####
# Comparacion de potencias trafico horaria 
#(ver si hay diferencia entre diferentes horas)
ggplot(potencias_data_ts, aes(x=hora, y=POTENCIA_TRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias horarias del trafico del establecimient')+labs(x="Hora", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))



# Comparacion de potencias trafico diaria
ggplot(potencias_data_ts, aes(Fecha_hora, POTENCIA_TRAFO2))+geom_line(color="darkblue", size=1)+ggtitle('Potencias diarias del trafico del establecimiento')+labs(x="Fecha", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))


# Comparacion de potencias trafico de los dias semanal
ggplot(potencias_data_ts, aes(x=dia_de_la_semana, y=POTENCIA_TRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias semanal del trafico del establecimiento')+labs(x="Dia de la semana", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))

# Comparacion de potencia trafico entre dias_jornada y fines_semana
ggplot(potencias_data_ts, aes(x=periodo_de_la_semana, y=POTENCIA_TRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias entre semanal del trafico del establecimiento')+labs(x="Periodo de la semana", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))


# Comparacion de potencias trafico mensual
ggplot(potencias_data_ts, aes(x=mes, y=POTENCIA_TRAFO2)) + 
  geom_boxplot() + ggtitle('Potencias mensuales del trafico del establecimiento')+labs(x="Mes", y="Potencia")+ theme(plot.title = element_text(hjust=0.5))



# Comprobacion de existencia de valores faltantes (NA)
anyNA(potencias_data_ts)


# Particion de datos en conjuntos de train, test y validacion.
summary(potencias_data_ts)
### Datos de train: ###
# los datos anteriores desde fecha 2018-01-01 hasta 2018-04-04
train_data<-filter(potencias_data_ts, Fecha_hora< as.Date("2017-12-30"))
summary(train_data)

### Datos de test:  ###
#va a quedar los datos de ultimo dia para testing
test_data <-filter(potencias_data_ts, Fecha_hora>=as.Date("2017-12-30"))
head(test_data,10)
summary(test_data)
length(test_data$Fecha_hora)
medidas_predecir=length(test_data$Fecha_hora) # que son 96 medidas del ultimo dia a predecir
summary(test_data)

#----------------------------------------------------------
###########################################################################################
##### METODO 1. FORECASTING DE SERIES TEMPORALES CON METODO K-NEAREST NEIGHBOR (LIBRERIA TFSKNN)
###########################################################################################

### 1.1. Estrategia MIMO
t <- proc.time() # Inicio del cronometro 
#- h=>num de valores de predecir, en este caso predecimos los dias de futuro anio 
#- lags=> como se incrementa los valores 
#- k=> 1 distancia
#- msas=> estrategia MIMO o recursiva 
tsfknn_pred_mimo <- knn_forecasting(train_data$POTENCIA_TRAFO2, 
                                    h = medidas_predecir, 
                                    lags = 1:medidas_predecir,   #predecir los futuros 96 medidas del dia  
                                    k = 1, 
                                    msas = "MIMO")

proc.time()-t # Detencion del cronometro


# Evaluacion del modelo
ro_mimo <- rolling_origin(tsfknn_pred_mimo, h = medidas_predecir)
ro_mimo


# Grafico de predicciones para el horizonte de pronostico
plot(ro_mimo, h = medidas_predecir)

# Resultados exportados a csv
matriz_pronosticos_mimo<-data.frame(ro_mimo$predictions)
write.csv(matriz_pronosticos_mimo, "out_of_sample_tsfknn_mimo_prediction.csv") 
matriz_pronosticos_mimo

# Calculo de rmse
knn_mimo_pred=t(matriz_pronosticos_mimo[1,1:(ncol(matriz_pronosticos_mimo))]) #extrae primera fila de datos
rmse(test_data$POTENCIA_TRAFO2[0:medidas_predecir],knn_mimo_pred)


### 1.2. Estrategia recursiva
t <- proc.time() # Inicio del cronometro 
tsfknn_pred_rec<- knn_forecasting(train_data$POTENCIA_TRAFO2, 
                                    h = medidas_predecir, 
                                    lags = 1:medidas_predecir, 
                                    k = 1, 
                                    msas = "recursive")
proc.time()-t # Detencion del cronometro

# Evaluacion del modelo
ro_rec<- rolling_origin(tsfknn_pred_rec, h = medidas_predecir)
ro_rec


# Grafico de predicciones para el horizonte de pronostico
plot(ro_rec, h = medidas_predecir)

# Resultados exportados a csv
matriz_pronosticos_rec<-data.frame(ro_rec$predictions)
matriz_pronosticos_rec
write.csv(matriz_pronosticos_rec, "out_of_sample_tsfknn_rec_prediction.csv") 

# Calculo de rmse (MIMO mejor que recursiva)
knn_rec_pred=t(matriz_pronosticos_rec[1,1:(ncol(matriz_pronosticos_rec))]) #extrae primera fila de datos
rmse(test_data$POTENCIA_TRAFO[0:medidas_predecir],knn_rec_pred)


######################################################################################
##### METODO 2. FORECASTING DE SERIES TEMPORALES CON REGLAS DIFUSAS (LIBRERIA FRBS)
######################################################################################
### 2.0 tratamiento de datos para entrenamiento del metodo FRBS
# Bucle que guarda grupos de 96 datos en vectores
# Cada vector contiene 96 observaciones (frecuencia diaria) + 1 variable objetivo
data_vectors<- list()
n <- length(potencias_data_ts$POTENCIA_TRAFO2)-(medidas_predecir)
for(i in 1:n){
  data_vector <- list(potencias_data_ts$POTENCIA_TRAFO2[i:(i+medidas_predecir)])
  data_vectors <- c(data_vectors, data_vector)
}


# Creacion de un dataframe con tantas filas como vectores se han creado y 97 columnas
# La ultima columna contiene los valores de la variable objetivo
frbs_data <- as.data.frame(t(as.data.frame(data_vectors)))

rownames(frbs_data)<-NULL

# Creacion de particiones de entrenamiento y test
set.seed(111) # Se fija una semilla para poder replicar el experimento
frbs_train_partition <- createDataPartition(y=frbs_data[,ncol(frbs_data)],
                                            p= 0.7, # Ratio de particion 70:30
                                            list = FALSE)


frbs_train_data<- frbs_data[frbs_train_partition,]
frbs_test_data<- frbs_data[-frbs_train_partition,]




### 2.1. Estrategia DENFIS (mas util en variables con valores en cluster)
#Dthr: valor umbral para el metodo de agrupacion entre 0 y 1
#step.size: tamanyo de paso del metodo de minims cuadrados entre 0 y 1
#d: ancho de la funcion de membresia triangular
denfis_ctrl <- list(Dthr = 1, 
                    max.iter = 10, 
                    step.size = 0,1, 
                    d = 2)

set.seed(111)

# Entrenamiento del modelo
t <- proc.time() # Inicio del cronometro 
frbs_fit_denfis<- frbs.learn(frbs_train_data, NULL, method.type = "DENFIS", control = denfis_ctrl)
proc.time()-t # Detencion del cronometro

# Resumen de caracteriasticas del modelo entrenado
summary(frbs_fit_denfis)

# Prediccion de valores para la particion de test dentro de muestra
frbs_test_data_without_target<- frbs_test_data[,-ncol(frbs_test_data)]
frbs_test_pred_denfis<- predict(frbs_fit_denfis, newdata = frbs_test_data_without_target)


# Calculo de RMSE dentro de muestra
rmse(data.matrix(frbs_test_data$V97),data.matrix(frbs_test_pred_denfis))


# Grafico de predicciones para la particion de test del modelo
plot(frbs_test_pred_denfis,type="l",col="red")
lines(frbs_test_data[,ncol(frbs_test_data)],col="black")


# Ultimos 96 registros de datos, para llevar a cabo la prediccion fuera de muestra
last_data<- frbs_data[(length(frbs_data)-medidas_predecir):length(frbs_data),]
last_data_without_target <- last_data[,-ncol(last_data)]
out_of_sample_denfis_prediction <- predict(frbs_fit_denfis, newdata = last_data_without_target)
write.csv(out_of_sample_denfis_prediction, "out_of_sample_denfis_prediction.csv") # Resultados exportados a csv


### 2.2. Estrategia HYFIS
#type.tnorm: t-normal de medias estandar
#type.dibuz: tipo de funcion de agregacion de centro de gravedad modificado por medias
#type.implicacion: tipo de funcion de agregacion
hyfis_ctrl<- list(num.labels = 5, 
                  max.iter = 2, 
                  step.size = 1, 
                  type.tnorm = "MIN",
                  type.defuz = "COG", 
                  type.implication.func = "ZADEH")

# Entrenamiento del modelo
set.seed(111)
t <- proc.time() # Inicio del cronometro 
frbs_fit_hyfis <- frbs.learn(frbs_train_data[1:480,], NULL, method.type = "HYFIS", control = hyfis_ctrl)
proc.time()-t # Detencion del cronometro

# Resumen de caracteristicas del modelo entrenado
summary(frbs_fit_hyfis)

frbs_test_data_without_target <- frbs_test_data[,-ncol(frbs_test_data)]
frbs_test_pred_hyfis <- predict(frbs_fit_hyfis, newdata = frbs_test_data_without_target[1:480,])


# Calculo de RMSE comparando con los datos dentro de muestra
rmse(data.matrix((frbs_test_data$V97)[1:480]), data.matrix(frbs_test_pred_hyfis))

# Grafico de predicciones para la particion de test del modelo
plot(frbs_test_pred_hyfis,type="l",col="red")
lines(frbs_test_data[,ncol(frbs_test_data)],col="black")


# Ultimos 96 registros de datos,  para llevar a cabo la prediccion horaria fuera de muestra
last_data <- frbs_data[(length(frbs_data)-medidas_predecir):length(frbs_data)]
last_data_without_target <- last_data[,-ncol(last_data)]
  
out_of_sample_hyfis_prediction <- predict(frbs_fit_hyfis, newdata = last_data_without_target)

write.csv(out_of_sample_hyfis_prediction, "out_of_sample_hyfis_prediction.csv") # Resultados exportados a csv



#---------------------------------------------------------------
########################################################################################################### 
##### METODO 3. FORECASTING DE SERIES TEMPORALES CON MODELOS ARIMA (LIBRERIA FORECAST, FUNCION AUTO.ARIMA)
########################################################################################################### 

# Entrenamiento del modelo
t <- proc.time() # Inicio del cronometro 
# d: orden de la primera diferenciacion
# D: orden de la diferenciacion estacional
# ic=> seleccion de modelo
#     aic: criterio de informacion Akaike
#     aicc: criterio de informacion Akaike corregido para tamanyos de muestra finitos
#     bic: criterio de informacion bayesiano
# test=> Tipo de prueba de raiz unitaria
fit_auto_arima <- auto.arima(train_data$POTENCIA_TRAFO2, 
                             d = NA, 
                             D = NA, 
                             stationary = FALSE,
                             seasonal = TRUE, 
                             ic = c("aicc", "aic", "bic"),
                             test = c("kpss", "adf", "pp"), 
                             test.args = list(),
                             seasonal.test = c("seas", "ocsb", "hegy", "ch"),
                             seasonal.test.args = list())
proc.time()-t # Detencion del cronometro

# Resumen de caracteristicas del modelo
summary(fit_auto_arima)


# Prediccion 
#-h=> num de periodo a predecir
#-level=> niivel de confianza de prediccion
#-va a salir valores predichos con numeros de ordenes mas original datos
pronostico <- forecast(fit_auto_arima, h =medidas_predecir, level=95)


plot(pronostico,
     main = "Forecasts from ARIMA(1,1,3) para Potencias trafico",
     ylab = "grado"
)


# Guardar resultado de prediccion
# Conversion de los valores predichos como resultados a dataframe y exportacion a csv
matriz_pronosticos_autoarima <-data.frame(pronostico$mean,pronostico$lower,pronostico$upper)
write.csv(matriz_pronosticos_autoarima, "out_of_sample_autoarima_prediction.csv") 




#########################################################################################################    
##### METODO 4. FORECASTING DE SERIES TEMPORALES CON REDES NEURONALES AUTORREGRESIVAS (LIBRERIA ARNN)
#########################################################################################################    

# Conversion de los datos a objeto de tipo serie temporal para entrenar 
arnn_ts <- as.ts(train_data$POTENCIA_TRAFO2)

# Entrenamiento del modelo
t <- proc.time() # Inicio del cronometro 
#h=>num de neuronas en la capa oculta
arnn_fit <- arnn(x=arnn_ts, 
                 lags = 1:192, 
                 H = 2)

arnn_fit <- arnn(x=arnn_ts, 
                 lags = 1:medidas_predecir, 
                 H = 2)


proc.time()-t # Detencion del cronometro

# Resumen de resultados del entrenamiento
arnn_fit


# Prediccion 
out_of_sample_arnn_prediction <- forecast(arnn_fit, h=192, level=90)
write.csv(out_of_sample_arnn_prediction, "out_of_sample_arnn_prediction.csv") # Resultados exportados a csv

# Grafico de la prediccion
plot(forecast(arnn_fit, h=192, level=90),main="Pontencias trafico")



###################################################################################### 
##### METODO 5. FORECASTING DE SERIES TEMPORALES CON COMBINACION DE PREDICCIONES
######################################################################################

# Dataframes con predicciones de los modelos individuales
knn_mimo_df<- read.csv("out_of_sample_tsfknn_mimo_prediction.csv", header = TRUE)
frbs_hyfis_df<- read.csv("out_of_sample_hyfis_prediction.csv", header = TRUE)
auto_arima_df<- read.csv("out_of_sample_autoarima_prediction.csv", header = TRUE)
arnn_df<- read.csv("out_of_sample_arnn_prediction.csv", header = TRUE)

#-------------
# Extraccion de valores de las predicciones individuales
#--knn mino   ;  # t()=> transpuesta,
knn_mimo_pred<- t(knn_mimo_df[1, 2:(ncol(knn_mimo_df))])
colnames(knn_mimo_pred) <- "knn_mimo_pred"

#--frbs hyfis
frbs_hyfis_pred<- frbs_hyfis_df[1:medidas_predecir, 2]

#--autoarima
auto_arima_pred<- auto_arima_df$pronostico.mean

#--arnn
arnn_pred<- arnn_df$Point.Forecast


#--------------
# Dataframe que unifica las predicciones de todos los modelos
comb_df<- data.frame(auto_arima_pred,
                      knn_mimo_pred,
                      frbs_hyfis_pred,
                      arnn_pred, row.names = NULL)

# Media combinatoria de las predicciones 
predictions_means<- rowMeans(comb_df)

# Muestra resultados
predictions_means


# graficos de resultado prediccion con los valores esperados.
plot(test_data$POTENCIA_TRAFO[0:medidas_predecir],type="l", col="black",main="comparativas para potencias trafico")
lines(predictions_means,col="red")




############################################################################################################
#####  Comparativa final de resultados predichos con los valores esperados entre todos los metodos    ######
############################################################################################################

#### Numerico/Calculo de errores de cada metodo
c_modelo=c("knn","frbs","autoarima","arnn","combinatorio")
### RMSE => raiz de error cuadrado medio
v01=rmse(test_data$POTENCIA_TRAFO[0:medidas_predecir],knn_mimo_pred)
v02=rmse(test_data$POTENCIA_TRAFO[0:medidas_predecir],frbs_hyfis_pred)
v03=rmse(test_data$POTENCIA_TRAFO[0:medidas_predecir],auto_arima_pred)
v04=rmse(test_data$POTENCIA_TRAFO[0:medidas_predecir],arnn_pred)
v05=rmse(test_data$POTENCIA_TRAFO[0:medidas_predecir],predictions_means)
c_rmse=c(v01,v02,v03,v04,v05)


# Crea dataframe para mostrar un resumen de resultados 
resultado_rmse=data.frame("Modelo"=c_modelo,"POTENCIA_TRAFO"=c_rmse)  
resultado_rmse

#--------
### MAPE => error medio de porcentaje absoluto
v21=mape(test_data$POTENCIA_TRAFO[0:medidas_predecir],knn_mimo_pred)
v22=mape(test_data$POTENCIA_TRAFO[0:medidas_predecir],frbs_hyfis_pred)
v23=mape(test_data$POTENCIA_TRAFO[0:medidas_predecir],auto_arima_pred)
v24=mape(test_data$POTENCIA_TRAFO[0:medidas_predecir],arnn_pred)
v25=mape(test_data$POTENCIA_TRAFO[0:medidas_predecir],predictions_means)
c_mape=c(v21,v22,v23,v24,v25)

# Crea dataframe para mostrar un resumen de resultados 
resultado_mape=data.frame("Modelo"=c_modelo,"POTENCIA_TRAFO"=c_mape)  
resultado_mape

####--------------------------
#### Grafico 
# Potencias del trafico
par(mfrow=c(1,5))
plot(test_data$POTENCIA_TRAFO[0:medidas_predecir],type="l", col="black",main="Knn")
lines(knn_mimo_pred,col="red")
plot(test_data$POTENCIA_TRAFO[0:medidas_predecir],type="l", col="black",main="Hyfis FRBS")
lines(frbs_hyfis_pred,col="red")
plot(test_data$POTENCIA_TRAFO[0:medidas_predecir],type="l", col="black",main="Autoarima")
lines(auto_arima_pred,col="red")
plot(test_data$POTENCIA_TRAFO[0:medidas_predecir],type="l", col="black",main="Arnn")
lines(arnn_pred,col="red")
plot(test_data$POTENCIA_TRAFO[0:medidas_predecir],type="l", col="black",main="Combinatorio")
lines(predictions_means,col="red")

