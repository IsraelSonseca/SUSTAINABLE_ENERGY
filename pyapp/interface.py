import pandas as pd

dfClima = pd.read_excel('C:/Users/Usuario/OneDrive/TFG ENERGIA/compartido/DATOS/datos/CLIMA.xlsx')
fechasbuenas = []
for i in dfClima.iloc[:, 0]:
    linea = str(i).strip().split()
    fechasbuenas.append(linea[0])
dfClima["Fecha_hora"] = fechasbuenas

horasbuenas = []
for i in dfClima.iloc[:, 1]:
    linea = str(i).strip().split()
    horasbuenas.append(linea[0])
dfClima["HORA"] = horasbuenas

columnasClima = dfClima.columns
dfClima["Fecha_hora"] = dfClima["Fecha_hora"] + " " + dfClima["HORA"]
for i in [0, 1]:
    dfClima = dfClima.drop(columnasClima[i], axis=1)

dfEVENTOS = pd.read_excel('C:/Users/Usuario/OneDrive/TFG ENERGIA/compartido/DATOS/EVENTOS.xlsx')
fechasbuenas = []
for i in dfEVENTOS.iloc[:, 1]:
    linea = str(i).strip().split()
    fechasbuenas.append(linea[0])
dfEVENTOS["Fecha_hora"] = fechasbuenas

horasbuenas = []
for i in dfEVENTOS.iloc[:, 2]:
    linea = str(i).strip().split()
    horasbuenas.append(linea[0])
dfEVENTOS["HORA"] = horasbuenas

columnasEv = dfEVENTOS.columns
dfEVENTOS["Fecha_hora"] = dfEVENTOS["Fecha_hora"] + " " + dfEVENTOS["HORA"]
for i in range(4):
    dfEVENTOS = dfEVENTOS.drop(columnasEv[i], axis=1)

dfEVENTOS["Evento"] = 1

df2017 = pd.read_excel('C:/Users/Usuario/Documents/GitHub/SUSTAINABLE_ENERGY/data/HISTORICO DATOS 2017.xlsx')
columnas7 = df2017.columns
df2018 = pd.read_excel('C:/Users/Usuario/OneDrive/TFG ENERGIA/compartido/DATOS/HISTORICO DATOS 2018.xlsx')
columnas8 = df2018.columns
a = len(columnas7)
b = len(columnas8)
for i in range(a, b):
    df2018 = df2018.drop(columnas8[i], axis=1)
df2018.columns = columnas7
dfTOTAL = pd.concat([df2017, df2018])
dfTOTAL.index = range(dfTOTAL.shape[0])  # para actualizar los indices
# dfTOTAL["Evento"]=0


# testFECHA_hORA=str(dfTOTAL.iloc[2,0])
fechahoraFormateada = []
for i in dfTOTAL.iloc[:, 0]:
    # linea=str(i)
    fechahoraFormateada.append(str(i))
dfTOTAL["Fecha_hora"] = fechahoraFormateada

DATOScorrectos = pd.merge(dfTOTAL, dfEVENTOS, on="Fecha_hora", how="left")
pendientes = 0
for i, j in enumerate(DATOScorrectos.iloc[:, 5]):
    if j == 1:
        pendientes = 16
    if pendientes > 0:
        DATOScorrectos.iloc[i, 5] = 1
        pendientes -= 1
    else:
        DATOScorrectos.iloc[i, 5] = 0

DATOScorrectos = pd.merge(DATOScorrectos, dfClima, on="Fecha_hora", how="left")
DATOScorrectos['TEMPERATURA']=DATOScorrectos['TEMPERATURA'].fillna(0)
DATOScorrectos['HUMEDAD']=DATOScorrectos['HUMEDAD'].fillna(0)
DATOScorrectos['PRESION']=DATOScorrectos['PRESION'].fillna(0)

pendientes = 0
for i, j in enumerate(DATOScorrectos.iloc[:, 6]):
    if j!=0:
        pendientes = 4
        temperatura = DATOScorrectos.iloc[i,6]
        humedad = DATOScorrectos.iloc[i,7]
        presion = DATOScorrectos.iloc[i,8]
    if pendientes > 0:
        DATOScorrectos.iloc[i, 6] = temperatura
        DATOScorrectos.iloc[i, 7] = humedad
        DATOScorrectos.iloc[i, 8] = presion
        pendientes -= 1
    #else:
    #    DATOScorrectos.iloc[i, 6] = 0
    #    DATOScorrectos.iloc[i, 7] = 0
    #    DATOScorrectos.iloc[i, 8] = 0
1 + 1

DATOScorrectos.to_excel('C:/Users/Usuario/OneDrive/TFG ENERGIA/compartido/DATOS/datos/DATOSPASADOSPORPYTHON.xlsx', index = False)

