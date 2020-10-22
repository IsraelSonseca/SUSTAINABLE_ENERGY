# Funcion para mapear cada fecha con su correspondiente periodo de la semana
# Asigna categoria "dia_entre_semana" o "fin_de_semana" a cada fecha
extrae_dia_semana <- function(x) {
  val <- weekdays(x) # Funcion que extrae el d???????as de la semana, dada una fecha
  if (grepl("s?bado", val, perl=TRUE) | val == "domingo") { 
    val2 = "fin_de_semana"
  }
  else {
    val2= "dia_entre_semana"
  }
  return(val2)
}



substrRight <- function(x, n){ 
  substr(x, nchar(x)-n+1, nchar(x)) 
} 


