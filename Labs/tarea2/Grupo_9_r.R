#Tarea 2 
#PARTE EN R Y PYTHON
#loops
#creamos el vector
utilidades_netas_anuales <- c(100, 152, -1 , 8, 12, 156, 35, -10, 100, -0.5, 30, 1050 , 7, -10)
#utilizaremos for
for (utilidad_neta_anual in utilidades_netas_anuales) {
  if (utilidad_neta_anual <  0) {
    next
  } else if (utilidad_neta_anual >  1000) {
    break
  } else {
    cat("La utilidad neta anual es", utilidad_neta_anual,"millones.\n")
  }
}



#PARTE SOLO EN R

#Función aplicada a dos activos financieros

#Directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Relative Path
#Cargamos la Base de Datos
portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")

# Hallamos el Coeficiente de Pearson

coeficiente_pearson <- function(data){
  
  x <- data$X
  y <- data$Y
  
  coeficiente_pearson_cal <- (cov(x,y))/(sqrt(var(x))*sqrt(var(y)))
  return(coeficiente_pearson_cal)                 
}

coeficiente_pearson(portfolio)

 #Hallamos la varianza 

varianza <-function(data){
  
  W_x <-0.2
  W_y <-0.8
  x <- data$X
  y <- data$Y
  
varianza_cal <- (W_x^2)*var(x)+(W_y^2)*var(y)+2*W_x*W_y*cov(x,y)
  
  return(varianza_cal)
}
  
varianza(portfolio)





