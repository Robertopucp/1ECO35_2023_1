
### Script solo en R

# clean environment variables
rm(list = ls())

# clean plots
graphics.off()

## Función aplicada a 2 activos financieros
## Coeficiente de pearson de dos variables aleatorias:

# Cargamos las librerías
library(dplyr)
library(stringr)

getwd()

user <- Sys.getenv("USERNAME")  # extraemos el usuario
print(user)
setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2023_1/Labs/tarea2") ) # set directorio


#leemos el archivo csv
portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")
portfolio


calculadora <- function(data){ # aplicamos la funcion a una base de datos
  
  X <- data$X #extraemos las filas de datos
  Y <- data$Y
  w1 <- 0.2
  w2 <- 0.8
  #formula de Coef. de correlacion de Pearson: formula 
  pearson <- (cov(X,Y))/((var(X)*var(Y))^0.5)
  varianza <- (var(X)*w1^2)+(var(Y)*w2^2)+(2*cov(X,Y)*w1*w2)
  
  
  return(list(pearson,varianza))
}
calculadora(portfolio)

