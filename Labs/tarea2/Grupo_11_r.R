# Integrantes ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)

# Consideraciones previas: #####
## Borrando el environment ####
rm(list = ls())

## Borrando los graficos ####
graphics.off()

##Borrando la consola ####
cat("\014")

## Llamando a los directorios necesarios
library(dplyr)
library(stringr)

# Desarrollo de las preguntas ####

## Pregunta 1: Condicional 1 ####
pago <- 400

if (pago < 100) {
  cat ("Compra de", pago, "soles y pago en efectivo")
} else if ((pago>100) & (pago<300)) {
  cat ("Compra de", pago, "soles y pago con tarjeta de d?bito")
} else {
  cat ("Compra de", (pago*0.9), "soles y pago con tarjeta de cr?dito")
}
  

## Pregunta 2: Condicional 2 ####

fx1 <- function(x)
{
  if ((x>0) & (x<100)) {
    result = x^(1/2)
  } else if ((x>100) & (x<300)) {
    result = x-5
  } else {
    result = 50
  }
  return(result)
}


fx1( 400 )
## Pregunta 3: Loops ####

# Vector con las utilidades netas anuales
utilidades <- c(100, 152, -1 , 8, 12, 156,35, -10, 100, -0.5, 30, 1050 , 7, -10)

# Creamos el bucle for
for (uneta in utilidades) {
  # Cuando la utilidad es negativa, saltar a la siguiente iteración
  if (uneta < 0) {
    next
  }
  # Cuando la utilidad es mayor que 1000, salir del bucle
  if (uneta > 1000) {
    break
  }
  # Mensaje con la utilidad neta anual
  mensaje <- paste("La utilidad neta anual es", uneta, "millones")
  print(mensaje)
}

#PREGUNTAS SOLO EN R
## Pregunta 4: Funcion para calcular el factorial de un numero n! #### 

factorial <- function(n) {
  if(n == 0) {
    return(1) # El factorial de 0 es 1
  } else {
    return(n * factorial(n-1))
  }
}
#a modo de ejemplo probamos el factorial de 7:
factorial (7)

## Pregunta 5: Funcion de masa corporal #### 

#creamos la función del imc
calcular_imc <- function(peso, talla){
  imc <- peso / talla^2
#colocamos los criterios según la tabla IMC
  clasificacion <- "Desconocido"
  if (imc>= 18.5 && imc <= 24.9){
    clasificacion <- "Normal"
  } else if(imc >= 25 && imc <= 29.9){
    clasificacion <- "Sobrepeso"
  } else if(imc >= 30 && imc <= 34.9){
    clasificacion <- "Obesidad grado I"
  } else if(imc >= 35 && imc <=39.9){
    clasificacion <- "Obesidad grado II"
  } else if (imc >= 40){
    clasificacion <- "Obesidad grado III"
  }
  resultado <- list(peso = peso, talla = talla, imc = imc, clasificacion = clasificacion)
  return(resultado)
 }
#colocamos los datos de cada estudiante
estudiante_1 <- calcular_imc(peso = 70, talla = 1.5)
estudiante_2 <- calcular_imc(peso = 85, talla = 1.8)
estudiante_3 <- calcular_imc(peso = 50, talla = 1.6)

#vemos los 4 outcomes:
estudiante_1
estudiante_2
estudiante_3

## Pregunta 6: Funcion aplicando activos financieros #### 

#obtnemos el directorio de trabajo
getwd()

user <- Sys.getenv("USERNAME")  # username estamose xtrayencdo el usuario automaticamente
print(user)
setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2023_1/Labs/tarea2") ) # set directorio

#codigo para leer un archivo csv
portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")
portfolio

calculadora <- function(data){ # se aplica la funcion aplicada a una base de datos
  
  X <- data$X #extrayendo cada fila de datos
  Y <- data$Y
  w1 <- 0.2
  w2 <- 0.8
  #formula de Coef. de correlacion de Pearson: COV(X,Y)/(var(x)*var(y))^0.5
  pearson <- (cov(X,Y))/((var(X)*var(Y))^0.5)
  varianza <- (var(X)*w1^2)+(var(Y)*w2^2)+(2*cov(X,Y)*w1*w2)
  
  
  return(list(pearson,varianza))
}
calculadora(portfolio)
