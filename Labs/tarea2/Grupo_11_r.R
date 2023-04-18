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
  cat ("Compra de", pago, "soles y pago con tarjeta de débito")
} else {
  cat ("Compra de", (pago*0.9), "soles y pago con tarjeta de crédito")
}
  

## Pregunta 2: Condicional 2 ####


## Pregunta 3: Loops ####

## Pregunta 4: Función para calcular el factorial de un número n! #### 

## Pregunta 5: Funcion de masa corporal #### 


## Pregunta 3: Funcion aplicando activos financieros #### 

