#Script R

rm(list = ls())

graphics.off()

cat("\014")

library(pacman)  
library(readr)

p_load(dplyr, readxl, rstudioapi, stringr)

getwd()

setwd("C:/Users/Ademir/Documents/GitHub/1ECO35_2023_1/data")

getwd()

#Ejercicio 1: Utilizando un vector con 100 valores aleatorios entre los
#números 1 al 10000. Se obtiene el máximo y el mínimo, luego se crea una
#función que corresponda con lo solicitado.

vector <- sample(1:10000,100,FALSE)
print(min(vector))
print(max(vector))
d <- max(vector) - min(vector)

sapply(vector, function(x){
  out = (x - min(vector))/d
})

#Primero se extrae la base de datos
datos <- read.csv("BDD_compras_consumidores.csv", sep = ";")

#Se hallan el máximo y mínimo
datos_min <- apply(datos[ ,3:8], 2, min)
print(datos_min)
datos_max <- apply(datos[ ,3:8], 2, max)
print(datos_max)
dife <- datos_max - datos_min

sapply(datos[100,3:8], function(x){
  out = (x - datos_min)/dife
  return(out)
} )

#Ejercicio 2: Se obtiene la base

data <- read_csv("siagie.csv")
str(data1)

#Observamos la base de datos usando str(), notamos que no todas las variables
#son numéricas
#Cambiamos algunas variables para volverlas numéricas y puedan realizarse
#operaciones, estas son:
#comunicacion
#educacion_fisica
#educacion_para_el_trabajo
#matematica

data1$comunicacion <- as.numeric(data1$comunicacion)
data1$educacion_fisica <- as.numeric(data1$educacion_fisica)
data1$educacion_para_el_trabajo <- as.numeric(data1$educacion_para_el_trabajo)
data1$matematica <- as.numeric(data1$matematica)
str(data1)

#Se hallan los promedios solicitados:
promedio_final <- apply(data1[ ,6:16], 1, mean)
print(promedio_final)
max(promedio_final)
min(promedio_final)

promedio_notas <- apply(data1[ ,6:16], 2, mean)
print(promedio_notas)
mediana_notas <- apply(data1[ ,6:16], 2, median)
print(mediana_notas)



