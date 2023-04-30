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
library(pacman) 
p_load(dplyr, readxl, tidyverse, foreign, datos) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Desarrollo de las preguntas ####

## Pregunta 1: Map, sapply, apply, función Lambda ####

vector <- sample(100) # Creamos el vector aletoario
base_compras <- read.csv("../../data/BDD_compras_consumidores.csv", sep = ";")
base_compras$escalmamiento <- apply(base_compras[, 3:8], 2, function(x)
  (x - min(vector))/(max(vector)- min(vector)))



## Pregunta 2: Apply ####
siagie <- read.csv("../../data/siagie.csv")
str(siagie)

# Hallando el promedio de cada alumno 
siagie$Promedio <- apply(siagie[, 7:17], 1, mean)
 
# Hallando nota máxima de cada alumno 
siagie$Nota_max <- apply(siagie[, 7:17], 1, max)

# Hallando nota mínima de cada alumno
siagie$Nota_max <- apply(siagie[, 7:17], 1, min)

# Hallando el promedio de cada curso
