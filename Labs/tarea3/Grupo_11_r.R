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

vector <- runif(100, min=0, max=1) # Creamos el vector 
print (vector) # Imprimimos el vector para ver los números 

# Aplicamos el escalamiento al vector 
sapply(vector, function(x) (x - min(vector))/(max(vector) - min(vector)))

# Llamamos a la base de datos 
base_compras <- read.csv("../../data/BDD_compras_consumidores.csv", sep = ";") 
    # Debemos comolocar ";", pues esta es la forma de separación presente en el 
    # csv

#Aplicamos el escalamiento a la base_compras 
base_compras$escalamiento <- apply(base_compras[, 3:8], 2, function(x)
  (x - min(base_compras))/(max(base_compras)- min(base_compras)))

## Pregunta 2: Apply ####
# Llamamos a la base de datos
siagie <- read.csv("../../data/siagie.csv")
str(siagie)

# Hallando el promedio de cada alumno 
siagie$Promedio <- apply(siagie[, 7:17], 1, mean)
 
# Hallando nota máxima de cada alumno 
siagie$Nota_max <- apply(siagie[, 7:17], 1, max)

# Hallando nota mínima de cada alumno
siagie$Nota_min <- apply(siagie[, 7:17], 1, min)

# Hallando el promedio de cada curso
Promedio_cursos <- apply(siagie[, 7:17], 2, mean)
print (Promedio_cursos)

# Hallando el mediana de cada curso
Mediana_cursos <- apply(siagie[, 7:17], 2, median)
print (Mediana_cursos)

