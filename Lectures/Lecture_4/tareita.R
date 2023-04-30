
rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

#install.packages("pacman")

library(pacman) 
# permite llamar a varias librerias de manera simultánea
# Si la librería no está instalada, entonces lo instala y llama para su uso

p_load(dplyr, readxl, tidyverse, foreign, datos) 

# tidyverse es una recopilación de varias librerias (dply, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Cargar paquete "readxl" para leer el archivo Excel
library(readxl)

# Leer la base de datos desde el archivo Excel
siagie <- read.csv("../../data/siagie.csv", row.names = F)
# Calcular la nota promedio final de cada alumno
datos$promedio <- rowMeans(datos[,2:ncol(datos)])

# Calcular la nota m�xima y m�nima de cada alumno
datos$maxima <- apply(datos[,2:ncol(datos)], 1, max)
datos$minima <- apply(datos[,2:ncol(datos)], 1, min)

# Calcular el promedio y mediana de notas de cada curso
curso_promedio <- apply(datos[,2:ncol(datos)], 2, mean)
curso_mediana <- apply(datos[,2:ncol(datos)], 2, median)

# Imprimir los resultados
print(datos)
print(curso_promedio)
print(curso_mediana)
