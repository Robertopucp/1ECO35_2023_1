################  Workgroup3 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Grupo 6

#### 1 EJERCICIO R:####

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

#Llamamamo a las librerías que vamos a usar en los ejercicios

library(pacman) 
library(dplyr)
#install.packages("tidyr")
#library(tidyr)
# permite llamar a varias librerias de manera simultÃ¡nea
# Si la librería no estÃ¡ instalada, entonces lo instala y llama para su uso

p_load(dplyr, readxl, tidyverse, foreign, datos) 

# tidyverse es una recopilación de varias librerias (dply, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Cargar paquete "readxl" para leer el archivo Excel
library(readxl)

# Cargar la base de datos
datos <- read.csv("../../data/BDD_compras_consumidores.csv")

# FunciÃ³n de escalamiento
 escalamiento <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

#Aplicar la función a las columnas numÃ©ricas de la base de datos
datos[, sapply(datos, is.numeric)] <- apply(datos[, sapply(datos, is.numeric)], 2, escalamiento)

# Aplicar la función al vector de 100 nÃºmeros aleatorios
vector <- runif(100) # generar vector de 100 nÃºmeros aleatorios
vector_esc <- escalamiento(vector) # aplicar funciÃ³n de escalamiento
print (vector_esc)

#### 2 EJERCICIO R:####

# Llamamos a la base de datos
siagie <- read.csv("../../data/siagie.csv")
str(siagie)

#Calculamos la nota promedio final de cada alumno
siagie_promedio <- siagie %>% 
  mutate(promedio_final = rowMeans(.[7:ncol(.)], na.rm = TRUE))
print(siagie_promedio)

#Hallamos la nota maxima y minima
siagie_max <- siagie %>% 
  mutate(nota_max = apply(.[7:ncol(siagie)], 1, max, na.rm = TRUE))
print(siagie_max)

siagie_min <- siagie %>% 
  mutate(nota_min = apply(.[7:ncol(siagie)], 1, min, na.rm = TRUE))
print(siagie_min)

#Hallamos el promedio y mediana de notas de cada curso.

siagie_promedio_curso <- siagie %>%
  select(7:ncol(.)) %>%
  pivot_longer(everything(), names_to = "curso", values_to = "nota") %>%
  group_by(curso) %>%
  summarize(promedio = mean(nota, na.rm = TRUE), mediana = median(nota, na.rm = TRUE))
print(siagie_promedio_curso)
#Podemos ver que el curso con mayor promedio es el de Ingles con un puntaje de 14.2 puntos.
#Mientras que el de menor promedio es el de Matematica con un 12.3 puntos.