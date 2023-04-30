# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

library(pacman) 
# permite llamar a varias librerias de manera simultánea
# Si la librería no está instalada, entonces lo instala y llama para su uso

p_load(dplyr, readxl, tidyverse, foreign, datos) 

# tidyverse es una recopilación de varias librerias (dply, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargar base de datos
siagie <- read.csv("../../data/siagie.csv")

# Calcular nota promedio final, nota m�xima y m�nima por alumno
siagie <- siagie %>%
  group_by(alumno) %>%
  mutate(promedio_final = mean(nota_final),
         nota_maxima = max(nota_final),
         nota_minima = min(nota_final))

# Calcular promedio y mediana de notas por curso
siagie <- siagie %>%
  group_by(curso) %>%
  mutate(promedio_curso = mean(nota_final),
         mediana_curso = median(nota_final))