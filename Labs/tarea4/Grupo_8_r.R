#TAREA 4------------------------------------------------
#Grupo 8
# Integrantes:
#- Renzo Mosquera (20181960)
#- Yenner Huancahuire (20173340)
#- Pamela Obregón (20173040)

# Instalamos librerias

install.packages("dplyr")
install.packages("readxl")
install.packages("pacman")
install.packages("stringr")

# Limpiamos la consola

cat("\014")

# Library y otras opciones

library(pacman) 
p_load(dplyr, readxl, rstudioapi)
p_load(dplyr, readxl, tidyverse, foreign, datos)

options(scipen = 999) 

# Determinamos directorio de trabajo

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# Ejercicio sobre Datos de la CVR-----------------------

# Importamos la base de datos (haciendo los ajustes necesarios)

datoscvr <- read.spss("../../data/actos_est.sav",
                      to.data.frame = TRUE)
str(datoscvr)

# Etiquetas de variables

attributes(datoscvr)$variable.labelseer


