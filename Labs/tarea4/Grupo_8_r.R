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
p_load(dplyr, readxl, rstudioapi, tidyverse, foreign, datos)


options(scipen = 999) 

# Determinamos directorio de trabajo

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# Ejercicio sobre Datos de la CVR-----------------------

# Importamos la base de datos (haciendo los ajustes necesarios).

# Con etiquetas como datos

datoscvr <- read.spss("../../data/actos_est.sav",
                      use.value.labels = F, 
                      to.data.frame = TRUE,
                      encoding = "UTF-8")

datoscvr <- read.spss("../../data/actos_est.sav",
                      use.value.labels = T, 
                      to.data.frame = TRUE,
                      useBytes = TRUE,
                      encoding = "UTF-8")

str(datoscvr)

# Mostramos las etiquetas de valores y variables:

attributes(datoscvr)$variable.labels
attributes(datoscvr)$value.labels

# Filtramos la base de datos por eventos
# (desaparición, secuestros, reclutamiento forzado y muertes
# en atentados):

table(datoscvr$IDTIPOAC)

cvr <- filter(datoscvr, IDTIPOAC == 
                c("LDS", "LSE", "LRC", "MAT"))

# Eliminamos duplicados de la variable "IDACTO"






