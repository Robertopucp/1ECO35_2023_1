################  Workgroup 4 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Grupo6

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

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

# tidyverse es una recopilación de varias librerias (dply, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones
# haven tambien permite la lectura de base de datos de diferentes extensiones (i.e stata)


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

