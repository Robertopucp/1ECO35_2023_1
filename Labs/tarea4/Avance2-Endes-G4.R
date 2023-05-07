
############       Workgroup 4       ################

## Curso: Laboratorio de R y Python ####
## Profesor: Roberto Mendoza  ####
## Autor: Grupo 5 ####

# clean environment variables
rm(list = ls())

# clean console
cat("\014")

# Library 
library(pacman)  # me permite llamar a varias librerias a la vez

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

# Change working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# cargar datos 
rech84dv <- read_dta("../../data/endes/REC84DV.dta")
rech0 <- read_dta("../../data/endes/RECH0.dta")
rech1 <- read_dta("../../data/endes/RECH1.dta")
rech23 <- read_dta("../../data/endes/RECH23.dta")

str(rech84dv)

# para ver los nombres de las columnas de las bases de datos
head(rech84dv, n = 13)
head(rech0, n = 13)
head(rech1, n = 13)
head(rech23, n = 13)




#identifiicar las columnas repetidas de las bases de datos
intersect(colnames(rech1), colnames(rech23))
intersect(colnames(rech0), colnames(rech1))
intersect(colnames(rech0), colnames(rech84dv))

# Combina rech0, rech1 y rech23 en una sola tabla utilizando la columna común ID
datos_combinados <- merge(rech0, rech1, by = "HHID")

datos_combinados <- merge(datos_combinados, rech23, by = "HHID")

# no se puede unir datos_combinados con rech84dv porque no comparten una columna en común
names(rech84dv)

names(datos_combinados)

# crear una columna nueva para las dos bases de datos y luego unirlas 
datos_combinados$clave <- 1:nrow(datos_combinados)
rech84dv$clave <- 1:nrow(rech84dv)

datos_combinados_con_rech84dv <- merge(datos_combinados, rech84dv, by = "clave")

names(datos_combinados_con_rech84dv)

# variable dummy de vionecia fisica

datos_combinados_con_rech84dv$violencia_fisica <- ifelse(datos_combinados_con_rech84dv$d103a == 1 |
                                             datos_combinados_con_rech84dv$d103b == 1 |
                                             datos_combinados_con_rech84dv$d103c == 1 |
                                             datos_combinados_con_rech84dv$d105a == 1,
                                           1, 0)




