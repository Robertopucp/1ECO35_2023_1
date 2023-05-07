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
                      reencode = "UTF-8")
                      
str(datoscvr)
sapply(datoscvr, class)

# Mostramos las etiquetas de valores y variables:

attributes(datoscvr)$variable.labels
attributes(datoscvr)$value.labels


# Filtramos la base de datos por eventos
# (desaparición, secuestros, reclutamiento forzado y muertes
# en atentados):

table(datoscvr$IDTIPOAC)

datoscvr <- filter(datoscvr, IDTIPOAC == 
                c("LDS", "LSE", "LRC", "MAT"))

# Eliminamos duplicados de la variable "IDACTO"

datoscvr <- datoscvr[!duplicated(datoscvr$IDACTO), ]

# Creamos las bases de datos solicitadas:

# Renombramos variables

datoscvr <-rename(datoscvr, DEPARTAMENTO = DEPNA0,
       DISTRITO = UBIDIST)

# Agregamos etiquetas necesarias

datoscvr$DEPARTAMENTO <- factor(datoscvr$DEPARTAMENTO, 
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 90),
                             labels = c("Ayacucho", "Apurímac", "Huancavelica",
                                        "Cusco", "Huánuco", "Ucayali",
                                        "San Martín", "Puno", "Junín",
                                        "Lima-Callao", "Otros"))

datoscvr$PERIODO <- factor(datoscvr$PERIODO, 
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("1980-1982", "1983-1985",
                                           "1986-1988", "1989-1992",
                                           "1993-2000"))
                                          

# Base de datos que se agrupa por departamento y obtenemos el total de eventos
# de violencia

EventosxDpto <- datoscvr %>% group_by(DEPARTAMENTO) %>% 
  summarise(Total_Desaparición = sum(LDS_LDT),
            Total_Secuestros = sum(LSE),
            Total_RecluForza = sum(LRC),
            Total_MuAtentados = sum(MAT))

# Base de datos que se agrupa por distrito y obtenemos el total de eventos
# de violencia

EventosxDistr <- datoscvr %>% group_by(DISTRITO) %>% 
  summarise(Total_Desaparición = sum(LDS_LDT),
            Total_Secuestros = sum(LSE),
            Total_RecluForza = sum(LRC),
            Total_MuAtentados = sum(MAT))

# Base de datos que se agrupa por departamento y periodo, y luego obtenemos el total de eventos
# obtenemos el total de eventos de violencia

EventosDyP <- datoscvr %>% group_by(DEPARTAMENTO, PERIODO) %>% 
  summarise(Total_Desaparición = sum(LDS_LDT),
            Total_Secuestros = sum(LSE),
            Total_RecluForza = sum(LRC),
            Total_MuAtentados = sum(MAT))


