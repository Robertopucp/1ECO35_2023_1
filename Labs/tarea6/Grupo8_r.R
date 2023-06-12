#TAREA 6------------------------------------------------
#Grupo 8
# Integrantes:
#- Renzo Mosquera (20181960)
#- Yenner Huancahuire (20173340)
#- Pamela Obregón (20173040)


# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library

library(pacman)
p_load(reshape, tidyverse, haven,dplyr,Hmisc)


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Cargamos las bases de datos

enaho <- read_dta("../../data/Juntos_program/data.dta")
unidos <- read_dta("../../data/Juntos_program/unidos.dta")

#
#PREGUNTA 1 ----
#

# Renombramos la variable numpanh añadiendole un guion 

enaho <- enaho    |> dplyr::rename( "numpanh_15" = "numpanh15", "numpanh_16" = "numpanh16",
                                    "numpanh_17" = "numpanh17","numpanh_18" = "numpanh18","numpanh_19" = "numpanh19")

# Mostramos nuestras variables de interés 

colnames(enaho)

# Creamos la nueva de base de datos en formato long

reshape_enaho <- enaho %>% pivot_longer(cols = -numper, names_to = c(".value", "año"), 
                                        names_sep = "_")


#
#PREGUNTA 2 ----
#

# Revisamos las variables de ambas bases de datos para identificar la variable en comun

colnames(enaho)
colnames(unidos)

# Unimos las bases de datos

bases <- merge(reshape_enaho, unidos, by = c("ubigeo")) 

#
#PREGUNTA 3 ----
#

# Colocamos etiquetas a todas las variables

label(bases$numper)<-'Identificador individual único'
label(bases$numpanh)<-'Identificador único de hogar en 2015'
label(bases$p400a3)<-'Año de nacimiento de la persona, reportado en 2015'
label(bases$p4022)<-'¿Estabas enfermo las ultimas 4 semanas?'
label(bases$unidos)<-'¿Programa Juntos aplicado en el distrito?'

# Colocamos etiquetas de valores a las siguientes variables p4022 y unidos

bases$p4022  <- ifelse(bases$p4022 == 1, "Si", "No")
bases$unidos  <- ifelse(bases$unidos == 1, "Si", "No")


