# Integrantes ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)


#ENDES
# Consideraciones previas: #####
## Borrando el environment ####
rm(list = ls())

## Borrando los graficos ####
graphics.off()

##Borrando la consola ####
cat("\014")

## Llamando a los directorios necesarios
library(pacman) 
p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

install.packages("dplyr")
library(dplyr)

# Change working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd("C:/Users/Usuario/OneDrive/Documentos/1ECO35_2023_1/data/endes")
getwd()

# Desarrollo de las preguntas ####

#Unir los módulos RECH84DV, RECH0, RECH1 y RECH23. Crear una variable dummy de violencia física y sexual respectivamente.

# Cargamos los datos de los módulos RECH84DV, RECH0, RECH1 y RECH23
rech84dv <- read_dta("C:/Users/Usuario/OneDrive/Documentos/1ECO35_2023_1/data/endes/REC84DV.dta")
rech0 <- read_dta("C:/Users/Usuario/OneDrive/Documentos/1ECO35_2023_1/data/endes/RECH0.dta")
rech1 <- read_dta("C:/Users/Usuario/OneDrive/Documentos/1ECO35_2023_1/data/endes/RECH1.dta")
rech23 <- read_dta("C:/Users/Usuario/OneDrive/Documentos/1ECO35_2023_1/data/endes/RECH23.dta")

names(rech84dv)
names(rech0)

# Renombramos "caseid" a "HHID" en rech84dv
names(rech84dv)[names(rech84dv) == "caseid"] <- "HHID"

# Unimos los data frames
datos_endes <- merge(rech84dv, rech0, by = "HHID")
datos_endes <- merge(datos_endes, rech1, by = "HHID")
datos_endes <- merge(datos_endes, rech23, by = "HHID")

#Crear una variable dummy por tipo de violencia física: d105a d105b d105c d105d d105e. Cada variable toma los siguientes valores: 0 (respuesta No), 1 (Frecuentemente), 2 (Algunas veces) y 3 (Nunca).

#d103a : Su esposo/compañero alguna vez la empujó, sacudió o le tiró algo
#d103b : Su esposo/compañero alguna vez la abofeteó o le retorció el brazo
#d103c : Su esposo/compañero alguna vez la golpeó con el puño o con algo que pudo hace
#d103d: Su esposo/compañero alguna vez la ha pateado o arrastrado
#d103e: Su esposo/compañero alguna vez trató de estrangularla o quemarla

# Creamos variables dummy para cada tipo de violencia física
datos_endes$d105a_dummy <- ifelse(datos_endes$d105a %in% c(1, 2), 1, 0)
datos_endes$d105b_dummy <- ifelse(datos_endes$d105b %in% c(1, 2), 1, 0)
datos_endes$d105c_dummy <- ifelse(datos_endes$d105c %in% c(1, 2), 1, 0)
datos_endes$d105d_dummy <- ifelse(datos_endes$d105d %in% c(1, 2), 1, 0)
datos_endes$d105e_dummy <- ifelse(datos_endes$d105e %in% c(1, 2), 1, 0)

# Creamos una variable dummy para violencia física en general
datos_endes$physical <- ifelse(rowSums(datos_endes[, c("d105a_dummy", "d105b_dummy", "d105c_dummy", "d105d_dummy", "d105e_dummy")]) > 0, 1, 0)

#Crear una variable dummy por cada variable. La Dummy toma el valor de 1 si la variable de violencia toma los valores 1 y 2, mientras el valor 0, para los valores 0 y 2. Finalmente, crear la variable dummy llamada physical = 1 si alguna de las variables dummies toma el valor de 1, mientras toma el valor de cero si cada variable dummy toma el valor de cero.

# Creamos una variables dummy de violencia física
rech84dv <- rech84dv %>%
  mutate(
    d105a_dummy = ifelse(d105a %in% c(1,2), 1, 0),
    d105b_dummy = ifelse(d105b %in% c(1,2), 1, 0),
    d105c_dummy = ifelse(d105c %in% c(1,2), 1, 0),
    d105d_dummy = ifelse(d105d %in% c(1,2), 1, 0),
    d105e_dummy = ifelse(d105e %in% c(1,2), 1, 0)
  )

# Creammos una variable dummy physical
rech84dv <- rech84dv %>%
  mutate(
    physical = ifelse(d105a_dummy == 1 | d105b_dummy == 1 | d105c_dummy == 1 | d105d_dummy == 1 | d105e_dummy == 1, 1, 0)
  )
