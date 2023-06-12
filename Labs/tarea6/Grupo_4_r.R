

# Grupo_4_r 
# Integrantes: Mishell Delgado, Lisbeth Ccoyo y Steven Atoche

rm(list = ls())

# Llamamos las librer�as 

library(readxl)
library(dplyr) 
library(rstudioapi)  
library(readr)
library(haven) 
library(pacman) 
library(stringr)
library(lubridate)
library(foreign)


# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

############################################################

# Leemos el archivo data.dta

ruta_archivo <- "C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/Juntos_program/data.dta"

# Revisamos las columnas

data <- haven::read_dta(ruta_archivo)
colnames(data) <- tolower(colnames(data)) 

colnames(data)
library(tidyr)
library(dplyr)

data_long <- reshape(data, idvar=c("numper", "numpanh15"), varying = 7:ncol(data), sep = "_", direction = "long")
library(dplyr)
library(tidyverse)
library(foreign)

# Leemos el archivo unidos.dta

data_unidos <- read_dta("C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/Juntos_program/unidos.dta")

# Realizamos el merged

merged_data <- merge(data_long, data_unidos, by = "ubigeo")

# Colocamos etiquetas a las variables p4022 y unidos

if (!require(sjlabelled)) {
  install.packages("sjlabelled")
}

library(sjlabelled)

set_label(merged_data$ubigeo) <- "Ubigeo"
set_label(merged_data$numper) <- "Número de la persona"
set_label(merged_data$time) <- "Año"
set_label(merged_data$mes) <- "Mes"
set_label(merged_data$dominio) <- "Dominio"
set_label(merged_data$p400a3) <- "P400a3"
set_label(merged_data$p4022) <- "P4022"
set_label(merged_data$distrito) <- "Distrito"
set_label(merged_data$provincia) <- "Provincia"
set_label(merged_data$region) <- "Región"
set_label(merged_data$unidos) <- "Unidos"