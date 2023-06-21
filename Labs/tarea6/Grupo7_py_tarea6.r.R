# clean environment variables
rm(list = ls())
# clean plots
graphics.off()
# clean console
cat("\014")

############################################################
#instalacion de paquetes a usar y cargar el archivo

install.packages("foreign")

library(foreign)

library(haven)

ruta_archivo <- "C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/Juntos_program/data.dta"

data <- haven::read_dta(ruta_archivo)

################################################################
#revisión de los datos y minusculas

View(data)

colnames(data) <- tolower(colnames(data)) 

colnames(data)

##################################################################################################################
#comando reshape y data_long

library(tidyverse)
library(tidyr)
library(dplyr)

data_long <- reshape(data, idvar=c("numper", "numpanh15"), varying = 7:ncol(data), sep = "_", direction = "long")

###################################################################################################################
#cargar archivo data_unidos y merge con data_long

data_unidos <- read_dta("C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/Juntos_program/unidos.dta")

merged_data <- merge(data_long, data_unidos, by = "ubigeo", all = FALSE)

#####################################################################################################################
#Etiquetas

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



