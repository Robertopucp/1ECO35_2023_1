#GRUPO 1 - TAREA 6
#Melani Geng, Keyth Hurtado, Fatima Trujillo

#Instalamos las librerías 

install.packages("stringr")
install.packages("rebus")

library(stringr)
library(rebus)
library(haven)  
library(dplyr) 
library(tidyverse)
library(tidyr)

#Establecemos nuestro directorio y cargamos las bases de datos

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

data <- read_dta("../../data/Juntos_program/data.dta")
unidos <- read_dta("../../data/Juntos_program/unidos.dta")

#Visualizamos los nombres de las columnas del archivo
columnas <- colnames(data)
columnas

columnas_u <- colnames(unidos)
columnas_u

#Renombramos las columnas específicas

data <- data %>%
  rename(numpanh_15 = numpanh15,
         numpanh_16 = numpanh16,  
         numpanh_17 = numpanh17, 
         numpanh_18 = numpanh18, 
         numpanh_19 = numpanh19)  

data

#Visualizamos de nuevo los nombres de las columnas del archivo
columnas <- colnames(data)
columnas

#Creamos una lista con valores únicos de la lista de nombres de columnas
lista <- unique(columnas)
lista

#Sacamos el prefijo común en los nombres de las columnas
prefijo <- unique(sub("_.*", "", columnas[-1]))

#Utilizamos reshape y gather
reshape_data <- data %>%
  gather(key, value, -numper) %>%
  separate(key, into = c("variable", "period"), sep = "_", remove = FALSE) %>%
  filter(variable %in% c("numpanh", "mes", "ubigeo", "dominio", "p400a3", "p4022")) %>%
  select(numper, period, variable, value) %>%
  spread(variable, value)

print(reshape_data)

#Agregamos una columna de año
reshape_data$year <- 2000 + as.numeric(reshape_data$period)
reshape_data

#Usamos merge para unirlo con unidos mediante el ubigeo
merge_datau <- merge(reshape_data, unidos, by = "ubigeo", all = FALSE)
merge_datau

# Asignamos etiquetas a las variables
labels <- c("Número de persona", "Período", "Número de panh", "Mes", "Ubigeo", "Dominio", "P400a3", "P4022")
for (var in names(reshape_data)) {
  attr(reshape_data[[var]], "label") <- labels[var]
}

print(reshape_data)

#Instalamos el paquete sjlabelled 
if (!require(sjlabelled)) {
  install.packages("sjlabelled")
}

#se carga el paquete sjlabelled
library(sjlabelled)

#se añade etiquetas a todas las variables en 'merge_datau'
set_label(merge_datau$numper) <- "Número de persona"
set_label(merge_datau$period) <- "Período"
set_label(merge_datau$numpanh) <- "Número de panh"
set_label(merge_datau$mes) <- "Mes"
set_label(merge_datau$ubigeo) <- "Ubigeo"
set_label(merge_datau$dominio) <- "Dominio"
set_label(merge_datau$p400a3) <- "P400a3"
set_label(merge_datau$p4022) <- "P4022"
set_label(merge_datau$unidos) <- "Etiqueta de 'unidos'"

#se añaden etiquetas de valores a las columnas 'p4022' y 'unidos'
set_labels(merge_datau$p4022, labels = c("Valor1", "Valor2", "Valor3", "Valor4", "Valor5"))
set_labels(merge_datau$unidos, labels = c("Etiqueta1", "Etiqueta2", "Etiqueta3", "Etiqueta4", "Etiqueta5"))

print(merge_datau)

#se añaden etiquetas de valores a las columnas 'p4022' y 'unidos'
merge_datau$p4022 <- factor(merge_datau$p4022, levels = c(1, 2, 3, 4, 5), labels = c("Valor1", "Valor2", "Valor3", "Valor4", "Valor5"))
merge_datau$unidos <- factor(merge_datau$unidos, levels = c(1, 2, 3, 4, 5), labels = c("Etiqueta1", "Etiqueta2", "Etiqueta3", "Etiqueta4", "Etiqueta5"))

print(merge_datau)
