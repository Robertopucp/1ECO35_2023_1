
## **Tarea 6** 

## Script en R - Reshape

# Primero debemos establecer nuestro directorio y cargar las bases de datos

library(haven) #como están en formato .dta, se debe usar la librería haven
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

enaho <- read_dta("../../data/Juntos_program/data.dta")
unidos <- read_dta("../../data/Juntos_program/unidos.dta")

# Cuando abrimos la base de datos de la ENAHO, podemos observar que no se está en orden para poder aplicar pivot y pasarla a formato long.
# Por ello, ordenamos las columnas de numpanh

enaho <- enaho[, c(1, 3:11, 2, 12:ncol(enaho))] #para numpanh_15
enaho <- enaho[, c(1, 3:16, 2, 17:ncol(enaho))] #para año 2016
enaho <- enaho[, c(1, 3:21, 2, 22:ncol(enaho))] #para año 2017
enaho <- enaho[, c(1, 3:26, 2, 27:ncol(enaho))] #para año 2018
enaho <- enaho[, c(1, 3:31, 2)] #para año 2019

# Debemos añadir el guion bajo en las columnas numpanh para que el código también las transforme

colnames(enaho)[colnames(enaho) == "numpanh15"] <- "numpanh_15"
colnames(enaho)[colnames(enaho) == "numpanh16"] <- "numpanh_16"
colnames(enaho)[colnames(enaho) == "numpanh17"] <- "numpanh_17"
colnames(enaho)[colnames(enaho) == "numpanh18"] <- "numpanh_18"
colnames(enaho)[colnames(enaho) == "numpanh19"] <- "numpanh_19"

## 1. Transformar la base data.dta a formato long, usando la variable numper como identificador

library(tidyverse)
# Creamos nuestra nueva base long_enaho
long_enaho <- enaho %>%
  pivot_longer(cols = -numper,
               names_to = c(".value", "year"),
               names_pattern = "(\\w+)_(\\d+)",
               values_drop_na = TRUE) 
# Ahora sí, la base está lista para hacer el merge con unidos.

## 2. Formar una base de datos a partir del merge de las dos bases

library(dplyr) # librería necesaria para el merge

merged_bases <- merge(long_enaho, unidos, by = c("ubigeo")) # para este paso, debemos considerar la columna en común, es decir, ubigeo

# Visualizamos la nueva base de datos

print(merged_bases)

## 3. Añadir labels (etiquetas) a todas las variables y labels de valores a las siguientes variables (p4022 y unidos)

# Para p4022 que indica si la persona ha estado enferma en las últimas 4 semanas
merged_bases <- merged_bases %>%
  mutate(p4022 = ifelse(p4022 == 1, "Yes", "No"))

# Para unidos que indica si la persona pertence o no al programa Juntos
merged_bases <- merged_bases %>%
  mutate(unidos = ifelse(unidos == 1, "Yes", "No"))

merged_bases

