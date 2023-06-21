# TAREA 6#

# RESHAPE
# Cagamos la libreria y base de datos

install.packages("stringr")
install.packages("rebus")

library(stringr)
library(rebus)
library(haven)  
library(dplyr) 
library(tidyverse)
library(tidyr)


enaho <- read_dta("../../data/Juntos_program/data.dta")
unidos <- read_dta("../../data/Juntos_program/unidos.dta")

# Ordenamos la base de datos ENAHO ya que no se está en orden para poder aplicar pivot y pasarla a formato long.
# Por ello, ordenamos las columnas de numpanh

enaho <- enaho[, c(1, 3:11, 2, 12:ncol(enaho))] #para numpanh_15
enaho <- enaho[, c(1, 3:16, 2, 17:ncol(enaho))] #para año 2016
enaho <- enaho[, c(1, 3:21, 2, 22:ncol(enaho))] #para año 2017
enaho <- enaho[, c(1, 3:26, 2, 27:ncol(enaho))] #para año 2018
enaho <- enaho[, c(1, 3:31, 2)]                 #para año 2019

colnames(enaho)[colnames(enaho) == "numpanh15"] <- "numpanh_15"
colnames(enaho)[colnames(enaho) == "numpanh16"] <- "numpanh_16"
colnames(enaho)[colnames(enaho) == "numpanh17"] <- "numpanh_17"
colnames(enaho)[colnames(enaho) == "numpanh18"] <- "numpanh_18"
colnames(enaho)[colnames(enaho) == "numpanh19"] <- "numpanh_19"

# 1. Transformar la base data.dta a formato long, usando la variable numper como identificador

library(tidyverse)
# Creamos nuestra nueva base long_enaho
long_enaho <- enaho %>%
  pivot_longer(cols = -numper,
               names_to = c(".value", "year"),
               names_pattern = "(\\w+)_(\\d+)",
               values_drop_na = TRUE) 
# Hacemos el marge con unidos

# 2. Formar una base de datos a partir del merge de las dos bases

library(dplyr) # librería necesaria para el merge

merged_bases <- merge(long_enaho, unidos, by = c("ubigeo")) # Consideraramos la columna en común que es ubigeo


print(merged_bases)

# 3. Añadimos etiquetas a todas las variables

# Para p4022 que dice: Were you sick in the last 4 weeks?
merged_bases <- merged_bases %>%
  mutate(p4022 = ifelse(p4022 == 1, "Yes", "No"))

# Para unidos que dice: Programa Juntos applied in the district?
merged_bases <- merged_bases %>%
  mutate(unidos = ifelse(unidos == 1, "Yes", "No"))

merged_bases