#############################################
##                 Tarea 3             ##
#############################################

## Script en R 

rm(list = ls())

# Primero se cargan las librerias
library(readxl)
library(dplyr) 
library(rstudioapi)  
library(readr)

#1.

# Leer el archivo CSV
mi_dataframe <- read_csv("BDD_compras_consumidores.csv")

# Escalar un vector de 100 números aleatorios
vector_aleatorio <- runif(100)
vector_escalamiento <- (vector_aleatorio - min(vector_aleatorio)) / (max(vector_aleatorio) - min(vector_aleatorio))

# Escalar todas las columnas numéricas de la base de datos
mi_dataframe_escalamiento <- as.data.frame(lapply(mi_dataframe, function(x) if(is.numeric(x)) (x - min(x)) / (max(x) - min(x)) else x))

# Ver los primeros registros de la base de datos escalada
head(mi_dataframe_escalamiento)


#2.  
siagie <- read.csv("siagie.csv")

library(dplyr)


#Calcular la nota promedio final de cada alumno
siagie_promedio <- siagie %>% 
  mutate(promedio_final = rowMeans(.[7:ncol(.)], na.rm = TRUE))

#hallar la nota maxima y minima

siagie_max <- siagie %>% 
  mutate(nota_max = apply(.[7:ncol(siagie)], 1, max, na.rm = TRUE))

siagie_min <- siagie %>% 
  mutate(nota_min = apply(.[7:ncol(siagie)], 1, min, na.rm = TRUE))

#hallar el promedio y mediana de notas de cada curso.

install.packages("tidyr")
library(tidyr)
siagie_promedio_curso <- siagie %>%
  select(7:ncol(.)) %>%
  pivot_longer(everything(), names_to = "curso", values_to = "nota") %>%
  group_by(curso) %>%
  summarize(promedio = mean(nota, na.rm = TRUE), mediana = median(nota, na.rm = TRUE))

