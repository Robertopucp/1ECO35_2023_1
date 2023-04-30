#TAREA 3 
#PARTE EN R Y PYTHON
#Map, sapply, apply, función Lambda
#Aplique la siguiente función a un vector de 100 numeros aleatorios y 
#a las columnas numéricas de la base de datos BDD_compras_consumidores.xlsx
#escalamiento=(Xi-min(x))
#donde min(x) y max(x) son el minimo y maximo valor del vector asi como 
#de cada columna según corresponda. Por su parte, representa cada número. 
#Este transformación de los permite reescalarlos entre valores de 0 a 1. 
#Los modelos de aprendizaje (machine learning) requieron este tipo de 
#transformación en los datos.

rm(list = ls())
graphics.off()
cat("\014")
library(pacman) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
 
#Los datos estaban en formato csv
#Hacemos la separación de comas
datos <- read.csv("../../data/BDD_compras_consumidores.csv", sep = ";")
View(datos)
str(datos)
datos$Channel <- factor(datos$Channel)
datos$Region  <- factor(datos$Region)
str(datos)
#creamos el vector
vector <- runif(100)
#generamos el min y el max
minimo <- min(vector)
maximo <- max(vector)
vector_esc <- (vector - minimo) / (maximo - minimo)
View(vector_esc)

#para los datos
minimos <- apply(datos[, 3:8], 2, min)
maximos <- apply(datos[, 3:8], 2, max)
library(purrr)
datos_esc <- datos[, 3:8] %>% 
  map_dfc(~(. - min(.)) / (max(.) - min(.)))
view(datos_esc)
write_csv(datos_esc, "datos_esc.csv", col_names = TRUE)







