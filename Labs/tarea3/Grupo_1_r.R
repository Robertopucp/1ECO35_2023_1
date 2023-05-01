#TAREA 3 
#Curso: Laboratorio de R y Python 
#Grupo 1 - Melani Geng, Keyth Hurtado y Fátima Trujillo

#PARTE EN R Y PYTHON
#Map, sapply, apply, función Lambda
#Aplique la siguiente función a un vector de 100 numeros aleatorios y a las columnas numéricas de la base de datos BDD_compras_consumidores.xlsx

#Limpiamos enviroment
rm(list = ls())
#Borramos gráficos
graphics.off()
#Limpiamos consola
cat("\014")

#Llamamos paquetes y definimos directorio
library(pacman) 
p_load(dplyr, readxl, tidyverse, foreign, datos)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Importamos los datos
data <- read.csv("../../data/BDD_compras_consumidores.csv", sep = ";")
View(data)
str(data)

#Creamos el vector
vector <- runif(100, min=0, max=1)
print (vector)

#Generamos el valor mínimo y máximo del vector
min_x <- min(vector)
max_x <- max(vector)

#Aplicamos el escalamiento al vector
escalamiento<- sapply(vector, function(x) (x - min_x/(max_x - min_x)))
view(escalamiento)

#Generamos mínimo y máximo para los datos
minimo <- apply(data[, 3:8], 2, min)
maximo <- apply(data[, 3:8], 2, max)

#Aplicamos escalamiento a los datos
data_escalamiento <- datos[, 3:8] %>% 
  map_dfc(~(. - min(.)) / (max(.) - min(.)))
view(data_escalamiento)

#Apply
#La base de datos siagie.xlsx contiene las notas finales de alumnos de nivel secundaria de una institución pública. Hallar la nota promedio final de cada alumno; en adición, hallar la nota maxima y minima. Por otro lado, hallar el promedio y mediana de notas de cada curso.

#Importamos datos
notas <- read.csv("../../data/siagie.csv")
str(notas)

# Hallamos el promedio de cada alumno 
notas$Promedio <- apply(notas[, 7:17], 1, mean)

# Hallamos nota máxima de cada alumno 
notas$Notamax <- apply(notas[, 7:17], 1, max)

# Hallamos nota mínima de cada alumno
notas$Notamin <- apply(notas[, 7:17], 1, min)

# Hallamos el promedio de cada curso
Promedio_cursos <- apply(notas[, 7:17], 2, mean)
print (Promedio_cursos)

# Hallamos el mediana de cada curso
Mediana_cursos <- apply(notas[, 7:17], 2, median)
print (Mediana_cursos)

