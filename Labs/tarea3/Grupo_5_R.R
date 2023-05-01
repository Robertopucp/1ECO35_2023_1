
# *Tarea 3* 

library(pacman)  # permite llamar a varias librerias de manera simultánea

p_load(dplyr, readxl, rstudioapi, stringr)

## 1. Map, sapply, apply, función Lambda

### a) Primero creamos un vector ejemplo con el comando seq 

vector <- seq(100)
lapply(vector, function(i)  ( i -  min(vector) ) / (max(vector) - min(vector) ))

### Despúes llamamos a la base de datos

compras  <- read.csv("https://github.com/Robertopucp/1ECO35_2023_1/raw/main/data/BDD_compras_consumidores.csv", sep=";")

### No se considera las variables categóricas, sino las numéricas. además que se coloca el 2 (MARGIN == 2) para referirnos a las columnas.

apply(compras[, 3:8], 2, function(i) {
  ( i -  min(i) ) / (max(i) - min(i))
} )


## 2. Apply

### a) Primero llamamos a la base de datos siagie

siagie <- read.csv("https://github.com/Robertopucp/1ECO35_2023_1/raw/main/data/siagie.csv")
  
### Para hallar la nota promedio de cada alumno usamos apply. Además, seleccionamos de las columnas 7 a 17 que contienen los cursos y el 1 (MARGIN==1) para que se aplique a las filas.

siagie$promedio <- apply(siagie[, 7:17], 1, mean)

print(siagie$promedio)

### Para calcular la nota máxima y mínima de cada estudiante seguimos el mismo proceso, pero solicitando max y min

siagie$maxima <- apply(siagie[, 7:17], 1, max)

print(siagie$maxima)

siagie$minima <- apply(siagie[, 7:17], 1, min)

print(siagie$minima)

### b) Para obtener el promedio de cada curso usamos apply, indicamos las columnas, pero esta vez ponemos 2 (MARGIN ==2) para que se aplique a las columnas

promedios <- apply(siagie[, 7:17], 2, mean)

print(promedios)

### Realizamos lo mismo para obtener la mediana de cada curso e indicamos median

medianas <- apply(siagie[, 7:17], 2, median)

print(medianas)

