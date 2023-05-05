
# TAREA 3

## Limpiamos el área de trabajo
rm(list = ls())
# clean plots
graphics.off()
# clean console
cat("\014")

# EJERCICIO 1 -----

## Crear vector aleatorio de 100 números
x <-  rnorm(100)*100

## Aplicar función de escalamiento
escalamiento <- (x - min(x)) / (max(x) - min(x))
escalamiento

## Ahora, repetimos el proceso con la base de datos BDD_compras_consumidores
rm(list = ls())
datos <- read.csv("C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/BDD_compras_consumidores.csv", sep = ";")
str(datos)

## Indicamos cuales serán las variables
datos$Channel <- factor(datos$Channel)
datos$Region  <- factor(datos$Region)

## Aplicamos dos tipos de expresiones, uno para el máximo y otra para el minimo
minimos <- apply(datos[, 3:8], 2, min)
maximos <- apply(datos[, 3:8], 2, max)

## Convirtiendo la misma base de datos

datos[, 3:8] <- apply(datos[, 3:8], 2, function(x) (x - minimos) / (maximos - minimos))
   
#creando una nueva base de datos
rm(list = ls())
datos <- read.csv("C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/BDD_compras_consumidores.csv", sep = ";")
datos$Channel <- factor(datos$Channel)
datos$Region  <- factor(datos$Region)

minimos <- apply(datos[, 3:8], 2, min)
maximos <- apply(datos[, 3:8], 2, max)

datos2 <- datos
datos2[, 3:8] <- sapply(datos[, 3:8], function(x) (x - minimos) / (maximos - minimos))

# EJERCICIO 2 -----

#descargamos paquetes
rm(list = ls())
install.packages("tidyverse")

library(tidyverse)

# Corremos la base de datos#
library(readr)
siagie <- read_csv("GitHub/1ECO35_2023_1/data/siagie.csv")

## Calculo de promedios, maximos y minimos##

## Convertimos las variables de interes a numericas
siagie[, 7:17] <- lapply(siagie[, 7:17], as.numeric)

# Promedio, min y max por individuo#
siagie$promedio <- rowMeans(siagie[, 7:17], na.rm = TRUE)
siagie$minimo <- apply(siagie[, 7:17], 1, min)
siagie$maximo <- apply(siagie[, 7:17], 1, max)

# Promedio y mediana por curso#
apply(siagie[, 7:17], 2, mean)
apply(siagie[, 7:17], 2, median)