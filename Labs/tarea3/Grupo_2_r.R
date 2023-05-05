# Tarea 3 ####

# Limpia el environment

rm(list = ls())

# Limpia gráficos
graphics.off()

# Limpia consolas

cat("\014")
#Abrir librerias

library(readxl)
library(openxlsx)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Se selecciona el directorio


## Map, sapply, apply, funcion Lambda ####

# Aplique la siguiente funcion a un vector de 100 numeros aleatorios y a las 
# columnas numericas de la base de datos BDD_compras_consumidores.xlsx
# escalamiento = (x - min(x))/ (max(x)-min(x))
# donde min(x) y max(x) son el minimo y maximo valor del vector asi como de cada
# columna segun corresponda. Por su parte,representa cada numero. 
# Este transformacion de los permite reescalarlos entre valores de 0 a 1. 
# Los modelos de aprendizaje (machine learning) requirieron este tipo de 
# transformacion en los datos.

### Numeros aleatorios ####
x <- runif(100, min=1, max = 100)

# Con "runif" podemos obtener numeros racionales, en este caso hemos elegido que
# el minimo sea desde 1 hasta el n?mero 100. Asimismo, tambien hubiera funcionado
# con "sample" y pudimos haberlo utilizado ("sample" nos da numeros aleatorios
# enteros) 

minimo <- min(x)
maximo <- max(x)

# Como nos pedian los minimos y maximo, hemos asignado a "minimo" y "maximo" los 
# min y max de x

escalamiento <- (x-minimo) / (maximo - minimo)

# Al final hemos puesto la funcion que nos proporcionaron para "escalamiento"

print(x)
print(escalamiento)

# En esta parte hemos usado "print" para poder poner los resultados de los vectores
# aleatorios y de escalamiento


### Base de datos ####


bdd <- read.csv("../../data/BDD_compras_consumidores.csv", header = TRUE,sep = ";")

# Cargamos la base de datos BBD_compras_consumidores, en este caso

cols_numericas <- sapply(bdd, is.numeric)

# Luego usamos sapply para aplicar una funcion a cada columna y que nos retorne
#un vector, y al aplicar el "is.numeric" nos retorna TRUE si es que es un numero
# FALSE en caso contrario.

minimos_bbd <- apply(bdd[, cols_numericas], 2, min)
maximos_bbd <- apply(bdd[, cols_numericas], 2, max)

# Definimos los maximos y mininos de la base de datos. Aqui, usamos apply para
# aplicar a operacion a cada columna de la base de datos, en ese caso, aplicariamos
# min o max a cada columna de la base de datos (por eso ponemos el 2, para indicar que
# se aplica a columnas y no filas).

bdd_esc <- bdd
bdd_esc[, cols_numericas] <- apply(bdd[, cols_numericas], 2, function(x) (x - min(x)) / (max(x) - min(x)))

print(bdd_esc)

#Al final, armamos nuestra ecuacion de bdd_esc y obtendriamos el resultado


## Apply ####

# La base de datos siagie contiene las notas finales de alumnos de nivel 
# secundaria de una institucion publica. Hallar la nota promedio final de cada 
# alumno; en adicion, hallar la nota maxima y minima. 
# Por otro lado, hallar el promedio y mediana de notas de cada curso.


siagie <- read.csv("../../data/siagie.csv")

### Notas promedio final de cada alumno ####

notas_promedio <- apply(siagie[,7:17],1, mean)
siagie <- cbind(siagie, notas_promedio)
siagie$notas_promedio <- round(siagie$notas_promedio, digits = 2)




# En este caso, hemos utilizado el apply para aplicar una funcion. Ademas, hemos
# definido desde que columnas vamos a aplicar toda la funcion y al final la funcion. 
# Asimismo, hemos usado round para poder redondear los numeros a solo dos decimales

### Nota maxima de cada alumno ####

nota_maxima <- apply(siagie[, 7:17], 1, max)
siagie <- cbind(siagie, nota_maxima)

### Nota minima ####

nota_minima <- apply(siagie[, 7:17], 1, min)
siagie <- cbind(siagie, nota_minima)

print(siagie)

### Promedio de cada curso ####

promedio_cursos <- apply(siagie[,7:17],2, mean)
round(promedio_cursos, digits=2)



### Mediana de los cursos ####

notas_mediana <- apply(siagie[,7:17],2, median)
round(notas_mediana, digits=2)
