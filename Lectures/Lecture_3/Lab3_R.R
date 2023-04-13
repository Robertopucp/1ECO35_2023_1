################  laboratorio 2 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 

# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

#install.packages("stringr")

# Library ####

library(pacman)  # permite llamar a varias librerias de manera simultánea

p_load(dplyr, readxl, rstudioapi, stringr)

# dplyr: para manejo de base de datos
# readxl: lectura de archivo excel, csv
# stringi: manejo de texto
# rstudioapi: se aplicará para definir la ruta de trabajo

getwd()

# Set working directory (directorio de trabajo : ruta en la computadora)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

#-----------------------#
#### Lappy & Sapply #####
#-----------------------#

"Lapply and Sapply"

genero <- c('F', 'M', 'M', 'F', 'M')
sapply(genero, function(x)  {if (x == 'M') 0 else 1})

NSE <- c('A', 'A', 'C', 'B', 'C')
sapply(NSE, function(x)  {if (x == 'A') 3 else if (x == 'B') 2 else 1})


vector <- seq(100)

lapply(vector, function(square) {square^2-mean(vector)}) # resultado en formato lista
sapply(vector, function(square) {square^2-mean(vector)}) # vector canonico simple 

# A cada elemento del vector se eleva al cuadrado y se resta la media del vector

# Forma directa

vector^2-mean(vector)


"Función 1"

lapply(vector, function(x){
  out = x*(1/3) - 0.5*x
  return(out)
} ) # resultado en formato lista


sapply(vector, function(x){
  out = x*(1/3) - 0.5*x
  return(out)
} ) # resultado en formato vector


"Función 2, de estandarización de los elementos de un vector"


lapply(vector, function(i)  ( i -  mean(vector) ) / sd(vector) )


# Asignando el nombre de la función: standarize

standarize <- function(i, mean, sd){
  ( i -  mean ) / sd
}


lapply(vector,standarize,  mean = mean(vector), sd = sd(vector))
sapply(vector,standarize,  mean = mean(vector), sd = sd(vector))


# lapply(x, FUN, ...) ... significa argumentos adicionales para la función


"Función 3"

lapply(vector, function(i){
  if (i < 50){
    
    out = 1
    
  } else {
    
    out = NA
    
  }
  return(out)
  
})


sapply(vector, function(i){
  if (i < 50) out = 1  else  out = NA

  return(out)
}
)
  

# Regular expresion aplicado a un vector de string #

vector_tx <- c("Tiene 9 años","Dice tener 24 ", "35 years",
               "Acabod e cumplir 40", "El año paso tuvo 20")

extract_numb <- function(x){
  
 return(str_extract(x,"[0-9]+") )
  
}

sapply(vector_tx, extract_numb)

lapply(vector_tx, extract_numb)


#-------------------------------#
# Loop replacement BBDD ####
#-------------------------------#

# Matrix #

set.seed(756)

x1 <- rnorm(50) # distribución uniforme entre 0 y 1
x2 <- rnorm(50)
x3 <- rnorm(50)
x4 <- rnorm(50)

X <- cbind(x1,x2,x3,x4)

# matrix(1,500) vector columna de unos (500 observaciones)

apply(X, 2, mean)  # MARGIN == 2 para columnas (columns)
apply(X, 1, mean)  # MARGIN == 1 para filas (rows)

apply(X, 1, sd)  # MARGIN == 1 para filas

apply(X, 2, min)

apply(X, 1, max)


"Estandarizar una matriz"

apply(X, 2, function(i){
  ( i -  mean(i) ) / sd(i)
} )

apply(X, 2, function(i) ( i -  mean(i) ) / sd(i) )


# En este caso mean(i) es la media por columna
# mientras sd(i) es la desviación estandar por columna


cps2012  <- get(load("../../data/cps2012.Rdata")) 

# load R dataset format, extensión Rdata

# Tomamos la varianza de cada columna

apply(cps2012, 2, var) # tomando la varianza por columna (Margin:2)
  
X <- cps2012[ , which(apply(cps2012, 2, var) != 0)] 

# Se exlucye las columnas constantes

demean<- function (x){ x- mean(x)}
X<- apply(X, 2, demean)



#------------------------------------------------------------#
#             Equivalent *args de Python en R (...)

# args permite observar los argumentos de una función que corresponde a una liberia
# (...) argumentos adicionales que usará la función

args(read.csv)
args(lapply)

# aqui ... significa que se admite cualquier tipo de argumento

caso1 <- function(...) {
  return(sum(...))  # suma de estos argumentos desconocidos
}

caso1(2,4,5)

caso1(2,4,5,12,45,3,6,9)


caso2 <- function(...) {
  
  return(prod(...))
  
  
}  

caso2(sample(1:50, size = 5))


# Ejemplo con base de datos

datos <- read.csv("../../data/BDD_compras_consumidores.csv", sep = ";")

str(datos) # muestra el tipo de variable de cada columna

# Convirtiendo a variables categórica

datos$Channel <- factor(datos$Channel)
datos$Region  <- factor(datos$Region)

str(datos)

# Tabla de frecuencia de algunas variables 

table(datos$Channel)
table(datos$Region)


#--------------------------#
#### Apply Base de datos #####
#--------------------------#

# Apply. Aplica una función a un dataframe

# Total de ventas

apply(datos[, 3:8], 1, sum)  # 1 es por filas

datos$Ventas <- apply(datos[, 3:8], 1, sum) # creando nueva columna de ventas

apply(datos[,3:8], 2, sum)    
# 2 es por columnas (total de ventas por tipo de producto)
apply(datos[,3:8], 2, mean)
# 2 es por columnas (promedio de ventas por tipo de producto)



# Con apply(), además de sumar, podemos hacer otras operaciones 
# que vienen cargadas por defecto en R.

getGroupMembers("Summary")

apply(datos[,3:8], 2, min)
apply(datos[,3:8], 2, max)
apply(datos[,3:8], 2, range) # range devuelve el máximo y minimo 


# Aplicando una función

# conversión de las ventas a dólares

datos2 <- apply(datos[, 3:8], 2, function(x) x/3.9) 
datos3 <- cbind(datos[, 1:2], datos2)  # uniendo bases de columa en columnas

#---------------#
#### Tapply ####
#---------------#

# tapply(variable numérica, variable categórica, estadístico)

# tapply operaciones por categorías

tapply(datos$Ventas, datos$Region, mean) 
# promedio ventas por region

tapply(datos$Ventas, datos$Channel, mean) 
# promedio de ventas por canal (tipo comercio)


# Reference -----

# Base de datos 

browseURL("https://archive.ics.uci.edu/ml/datasets/Wholesale+customers")



