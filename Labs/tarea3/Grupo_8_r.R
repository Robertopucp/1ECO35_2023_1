#TAREA 3------------------------------------------------
#Grupo 8
# Integrantes:
#- Renzo Mosquera (20181960)
#- Yenner Huancahuire (20173340)
#- Pamela Obregón (20173040)

install.packages("dplyr")
install.packages("readxl")
install.packages("pacman")

# clean console

cat("\014")

install.packages("stringr")

# Library ####

library(pacman)  # permite llamar a varias librerias de manera simultánea
p_load(dplyr, readxl, rstudioapi)


# dplyr: para manejo de base de datos
# readxl: lectura de archivo excel, csv
# stringi: manejo de texto
# rstudioapi: se aplicará para definir la ruta de trabajo

getwd()

# Set working directory (directorio de trabajo : ruta en la computadora)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

#------------------------------------#
##Pregunta 1: Sapply, Lapply --------
#------------------------------------#

#Parte 1

# Creamos un vector que contiene 100 numeros aleatorios

vector <-c(sample(1:1000,100,replace=F) )
vector
#Creamos una funcion de escalonamiento

escalonamiento<-function(x,minimo,maximo) {
  (x-minimo)/(maximo-minimo)
}
#Aplicamos la funcion a dicho vector creado 
#Obtenemos el resultado en formato lista y en formato vector canonico simple

lapply(vector, escalonamiento, minimo=min(vector), maximo=max(vector))
sapply(vector, escalonamiento, minimo=min(vector), maximo=max(vector))



#Parte 2

#Extraemos la base de datos

datos <- read.csv("../../data/BDD_compras_consumidores.csv", sep = ";")

#Aplicamos la funcion creada, anteriormente,a cada columna numerica de la base de datos


for(x in 1:8) {
   n<-c(names(datos))
   v<-sapply(datos[,x], escalonamiento, minimo=min(datos[,x]), maximo=max(datos[,x])) 
   print(n[x])
   print(v)
}


#------------------------------------#
##Pregunta 2: Apply ------------------
#------------------------------------#

#Extraemos la base de datos

datos1 <- read.csv("../../data/siagie.csv", sep = ",")

#Hallamos el promedio de cada alumno(a)
#Debido a que aplicaremos una funcion a cada fila colocamos 1

apply(datos1[,7:17], 1, mean)

#Hallamos la nota maxima de cada alumno(a)

apply(datos1[,7:17], 1, max)

#Hallamos la nota minima de cada alumno(a)

apply(datos1[,7:17], 1, min)

#Hallamos el promedio de cada curso
#Debido a que aplicaremos una funcion a cada columna colocamos 2

apply(datos1[,7:17], 2, mean)

#Hallamos la mediana de cada curso

apply(datos1[,7:17], 2, median)



