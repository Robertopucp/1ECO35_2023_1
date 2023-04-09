#Tarea 1: R studio - Grupo 6#####

#Limpiamos
rm(list=ls())

#Descargamos librerias
library(magrittr)
library(dplyr)
library(stringr)

##Ejercicio 1#######
#Use paste o paste0 para crear la variable 
#de texto: "Facultad de Ciencias Sociales \ 2023"
c1 <- "Facultad de Ciencias Sociales"
c2 <- "Semestre 2023"

#Paste
a <- paste(c1, c2, sep = " / ") 
print(a)

##Ejercicio 2#######
#Como podriamos resolverlo individualmente
a <- -3.1416              # Asigno un valor a x
b <- abs(3.1416) ; b      # Saco valor absoluto y el ; me imprime y
c <- b^2 ; c          # Saco el cuadrado de y y me sale valor z
w <- log(c) ; w           # Saco el logaritmo de z y me sale valor w
r <- round(w, 2) ; r      # Redondeo el valor de z con 2 decimales y sale r

#Sin embargo, el ejercio nos pide usar el Operador pip %>%
x <- -3.1416 %>% abs()
y <- x^2 # no hay funcion para elevar al cuadrado
z <- y %>% log() %>% round(2)
print (z)

#queremos señalar que el dato que nos sale cuando 
#hallamos el logaritmo es distinto a cuando lo hallamos
#con calculadora.
