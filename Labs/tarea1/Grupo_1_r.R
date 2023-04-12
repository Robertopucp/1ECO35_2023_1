#GRUPO 1 - TAREA 1

#Limpiamos el enviroment
rm(list = ls())

#Instalamos los paquetes que necesitaremos
install.packages("dplyr")
install.packages("stringr")

library(dplyr)
library(stringr)

##Pregunta 1

##Use paste o paste0 para crear la siguiente variable de texto: "Facultad de Ciencias Sociales \ 2023"
# Creamos el String "Facultad de Ciencias Sociales"
c1 <- "Facultad de Ciencias Sociales" 
#Usamos el comando paste0 para unir c1 con 2023
a <- paste0(c1,' \\',' 2023 ')  
# Imprimos el comando creado
print(a)

##Pregunta 2

##Use el operador %>% para tomar el valor absoluto de -3.1416 , luego elevar al cuadrado, tomar logaritmo y convertir a número entero.
#Creamos una funcion para sacar potencias 
cuadrado <- function(arg1 ) {
  arg1^2
}

#Ejecutamos las operaciones 
{-3.1416 %>% 
    abs() %>% 
    cuadrado() %>% 
    log() %>% 
    as.integer()
}

#El resultado es 2

#Podemos comprobar nuestro resultado de la siguiente forma
x <- abs(-3.1416)
y <- x^2
z <- log(y)
result <- as.integer(z)