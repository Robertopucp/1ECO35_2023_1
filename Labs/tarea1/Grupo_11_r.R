# Integrantes ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# María Alejandra Colán (20190515)

# Consideraciones previas: #####
## Borrando el environment ####
rm(list = ls())

## Borrando los gráficos ####
graphics.off()

##Borrando la consola ####
cat("\014")

## Llamando a los directorios necesarios
library(dplyr)
library(stringr)

# Desarrollo de las preguntas ####

## Pregunta 1: Paste #### 
# Use paste o paste0 para crear la siguiente variable de texto: "Facultad de Ciencias Sociales \ 2023"

c1 <- "Facultad de ciencias sociales" # Creamos el String "Facultad de ciencias sociales"

a <- paste0(c1,' 2023 ') #Usamos el comando paste0 para unir c1 con 2023 

print(a) # Imprimos el comando creado


## Pregunta 2: Operador pip %>% ####
##Use el operador %>% para tomar el valor absoluto de -3.1416 , luego elevar al cuadrado, tomar logaritmo y convertir a número entero.


# El equipo intentó utilizar el signo de potencia "^"; sin embargo, este no funciona porque requiere que sea precedido por un número
#    -3.1416 %>% abs() %>% (^)(2) %>% log() %>% as.integer()
# 1) PRIMERA FORMA ---------------------
#Por ello se optó por dividir el código en 2 partes y se pueda ejecutar con la potencia.

A<- -3.1416 %>% abs()
A^2 %>% log() %>% as.integer()

# SEGUNDA FORMA ---------- para comprobar el resultado
x <- abs(-3.1416) ; x
y <- x^2 ; y
z <- log(y) ; z
w <- as.integer(z) ; w
