# Integrantes ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)

# Consideraciones previas: #####
## Borrando el environment ####
rm(list = ls())

## Borrando los graficos ####
graphics.off()

##Borrando la consola ####
cat("\014")

## Llamando a los directorios necesarios
library(dplyr)
library(stringr)

# Desarrollo de las preguntas ####

## Pregunta 1: Condicional 1 ####
pago <- 400

if (pago < 100) {
  cat ("Compra de", pago, "soles y pago en efectivo")
} else if ((pago>100) & (pago<300)) {
  cat ("Compra de", pago, "soles y pago con tarjeta de d?bito")
} else {
  cat ("Compra de", (pago*0.9), "soles y pago con tarjeta de cr?dito")
}
  

## Pregunta 2: Condicional 2 ####

fx1 <- function(x)
{
  if ((x>0) & (x<100)) {
    result = x^(1/2)
  } else if ((x>100) & (x<300)) {
    result = x-5
  } else {
    result = 50
  }
  return(result)
}


fx1( 400 )
## Pregunta 3: Loops ####

# Vector con las utilidades netas anuales
utilidades <- c(100, 152, -1 , 8, 12, 156,35, -10, 100, -0.5, 30, 1050 , 7, -10)

# Creamos el bucle for
for (uneta in utilidades) {
  # Cuando la utilidad es negativa, saltar a la siguiente iteración
  if (uneta < 0) {
    next
  }
  # Cuando la utilidad es mayor que 1000, salir del bucle
  if (uneta > 1000) {
    break
  }
  # Mensaje con la utilidad neta anual
  mensaje <- paste("La utilidad neta anual es", uneta, "millones")
  print(mensaje)
}

## Pregunta 4: Funci?n para calcular el factorial de un n?mero n! #### 

factorial <- function(n) {
  if(n == 0) {
    return(1) # El factorial de 0 es 1
  } else {
    return(n * factorial(n-1))
  }
}
#a modo de ejemplo probamos el factorial de 7:
factorial (7)

## Pregunta 5: Funcion de masa corporal #### 


## Pregunta 3: Funcion aplicando activos financieros #### 

