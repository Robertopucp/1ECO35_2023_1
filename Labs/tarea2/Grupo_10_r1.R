cat("\014")

library(readxl)
library(dplyr) 
library(rstudioapi)

library(pacman) 
p_load(dplyr, readxl, rstudioapi)

# En los siguientes códigos se utilizarán dos listas (x e y) 
# elaboradas con números al azar dentro de los marcos. Esto con 
# la intención de que el condicional permita ofrecer resultados 
# distintos dependiendo del valor que se tome.



#Ejercicio 1: Use un condicional que imprima el valor
#de compra y el tipo de pago

x <- list(50, 70, 120, 250, 350, 420)
for (i in 1:6) {
  if (x[[i]] <= 100) {
    cat("Compra de", x[[i]], "soles y pago en efectivo","\n" )
  }
  else if (x[[i]] <= 300) {
  cat("Compra de", x[[i]], "soles y pago con tarjeta de débito ", "\n")
    }
  else if (300 < x[[i]]) {
  cat("Compra de", x[[i]], "soles y pago con tarjeta de crédito ","\n" )
    }
}

#Este código imprime el valor de la compra en soles y 
#el medio de pago, sea efectivo, tarjeta de débito o tarjeta
#de crédito. Claramente, dependiendo del rango en que se
#encuentre el valor de la compra, menor a 100, mayor a 100 o
#mayor a 300.

#Ejercicio 2: Elaborar una estructura condicional 

y <- list(20, 70, 100, 180, 250, 300, 420, 500)
for (i in 1:8) {
  if (y[[i]] <= 100) {
   print(sqrt(y[[i]]))
  }
  else if (y[[i]] <= 300) {
   print(y[[i]] - 5)
  }
  else if (y[[i]] > 300) {
    print(50)
  }
} 

#Este código permite realizar distintas operaciones a los
#valores de la lista (tomados al azar) dependiendo del
#rango al que pertenezcan, es decir, [0,100] <100,300] y <300

#Ejercicio 3: Loops

t <- list(100, 152, -1, 8, 12, 156.35, -10, 100, -0.5, 30, 1050, 7, -10)
for (i in 1:13) {
  if (t[[i]] < 0) {
    next
    
  }
  if (t[[i]] > 1000) {
    break
  }
  cat("La utilidad neta anual es", t[[i]], "millones ", "\n")
}

#Se creo la lista t, para poder colocar todos los valores
#de las utilidades mencionadas en el ejercicio. Luego
#se tomó elemento por elemento para ser evaluado y analizar
#si cumplían con las condiciones solicitadas. Las utilidades
#negativas no se imprimen en el loop y, finalmente, el loop
#se detiene si la utilidad supera los 1000 millones




#segunda parte



#Funcion para calcular un factorial 

factorial <- function(n) {
  if (n < 0) {
    stop("No se puede calcular el factorial de un número negativo.")
  }
  if (n == 0) {
    return(1)
  }
  fact <- 1
  for (i in 1:n) {
    fact <- fact * i
  }
  return(fact)
}


# Funcion para calcular la masa 
masa_corporal <- function(p,t,e) {
  if ( 18.5 < p/t^2 & p/t^2 < 24.9 ) {
    c <- "Normal"
  }
  if ( 25 < p/t^2 & p/t^2 < 29.9 ) {
    c <- "Aumentado"
  }
  if ( 30 < p/t^2 & p/t^2 < 34.9 ) {
    c <- "Moderado"
  }
  if ( 35 < p/t^2 & p/t^2 < 39.9 ) {
    c <- "Severo"
  }
  if ( 40 < p/t^2 ) {
    c <- "Muy Severo"
  }
  paste("El estudiante",e, "tiene un peso de ", p, "y una talla de", t, "por lo tanto su indice es de ",round(p/t^2,2), "y su clasificacion es",c)
}

b<-masa_corporal(70,1.5,1)
b

#Calcular el coeficiente de correlacion y la varianza 

#librerias necesarias 
library(readxl)
library(stats)

# extraccion y orden de datos
datos <- read.csv("G:/Mi unidad/1ECO35_2023_1/data/Portafolio.csv")

datos_x <- data.frame(datos$X)
datos_x

datos_y <- data.frame(datos$Y)
datos_y

#calculo de los valores 

corr <- round(cor(datos_x, datos_y), 2)

Var <- 0.2**2*var(datos_x) + 0.8**2*var(datos_y) + 2*0.2*0.8*cov(datos_x, datos_y)






