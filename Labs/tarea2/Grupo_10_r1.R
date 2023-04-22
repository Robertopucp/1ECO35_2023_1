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
    cat("Compra de", x[[i]], "soles y pago en efectivo ")
  }
  else if (x[[i]] <= 300) {
  cat("Compra de", x[[i]], "soles y pago con tarjeta de débito ")
    }
  else if (300 < x[[i]]) {
  cat("Compra de", x[[i]], "soles y pago con tarjeta de crédito ")
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
  cat("La utilidad neta anual es", t[[i]], "millones ")
}

#Se creo la lista t, para poder colocar todos los valores
#de las utilidades mencionadas en el ejercicio. Luego
#se tomó elemento por elemento para ser evaluado y analizar
#si cumplían con las condiciones solicitadas. Las utilidades
#negativas no se imprimen en el loop y, finalmente, el loop
#se detiene si la utilidad supera los 1000 millones
