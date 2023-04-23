#Tarea 2 
#PARTE EN R Y PYTHON

#Condicionales(parte en Rstudio):
#Pregunta 1:
#Se realiza una compra de 150
compra <- 150

# De acuerdo a los datos del ejercicio, debemos plantear condicionales para el tipo de pago de acuerdo al valor de la compra, entonces:
if (compra <= 100) {
  tipo_de_pago <- "pago en efectivo"
} else if (compra > 100 & compra < 300) {
  tipo_de_pago <- "pago con tarjeta de débito"
} else {
  tipo_de_pago <- "pago con tarjeta de crédito"
}

# Ahora bien, se le aplica un descuento del 10% a las compras mayores a 300 soles
if (compra > 300) {
  descuento <- compra * 0.1
  compra_con_descuento <- compra - descuento
  observación <- paste("Compra de", compra, "soles con", tipo_de_pago, "y descuento del 10% por un total de", compra_descuento, "soles")
} else {
  observación <- paste("Compra de", compra, "soles con", tipo_de_pago)
}

# Imprimir la observación de cómo de paga la compra:
print(observación)


#Pregunta 2:
#Para la primera función:
Function_1 <- function(x) {
  if (x >= 0 & x <= 100) {
    return(sqrt(x))
  } else {
    return("El valor de x se encuenta fuera del rango [0, 100]")
  }
}
Function_1 (1)
Function_1 (200)

#Para la segunda función:
Function_2 <- function(x) {
  if (x >= 100 & x <= 300) {
    return(x - 5)
  } else {
    return("El valor de x se encuentra fuera del rango [100, 300]")
  }
}
Function_2(1)
Function_2(200)

#Para la tercera función:
Function_3 <- function(x) {
  if (x >= 300) {
    return(50)
  } else {
    return("El valor de x se encuentra fuera del rango [300,]")
  }
}
Function_3 (300)
Function_3 (200)



#Pregunta3
#loops
#creamos el vector
utilidades_netas_anuales <- c(100, 152, -1 , 8, 12, 156, 35, -10, 100, -0.5, 30, 1050 , 7, -10)
#utilizaremos for
for (utilidad_neta_anual in utilidades_netas_anuales) {
  if (utilidad_neta_anual <  0) {
    next
  } else if (utilidad_neta_anual >  1000) {
    break
  } else {
    cat("La utilidad neta anual es", utilidad_neta_anual,".\n")
  }
}



#PARTE SOLO EN R

#Función para calcular el factorial de un número n!:
#Para crear la función que nos permita calcular el factorial de un número se deben considerar los casos para números negativos, positivos y cero.
#Para ello, usaremos los condicionales:
factorial <- function(n){
  if(n < 0){
    return("Error: no existe factorial de un número negativo")
  }else if(n == 0){
    return(1)
  }else{
    return(n * factorial(n-1))
  }
}

factorial (5)
factorial (100)
factorial (0)
factorial (-20)


#Función de masa corporal:
#Primero, considerando la tabla propuesta de IMC, utilizamos condicionales para clasificar el IMC.
calcular_imc_estudiantes <- function(peso, talla){
  indice <- peso / (talla^2)
  
  if(indice >= 18.5 & indice <= 24.9){
    clasificacion <- "Promedio"
  }else if(indice >= 25 & indice <= 29.9){
    clasificacion <- "Aumentado"
  }else if(indice >= 30 & indice <= 34.9){
    clasificacion <- "Moderado"
  }else if(indice >= 35 & indice <= 39.9){
    clasificacion <- "Severo"
  }else{
    clasificacion <- "Muy severo"
  }
  
  #Lo siguiente es indicar que la función nos debe entregar 4 outcomes: peso, talla, IMC y clasificación según el IMC
  return(list(Peso = peso, Talla = talla, Indice_IMC = indice, Clasificacion = clasificacion))
}

#Ahora, hallamos el IMC para cada estudiante
# Para el estudiante 1:
calcular_imc_estudiantes (70, 1.5)

# Para el estudiante 2:
calcular_imc_estudiantes (85, 1.8)

# Para el estudiante 3:
calcular_imc_estudiantes (50, 1.6)


#Función aplicada a dos activos financieros

#Directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Relative Path
#Cargamos la Base de Datos
portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")

# Hallamos el Coeficiente de Pearson

coeficiente_pearson <- function(data){
  
  x <- data$X
  y <- data$Y
  
  coeficiente_pearson_cal <- (cov(x,y))/(sqrt(var(x))*sqrt(var(y)))
  return(coeficiente_pearson_cal)                 
}

coeficiente_pearson(portfolio)

 #Hallamos la varianza 

varianza <-function(data){
  
  W_x <-0.2
  W_y <-0.8
  x <- data$X
  y <- data$Y
  
varianza_cal <- (W_x^2)*var(x)+(W_y^2)*var(y)+2*W_x*W_y*cov(x,y)
  
  return(varianza_cal)
}
  
varianza(portfolio)





