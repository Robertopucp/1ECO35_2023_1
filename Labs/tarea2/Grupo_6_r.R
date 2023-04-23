#  Workgroup 2 ####
# Curso: Laboratorio de R y Python 

##Condicionales------
  
  #1.
compra <- 100
  #utilizamos una serie de declaraciones condicionales if, else if y else 
  #para determinar la forma de pago de la "compra" en función de su valor.
if (compra <= 100) {
  cat("Monto de la compra: S/.", compra, "\n")
  cat("Tipo de pago: Efectivo\n")
} else if (compra > 100 & compra < 300) {
  cat("Monto de la compra: S/.", compra, "\n")
  cat("Tipo de pago: Tarjeta de débito\n")
} else {
  descuento <- compra * 0.1 #incluyendo un descuento en caso de que el monto de la
  #compra sea superior a 300.
  total <- compra - descuento
  cat("Monto de la compra: S/.", compra, "\n")
  cat("Descuento: S/.", descuento, "\n")
  cat("Total a pagar: S/.", total, "\n")
  cat("Tipo de pago: Tarjeta de crédito\n")
}
  # este código imprime información relevante sobre el monto de la compra y el tipo
  #de pago según su valor. Es importante destacar que el código pretende brindar
  #información a modo de ejemplo y debería ser modificado en otra situación.
  
  #2.
x <- 200
  #utilizamos una serie de declaraciones condicionales if, else if y else para 
  #asignar un valor a la variable resultado en función del valor de "x".
if (x >= 0 & x <= 100) {
  resultado <- sqrt(x)
} else if (x > 100 & x <= 300) {
  resultado <- x - 5
} else {
  resultado <- 50 
}

cat("El resultado de la función F(", x, ") es:", resultado)
  #se utiliza la función cat() para imprimir un mensaje que indica el resultado 
  #de la función F(x), donde x es el valor inicial de x y resultado es el valor 
  #asignado durante la evaluación de las condiciones.

##Loops-----

  #3.
utilidades <- c(100, 152, -1, 8, 12, 156, 35, -10, 100, -0.5, 30, 1050, 7, -10)
  #utilizamos for para recorrer los elementos de utilidades.
for (utilidad in utilidades) {
  if (utilidad < 0) {
    next  # ignora las utilidades negativas
  } else if (utilidad > 1000) {
    break  # detiene el loop si la utilidad supera los 1000 millones
  } else {
    cat("La utilidad neta anual es", utilidad, "millones.\n")
  }
}
  #en resumen, el código recorre el vector utilidades e imprime las utilidades 
  #neta anuales mayores a cero y menores o iguales a 1000 millones

# -------------------------------------------------------#

##Funciones-----
 
 #4.
factorial <- function(n) {
  result <- 1 
  for(i in 1:n) { #el bucle for recorre todos los valores desde 1 hasta n, 
                  #multiplicando el resultado parcial por cada uno de ellos.
    result <- result * i
  }
  return(result) #El resultado final se almacena en la variable result y se 
                #devuelve al llamar la función factorial con un número entero.
}
factorial(5)

  #5.
  #Dentro de la función, se utiliza una serie de declaraciones condicionales if, 
  #else if y else para asignar una clasificación en función del IMC calculado
calcular_IMC <- function(peso, talla){
  imc <- peso/(talla^2)
  if(imc >= 18.5 && imc <= 24.9){ 
    clasificacion <- "Normal - Riesgo Promedio" #La clasificación se almacena en la variable clasificación.
  }else if(imc >= 25 && imc <= 29.9){
    clasificacion <- "Sobrepeso - Riesgo Aumentado"
  }else if(imc >= 30 && imc <= 34.9){
    clasificacion <- "Obesidad grado I - Riesgo Moderado"
  }else if(imc >= 35 && imc <= 39.9){
    clasificacion <- "Obesidad grado II - Riesgo Severo"
  }else{
    clasificacion <- "Obesidad grado III - Riesgo Muy Severo"
  }
  resultado <- list(Peso = peso, Talla = talla, IMC = imc, Clasificacion = clasificacion)
  return(resultado) 
}
  #se crea una lista llamada resultado que contiene los valores del peso, talla, IMC 
  #y clasificación calculados, y se retorna esta lista a la llamada de la función

estudiante1 <- calcular_IMC(peso = 70, talla = 1.5)
estudiante2 <- calcular_IMC(peso = 85, talla = 1.8)
estudiante3 <- calcular_IMC(peso = 50, talla = 1.6)
  #se llama a la función calcular_IMC con diferentes valores de peso y talla 
  #para tres estudiantes diferentes y se almacenan los resultados en las variables 
  #estudiante1, estudiante2 y estudiante3
estudiante1
estudiante2
estudiante3

  #6.
getwd()

user <- Sys.getenv("USERNAME")  # Con username extraemo el usuario automaticamente
print(user)
setwd( paste0("C:/Users/",user,"/Documents/GitHub/GITHUB/1ECO35_2023_1/Labs/tarea2") ) # ubicacion del set directorio


portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8") #codigo para leer un archivo csv
portfolio

calculadora <- function(data){ # se aplica la funcion aplicada a una base de datos
  
  X <- data$X  #extraemos cada fila de datos
  Y <- data$Y
  w1 <- 0.2
  w2 <- 0.8
  
  #Creamos la formula de Coef. de correlacion de Pearson: formula --> COV(X,Y)/(var(x)*var(y))^0.5
  
  Coeficiente_de_correlacion <- (cov(X,Y))/((var(X)*var(Y))^0.5)
  varianza <- (var(X)*w1^2)+(var(Y)*w2^2)+(2*cov(X,Y)*w1*w2)
  
  
  return(list(Coeficiente_de_correlacion,varianza))
}
calculadora(portfolio) #Obtenemos el resultado
  