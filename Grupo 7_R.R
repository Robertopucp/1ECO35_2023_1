
#condicionales##

monto_compra <- 500

if (monto_compra > 300) {
  descuento <- monto_compra * 0.1
  monto_compra <- monto_compra - descuento
}

if (monto_compra <= 100) {
  tipo_pago <- "efectivo"
} else if (monto_compra < 300) {
  tipo_pago <- "tarjeta de débito"
} else {
  tipo_pago <- "tarjeta de crédito"
}

cat("Compra de ", monto_compra, "y pago en ", tipo_pago)

################################################################################

fx <- function(x)
{
  if ((x>=0) & (x<100)) {result = x^(1/2)}
  else if ((x>=100)&(x<300)) {result = x-5}
  else {result = 50}
  return(result)
}

fx (9) ##comprobar si el codigo esta bien##

#loops#

utilidades <- c(100, 152, -1 , 8, 12, 156,35, -10, 100, -0.5, 30, 1050 , 7, -10)

for (utilidad_neta in utilidades) {
  if (utilidad_neta < 0) {next}
  if (utilidad_neta > 1000) {break}
  
  cat ("la utilidad neta anual es", utilidad_neta, "millones ", "\n") }

################################################################################

# factorial para el numero n#

factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n-1))
  }
}

factorial(4) #comprobando si el codigo esta bien#
#####################################################################################

# Preparación del espacio ----

## Limpiamos nuestro ambiente de trabajo
rm(list = ls()) # clean environment variables
graphics.off() # clean plots
cat("\014") # clean console

## Llamamos a las librerias vistas en clase
library(stringr)
install.packages("dplyr")
install.packages("readxl")
install.packages("pacman") #es una forma de llamar a varias librerias

# Ejercicio 1 ----

## Creamos la función que recibe el nombre de calculador_factorial y la definimos a través del argumento n
calculador_factorial <- function(n) { 

## Creamos la variable resultado que contendrá el producto iterativo de los numeros que van desde 1 a n 
  resultado <- 1 
  
## Generamos un loop que corra desde 1 hasta n
  for (i in 1:n) { 
    
## En cada iteración, se multiplicara el valor actual de i con el valor actual del resultado de la multiplicacion anterior
    resultado <- resultado * i 
  }
  
## Definimos que el resultado de la función debe ser la nuestra expresion resultado
  return(resultado)
}

## Definimos n, como input para que empiece a correr la función
calculador_factorial(5) 

#Ejercicio 2 ----

##Definimos la función calculadora_imc, cuyos argumentos son el peso y la altura
calculadora_imc <- function(peso, altura) {
  
## Definimos la expresión a ser calculada por la función
  imc <- peso / altura^2

## Indicamos que queremos que el output incluya los datos peso, talla, y el IMC
## La expresión \n nos permite hacer enter y descender a la siguiente línea
  cat("Peso:", peso, "\n","Talla:", altura, "\n", "IMC:", round(imc, 2), "\n")

## Generamos formulaciones if para cada intervalo de nuestra clasificación 
## Tambien se podria utilizar la función print para mostrar los resultados
  if (imc < 18.5) {
    cat("Clasificación: Peso bajo\n")
  } else if (imc < 25) {
    cat("Clasificación: Normal\n")
  } else if (imc < 30) {
    cat("Clasificación: Sobrepeso\n")
  } else if (imc < 35) {
    cat("Clasificación: Obesidad grado I\n")
  } else if (imc < 40) {
    cat("Clasificación: Obesidad grado II\n")
  } else {
    cat("Clasificación: Obesidad grado III\n")
  }
}

## Testeamos nuestra calculadora
calculadora_imc(70, 1.5)
calculadora_imc(85, 1.8)
calculadora_imc(50, 1.6)


#Ejercicio 3 ----

## Verificamos en que lugar de nuestro directorio nos encontramos
getwd()

## Mediante este comando, visualizamos que archivos hay en el lugar donde nos encontramos
list.files()

## Definimos el directorio deseado. 
## Debe cambiar esto para que el código corra 
setwd("/Users/lala/Documents/GitHub/Mariale_repositorio/Curso_Python_R/Workgroups")

## Extraemos el documento donde se encuentran los datos que usaremos de input en la función
## Debe cambiar esto para que el código corra
portfolio <- read.csv("../data/Portafolio.csv", encoding = "UTF-8") #lo de encodign es para que se pueda ver caracteres latinos

## Definimos la función calculadora, donde el argumento es la data extraida del archivo portfolio
calculadora <- function(data){ 

## Indicamos el origen de nuestras variables
  x <- data$X
  y <- data$Y

## Definimos las expresiones que serán calculadas
  coeficiente_de_correlacion <- ((cov(x,y))/(((var(y)^(1/2))*(var(x)^(1/2)))))
  varianza <- (0.2^2)*var(x)+(0.8^2)*var(y)+2*0.2*0.8*cov(x,y)

## Definimos que el resultado de la función sean los dos valores calculados 
   return(list(coeficiente_de_correlacion,varianza))
}

## Testeamos la función, introduciendo el nombre del archivo donde se encuentran los datos
calculadora(portfolio)
