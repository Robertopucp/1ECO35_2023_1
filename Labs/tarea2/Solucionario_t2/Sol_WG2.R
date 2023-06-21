#---Solucionario WG2---#

#  -> Comentarios
## -> Pregunta

#....................................................................................#

# Clean environment variables
rm(list = ls())

# Clean plots
graphics.off()

# Clean console

cat("\014") #Ctrl + shift + c


# Libraries

#install.packages("random")
#install.packages("readxl") # excel, csv
library(random)
library(readxl)

#....................................................................................#
## Condicionales ----

### 1. ----

x <- sample(1:500, 1)

if (x <= 100) {
  print(paste("Compra de", x, "soles y pago en efectivo."))
} else if (x < 300) {
  print(paste("Compra de", x, "soles y pago con tarjeta de débito."))
} else {
  x <- 0.9 * x
  print(paste("Compra de", x, "soles y pago con tarjeta de crédito."))
}


### 2.----

aleatorio <- sample(0:500, 1)
  
  if (x <= 100) {
    y <- sqrt(x)  # Si x se encuentra entre 0 y 100, se le aplica la raíz cuadrada.
  } else if (x > 100 & x <= 300) {
    y <- x - 5  # Si x se encuentra entre 100 y 300, se le resta 5.
  } else if (x > 300) {
    y <- 50  # Si x es mayor a 300, y toma el valor de 50.
  }

cat("F(x)=",y)
cat("x=",x)

#....................................................................................#
# Loops ----

utilidades <- c(100, 152, -1, 8, 12, 156, 35, -10, 100, -0.5, 30, 1050, 7, -10)

for (i in 1:length(utilidades)) {
  if (utilidades[i] < 0) {
    next  # Si las utilidades son menores a 0 (negativas), se utiliza next.
  } else {
    if (utilidades[i] > 1000) {
      cat("Loop detenido. Utilidad mayor a 1000 millones.\n")
      break  # Si la utilidad es mayor a 1000, se detiene el loop.
    } else {
      cat(paste("La utilidad neta anual es", utilidades[i], "millones.\n"))
      # Recordemos que usamos "paste" para concatenar dos inputs en un solo string.
    }
  }
}


#....................................................................................#
# Funciones ----

### 1. Función factorial ----

factorial <- function(n) {
  if (n == 0) {
    return(1)  # El factorial de 0 es 1.
  } else if (n < 0) {
    return(NULL)  # Restringimos a los números positivos.
  } else {
    respuesta <- 1
    for (i in 1:n) {
      respuesta <- respuesta * i
    }
    return(respuesta)
  }
}

respuesta <- factorial(13) # Elegimos un número al azar para comprobar nuestra función.
print(respuesta) # Comprobamos que es correcto.


### 2.IMC ---- 

f_imc <- function(peso, talla) {
  imc <- peso / (talla^2)
  clasificacion <- ""
  
  if (imc >= 18.5 & imc < 24.9) {
    clasificacion <- "Normal"
  } else if (imc >= 25 & imc < 29.9) {
    clasificacion <- "Sobrepeso"
  } else if (imc >= 30 & imc < 34.9) {
    clasificacion <- "Obesidad grado I"
  } else if (imc >= 35 & imc < 39.9) {
    clasificacion <- "Obesidad grado II"
  } else if (imc >= 40) {
    clasificacion <- "Obesidad grado III"
  }
  
  outcomes <- list(
    peso = peso,
    talla = talla,
    imc = imc,
    clasificacion = clasificacion
  )
  
  return(outcomes)
}

# Outcomes de los 3 estudiantes:

estudiante1 <- f_imc(peso = 70, talla = 1.5)
estudiante2 <- f_imc(peso = 85, talla = 1.8)
estudiante3 <- f_imc(peso = 50, talla = 1.6)

print(estudiante1)
print(estudiante2)
print(estudiante3)

#....................................................................................#

### 3. Coeficiente Pearson ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

portafolio <- read.csv("../../../data/Portafolio.csv", encoding = "UTF-8") # Encoding : Leer caracteres especiales

rpta <- function(data){ 
  
  X <- data$X 
  Y <- data$Y
  
  wx <- 0.2
  wy <- 0.8
  
  coeficiente_pearson <- (cov(X,Y))/((var(X))^0.5)*((var(Y))^0.5)
  vari <- ((wx^2)*var(X))+((wy^2)*var(Y))+(2*wx*wy*cov(X,Y))
  
  return(list(coeficiente_pearson,vari))
}

rpta(portafolio)

#....................................................................................#
