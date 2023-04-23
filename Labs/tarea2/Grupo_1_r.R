# WORKGROPUP 2 - Grupo 2
# Curso: Laboratorio de R y Python 
# @author: Grupo 1 - Melani Geng, Keyth Hurtado y Fátima Trujillo

# Limpiamos la consola

cat("\014")


# 1. Condicionales--------------------------------------------------------
# * 1.1. Ejercicio 1 ------------------------------------------------------

  # Usamos condicionales para imprimir el valor y el tipo de pago.
  # Se tienen tres tipos de pago: efectivo, tarjeta de crédito y tarjeta de débito. 
  # En cada caso, el tipo de pago dependerá del monto de la compra ($X$), tal que:
  # * Si $X \leqslant 100$, paga en efectivo.
  # * Si $100 <X \leqslant 400$, paga con tarjeta de débito.
  # * Si $400 <X $ , paga con tarjeta de crédito.
  # Además, si la compra es mayor a $300$ soles, se realiza un descuento de $10 \%$

# Definimos X
x <- 350

#Definimos el descuento de 10%
y <- 0.9*x

# Realizamos el condicional
if (x <= 100){
  cat("Compra de",x, "soles y  paga en efectivo")
} else if (x<=300){
  cat("Compra de",x, "soles y  pago con tarjeta de débito")
} else if (x<=400){
  cat("Compra de",x, "soles con descuento de 10%. Total a pagar", y,"y  pago con tarjeta de débito")
} else if (x>400){
  cat("Compra de",x, "soles con descuento de 10%. Total a pagar", y,"y  pago con tarjeta de crédito")
}


# * 2.2. Ejercicio 2------------------------------------------------------

  #Recreamos la función indicada utilizando condicionales.
  
#Definimos x
x_2<- 120

#Definimos f(x)
y_2<- 0

#Realizamos el condicional
if (x_2<100){
  y_2<-x_2^0.5
} else if (x_2<300){
  y_2<-x_2-5
} else if (x_2>=300){
  y_2<-50
}
#Imprimimos F(x)
cat("F(x)=",y_2)


# 2. Loops----------------------------------------------------------------

  # Imprimimos la utilidades netas anuales de varias empresas, medidas en millones con las siguientes condiciones:
  # * Si la utilidad es negativa, se omite el mensaje.
  # * Si la utilidad supera los 1000 millones, se interrumpe el loop.

#Definimos las utilidades netas
u<-c(100,152,-1 , 8, 12, 156,35, -10, 100, -0.5, 30, 1050 , 7, -10)
for (x in u){
  if (x<0){
    next
  } else if(x>1000){
    cat("\n","¡Utilidad superior a los 1000 millones, loop interrumpido!")
    break
  } else {
    cat("\n","La utilidad neta es",x)
  }
}

# 3. Funciones -----------------------------------------------------------

  # Limpiamos las variables del environment

  rm(list = ls())



# * 3.1. Factorial de un número -------------------------------------------
 
   #definimos la función
   factorial <- function(n) 
  {    
  num <- 1 #partimos del primer entero positivo 
    for (i in 1:n) { #listamos todos los enteros positivos hasta n
      num <- num * i #multiplicamos el número actual por el resultado de la multiplicación previa de todos los números
    }
    return(num) #asignamos como resultado la multiplicación final
  }
  
  factorial(4) #ejemplo para el factorial de n = 4
  
# * 3.2. Índice de masa corporal --------------------------------------------
  
    IMC <- function (peso, talla) #definimos la función
   {
    IMC <- peso/(talla**2) #creamos el indicador IMC
    
    riesgo <- "N/D" #partimos de un riesgo desconocido según los datos
    
    #Asignamos riesgos con condicionales, según el IMC
    if (IMC >= 18.5 & IMC < 25) {
      riesgo <- "Promedio"
    
    }  else if ( IMC >= 25 & IMC < 30) {
      riesgo <- "Aumentado"

    }  else if ( IMC >= 30 & IMC < 35) {
      riesgo <- "Moderado"
    
    }  else if ( IMC >= 35 & IMC < 40) {
      riesgo <- "Severo"
      
    } else if ( IMC >= 40) {
    riesgo <- "Muy severo"
  } 
    return(list(text = paste("El peso es", peso),
                text = paste("La talla es", talla),
                text = paste("El índice de masa corporal es",round(IMC,1)), 
                text = paste("El nivel de riesgo es",riesgo)))
    #Generamos el resultado, incluyendo el peso, lla talla, el IMC y el nivel de riesgo
  }
  
  Estudiante_1 <- IMC(70, 1.5); Estudiante_1
  
  Estudiante_2 <- IMC(85, 1.8); Estudiante_2
  
  Estudiante_3 <- IMC(50, 1.6); Estudiante_3

# 3.3. Función aplicado a dos activos financieros ----------------
  
  #Cambiamos nuestro directorio de trabajo
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd()
  
  #Cargamos la base de datos que es un archivo csv
  portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")
  
  #Calculamos el coeficiente de pearson y la varianza del portafolio
  calcular<- function(data){ 
    
    #Extraemos las filas de datos
    X <- data$X 
    Y <- data$Y
    
    #Ponemos los datos destinados a la inversión
    wx <- 0.2
    wy <- 0.8
    
    #La formula para calcular el coeficiente de correlación de Pearson es COV(X,Y)/(var(X)*var(Y))^0.5
    coeficiente_pearson <- (cov(X,Y))/((var(X)*var(Y))^0.5)
    
    #La formula para calcular la varianza del portafolio es wx^2*var(X)+wy^2*var(Y)+2*wx*wy*cox(X,Y)
    varianza <- ((wx^2)*var(X))+((wy^2)*var(Y))+(2*wx*wy*cov(X,Y))
    
    
    return(list(coeficiente_pearson,varianza))
  }
  calcular(portfolio)
