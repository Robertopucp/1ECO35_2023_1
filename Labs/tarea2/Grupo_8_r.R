#TAREA 2------------------------------------------------
#Grupo 8
# Integrantes:
#- Renzo Mosquera (20181960)
#- Yenner Huancahuire (20173340)
#- Pamela Obregón (20173040)

#Intalacion de librerias: 



##Condicionales----------------------------------------

#Generamos un numero aleatorio entre 50 a 400

monto <- sample(50:400,1,replace=F) 
print(monto)

#Creamos el condicional con 'if', 'else if' y 'else'

if (monto <= 100)  {
    cat(" Compra de", monto , "y pago con efectivo.")
  
}   else if (100 < monto & monto < 300) {
    cat(" Compra de", monto , "y pago con tarjeta de débito.")
  
}   else {
    monto1 <- (0.9*monto)
    cat(" Compra de", monto1 , "y pago con tarjeta de crédito.")
} 



#Creamos el sistema de ecuaciones a partir de una funcion

sistema <- function(x){
  
if (0 <= x & x<= 100) {
   F <- x**0.5
   return(F)

}  else if (100 <= x & x<= 300) {
   F = x-5
   return(F)
}  else {
   F <- 50
   return(F)
}
}

#Probamos algunos valores en el sistema de ecuaciones como ejemplo

sistema(x=90)
sistema(x=150)
sistema(x=400)


##Loops-----------------------------------------------

# Creamos el vector que contiene las utilidades

util <-list(100, 152, -1 , 8, 12, 156,35, -10, 100, -0.5, 30, 1050 , 7, -10)
print(util)

# For + continue + break

for (i in util){
  
  if (i <= 0) {
  next
  cat( "La utilidad neta anual es",i,"millones\n")
  
} else if (i >= 1000) {
  break

} else {
  cat("La utilidad neta anual es",i,"millones\n")
}
}

##Funciones-----------------------------------------


#Funcion para calcular el factorial de un numero (n!)

factorial <- function(n) {
  if(n==0) {
    return(1) # ya que el factorial de 0 es 1
  } else{
    return(n*factorial(n-1))
    
  }
}  

#Por ejemplo, hallamos el factorial de 0 y 5 para probar la funcion:

factorial(0)
factorial(5)



#Funcion de masa corporal 

#Creamos una funcion que indica el Indice de masa corporal (peso/talla^2)

IMC <- function(peso, talla) {
  IMC = peso / talla^2
  
#Creamos una estructura condicional en base a la tabla IMC
  
  clasificacion <- "No conocido"
  
  if (IMC >= 18.5 && IMC <= 24.9){
    clasificacion <- "Normal"
  } else if(IMC >= 25 && IMC <= 29.9){
    clasificacion <- "Sobrepeso"
  } else if(IMC >= 30 && IMC <= 34.9){
    clasificacion <- "Obesidad grado I"
  } else if(IMC >= 35 && IMC <=39.9){
    clasificacion <- "Obesidad grado II"
  } else if (IMC >= 40){
    clasificacion <- "Obesidad grado III"
  }
  resultado <- list(peso = peso, talla = talla, IMC = IMC, clasificacion = clasificacion)
  return(resultado)
}

#Pasamos a colocar los datos de los estudiantes propuestos

Estudiante_1 <- IMC(peso = 70, talla = 1.5)
Estudiante_2 <- IMC(peso = 85, talla = 1.8)
Estudiante_3 <- IMC(peso = 50, talla = 1.6)

#Vemos los 3 resultados 

Estudiante_1
Estudiante_2
Estudiante_3



#Funcion aplicado a dos activos financieros

getwd()

# Extraemos el usuario 

user <- Sys.getenv("USERNAME")
print(user) 

# Fijamos el directorio de trabajo

setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2023_1/Labs/tarea2") ) 

# Importamos el archivo csv

portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")
portfolio

#Aplicamos la funcion a una base de datos

calculadora <- function(data){ 
  X <- data$X #Extramos cada columna de datos
  Y <- data$Y 
  w1 <- 0.2   #Fijamos el peso del activo 'X'
  w2 <- 0.8   #Fijamos el peso del activo 'Y'
  
#Formula de Coeficiente de correlacion de Pearson 
#(COV(X,Y)/(var(x)*var(y))^0.5)
  
  pearson <- (cov(X,Y))/((var(X)*var(Y))^0.5)
  
#Determinamos la varianza del portafolio
  
  varianza <- (var(X)*w1^2)+(var(Y)*w2^2)+(2*cov(X,Y)*w1*w2)
  
  return(list(pearson,varianza))
}
calculadora(portfolio)




