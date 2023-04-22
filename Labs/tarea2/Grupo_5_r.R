#Tarea 2
##Parte R

# 1) Condicionales ####

###Pregunta 1 ####

compra <- 4000 #se realizara una condicional con tres alternativas evitando el else, ya que se entiende que el pago es un número positivo

if (compra < 100) {
  
  print(paste0('compra de ',compra,' soles y pago en efectivo'))
} else if (100 < compra & compra < 300) {
  
  print(paste0('compra de ',compra,' soles y pago en debito'))
} else if (compra > 300) {
  
  print(paste0('compra de ',compra,' soles y pago en credito'))
}
#en R para agregar un intervalo se usa & 

###Pregunta 2####
x = 400 
if (0 < x & x < 100) { #se escribe una funcion cerrada entre 0 y 100
  Fx = x^(1/2)
  print(paste0("la funcion F(x) es ",Fx))
} else if (100 < x & x < 300) {
  Fx = x-5
  print(paste0("la funcion F(x) es ",Fx))
} else if (x >= 300) {
  Fx = 50
  print(paste0("la funcion F(x) es ",Fx))
} else { #se agrega el else, ya que se le puede dar valor de negativos a Fx
  print ("0, X es un numero negativo")
}    
# 2) Loops en R ####

utilidades_anuales <- c(100, 152, -1 , 8, 12, 156, 35, -10, 100, -0.5, 30, 1050, 7, -10)

for (utilidad in utilidades_anuales) {
  if (utilidad < 0) { #indicamos que la utilidad no puede ser menor a 0
    next  # para saltar los valores negativos
  } else if (utilidad > 1000) {
    break  # romper el loop cuando la utilidad es mayor a 1000 millones
  } else {
    print(paste("La utilidad neta anual es", utilidad, "millones"))
  }}
#Según lo solicitado, el loop debe detenerse si la utilidad supera los 1000 millones, por lo que se realiza el break despues de "if utilidad > 1000 " y el loop ya no considera los siguientes valores. Por ello, el 7, a pesar de que no sea una utilidad negativa no es considerada por el loop.


# 3) Funciones ####

###Función para calcular el factorial ####
###En el ejemplo mostrado a continuación, creamos la función para resolver el factorial del múmero 5, que es 120. 

num=5
fact= 1
if (num < 0) {
  print("Factorial para números negativos no está permitido")
} else if (num == 0) {
  print("El factorial de 0 es 1")
} else {
  for(i in 1:num){
    fact=fact*i
  }
  print(fact)
}



###Función de masa corporal####
###La siguiente función entre 4 outputs: el dato de peso, el dato de talla, el indice corporal y clasificación.

calculator <- function(x,y)
{
  result = x/y^2
  
  if (result <= 18.4) {return( cat("peso:", x,"talla:",y, "índice corporal:", result,"introducir valores nuevamente"))} 
  else if(result <=24.9){return( cat("peso:", x,"talla:",y, "índice corporal:", result,"la clasificación es normal"))} 
  else if(result <= 29.9) {return( cat("peso:", x,"talla:",y, "índice corporal:", result,"la clasificación es sobrepeso"))} 
  else if(result <= 34.9) {return( cat("peso:", x,"talla:",y, "índice corporal:", result,"la clasificación es obesidad grado 1"))}  
  else if(result <= 39.9) {return( cat("peso:", x,"talla:",y, "índice corporal:", result,"la clasificación es obesidad grado 2"))} 
  else if(result <= 40) {return( cat("peso:", x,"talla:",y, "índice corporal:", result,"la clasificación es obresidad grado 3"))} 
}
calculator(70,1.5)
calculator(85,1.8)
calculator(50,1.6)


###Función aplicado a dos activos financieros####

###Coeficiente de correlación  

library(readr)
Portafolio <- read_csv("~/GitHub/1ECO35_2023_1/data/Portafolio.csv")
alpha <- function(data){
  X <- data$X
  y <- data$Y
  coeficiente= cov(X,y)/sqrt(var(X))*sqrt(var(y))
  return(coeficiente)   
}
alpha(Portafolio)


###Varianza portafolio

wx=0.2
wy=0.8
alpha2 <- function(data){
  X <- data$X
  y <- data$Y
  varianza= wx^2*var(X)+wy^2*var(y)+2*wx*wy*cov(X,y)
  return(varianza)
}
alpha2(Portafolio)

