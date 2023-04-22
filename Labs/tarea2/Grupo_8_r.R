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

#Probamos algunos valores en el sistema de ecuaciones
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







