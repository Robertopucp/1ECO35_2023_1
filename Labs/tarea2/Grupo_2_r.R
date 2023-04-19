#Script en R----
##Condicionales----
#Primer ejercicio

if (montoCompra>=0){
  descuento<-0 #0 inicialmente solo un caso tendrá descuento y se le asignará el valor si ingresa a la condicional
  if(montoCompra<=100){
    tipoPago<- "en efectivo"
  }else{
    if(montoCompra<=300){
      tipoPago <- "con tarjeta de débito"
    }else{
      tipoPago<- "con tarjeta de crédito"
      descuento<- 0.1*montoCompra
    }
  }
  cat("Compra de ",montoCompra," y pago ",tipoPago," y descuento",descuento) #Se imprime los resultados
}


#Segundo ejercicio
if (x>=0){ #Se plantea la condicional que refleja la funcion matematica
    if (x<=100){
      y <- x^(1/2)
    }else{
      if(x<=300){
        y<-x-5
      }else{
        y<-50
      }
    }
    
}
  
##Loops----

utilidades <-  c(100,152,-1,8,12,156,35,-10,100,-0.5,30,1050,7,-10)
for (monto in utilidades) {
  if(monto>1000) break #Se detiene si supera los 1000 millones
  if (monto<0) next #Se evita imprimir utilidades negativas
  cat("La utilidad neta anual es ",monto,"\n")
}



#Script solo en R----
##Funciones----
###Función que calcula el factorial de n----
#Se genera la función factorial
funcionFactorial <- function(n){
  n_anterior<-n - 1 #Valores iniciales de las variables que se utilizarán
  factorial<-n
  while (n_anterior >= 1){ #Se utiliza un loop para general el factorial del número n
    factorial<-factorial*n_anterior
    if (n_anterior==1) break #Se utiliza un break para evitar que el factorial tome el valor de 0
    n_anterior<-n_anterior-1
  } 
  return(paste0("El factorial de ",n," es ",factorial))
}

funcionFactorial(5) #Se invoca la funciónFactorial
####


###Función de masa corporal----
funcionMasacorporal <- function(peso,talla){
  imc <- peso/(talla^2) #Se halla el IMC
  if (imc<=24.9 & imc>=18.5){
    clasificacion<- "Normal"
  }
  if (imc<=29.9 & imc>=25){
    clasificacion<- "Sobrepeso"
  }
  if (imc<=34.9 & imc>=30){
    clasificacion<- "Obesidad grado I"
  }
  if (imc<=39.9 & imc>=35){
    clasificacion<- "Obesidad grado II"
  }
  if (imc>=40){
    clasificacion<- "Obesidad grado III"
  }
  
  return(paste0("El individuo con talla ",talla," metros y peso ",peso," kilos posee un IMC de ",imc," y su clasificación es de ",clasificacion))
  
}
#Se prueba los resultados
funcionMasacorporal(70,1.5)
funcionMasacorporal(85,1.8)
funcionMasacorporal(50,1.6)

