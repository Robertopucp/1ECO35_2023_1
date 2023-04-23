
#CONDICIONALES------------
#####PREGUNTA 1--------

num <- 150
if(num <= 100){
  pago <- "pago en efectivo"
  precio<-num
#se estableció la condición if en caso el número sea menor igual a 100
#en caso sea así se pagará en efectivo
  
} else if (num > 100 & num < 300){
  pago<-"pago con tarjeta de débito"
  precio<-num
#se estableció la condición else if en caso el número esté entre 100 y 300
#en caso sea así se pagará con tarjeta de débito
  
} else {
  pago<-"pago con tarjeta de crédito"
  precio<-num * 0.9
}
#en caso no se cumple ninguna de las condiciones anteriores se usa else
#en caso sea así se aplicará un descuento del 10% 
#ademas se pagará con tarjeta de crédito
cat("Compra de", precio, "soles y", pago, "\n")
#en R para mostrar el resultado se usa cat
#se adicionó al inicio del monto compra de y al final soles y 
#se agrega un salto de línea al final de la cadena con "\n"




#####PREGUNTA 2------

x <- 200
F <- ifelse(x <= 100, sqrt(x), ifelse(x <= 300, x - 5, 50))
cat(paste("El resultado para x =", x, "es", F))
#se utilizó un short ifelse en donde se asumió un valor de 200
#la condición ifelse permite hacer varias condicionales
#en este caso una primera menor igual a 100 que da como resultado la elevación al cuadrado con sqrt 
#en un segundo escenario si está entre 100 y 300 que le reste a x 5
#y si no se cumple ninguno de los anteriores caso que x de como resultado 50
#cat imprime el valor tomado de x y el resultado final


#LOOPS------------
#####PREGUNTA 3-------
utilidades <- c(100, 152, -1, 8, 12, 156, 35, -10, 100, -0.5, 30, 1050, 7, -10)
# se coloca en una lista los valores que tomará la utilidad
for (utilidad in utilidades) {
  if (utilidad < 0) {
    next
  }
  cat("La utilidad neta anual es", utilidad, "\n")
  if (utilidad > 1000) {  
#calculará secuencialmente los valores adoptados por la utilidad
#y se detendrá si el valor de la utilidad supera los 100millones
    cat("¡La empresa ha superado los 1000 millones de utilidad!\n")
  }
}
#se coloca utilidad en utilidades para establecer un orden
#cat imprime finalmente la utilidad neta anual


#FUNCIONES-------
#####PREGUNTA 4--------
factorial <- function(n) {
#con factorial se toma un argumento n
  if (n < 0) {
    stop("El factorial no está definido para números negativos")
#stop excluye a los negativos
  } else if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}
factorial(5)



#####PREGUNTA 5--------
calcular_imc <- function(peso, talla) {
  imc <- peso / (talla^2)
  if (imc < 18.5) {
    clasificacion <- "Bajo peso"
  } else if (imc < 25) {
    clasificacion <- "Normal"
  } else if (imc < 30) {
    clasificacion <- "Sobrepeso"
  } else {
    clasificacion <- "Obesidad"
  }
  resultado <- list(imc = imc, clasificacion = clasificacion)
  return(resultado)
}

calcular_imc(70, 1.75)







calcular_imc <- function(peso, talla) {
  imc <- peso / (talla^2)
  if (imc < 18.5) {
    clasificacion <- "Bajo peso"
  } else if (imc < 25) {
    clasificacion <- "Normal"
  } else if (imc < 30) {
    clasificacion <- "Sobrepeso"
  } else {
    clasificacion <- "Obesidad"
  }
  resultado <- list(imc = imc, clasificacion = clasificacion)
  return(resultado)
}

calcular_imc(70, 1.75)





calcular_imc <- function(peso, talla) {
  
  # Calculamos el IMC
  imc <- peso / talla^2
  
  # Creamos un objeto con los resultados
  resultado <- list(
    peso = peso,
    talla = talla,
    imc = imc,
    clasificacion = NA
  )
  
  # Asignamos la clasificación según la tabla de IMC
  if (imc < 18.5) {
    resultado$clasificacion <- "Bajo peso"
  } else if (imc >= 18.5 & imc < 25) {
    resultado$clasificacion <- "Normal"
  } else if (imc >= 25 & imc < 30) {
    resultado$clasificacion <- "Sobrepeso"
  } else {
    resultado$clasificacion <- "Obesidad"
  }
  
  # Devolvemos el objeto con los resultados
  return(resultado)
  
}

calcular_imc(70,1.5)