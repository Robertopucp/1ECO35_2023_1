# SCRIPT EN R
# Todos los integrantes del grupo trabajaron en este documento :p

#======================PREGUNTA 1==================================

num <- 150   
if(num <= 100){   # se estableció la condición if en caso el número sea menor o igual a 100, de ser así se pagará en efectivo
  pago <- "pago en efectivo"
  precio<-num
  
} else if (num > 100 & num < 300){  # se estableció la condición else if en caso el núm esté entre 100 y 300, de ser así se pagará con tarjeta de débito
  pago<-"pago con tarjeta de débito"
  precio<-num

  
} else {    #en caso no se cumpla ninguna de las condiciones anteriores, se pagará con tarjeta de crédito
  pago<-"pago con tarjeta de crédito"
  precio<-num * 0.9
}

cat("Compra de", precio, "soles y", pago, "\n") #se adicionó al inicio del monto compra de y al final soles

#======================PREGUNTA 2==================================

x <- 200    #x puede tomar distintos valores, en este caso será de 200
F <- ifelse(x <= 100, sqrt(x), ifelse(x <= 300, x - 5, 50))    # se empleó funciones condicionales
cat(paste("El resultado para x =", x, "es", F))

# en este caso, primero si es menor o igual a 100 dará como resultado la elevación al cuadrado con sqrt 
# en un segundo escenario si está entre 100 y 300 que le reste a x 5
# y si no se cumple ninguno de los anteriores caso que x de como resultado 50
# cat imprime el valor tomado de x y el resultado final


#LOOPS------------
#======================PREGUNTA 3==================================

utilidades <- c(100, 152, -1, 8, 12, 156, 35, -10, 100, -0.5, 30, 1050, 7, -10) # se coloca en una lista los valores que tomará la utilidad
for (utilidad in utilidades) {
  if (utilidad < 0) {
    next
  }
  cat("La utilidad neta anual es", utilidad, "\n")
  if (utilidad > 1000) {  

#calculará secuencialmente los valores adoptados por la utilidad, y se detendrá si el valor de la utilidad supera los 100millones
    cat("¡La empresa ha superado los 1000 millones de utilidad!\n")
  }
}
#se coloca utilidad en utilidades para establecer un orden
#cat imprime finalmente la utilidad neta anual


#FUNCIONES-------
#======================PREGUNTA 4==================================
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



#======================PREGUNTA 5==================================
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
  if (imc >= 18.5 & imc < 25){
    resultado$clasificacion <- "Normal"
  } else if (imc >= 25 & imc < 30) {
    resultado$clasificacion <- "Sobrepeso"
  } else if (imc >= 30 & imc < 35) {
    resultado$clasificacion <- "Obesidad grado 1"
  } else if (imc >= 35 & imc < 40) {
    resultado$clasificacion <- "Obesidad grado 2"
    } else {
    resultado$clasificacion <- "Obesidad grado 3"
  }
  
  # Devolvemos el objeto con los resultados
  return(resultado)
  
}

estudiante1 <- calcular_imc(70, 1.5)   # se define la función al estudiante 1
estudiante2 <- calcular_imc(85, 1.8)
estudiante3 <- calcular_imc(50, 1.6)

print(estudiante1)
print(estudiante2)
print(estudiante3)

#======================PREGUNTA 6===========================================
# Se obtuvo el directorio de trabajo actual
getwd()

# Se cargaron los datos del archivo CSV en el directorio de datos
portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")

# Se extrajo los rendimientos de X e Y y combinarlos en una matriz
x <- portfolio$X
y <- portfolio$Y
returns <- as.matrix(cbind(x, y))

# Se calculó la covarianza entre X e Y
cov_xy <- cov(returns)[1, 2]

# Se calculó la varianza de X e Y
var_x <- var(x)
var_y <- var(y)

# Se calculó el coeficiente de Pearson
corr_xy <- cov_xy / (sqrt(var_x) * sqrt(var_y))

# Se calculó la varianza del portafolio
w_x <- 0.2
w_y <- 0.8
var_portfolio <- w_x^2 * var_x + w_y^2 * var_y + 2 * w_x * w_y * cov_xy

# Resultados finales 
cat("Coeficiente de Pearson: ", corr_xy, "\n") 
cat("Varianza del portafolio: ", var_portfolio, "\n")  
