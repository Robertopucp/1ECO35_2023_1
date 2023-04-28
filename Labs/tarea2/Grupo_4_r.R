
#####################################
#--------Script solo en R ---------#
####################################

## Script en R y Python

# Primero se cargan las librerías
library(readxl)
library(dplyr) 
library(rstudioapi) 

## 1. 

# Definimos el monto de la compra
compra <- 350

# Generamos el condicional del tipo de pago
if (compra <= 100) {
  mensaje <- paste("Compra de", compra, "y pago en efectivo")
} else if (compra > 100 & compra <300){
  mensaje <- paste("Compra de", compra, "y pago con tarjeta de débito")
} else {
  compra = compra*0.9
  mensaje <- paste("Compra de", compra, "y pago con tarjeta de crédito")
  print(mensaje)
}


## 2.

# Definimos el valor de x
x <- 50

# Generamos la estructura condicional
if (x >= 0 & x <= 100) {
  resultado <- sqrt(x)
} else if (x>100 & x <= 300) {
  resultado <- x-5
} else {
  resultado <- 50
}

#imprimirse el resultado
cat("El resultado de F(x) es:", resultado)


## 3. 

# Definimos el vector de utilidades netas anuales
utilidades_na <- c(100, 152, -1, 8, 12, 156, 35, -10, 100, -0.5, 30, 1050, 7, -10)

# Generamos el Loop que cumpla con las condiciones
for (utilidad in utilidades_na) {
  # Continua la iteración si la utilidad es negativa
  if (utilidad < 0) {
    next
  }
  # Si la utilidad supera los 1000 millones, se detiene el loop
  if (utilidad > 1000) {
    break
  }
  # Imprime la utilidad neta anual si se cumple ninguna de las condiciones 
  cat("La utilidad neta anual es", utilidad, "millones")
}



## Script solo en R

## 1. Función para calcular el factorial 
factorial (3)

factorial_x <- function(x)
{

  #calculamos el factorial
  
  return( factorial(x) )
  }

## ejemplo
factorial_x (4)


## 2. Calculamos la función de masa corporal
IMC <- function (peso, talla) {  
  result = peso / (talla**2)
  
if( result >18.5 & result<24.9){
  return(cat("peso:", peso, "kilos","talla:",talla,"metros", "IMC:",result,
             "Clasificación:","Normal"))
  }else if ( result>25 & result<29.9){
  return(cat("peso:", peso, "kilos","talla:",talla,"metros", "IMC:",result,
             "Clasificación:","Sobrepeso"))
  }else if(result>30 & result<34.9){
  return(cat("peso:", peso, "kilos","talla:",talla,"metros", "IMC:",result,
             "Clasificación:","Obesidad grado I"))
  }else if(result>35 & result<39.9){
  return(cat("peso:", peso, "kilos","talla:",talla,"metros", "IMC:",result,
             "Clasificación:","Obesidad grado I"))
  }else if(result<=40){
    return( print("peso:", peso, "kilos","talla:",talla,"metros", "IMC:",result,
                  "Clasificación:","Obesidad grado I"))
  }
  
}

IMC (70, 1.5 )
IMC (85, 1.8 )
IMC (50, 1.6 )


## 3. Función aplicado a dos activos financieros





