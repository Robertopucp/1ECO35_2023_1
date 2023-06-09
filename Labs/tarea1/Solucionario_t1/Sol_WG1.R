#--- Solucionario WG1 ---#

#  -> Comentarios
## -> Pregunta

#...................................................................................#

# Clean environment variables
rm(list = ls())

# Clean plots
graphics.off()

# Clean console

cat("\014") #Ctrl + shift + c


# Libraries 

#install.packages("dplyr")
#install.packages("stringr")

library(dplyr) 
library(stringr)

#....................................................................................#

## Paste ----
## Use paste o paste0 para crear la siguiente variable de texto: "Facultad de Ciencias Sociales \ 2023"

# Recordemos que "paste" nos permite unir strings y "paste0" nos permite unir strings sin espacios.

# Creamos el string "Facultad de Ciencias Sociales":
facu <- 'Facultad de Ciencias Sociales'

# Unimos "Facultad de Ciencias Sociales" con el año 2023:
ccss2023 <- paste0(facu,' \\ ','2023')

# Imprimimos el resultado:
cat(ccss2023) 

#....................................................................................#

## Operador pip %>% ----
## Use el operador %>% para tomar el valor absoluto de -3.1416, luego elevar al cuadrado, tomar
## logaritmo y convertir a número entero.

a <- abs(-3.1416) ; a # Valor absoluto
b <- (a)^2 ; b # Elevar al cuadrado
c <- log(b); c # Tomar logaritmo
d <- round(c); d # Convertir a número entero
# Notamos que la respuesta es = 2.


#Utilizando el operador %>%:
-3.1416 %>% abs() %>% `^`(2) %>% log() %>% as.integer()
# Obtenemos la misma respuesta, entonces, sabemos que es correcto.

 
