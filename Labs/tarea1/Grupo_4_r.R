#--------------------------#
#--------Tarea1------------#
## Integrantes: Mishell Delgado, Lisbeth Ccoyo y Steven Atoche

# Paste
b1 <- "Facultad de Ciencias Sociales"
b2 <- '\ 2023'

d <- paste0(b1, b2)

#ver el resultado
print(d)

# Activando las librerías
library(dplyr)  
library(stringr)

## operator pip %>% 
# valor absoluto
pi <--3.14 %>% abs() 
# se eleva al cuadrado
pialcua <-- pi^2     
# con round aproximamos al entero más cercano
pientero <--pialcua  %>% log() %>% round() 
# ver el resultado
pientero 


