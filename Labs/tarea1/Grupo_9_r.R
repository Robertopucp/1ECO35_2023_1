#Tarea en R

install.packages("dplyr")
install.packages("stringr")

library(dplyr) # librarfor cleaning datasets 
library(stringr)

##Pregunta 1

#Use paste o paste0 para crear la siguiente variable de texto: "Facultad de Ciencias Sociales \ 2023"
paste ("Facultad de Ciencias Sociales \ 2023")

##Pregunta 2

#se crea una funcion para sacar potencias 
potencia <- function(arg1 ) {
  arg1^2
}

#se ejecuta las operaciones 
-3.1416 %>% abs() %>% potencia() %>% log() %>% as.integer()
