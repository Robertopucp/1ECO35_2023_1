#Grupo2- Tarea1 ----


rm(list = ls()) #Se limpia las variables del environment
graphics.off() #Cierra graficos
cat("\014") #Se limpia la consola

#Se importan las Librerias
library(stringr)
library(dplyr)
library(magrittr)

#Paste ----

anio<-2023   # se crea la variable para el año 2023
cadena1<-"Facultad de Ciencias Sociales"  # se crea la variable que contenga el texto indicado
imprimir <- paste(cadena1,anio, sep = " \\ ")  
# con paste se unirá los textos de las variables cadena1 y anio, además se está indicando que
# se separe estos textos con este símbolo " / " 

cat(imprimir) # con cat, en la consola se indicará el resultado del paste ejecutado. Se usa cat para que aparezca el \

#Operador pip ----

numero<--3.1416 %>% abs()  # con "abs", se está hallando el valor absoluto de -3.1416,
# la variable número se define como el resultado de lo anteriormente indicado
numeroCuadrado<-numero^2   # de esta manera se está elevando al cuadrado
numeroEntero<-numeroCuadrado %>% log() %>% as.integer()  # con "log" se está tomando logaritmo y 
# con "as.interger" se convierte a número entero
numeroEntero   # resultado