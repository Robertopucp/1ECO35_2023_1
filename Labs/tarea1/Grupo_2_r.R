#Grupo2- Tarea1 ----


#Paste ----

anio<-2023   # se crea la variable para el año 2023
cadena1<-"Facultad de Ciencias Sociales"  # se crea la variable que contenga el texto indicado
imprimir <- paste(cadena1,anio, sep = " / ")  
# con paste se unirá los textos de las variables cadena1 y anio, además se está indicando que
# se separe estos textos con este símbolo " / " 

print(imprimir)  # con print, en la consola se indicará el resultado del paste ejecutado 


#Operador pip ----

numero<--3.1416 %>% abs()  # con "abs", se está hallando el valor absoluto de -3.1416,
# la variable número se define como el resultado de lo anteriormente indicado
numeroCuadrado<-numero^2   # de esta manera se está elevando al cuadrado
numeroEntero<-numeroCuadrado %>% log() %>% as.integer()  # con "log" se está tomando logaritmo y 
# con "as.interger" se convierte a número entero
numeroEntero   # resultado