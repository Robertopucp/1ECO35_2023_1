#TAREA 1------------------------------------------------

#Intalacion de librerias: 
#La instalacion de esta libreria nos permitira hacer el uso del 
#del operador pip %>% 

install.packages("dplyr")
library(dplyr)

##Paste0-------------------------------------------------

#La funcion paste0 permite unir textos (sin espacios)
#En el siguiente codigo se colocan las dos cadenas que se quieren unir entre parentensis.
#Asimismo, es importante mencionar que debido a que el backslah es un caracter especial
#se tiene que colocar dos de estos para que pueda ser leido literalmente, ya que si colocamos 
#un unico backslash no se podra leer.
a<-paste0('Facultad de Ciencias ','Sociales \\ 2023')
#Se usa la funcion writeLines para poder ver la cadena con el backslash como expresion regular
writeLines(a)


##Operador pip %>% (control+shift+M)-------------------- 

#El operador pip permite realizar varias operaciones matematicas sin la necesidad de crear
#muchas variables. Como se observa en el siguiente codigo basta colocar el numero para 
#obtener el resultado en un solo paso:

abs(-3.1416)^2 %>% log() %>% round()
