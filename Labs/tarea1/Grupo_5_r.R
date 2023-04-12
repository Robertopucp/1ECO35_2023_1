##Script de R----

### Paste----

a= "Facultad de Ciencias Sociales"  #creamos objeto "a"
c= " \\ "
d= 2023
b= paste0(a,c,d) #Pegamos nuestros objetos
b

#Usamos "witeLines" para que nos salga el texto tal como fue solicitado
writeLines(b)


### Operador pipe %>%-----

# llamamos a la librería
library(dplyr)

e<- (-3.1416) %>% abs() #debido a que el operador pipe solo acepta funciones, operadores aritméticos como la multiplicación, división y potenciación están restringidas. Por ello se opta por dividir en dos partes
f<- e^2 %>% log() %>% round(0)
f
