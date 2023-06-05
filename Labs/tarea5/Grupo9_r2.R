#Tarea 6: Regex

# 1.La base de datos metropolitano.xlsx posse información de latitud y longitud de las estaciones del metropiltano y dos lineas de alimentadores. Usar regex para cambiar el formato de las coordenadas a uno de coordenadas geográficos (i.e -11.25, -69.56).
library(readxl)
metropolitano <- read_excel("../../data/metropolitano.xlsx")
View(metropolitano)



# 2.En la carpeta estudiantes, ustedes encontrarán una base de datos llamada base_estudiantes.xlsx (Una base de datos muy sucia). La base de datos contiene información de estudiantes de la educación básica regular. Las variables son las siguientes: nombre, edad, fecha de nacimiento, tipo de administración de la institución educativa, género, correro electrónico, dni del apoderado, madre o padre, y una variable con observaciones.
library(readxl)
base_students <- read_excel("../../data/estudiantes/base_students.xlsx")
View(base_students)

# a)Limpiar el nombre de los estudiantes; es decir, retirar caracteres especiales, número , etc que no permite identificar los nombres adecuadamente.
#Primero, planteamos la función para poder limpiar los nombres:
limpiar_nombre <- function(nombre) {
  nombre_limpio <- gsub("[^[:alpha:]\\s]", "", nombre)
  return(nombre_limpio)
}
#Luego, limpiarmos la base de datos y mostramos los resultados en la base de datos:
base_students$NAME <- limpiar_nombre(base_students$NAME)
print(base_students)


# b)Limpiar la fecha de nacimiento y edad. Asimismo, asignar el formato date a la fecha de nacimiento. Crear una variable con el año de nacimiento.
#Primero, limpiamos la fecha de nacimiento, de la misma forma que el item previo, pero asignándole el formato date y luego lo mostramos en la base de datos.
library(lubridate)
base_students$BORN_DATE <- str_replace_all(base_students$BORN_DATE, "[^0-9-]", "")
print(base_students)

#c) Use la variable GENDER para crear una dummy que tome el valor de 1 para female y 0 para male. 
# Limpiar la variable GENDER y mostrar
base_students$GENDER <- tolower(base_students$GENDER)
base_students$GENDER <- gsub("(.)\\1+", "\\1", base_students$GENDER)
print(base_students)

#Creamos la variable dummy:
base_students$GENDER_DUMMY <- ifelse(base_students$GENDER == "female", 1, 0)
print(base_students)

#d)Creamos la variable MAIL que contiene los usuarios de los correos electrónicos de los estudiantes, a partir de la variable MAIL

base_students$EMAIL <- str_extract(base_students$MAIL, "\\b\\w+")

print(base_students)

# e)Crear una variable asociada al DNI de los padres o apoderado.
#Primero, limpiamos la variable DNI_NUMBER:
if ("DNI_NUMBER" %in% names(base_students)) {
  # Limpiar la variable DNI_NUMBER para que solo contenga números y símbolos
  base_students$DNI_NUMBER <- gsub("[^[:digit:][:punct:]]", "", base_students$DNI_NUMBER)
} else {
  print("La variable DNI_NUMBER no se encuentra en la base de datos.")
}
print(base_students)
#Luego, creamos la variable DNI que contiene únicamente números:
base_students$DNI <- gsub("[^[:digit:]]", "", base_students$DNI_NUMBER)
print(base_students)

