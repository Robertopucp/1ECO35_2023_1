

# Grupo_4_r 

rm(list = ls())

library(readxl)
library(dplyr) 
library(rstudioapi)  
library(readr)
library(haven) 
library(pacman) 
library(stringr)
library(lubridate)

# 1)

#cargar datos 
data <- data.frame(read_excel("C:/Users/Lisbeth/Documents/Tarea de R/metropolitano.xlsx"))


# Limpiando la base de datos

# Remover espacios en blanco de las columnas sur_latitud y oeste_longitud
data$sur_latitud <- gsub("\\s", "", data$sur_latitud)
data$oeste_longitud <- gsub("\\s", "", data$oeste_longitud)


# Función para convertir el formato de coordenadas
convert_coordinates <- function(coord) {
  # Extraer los grados, minutos, segundos y dirección de la coordenada
  regex <- "([0-9]+)°([0-9]+)'([0-9.]+)\"([NSEWO])"
  matches <- str_match(coord, regex)
  
  # Verificar si se encontró una coincidencia
  if (!is.na(matches[1, 1])) {
    # Obtener los grupos de la coincidencia
    degrees <- as.numeric(matches[1, 2])
    minutes <- as.numeric(matches[1, 3])
    seconds <- as.numeric(matches[1, 4])
    direction <- matches[1, 5]
    
    # Calcular la coordenada decimal
    decimal_coord <- degrees + minutes / 60 + seconds / 3600
    
    # Aplicar el signo negativo si es sur u oeste
    if (direction == "S" || direction == "W") {
      decimal_coord <- -decimal_coord
    }
    
    # Redondear la coordenada decimal a 6 decimales
    decimal_coord <- round(decimal_coord, 6)
    
    return(decimal_coord)
  } else {
    return(NA)
  }
}

# Aplicar la función a las columnas de latitud y longitud
data$sur_latitud_decimal <- sapply(data$sur_latitud, convert_coordinates)
data$oeste_longitud_decimal <- sapply(data$oeste_longitud, convert_coordinates)



# 2)
# cargar datos 
data <- data.frame(read_excel("C:/Users/Lisbeth/Documents/Tarea de R/base_students.xlsx"))

# Limpiar la columna NAME
data$NAME <- gsub("[0-9\\W]", "", data$NAME)
data$NAME <- gsub("\\*", "", data$NAME)
data$NAME <- gsub("=", "", data$NAME)
data$NAME <- gsub("_", "", data$NAME)
data$NAME <- gsub("-", "", data$NAME)
data$NAME <- gsub(")", "", data$NAME)
data$NAME <- gsub("[.+!#$%{…,;>/]", "", data$NAME)

# Limpiar la columna AGE

data$AGE <- gsub("[.+!#$%{…,;>/&*-]", "", data$AGE)
data$AGE <- gsub("dice tener", "", data$AGE)
data$AGE <- trimws(gsub("\\s+", "", data$AGE))
data$AGE <- gsub("\\b999\\b", "", data$AGE)
data$AGE <- gsub("99999", "", data$AGE)


# Limpiar la columna BORN_DATE

data$BORN_DATE <- gsub("\\s+", "", data$BORN_DATE)
data$BORN_DATE <- gsub("[#%]", "", data$BORN_DATE)

# Formato fecha de la columna BORN_DATE

data$BORN_DATE <-  as.Date(data$BORN_DATE,format='%d/%m/%Y')

#Crear una variable con el año de nacimiento.

data$ano_nacimiento <- year(data$BORN_DATE)

#Limpiar la columna GENDER

data$GENDER <- gsub("\\s+", "", data$GENDER)
data$GENDER <- tolower(data$GENDER)
data$GENDER <- str_replace_all(data$GENDER, "\\b(maaale|malee|maleeee|mal|mall+l*e*|mmle|male|mle|maloo)\\b", "males")
data$GENDER <- str_replace_all(data$GENDER, "\\b(femmallee|femae|fmale|ffemale|femmale|fele|feale|femaleeee|femle)\\b", "female")

#Crear una dummy de para GENDER

data$GENDER_DUMMY <- ifelse(str_detect(data$GENDER, "female"), 1, 0)

#Limpiar la columna TYPE_ADM_SCHOOL 

data$TYPE_ADM_SCHOOL <- gsub("\\b(prvade|privade|privado|pribado|privde)\\b", "privada", data$TYPE_ADM_SCHOOL)
data$TYPE_ADM_SCHOOL <- str_replace_all(data$TYPE_ADM_SCHOOL, "\\b(public|Public  ooo|publicp|public0|pu  blic|publico|pblic|publicoooooooo|publllic)\\b", "pública")

# Crear variable dummy para TYPE_ADM_SCHOOL

data$TYPE_ADM_SCHOOL_DUMMY <- ifelse(data$TYPE_ADM_SCHOOL == "pública", 1, 0)


#Crear una variable con el usuario del correo electronico 
correo <- "rmendozam31@gmail.com"

str_match(correo, "(\\w+)\\@.*")


#Crear una variable con el número de DNI del padre, madre o apoderado.

data$ROL_DNI <- sapply(str_extract_all(data$DNI_NUMBER, "\\d+"), paste, collapse = "-")


# Extraer el nombre y apellidos de la columna observaciones
data$nombre_completo <- ifelse(grepl("nombre correcto es ([^,]+)", data$observaciones), 
                               sub(".*nombre correcto es ([^,]+).*", "\\1", data$observaciones),
                               "")

# Reemplazar los valores correctos en la columna NAME

data$NAME <- ifelse(nchar(data$nombre_completo) > 0, data$nombre_completo, data$NAME)

# Eliminar la columna "nombre_completo"

data <- data[, -which(names(data) == "nombre_completo")]


# Extraer las frases que siguen a "es"

frases_extraidas <- str_extract(data$observaciones, "(?<=es\\s)\\d+")

# Crear una nueva columna con las frases extraídas

data$frases_extraidas <- frases_extraidas

data$frases_extraidas <- str_extract(data$observaciones, "(?<=edad correcta es )\\d+")


# Crear la columna "frases_extraidas" y asignar las frases que cumplen el patrón
data <- data %>%
  mutate(frases_extraidas = ifelse(grepl("edad correcta es \\d+", observaciones), 
                                   gsub(".*edad correcta es (\\d+).*", "\\1", observaciones), 
                                   ""))


# Extraer las frases que siguen a "es"

frases_extraidas <- str_extract(data$observaciones, "(?<=es\\s)\\d+")

# Crear una nueva columna con las frases extraídas

data$frases_extraidas <- frases_extraidas


# Extraer las frases que siguen a "es"
frases_extraidas <- str_extract(data$observaciones, "(?<=es\\s)\\d+")

# Reemplazar los valores NA con cadenas de texto vacías

frases_extraidas <- str_replace_na(frases_extraidas, "")

# Crear una nueva columna con las frases extraídas

data$frases_extraidas <- frases_extraidas

# Crear una columna auxiliar para almacenar los valores numéricos
data$AGE_aux <- as.numeric(data$frases_extraidas)

# Reemplazar los valores numéricos en la columna "AGE"

data$AGE[!is.na(data$AGE_aux)] <- data$AGE_aux[!is.na(data$AGE_aux)]

# Eliminar la columna auxiliar

data <- subset(data, select = -c(AGE_aux))

# Eliminar la columna "frases_extraidas"

data <- subset(data, select = -c(frases_extraidas))

# Crear una nueva columna llamada "cantidad_hermanos" con los números y género extraídos

data$cantidad_hermanos <- stringr::str_extract(data$observaciones, "(?i)tiene (\\d+) (hermanos|hermanas)")
data$cantidad_hermanos[is.na(data$cantidad_hermanos)] <- ""



# Eliminar letras de la columna "cantidad_hermanos"
data$cantidad_hermanos <- str_replace_all(data$cantidad_hermanos, "[A-Za-z]", "")



# Crear la variable dummy para identificar si es beneficiado del programa juntos 
data$programa_juntos <- ifelse(grepl("Beneficiado del programa Juntos", data$observaciones), 1, 0)


# Crear la variable dummy para identificar si se asiste a una II.EE de jornada completa
data$jornada_completa <- ifelse(grepl("asiste a una II.EE de jornada completa", data$observaciones), 1, 0)




