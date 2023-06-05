# Integrantes ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)

# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())

# Para limpiar el área de gráficos
graphics.off()

# Limpiar la consola
cat("\014")

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)      # Eliminar la notación científica
options(digits = 3)        # Número de decimales

# Paquetes
library(pacman)
p_load(readxl, tidyverse, foreign, ggthemes, datos,dplyr)


#PLOT


##### PREGUNTA 1 #####
#------------------------------------------------------------
#Leer data producción hoja de coca por hectárea
data <- read_excel("../../data/produccion_coca/6.1.1_-_Illicit_coca_bush_cultivation.xlsx")
data <- data[ c(4:6), c(2:13)]
data <- t(data)

#Le damos nombres a las columnas
colnames(data) <- c("Bolivia", "Colombia", "Peru")

#Convertimos en dataframe
data <- as.data.frame(data)

#Añadimos los años
data <- data %>%
  mutate(year = 2009:2020)

# Reordenar columnas
data <- data[, c("year", "Bolivia", "Colombia", "Peru")]


#Creamos el gráfico: Producción por hectárea de hoja de coca

ggplot(data, aes(x = year)) +
  geom_line(aes(y = Peru, color = "Peru"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Bolivia, color = "Bolivia"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 1) +
  geom_point(aes(y = Peru, color = "Peru"), shape = 1, size = 3) +
  geom_point(aes(y = Bolivia, color = "Bolivia"), shape = 15, size = 3) +
  geom_point(aes(y = Colombia, color = "Colombia"), shape = 4, size = 3) +
  labs(x = "Year", y = "Coca production", title = "Figure 1: Coca Production in the Andean Region") +
  scale_y_continuous(limits = c(0, 200000), breaks = seq(0, 200000, 50000), expand = c(0, 0)) +
  scale_color_manual(values = c(Peru = "#c10534", Bolivia = "#92a4b6", Colombia = "#26bb26"),
                     labels = c("Peru", "Bolivia", "Colombia")) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.box = "horizontal"
  ) +
  annotate("text", x = 2010, y = 0, label = "Notes: This graph shows coca production per year in the Andean region using UNODC data.",
           color = "black", hjust = 0, vjust = 0)



#---------------------------------------------------------------
#Leer data erradicación hoja de coca por hectárea
datos <- read_excel("../../data/produccion_coca/6.1.2_-_Eradication_of_coca_bush.xlsx")
datos <- datos[ c(1:4), c(4:15)]
datos <- t(datos)

#Le damos nombres a las columnas
colnames(datos) <- c("Years", "Bolivia", "Colombia", "Peru")

#Convertimos en dataframe
datos <- as.data.frame(datos)

#Creamos el gráfico: Erradicación por hectárea de hoja de coca

ggplot(datos, aes(x = Years)) +
  geom_line(aes(y = Peru, color = "Peru"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Bolivia, color = "Bolivia"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 1) +
  geom_point(aes(y = Peru, color = "Peru"), shape = 1, size = 3) +
  geom_point(aes(y = Bolivia, color = "Bolivia"), shape = 15, size = 3) +
  geom_point(aes(y = Colombia, color = "Colombia"), shape = 4, size = 3) +
  labs(x = "Year", y = "Coca erradication", title = "Figure 2: Coca Erradication in the Andean Region") +
  scale_y_continuous(limits = c(0, 200000), breaks = seq(0, 200000, 50000), expand = c(0, 0)) +
  scale_color_manual(values = c(Peru = "#c10534", Bolivia = "#92a4b6", Colombia = "#26bb26"),
                     labels = c("Peru", "Bolivia", "Colombia")) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.box = "horizontal"
  ) +
  annotate("text", x = 2010, y = 0, label = "Notes: This graph shows coca production per year in the Andean region using UNODC data.",
           color = "black", hjust = 0, vjust = 0)

#---------------------------------------------------------------
# Unimos data de producción y erradicación del Perú

peru <- data[, c("year", "Peru")]
peru <- as.data.frame(peru)
peru <- peru %>% rename(Years = year, Production = Peru)

peru <- cbind(peru[, c("Years", "Production")], datos["Peru"])
peru <- as.data.frame(peru)

peru <- peru %>% rename(Years = Years, Production = Production, Erradication = Peru)


#Creamos el gráfico: Producción y erradicación de hoja de coca Perú

ggplot(peru, aes(x = Years)) +
  geom_line(aes(y = Production, color = "Production"), linetype = "dashed", size = 1) +
  geom_line(aes(y = Erradication, color = "Erradication"), size = 1) +
  geom_point(aes(y = Production, color = "Production"), shape = 1, size = 3) +
  geom_point(aes(y = Erradication, color = "Erradication"), shape = 4, size = 3) +
  labs(x = "Year", y = "Hectarea", title = "Figure 3: Coca Production and Eradication in Peru") +
  scale_y_continuous(limits = c(0, 200000), breaks = seq(0, 200000, 50000), expand = c(0, 0)) +
  scale_color_manual(values = c("#92a4b6", "#c10534"), labels = c("Erradication", "Production")) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.box = "horizontal"
  ) +
  annotate("text", x = 2010, y = 0, label = "Notes: This graph shows coca production per year in the Andean region using UNODC data.",
           color = "black", hjust = 0, vjust = 0)





#REGEX: PARTE 1





#REGEX: PARTE 2

install.packages("tidyverse")
install.packages("readxl")

library(tidyverse)
library(readxl)

# Leer la base de datos desde el archivo Excel
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

ruta_archivo <- "../..//data/estudiantes/base_students.xlsx"
datos_estudiantes <- read_excel(ruta_archivo)

# Limpiar el nombre de los estudiantes

# Eliminar caracteres especiales, números y otros elementos no deseados en la columna "NAME"
datos_estudiantes$NAME <- gsub("[^[:alpha:] ]", "", datos_estudiantes$NAME)
datos_estudiantes$NAME <- gsub("\\s+", " ", datos_estudiantes$NAME)
datos_estudiantes$NAME <- sub("^\\s+", "", datos_estudiantes$NAME) 

print(datos_estudiantes$NAME)

# Limpiar la fecha de nacimiento y edad

datos_estudiantes$BORN_DATE <- gsub("[^0-9/]", "", datos_estudiantes$BORN_DATE) # Limpiar la fecha de nacimiento
datos_estudiantes$BORN_DATE <- format(as.Date(datos_estudiantes$BORN_DATE, "%d/%m/%Y"), "%d/%m/%Y") # Asignar formato "dd/mm/yyyy" a la fecha de nacimiento
datos_estudiantes$BORN_DATE <- as.Date(datos_estudiantes$BORN_DATE, "%d/%m/%Y") # Convertir la columna "BORN_DATE" en objetos de fecha
datos_estudiantes$YEAR_OF_BIRTH <- format(datos_estudiantes$BORN_DATE, "%Y") # Crear una variable con el año de nacimiento

print(datos_estudiantes$BORN_DATE)
print(datos_estudiantes$YEAR_OF_BIRTH)

# Limpiar la edad
datos_estudiantes$AGE <- gsub("[^[:digit:]]", "", datos_estudiantes$AGE)
#datos_estudiantes$AGE <- as.numeric(datos_estudiantes$AGE) # Convertir la columna "AGE" a tipo numérico
datos_estudiantes$AGE <- ifelse(grepl("9{2,}", datos_estudiantes$AGE), NA, datos_estudiantes$AGE) # Reemplazar las edades con varios 9 por NA

print(datos_estudiantes$AGE)

# Creación de la variable Dummy gender
# 0: no pertenece
# 1: si pertenece

datos_estudiantes$GENDER <- ifelse(grepl("^f|^F", datos_estudiantes$GENDER, ignore.case = TRUE, perl = TRUE), 1, 0)
datos_estudiantes$GENDER

# Crear una variable dummy para el tipo de administración de la institución educativa
# Creación de la variable Dummy público
# 0: no pertenece
# 1: si pertenece
datos_estudiantes$PUBLICO <- ifelse(grepl("^Pub|^pub", datos_estudiantes$TYPE_ADM_SCHOOL, ignore.case = TRUE, perl = TRUE), 1, 0)
datos_estudiantes$PUBLICO

#Crear una variable con el usuario del correo electrónico (rmendozam@pucp.edu.pe, usuario : rmendozam)

# Definir la función usuario
USUARIO <- function(x) {
  match <- regexpr("(\\w+)@", x)
  if (match != -1) {
    substring(x, match, match + attr(match, "match.length") - 2)
  } else {
    NA
  }
}

# Aplicar la función usuario a la columna "mail" del dataframe
datos_estudiantes$USUARIO <- sapply(datos_estudiantes$MAIL, USUARIO)
datos_estudiantes$USUARIO

# Paso 9: Crear una variable con el número de DNI del padre, madre o apoderado.

DNI <- function(x) {
  match <- regexpr("(\\d+-\\d+)", x)
  if (match != -1) {
    substring(x, match, match + attr(match, "match.length") - 1)
  } else {
    NA
  }
}

# Aplicar la función DNI a la columna "DNI_NUMBER" del dataframe
datos_estudiantes$DNI <- sapply(datos_estudiantes$DNI_NUMBER, DNI)

# Visualizar el dataframe
View(datos_estudiantes)


#VARIABLE OBSERVACIONES

# Para recuperar el nombre
library(stringr)

obs_info <- function(x) {
  regex_nombre <- "es ([A-Za-z]+)\\s+([A-Za-z]+)\\s+([A-Za-z]+)"
  match <- str_match(x, regex_nombre)
  if (!is.na(match[1, 2])) {
    return(paste(match[1, 2:4], collapse = " "))
  } else {
    return(NA)
  }
}

# Aplicar la función obs_info a la columna "observaciones"
datos_estudiantes$nombre <- sapply(datos_estudiantes$observaciones, obs_info)
datos_estudiantes$nombre <- toupper(datos_estudiantes$nombre)

# Reemplazar el nombre en la columna "NAME"
datos_estudiantes$NAME[!is.na(datos_estudiantes$nombre)] <- datos_estudiantes$nombre[!is.na(datos_estudiantes$nombre)]
# Verificar los cambios
datos_estudiantes$NAME

# Para recuperar la edad
library(stringr)

obs_info1 <- function(x) {
  regex_edad <- "es ([0-9]+)"
  match <- str_match(x, regex_edad)
  if (!is.na(match[1, 2])) {
    return(match[1, 2])
  } else {
    return(NA)
  }
}

# Aplicar la función obs_info1 a la columna "observaciones"
datos_estudiantes$edad <- sapply(datos_estudiantes$observaciones, obs_info1)
#Reemplazo la edad en la columna AGE
datos_estudiantes$AGE[!is.na(datos_estudiantes$edad)] <- datos_estudiantes$edad[!is.na(datos_estudiantes$edad)]
datos_estudiantes$AGE

#Crear una variable con la cantidad de hermana/os del estudiante

# Para identificar el número de hermanos
obs_info2 <- function(x) {
  regex_hermanos <- "tiene ([0-9]+)"
  hermanos <- str_extract(x, regex_hermanos)
  if (!is.na(hermanos)) {
    return(str_replace(hermanos, "tiene ", ""))
  } else {
    return(NA)
  }
}

# Aplicar la función obs_info2 a la columna "observaciones"
datos_estudiantes$num_hermanos <- sapply(datos_estudiantes$observaciones, obs_info2)
datos_estudiantes$num_hermanos


# Creación de la variable dummy "juntos"
datos_estudiantes$juntos <- ifelse(grepl("[Jj]untos", datos_estudiantes$observaciones, ignore.case = TRUE),
                                   1,
                                   0)
datos_estudiantes$juntos




