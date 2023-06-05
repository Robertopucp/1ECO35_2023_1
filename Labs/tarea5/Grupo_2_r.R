# Tarea 5 ####

# Limpia el environment

rm(list = ls())

# Limpia gráficos
graphics.off()

# Limpia consolas

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

#Abrir librerias

library(pacman) 
p_load(readxl, lubridate, tidyverse, stringi)
library(openxlsx)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Se selecciona el directorio


## 1. Plot ####


### 1.1 Replicar el siguiente gráfico (producción de hoja de coca en hectáreas) lo más parecido posible en términos de color de series por paises, diseño de la series, diseño de la leyenda, nota de pie de página y titulo de los ejes pues la disponibilidad de datos es diferente 2009-2020.####


# Importar base de producción de hoja de coca
b <- readxl::read_excel("../../data/produccion_coca/6.1.1_-_Illicit_coca_bush_cultivation.xlsx")
b1 <- b[4:6, ]  ## mantener solo las filas con información de los países
prod <- b1
colnames(prod) <- c("pais", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
## Renombrar las observaciones de la variable pais
renombrar <- c(3, 4, 5)
nuevo_nombre <- c('Bolivia', 'Colombia', 'Peru')

for (i in 1:length(renombrar)) {
  prod$pais[renombrar[i]-2] <- nuevo_nombre[i]
}

# Convertir columnas a valores numéricos
prod[, -1] <- lapply(prod[, -1], as.numeric)

# Reorganizar los datos en formato long
prod_long <- tidyr::pivot_longer(prod, cols = -pais, names_to = "Year", values_to = "Value")

# Gráfico
ggplot(prod_long) +
  geom_line(aes(x = Year, y = Value, linetype = pais, color = pais, group = pais), size = 0.6) +
  geom_point(aes(x = Year, y = Value, shape = pais, color = pais), size = 4, fill = "transparent") +
  geom_text(aes(x = Year, y = Value, label = ifelse(pais == "Colombia", "x", "")), size = 4, vjust = -0.5) +
  geom_point(aes(x = Year, y = Value, shape = pais, color = pais), size = 4, fill = "transparent") +
  theme_minimal() +
  scale_x_discrete(labels = unique(prod_long$Year)) +
  labs(
    x = "Years",
    y = "Illicit cultivation of coca bush, 2009-2020 (hectares)",
    title = "Illicit cultivation of coca bush, 2009-2020 (hectares)",
    caption = "Source: National illicit crop monitoring system supported by UNODC"
  ) +
  scale_linetype_manual(values = c(Bolivia = "solid", Colombia = "solid", Peru = "dashed"), name = "Country") +
  scale_shape_manual(values = c(Bolivia = 15, Colombia = NA, Peru = 1), name = "Country") +
  scale_color_manual(values = c(Bolivia = "#CCCCCC", Colombia = "#02BA26", Peru = "#8B0000"), name = "Country") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

#Se guarda grafico
ggsave("../../output/plots/cultivationCocaRGrupo2.jpg"
       , height = 7  # alto
       , width = 9  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)


### 1.2 Relizar un gráfico similar con los datos de erradicación de hectareas de hoja de Coca. ####

c <- readxl::read_excel("../../data/produccion_coca/6.1.2_-_Eradication_of_coca_bush.xlsx")
c1 <- c[2:4, ]  ## mantener solo las filas con información de los países
c1 <- c1[, -c(2, 3,16)]  ## eliminar la segunda y tercera columna


erad <- c1
colnames(erad) <- c("pais", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")
## Renombrar las observaciones de la variable pais
renombrar <- c(3, 4, 5)
nuevo_nombre <- c('Bolivia', 'Colombia', 'Peru')

for (i in 1:length(renombrar)) {
  erad$pais[renombrar[i]-2] <- nuevo_nombre[i]
}

# Convertir columnas a valores numéricos
erad[, -1] <- lapply(erad[, -1], as.numeric)

# Reorganizar los datos en formato long
erad_long <- tidyr::pivot_longer(erad, cols = -pais, names_to = "Year", values_to = "Value")

# Gráfico
ggplot(erad_long) +
  geom_line(aes(x = Year, y = Value, linetype = pais, color = pais, group = pais), size = 0.6) +
  geom_point(aes(x = Year, y = Value, shape = pais, color = pais), size = 4, fill = "transparent") +
  geom_text(aes(x = Year, y = Value, label = ifelse(pais == "Colombia", "x", "")), size = 4, vjust = -0.5) +
  geom_point(aes(x = Year, y = Value, shape = pais, color = pais), size = 4, fill = "transparent") +
  theme_minimal() +
  scale_x_discrete(labels = unique(erad_long$Year)) +
  labs(
    x = "Years",
    y = "Eradication of coca bush, 2009-2020 (hectares)",
    title = "Eradication of coca bush, 2009-2020 (hectares)",
    caption = "Source: United Nations Office on Drugs and Crime annual report questionnaire and government reports"
  ) +
  scale_linetype_manual(values = c(Bolivia = "solid", Colombia = "solid", Peru = "dashed"), name = "Country") +
  scale_shape_manual(values = c(Bolivia = 15, Colombia = NA, Peru = 1), name = "Country") +
  scale_color_manual(values = c(Bolivia = "#CCCCCC", Colombia = "#02BA26", Peru = "#8B0000"), name = "Country") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Guardar el gráfico
ggsave(
  filename = "../../output/plots/eradicationCocaRGrupo2.jpg",
  height = 7,  # alto
  width = 9,   # ancho
  dpi = 320    # resolución (calidad de la imagen)
)





### 1.3 Realizar un gráfico con la producción y erradicación de hoja de coca en el Perú. ####

# Importar datos de producción y erradicación de hoja de coca en Perú
prod_peru <- prod_long %>% filter(pais == "Peru")
erad_peru <- erad_long %>% filter(pais == "Peru")

# Gráfico
ggplot() +
  geom_line(data = prod_peru, aes(x = Year, y = Value, linetype = "Production", color = "Production"), size = 0.6) +
  geom_line(data = erad_peru, aes(x = Year, y = Value, linetype = "Eradication", color = "Eradication"), size = 0.6) +
  geom_point(data = prod_peru, aes(x = Year, y = Value, shape = "Production", color = "Production"), size = 4, fill = "transparent") +
  geom_point(data = erad_peru, aes(x = Year, y = Value, shape = "Eradication", color = "Eradication"), size = 4, fill = "transparent") +
  geom_text(data = erad_peru, aes(x = Year, y = Value, label = ifelse(Value > 0, "x", "")), size = 4, vjust = -0.5) +
  theme_minimal() +
  labs(
    x = "Years",
    y = "Eradication and illicit cultivation of coca bush in Peru",
    title = "Eradication and illicit cultivation of coca bush in Peru (2009-2020)",
    caption = "Source: Sources: United Nations Office on Drugs and Crime, government reports and UNODC"
  ) +
  scale_linetype_manual(values = c(Production = "solid", Eradication = "solid"), name = "Category") +
  scale_shape_manual(values = c(Production = 15, Eradication = 1), name = "Category") +
  scale_color_manual(values = c(Production = "#56B4E9", Eradication = "#E69F00"), name = "Category") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

# Guardar el gráfico
ggsave(
  filename = "../../output/plots/production_eradication_peruRGrupo2.jpg",
  height = 7,  # alto
  width = 9,   # ancho
  dpi = 320    # resolución (calidad de la imagen)
)














## 2. Regex ####

base1 <- read_excel("../../data/metropolitano.xlsx")
metropolitano <- base1
metropolitano


### 2.1 Usar regex para cambiar el formato de las coordenadas a uno de coordenadas geográficos para metropolitano.xlsx ####

# Se define la función
convert_gps <- function(x) {
  signo <- ifelse(str_detect(x, "[sswSWoO-]"), -1, 1)
  
  h <- as.numeric(str_extract(x, "\\d+"))
  m <- as.numeric(str_extract(x, "(?<=')\\d+"))
  s <- as.numeric(str_extract(x, "(?<=\")\\d+"))
  
  h <- ifelse(is.na(h), 0, h)
  m <- ifelse(is.na(m), 0, m)
  s <- ifelse(is.na(s), 0, s)
  
  return(signo * (h + m / 60 + s / 3600))
}

# Se crean nuevas columnas con los valores convertidos en coordenadas geográficas
metropolitano$sur_latitud2 <- sapply(metropolitano$sur_latitud, convert_gps)
metropolitano$oeste_longitud2 <- sapply(metropolitano$oeste_longitud, convert_gps)
#Se muestra el resultado
metropolitano




### 2.2 Realizar la siguiente limpieza de datos asi como creación de las variables usando Expresiones Regulares con base_estudiantes.xlsx ####

# Se lee la base de datos
baseEstudiantes <- read_excel("../../data/estudiantes/base_students.xlsx")

baseEstudiantes

#### 2.2.1 Limpiar el nombre de los estudiantes ####

# Se define la función que limpiará los nombres
limpiaNombres <- function(x) {
  nombreLimpio <- str_replace_all(x, "[^a-zA-Z\\s]+", "")
  nombreLimpio <- str_trim(nombreLimpio)
  return(nombreLimpio)
}

# Se aplica la función a la variable NAME
baseEstudiantes$NAME <- sapply(baseEstudiantes$NAME, limpiaNombres)

# Se muestra el resultado
baseEstudiantes



#### 2.2.2 Limpiar la fecha de nacimiento y edad. Asimismo, asignar el formato date a la fecha de nacimiento. Crear una variable con el año de nacimiento. ####

# Se define la función que limpiará la edad
limpiaEdad <- function(x) {
  xLimpio <- str_replace_all(x, "[^0-9]+", "")
  return(xLimpio)
}

# Se aplica la función a la columna AGE
baseEstudiantes$AGE <- sapply(baseEstudiantes$AGE, limpiaEdad)

# Se define la función que limpiará la fecha
limpiaFecha <- function(x) {
  xLimpio <- str_replace(x, "(\\d{2})#(\\d{1})", "\\1")
  xLimpio <- str_replace_all(xLimpio, "[^0-9/]+", "")
  xLimpio <- str_sub(xLimpio, end = 10)
  return(xLimpio)
}

# Se convierte a string
baseEstudiantes$BORN_DATE <- as.character(baseEstudiantes$BORN_DATE)

# Se aplica la función
baseEstudiantes$BORN_DATE <- sapply(baseEstudiantes$BORN_DATE, limpiaFecha)

# Se descartan los casos no válidos
baseEstudiantes$BORN_DATE[baseEstudiantes$BORN_DATE %in% c("000000", "00/00/00", "1000000")] <- NA

# Se extrae el año
baseEstudiantes$anio_nacimiento <- str_extract(baseEstudiantes$BORN_DATE, "\\d{4}")

# Se transforma a formato fecha
baseEstudiantes$BORN_DATE <- as.Date(baseEstudiantes$BORN_DATE, format = "%d/%m/%Y")

#Se presenta los resultados
baseEstudiantes



#### 2.2.3 Use la variable GENDER para crear una dummy que tome el valor de 1 para female y 0 para males. Similarmente, crear una variable dummy que tome el valor de 1 si el colegio al cual asiste la o el menor es pública, y 0 si es privada. ####

# Se convierten ambas variables a character
baseEstudiantes$GENDER <- as.character(baseEstudiantes$GENDER)
baseEstudiantes$TYPE_ADM_SCHOOL <- as.character(baseEstudiantes$TYPE_ADM_SCHOOL)

# Se genera la dummy GENDER
baseEstudiantes$FEMALE <- ifelse(grepl("^f|^F", baseEstudiantes$GENDER), 1, 0)

# Se genera la dummy PUBLICA
baseEstudiantes$PUBLICA <- ifelse(grepl("^pu", baseEstudiantes$TYPE_ADM_SCHOOL), 1, 0)

#Se muestran los resultados
baseEstudiantes

#### 2.2.4 Crear una variable con el usuario del correo electrónico (rmendozam@pucp.edu.pe, usuario : rmendozam)  ####
# Se define la función que obtendrá el usuario

obtieneUsuario <- function(x) {
  regex <- "(\\w+)\\@.*"
  coincidencia <- str_match(x, regex)
  usuario <- coincidencia[1, 2]
  return(usuario)
}
baseEstudiantes$usuario <- as.character(baseEstudiantes$MAIL)
baseEstudiantes$usuario <- sapply(baseEstudiantes$MAIL, obtieneUsuario)
#Se muestran los resultados
baseEstudiantes

#### 2.2.5 Crear una variable con el número de DNI del padre, madre o apoderado ####

# Se define la función que extraerá el número de DNI de 8 dígitos
extraerDNI <- function(x) {
  regex <- "(?<=-1)\\d{8}"
  dni <- stringr::str_extract(x, regex)
  return(dni)
}

baseEstudiantes$numeroDNI <- sapply(baseEstudiantes$DNI_NUMBER, extraerDNI)
#Se muestra los resultados
baseEstudiantes

#### 2.2.6 La variable observaciones contiene información del nombre y edad correctos del estudiante. Asimismo, tiene información de la cantidad de hermanos, si el menor es beneficiario del programa Juntos o si asiste a una institucón educativa de Jornada Escolar Completa. A partir de la variable observaciones, realizar lo siguiente: ####

##### 2.2.6.1 Recuperar el nombre y edad correctos, y reemplazarlo en las variables de nombre (NAME) y edad respectivamente (AGE) ####

# Utilizar str_match para extraer el nombre correcto de la columna "observaciones"
baseEstudiantes$nombre_correcto <- str_match(baseEstudiantes$observaciones, "nombre correcto es ([^\\s,:]+(?:\\s+[^\\s,:]+)*)")[, 2]

# Convertir a mayúsculas
baseEstudiantes$nombre_correcto <- toupper(baseEstudiantes$nombre_correcto)




# Extraer el número de la edad de la variable "observaciones" utilizando str_match
baseEstudiantes$edadCorrecta <- str_match(baseEstudiantes$observaciones, "(?i)\\b(edad)\\b[^0-9]*([0-9]+)")[, 3]

# Reemplazar los valores de "NAME" y "AGE" con los valores de "nombre_correcto" y "edadCorrecta"
baseEstudiantes$NAME <- ifelse(!is.na(baseEstudiantes$nombre_correcto), baseEstudiantes$nombre_correcto, baseEstudiantes$NAME)
baseEstudiantes$AGE <- ifelse(!is.na(baseEstudiantes$edadCorrecta), baseEstudiantes$edadCorrecta, baseEstudiantes$AGE)

#Se muestran los resultados
baseEstudiantes


##### 2.2.6.2 Crear una variable con la cantidad de hermana/os del estudiante ####
# Se define la función que extraerá el número de hermanos de la variable "observaciones"
extraer_numero_hermanos <- function(texto) {
  match <- str_match(texto, "tiene (\\d+)")
  numero <- match[1, 2]
  as.integer(numero)
}

# Se aplica la función a la columna "observaciones"
baseEstudiantes$numeroHermanos <- sapply(baseEstudiantes$observaciones, extraer_numero_hermanos)

# Se convierte a tipo de datos "integer" y se reemplazan los valores no válidos con NA
baseEstudiantes$numeroHermanos <- as.integer(baseEstudiantes$numeroHermanos)
baseEstudiantes$numeroHermanos[is.na(baseEstudiantes$numeroHermanos)] <- NA
baseEstudiantes

##### 2.2.6.3 Crear una variable con la cantidad de hermana/os del estudiante ####

# Se generan ambas dummies con nuevas columnas según lo que diga la variable "observaciones"
baseEstudiantes$beneficiadoJuntos <- ifelse(grepl("Juntos", baseEstudiantes$observaciones, ignore.case = TRUE), 1, 0)
baseEstudiantes$asisteIIEEJC <- ifelse(grepl("jornada completa", baseEstudiantes$observaciones, ignore.case = TRUE), 1, 0)

#Se muestran los resultados
baseEstudiantes


