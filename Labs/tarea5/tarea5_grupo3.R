### TAREA 5 - GRUPO3

# Importar las librerías necesarias
library(readxl)
library(haven)
library(dplyr)
library(ggplot2)
library(gridExtra)

                            # GRAFICO 1 #

# Ruta y nombre del archivo de Excel
data_1 <- read_excel("C:/Users/ALICIA/Documents/GitHub/1ECO35_2031_1_APUNTES/data_tarea5/6.1.1_-_Illicit_coca_bush_cultivation.xlsx")

# Filtrar las columnas correspondientes a países y fechas
data_1 <- data_1[c(2, 4, 5, 6,7), ]

# Transponer el dataframe data_1
data_transposed <- as.data.frame(t(data_1))


# Cambiar los nombres de los encabezados
nw_names <- c("year","Bolivia", "Colombia", "Peru")
colnames(data_transposed) <- replace(colnames(data_transposed), TRUE, nw_names)

#Eliminar la primera fila de un data frame
data_nw <- data_transposed[-1, ]

#Eliminar la ultima columna
data_nw <- data_nw[, -ncol(data_nw)]

# Definir los nuevos nombres de los valores
nw_names_2 <- c("2009","2010", "2011", "2012", "2013","2014","2015", "2016", "2017", "2018", "2019", "2020")

# Obtener la columna específica
clm <- data_nw$year

# Convertir la columna en un factor con los nuevos nombres
data_nw$year <- factor(clm, levels = unique(clm), labels = nw_names_2)


# Convertir las columnas a formato numérico
data_nw <- data_nw %>%
  mutate(Bolivia = as.numeric(Bolivia),
         Colombia = as.numeric(Colombia),
         Peru = as.numeric(Peru))
str(data_nw)

# GRAFICANDO

ggplot(data_nw) +
  aes(x = year) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed", group = 1) +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid", group = 1) +
  geom_line(aes(y = Peru, color = "Perú"), size = 0.6, linetype = "dashed", group = 1) +
  theme_minimal() +
  labs(x = "Años", y = "Coca production", title = "Figure 1: Coca production in the Andean region") +
  scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                     labels = c("Bolivia", "Colombia", "Perú"))+
  labs(color = "Leyenda")

                        # GRAFICO 2 #
# Ruta y nombre del archivo de Excel
data_2 <- read_excel("C:/Users/ALICIA/Documents/GitHub/1ECO35_2031_1_APUNTES/data_tarea5/6.1.2_-_Eradication_of_coca_bush.xlsx")


# Eliminar filas desde la fila 5 hasta el final del data frame
data_nw2 <- data_2 %>% slice(1:4)

# Eliminar las columnas con índices 2, 3 y 16
data_nw2 <- data_nw2[, -c(2, 3, 16)]

# Transponer el dataframe data_1
data_nw2 <- as.data.frame(t(data_nw2))

# Cambiar los nombres de los encabezados
nw_names_3 <- c("year","Bolivia", "Colombia", "Peru")
colnames(data_nw2) <- replace(colnames(data_nw2), TRUE, nw_names_3)

#Eliminar la primera fila de un data frame
data_nw2 <- data_nw2[-1, ]


# Definir los nuevos nombres de los valores
nw_names_4 <- c("2009","2010", "2011", "2012", "2013","2014","2015", "2016", "2017", "2018", "2019", "2020")

# Obtener la columna específica
clm <- data_nw2$year

# Convertir la columna en un factor con los nuevos nombres
data_nw2$year <- factor(clm, levels = unique(clm), labels = nw_names_4)


# Convertir las columnas a formato numérico
data_nw2 <- data_nw2 %>%
  mutate(Bolivia = as.numeric(Bolivia),
         Colombia = as.numeric(Colombia),
         Peru = as.numeric(Peru))
str(data_nw2)


# GRAFICANDO

ggplot(data_nw2) +
  aes(x = year) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed", group = 1) +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid", group = 1) +
  geom_line(aes(y = Peru, color = "Perú"), size = 0.6, linetype = "dashed", group = 1) +
  theme_minimal() +
  labs(x = "Year", y = "Reported Eradication (in hectare)", title = "Reported eradication of coca bush, 2009-2020") +
  scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                     labels = c("Bolivia", "Colombia", "Perú"))+
  labs(color = "Leyenda")

                                      # GRAFICO 3 #
# Hacemos el merge
DATA <- merge(data_nw,data_nw2,by="year")

# Eliminar las columnas con índices 
DATA_l <- DATA[, -c(2,3,5,6)]


# Cambiar los nombres de los encabezados
nw_names_5 <- c("year","Production", "Erradication")
colnames(DATA_l) <- replace(colnames(DATA_l), TRUE, nw_names_5)


# Convertir las columnas a formato numérico
DATA_l <- DATA_l %>%
  mutate(Production = as.numeric(Production),
         Erradication = as.numeric(Erradication))
str(DATA_l)


ggplot(DATA_l) +
  aes(x = year) +
  geom_line(aes(y = Production, color = "Producción"), size = 0.6, linetype = "dashed", group = 1) +
  geom_line(aes(y = Erradication, color = "Erradicación"), size = 0.6, linetype = "solid", group = 1) +
  theme_minimal() +
  labs(x = "Year", y = "Hectareas", title = "Figure 3: Producción y erradicación de hoja de coca en el Perú (2009 - 2020)") +
  scale_color_manual(values = c(Producción = "purple", Erradicación = "turquoise"),
                     labels = c("Producción", "Erradicación")) +
  labs(color = "Leyenda")


# =================== PARTE 2 : REGEX =====================

##pregunta 1 -----------------

# Instalar librerías
install.packages("data.table") 

base1 <- read_excel("C:/Users/ALICIA/Documents/GitHub/1ECO35_2023_1/data/metropolitano.xlsx")

# Crear data1 con valores nulos en la columna "id"
data1 <- base1[is.na(base1$id), ]

# Restablecer el índice
data1 <- data1[order(rownames(data1)), ]
row.names(data1) <- NULL

# Crear data2 con valores no nulos en la columna "id"
data2 <- base1[!is.na(base1$id), ]

# Restablecer el índice
data2 <- data2[order(rownames(data2)), ]
row.names(data2) <- NULL

# Obtener la composición de filas y columnas de data2
dim(data2)

# Definir la función de conversión de coordenadas
convert_gps <- function(x) {
  signo <- ifelse(grepl("[swSWoO-]", x), -1, 1)
  values <- regmatches(x, gregexpr("[0-9.]+", x))[[1]]
  if (length(values) >= 2) {
    h <- as.numeric(values[1])
    m <- as.numeric(values[2])
    if (length(values) == 3) {
      s <- as.numeric(values[3])
    } else {
      s <- 0.0
    }
    return(signo * (h + m / 60 + s / 3600))
  }
  return(NULL)
}

# Aplicar la función convert_gps a las columnas 'sur_latitud' y 'oeste_longitud'
base1$Latitud <- sapply(base1$sur_latitud, convert_gps)
base1$Longitud <- sapply(base1$oeste_longitud, convert_gps)

# Guardar el DataFrame en un archivo CSV
write.csv(base1, file = "metropolitano.csv", row.names = FALSE)

print("DataFrame guardado como metropolitano.csv")
base1

##pregunta 2 --------------------


# Leer el archivo Excel
estudiantes <- read_excel("C:/Users/ALICIA/Documents/GitHub/1ECO35_2023_1/data/estudiantes/base_students.xlsx")
estudiantes


# Eliminar caracteres no deseados y preservar las tildes en la columna "NAME"
estudiantes$NAME <- str_replace_all(estudiantes$NAME, "[^[:alpha:][:space:]áéíóúÁÉÍÓÚñÑ]", "")
estudiantes$NAME <- str_to_title(estudiantes$NAME)

# Mostrar el DataFrame actualizado
estudiantes


# Eliminar caracteres no numéricos y convertir a tipo numérico en la columna "AGE"
estudiantes$AGE <- as.numeric(gsub("[^0-9]", "", estudiantes$AGE))

# Mostrar el DataFrame actualizado
estudiantes

# Eliminar caracteres no deseados y espacios en la columna "BORN_DATE"
estudiantes$BORN_DATE <- gsub("[^0-9/]|^\\s+|\\s+$", "", estudiantes$BORN_DATE)

# Verificar si la columna contiene solo la fecha o también la hora
estudiantes$BORN_DATE <- sub(" .*", "", estudiantes$BORN_DATE)

# Eliminar los cuatro ceros al final de la fecha
estudiantes$BORN_DATE <- gsub("0000$", "", estudiantes$BORN_DATE)

# Convertir los valores a fechas y asignar NaN a los valores no válidos
estudiantes$BORN_DATE <- as.Date(estudiantes$BORN_DATE, format = "%d/%m/%Y")

# Precisar que hay valores en BORN_DATE en los que la fecha no es reconocible, por lo que se asigna NA
estudiantes[is.na(estudiantes$BORN_DATE), "BORN_DATE"] <- NA

# Mostrar el DataFrame actualizado
estudiantes


# Convertir la columna "BORN_DATE" a formato de fecha
estudiantes$BORN_DATE <- as.Date(estudiantes$BORN_DATE)

# Extraer el año de la columna "BORN_DATE" utilizando expresiones regulares
estudiantes$año_de_nacimiento <- as.numeric(sub(".?(\\d{4}).", "\\1", estudiantes$BORN_DATE))

# Mostrar el DataFrame actualizado
estudiantes



# Crear una variable dummy para el género utilizando expresiones regulares
estudiantes$género <- ifelse(grepl("^f", estudiantes$GENDER, ignore.case = TRUE), 1, 0)

# Mostrar el DataFrame actualizado
estudiantes


# Crear una variable dummy para el tipo de escuela utilizando expresiones regulares
estudiantes$tipo_de_escuela <- ifelse(grepl("pri", estudiantes$TYPE_ADM_SCHOOL, ignore.case = TRUE), 0, 1)

# Mostrar el DataFrame actualizado
estudiantes

# Crear una variable dummy para el usuario extrayéndolo del correo electrónico utilizando expresiones regulares
estudiantes$USER <- sub("^([^@]+).*", "\\1", estudiantes$MAIL)

# Mostrar el DataFrame actualizado
estudiantes


# Crear una variable dummy para el DNI extrayendo los 8 dígitos correspondientes a un DNI peruano utilizando expresiones regulares
estudiantes$DNI <- sub(".-(\\d{2})(\\d{8}).", "\\1\\2", estudiantes$DNI_NUMBER)

# Mostrar el DataFrame actualizado
estudiantes


# Instalar el paquete 'stringr' si no está instalado
# install.packages("stringr")

library(stringr)

# Recuperar el nombre y edad correctos de las observaciones
estudiantes$Nombre_Correcto <- str_match(estudiantes$observaciones, "nombre correcto es ([a-zA-Z ]+)")[,2]
estudiantes$Edad_Correcta <- str_match(estudiantes$observaciones, "edad correta es (\\d+)")[,2]

# Reemplazar el nombre y edad en las columnas correspondientes
estudiantes$NAME <- ifelse(!is.na(estudiantes$Nombre_Correcto), estudiantes$Nombre_Correcto, estudiantes$NAME)
estudiantes$AGE <- ifelse(!is.na(estudiantes$Edad_Correcta), estudiantes$Edad_Correcta, estudiantes$AGE)

# Buscar nombre y edad en las observaciones y reemplazar si se encuentran
estudiantes$Nombre_Correcto_Obs <- str_match(estudiantes$observaciones, "nombre correcto es ([a-zA-Z ]+)")[,2]
estudiantes$Edad_Correcta_Obs <- str_match(estudiantes$observaciones, "edad correta es (\\d+)")[,2]

estudiantes$NAME <- ifelse(!is.na(estudiantes$Nombre_Correcto_Obs), estudiantes$Nombre_Correcto_Obs, estudiantes$NAME)
estudiantes$AGE <- ifelse(!is.na(estudiantes$Edad_Correcta_Obs), estudiantes$Edad_Correcta_Obs, estudiantes$AGE)

# Eliminar las columnas temporales
estudiantes <- subset(estudiantes, select = -c(Nombre_Correcto, Edad_Correcta, Nombre_Correcto_Obs, Edad_Correcta_Obs))
estudiantes


# Crear una columna "Hermanos" con valores iniciales en 0
estudiantes$Hermanos <- 0

# Buscar información sobre hermanos en las observaciones y actualizar la columna "Hermanos"
estudiantes$Hermanos <- sapply(estudiantes$observaciones, function(x) {
  matches <- regmatches(x, gregexpr("tiene (\\d+)", x, ignore.case = TRUE))
  ifelse(length(matches[[1]]) > 0, as.numeric(gsub("\\D", "", matches[[1]])), 0)
})

# Crear la columna dummy "Tiene_Hermanos"
estudiantes$Tiene_Hermanos <- ifelse(estudiantes$Hermanos > 0, 1, 0)
estudiantes


# Crear la columna dummy "Beneficiado_Juntos" utilizando expresiones regulares
estudiantes$Beneficiado_Juntos <- grepl("Beneficiado del programa Juntos", estudiantes$observaciones, ignore.case = TRUE)

# Reemplazar los valores NA por FALSE
estudiantes$Beneficiado_Juntos[is.na(estudiantes$Beneficiado_Juntos)] <- FALSE

# Convertir los valores TRUE/FALSE a 1/0
estudiantes$Beneficiado_Juntos <- as.integer(estudiantes$Beneficiado_Juntos)
estudiantes


# Crear la columna dummy "Jornada_Completa" utilizando expresiones regulares
estudiantes$Jornada_Completa <- grepl("jornada completa", estudiantes$observaciones, ignore.case = TRUE)

# Reemplazar los valores NA por FALSE
estudiantes$Jornada_Completa[is.na(estudiantes$Jornada_Completa)] <- FALSE

# Convertir los valores TRUE/FALSE a 1/0
estudiantes$Jornada_Completa <- as.integer(estudiantes$Jornada_Completa)
estudiantes





