################  laboratorio 4 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 

# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

#install.packages("pacman")

library(pacman) 
# permite llamar a varias librerias de manera simultánea
# Si la librería no está instalada, entonces lo instala y llama para su uso

p_load(dplyr, readxl, tidyverse, foreign, datos) 

# tidyverse es una recopilación de varias librerias (dply, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Load datasets ----------------------

# csv (comma separated values)

datoscsv <- read.csv("../../data/Riesgo_morosidad.csv")

# punto y coma separa los datos

datoscsv <- read.csv("../../data/Riesgo_morosidad.csv", sep = ";")


# read.csv(file, header = TRUE, sep = ",", quote = "\"",
#          dec = ".", fill = TRUE, comment.char = "", ...

str(datoscsv)


# Variables categoricas

datoscsv$sexo      <- factor(datoscsv$sexo, levels = c(1, 2),
                             labels = c("Masculino", "Femenino"))

datoscsv$morosidad <- factor(datoscsv$morosidad, levels = c(1,2),
                             labels = c("No moroso", "Moroso"))

# factor (variable, niveles, etiquetas labels )

levels(datoscsv$sexo)
levels(datoscsv$sexo) <- c("M","F")

datoscsv$fonopart  <- factor(datoscsv$fonopart, levels = c(1, 2),
                             labels = c("No", "Si"))

datoscsv$fonolab   <- factor(datoscsv$fonolab, levels = c(1, 2),
                             labels = c("No", "Si"))

datoscsv$autovaluo <- factor(datoscsv$autovaluo, levels = c(1, 2),
                             labels = c("No", "Si"))

datoscsv$esaval    <- factor(datoscsv$esaval, levels = c(1, 2),
                             labels = c("No", "Si"))

datoscsv$tieneaval <- factor(datoscsv$tieneaval, levels = c(1, 2),
                             labels = c("No", "Si"))

datoscsv$tiporenta <- factor(datoscsv$tiporenta, levels = c(2, 3),
                             labels = c("Fijo", "Variable"))

datoscsv$dpto      <- factor(datoscsv$dpto, 
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c("Lima", "Trujillo", "Arequipa",
                                        "Cusco", "Ica", "Piura"))

# datoscsv$morosidad <- factor(datoscsv$morosidad, levels = c(1, 2),
#                              labels = c("No Moroso", "Moroso"))

str(datoscsv)

# Tabla cruzada

table(datoscsv$dpto, datoscsv$morosidad)

table(datoscsv$morosidad, datoscsv$dpto)

# table morosidad y tipo de renta 

# morosidad 1: no moroso, 
# morosidad 2: moroso 

#### 1.1 Datos *.TXT ####

                                                      

datost <- read.table("../../data/Riesgo_morosidad.dat",
                     sep = "\t", header = F)
                     

# primera fila como nombre de las columnas 

datost <- read.table("../../data/Riesgo_morosidad.dat",
                     sep = "\t",
                     header = T)

str(datost)

# read.table() puede leer archivos *.csv

datoscsv2 <- read.table("../../data/Riesgo_morosidad.csv",
                        sep = ";",
                        header = T)


str(datoscsv2)

# Uso read.delim para la lectura de datos delimitados
# En este caso los datos estan delimitados por tab \t

datosd <- read.delim("../../data/Riesgo_morosidad.dat",
                     sep = "\t",
                     header = TRUE)
str(datosd)


#### 1.2 Datos SPSS *.sav ####

# read.spss de la libreria foreign 
# permite asignar las etiquetas como datos 
value_label <- read.spss("../../data/Riesgo_morosidad.sav"
                      )

# etiqueta de valores

attributes(value_label)$label.table

# load dataset as datafrmae

datospss <- read.spss("../../data/Riesgo_morosidad.sav",
                      use.value.labels = F, 
                      to.data.frame = TRUE)

attributes(datospss)$variable.labels  # etiqueta de variable 



# encoding: detectar el tipo de numero y letras 



#### 1.3 Datos EXCEL *.XLS *.XLSX ####

datos1_xls  <- read_excel("../../data/Riesgo_morosidad.xls")
datos2_xls  <- read_xls("../../data/Riesgo_morosidad.xls")

datos1_xlsx <- read_excel("../../data/Riesgo_morosidad.xlsx")
datos2_xlsx <- read_xlsx("../../data/Riesgo_morosidad.xlsx")

str(datos1_xlsx)
str(datos1_xls)

datos1_xlsx <- as.data.frame(datos1_xlsx)
str(datos1_xlsx)



# Lectura de caracteres especiales 

# UTF-8 es el encoding universal 

netflix <- read.csv("../../data/netflix_titles.csv")
                    
                    
netflix <- read.csv("../../data/netflix_titles.csv", encoding = "UTF-8")


netflix <- read.csv("../../data/netflix_titles.csv", encoding = "UTF-8",
                    na.strings=c("",NA)) 

# NA

# na.strings=c("",NA) reemplaza vacios en las variables character (texto) por missing 

#--------------------------------------------#

bbdd <- paises # dataset from "datos" library

write.csv(bbdd, "../../data/paises.csv", row.names = F)

# row.names = F no guarda el indexing de filas

# Base de datos en formato tibble

str(bbdd) # tipo de variable 

View(bbdd) # visualización de la base de datos 

dim(bbdd) # dimensiones de la base de datos

ncol(bbdd) # numero de columnas
nrow(bbdd) # numero de filas

sapply(bbdd, class)  # tipo de varaible 

summary(bbdd)   # principales estadisticas descriptivas

sum(is.na(bbdd))  # ningun missing en la base país

sapply(bbdd, function(x) sum(is.na(x))) # check missing all variables 

table(bbdd$continente) # tabla

table( bbdd$anio, bbdd$continente) # tabla cruzada

#------------------------------------------------#


paises.d <- as.data.frame(bbdd) # formato data.frame
str(paises.d)

# Limpieza base de datos -------------------

### 2.1 Filtro ----------------------

# dplyr 

bbdd_1957 <- filter(bbdd, anio == 1957)  # > >= < <=  & | == !=
filter(bbdd, anio == 1957) -> bbdd_1957


# guardar la base de datos filtrada

# usando pip y las funciones de la libreria dplyr 

bbdd %>% filter(anio == 1957)   # Es equivalente

filter(bbdd, anio != 1957)

#-------------------------------#

bbdd %>% filter(pais == "China" & anio == 2002)

bbdd %>% filter(pais == "China", anio == 2002) # Es equivalente

bbdd %>% filter(pais == "Chile" | pais == "Perú") %>% View

bbdd %>% filter(pais == "Perú", anio == 1997 | anio == 2002 | anio == 2007)

bbdd %>% filter(pais == "Perú", anio %in% c(1997, 2002, 2007)) %>% 
  View # Es equivalente


### 2.2 Ordenando datos ------------------------------------------

#  Ordenando observaciones según la esperanza de vida

bbdd %>% arrange(esperanza_de_vida) %>% 
  View # Por defecto ascendente

bbdd %>% arrange(desc(esperanza_de_vida)) # ordenamiento descendente

bbdd %>% arrange(-esperanza_de_vida) %>% 
  View# Equivalente al anterior


#  Filtrando y ordenando

bbdd %>% filter(anio == 1957) %>% 
  arrange(desc(esperanza_de_vida)) 


#      Ejercicio    #
# Muestre la información de Europa del año 1987 
# ordenamiento según el PIB percapita de manera descendente

### 2.3 Creación de variable --------

# Usando mutate para cambiar o crear una columna

# esperanza de vida en meses
# mutate es de la libreria dplyr 

bbdd %>% mutate(esperanza_de_vida_meses = 12*esperanza_de_vida,
                pbipc_miles = pib_per_capita/1000)


#      Ejercicio    #

# población en millones, año 2007, países de áfrica, orden de forma ascendente (pob)



### 2.4 Selección variables  ----------------------

#bbdd[filtro de filas, filtro de columnas]

bbdd[ , c(1, 3, 5)]
bbdd[ , 1:3]
bbdd[ , c(1:3, 5)]

bbdd[c(1, 3, 5)]
bbdd[1:3]
bbdd[c(1:3, 5)]

bbdd[ , c("pais", "anio", "poblacion")]

# desde tercera columna hasta el final 

bbdd[,3:ncol(bbdd)]

# hasta la tercera columna

bbdd[,1:3]

bbdd[,1:3]

#seleccionar ultima columna ?

# selecting rows #

bbdd[1:100, ] # 100 primeras observaciones

bbdd[c(1,100,50,25), ]

bbdd[ c(100:200), c("pais", "anio", "poblacion")]


# using dplyr for selecting 

bbdd %>% select(pais, anio, poblacion)

bbdd  %>% select(pais, poblacion, anio) %>% 
  slice(100:n())  # desde la observación hasta la final

bbdd  %>% select(pais, poblacion, anio) %>% 
  slice(100:150) 


### 2.5 Rename() ------------------------

paises2 <- bbdd %>% dplyr::rename(Pais = pais, Año = anio, 
                             PBI = pib_per_capita) %>%
  dplyr::select(Año, Pais, PBI)

paises2

# write permite guardar bases de datos

write.csv(paises2, "../../data/Paises_renombrados.csv")

write.csv(paises2, "../../data/Paises_renombrados.csv", row.names = F)


# borrar el indexing de las filas en el archivo csv

### 2.6 Drop variables -----------------

bbdd$var1 <- 1000
bbdd$var2 <- "Indicadores"


bbdd[, ! names(bbdd) %in% c('var1', 'var2')] -> bbdd


bbdd$var <- 1000
bbdd$var <- NULL  # borrar variable de forma rapida 

bbdd$var <- NULL

# Ordenar la posición de las variables 

bbdd %>% select(anio, continente, pais, everything()) %>% View()



#----------------------------#
### 2.7 Groupby (agrupar) -------------
#----------------------------#

# Tendencia Central: mean(), median()
# Dispersión: sd()
# Rango: min(), max(), quantile()
# Posición: first(), last()
# Conteo: n(), n_distinct()
# Lógica: any(), all()
# aritmetica: sum()

### 2.8 Resumiendo con summarise() y count() ---------------------

# Resumiendo la esperanza de vida

bbdd %>% summarise(mean(esperanza_de_vida))   # summarize()

# Añadiendo una etiqueta 

bbdd %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida))

# Resumiendo la esperanza de vida en 2007
bbdd %>% filter(anio == 2007) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida))

bbdd %>% filter(anio%in% c(1957,2007)) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida),
            max_pib_per_capita = max(pib_per_capita))


bbdd %>% filter(between(anio, 1950, 1990)) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida),
            max_pib_per_capita = max(pib_per_capita))


# Resumiendo por año y continente

bbdd %>% dplyr::group_by(anio, continente) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida)) %>%
  View

bbdd %>% dplyr::group_by(anio, continente) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida), 
            sd_esperanza_vida = sd(esperanza_de_vida)) %>%
  View


# Número de países por continente en 2007
bbdd %>% filter(anio == 2007) %>% 
  dplyr::group_by(continente) %>% summarise(n())

bbdd %>% filter(anio == 2007) %>% 
  dplyr::group_by(continente) %>% count()    # Es equivalente

bbdd %>% filter(anio == 2007) %>% 
  dplyr::group_by(continente) %>% summarise(Número_Paises = n()) 


# Añadiendo el summarise a la base de datos 

bbdd %>% dplyr::group_by(pais) %>% 
  summarise(mean_pbipc_pais = mean(pib_per_capita)) 


bbdd %>% dplyr::group_by(pais) %>% 
  mutate(mean_pbipc_pais = mean(pib_per_capita),
         dif_pbipc = pib_per_capita- mean_pbipc_pais) 


bbdd %>% dplyr::group_by(continente, anio) %>% 
  mutate(mean_pbipc_continente = mean(pib_per_capita),
         dif_pbipc = pib_per_capita - mean_pbipc_continente) %>% 
  filter(pais =="Perú") %>% View()



clean_data <- bbdd %>% dplyr::group_by(pais) %>% 
  mutate(mean_pbipc_pais = mean(pib_per_capita)) %>% ungroup() %>% 
  group_by(continente) %>%  mutate(median_pob = median(poblacion)) %>% 
  as.data.frame()


# |> alterantiva de pip 

clean_data <- bbdd |> dplyr::group_by(pais) |>
  mutate(mean_pbipc_pais = mean(pib_per_capita)) |> ungroup() |>
  group_by(continente) |> mutate(median_pob = median(poblacion)) |>
  as.data.frame()

# summrise del pbi percapita de los paises de las Américas, mediana, minimo y máximo



# References ####


browseURL("https://allisonhorst.shinyapps.io/edge-of-the-tidyverse/#section-welcome")















