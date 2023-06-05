#TAREA 5

rm(list = ls())   # clean environment variables
graphics.off()    # clean plots
cat("\014")       # clean console
options(scipen = 999)   # additional options

install.packages("pacman")   # Library
library(pacman) 


p_load(dplyr, readxl, tidyverse, foreign, datos) 

p_load(readxl, tidyverse, foreign, ggthemes, datos)

##PLOT
###Ejercicio 1:

getwd()
setwd("/Users/Gamer/Documents/GitHub")

library(readxl)
produccion_coca <- read_excel("/Users/Gamer/Documents/GitHub/1ECO35_2023_1/data/produccion_coca/6.1.1_-_Illicit_coca_bush_cultivation.xlsx")

###Mantenemos lo que necesitaremos
produccion_coca2 <- produccion_coca[-c(1:3, 7:12),]
produccion_coca3 <- produccion_coca2[,-1]


### Se crea una variable nueva que tenga los años
produccion_coca3[nrow(produccion_coca3) + 1,] <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

### Nombramos columnas y filas
row.names(produccion_coca3) <- c('Bolivia', 'Colombia', 'Perú', 'Año')

### Se crea una variable nueva que tenga los años
produccion_coca3[nrow(produccion_coca3) + 1,] <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

### Se Transpone la matriz para poder graficarla
produccion_coca_T <- transpose(produccion_coca3)

### Generamos un dataframe a partir del nested list que ha producido transposición
produccion_coca_TDF <- as.data.frame(do.call(cbind, produccion_coca_T))

### Colocamos los nombres
colnames(produccion_coca_TDF) <- c('Bolivia', 'Colombia', 'Perú', 'Año')


### Formateamos las variables para que permita realizar los graficos

str(produccion_coca_TDF)
produccion_coca_TDF$Año <- as.Date(paste(produccion_coca_TDF$Año, "01-01", sep = "-"), format = "%Y-%m-%d")
produccion_coca_TDF$Bolivia <- as.numeric(produccion_coca_TDF$Bolivia)
produccion_coca_TDF$Perú <- as.numeric(produccion_coca_TDF$Perú)
produccion_coca_TDF$Colombia <- as.numeric(produccion_coca_TDF$Colombia)

## Finalmente ponemos los codigos para hacer los graficos
ggplot(produccion_coca_TDF) +
  aes(x = Año) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed") +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid") +
  geom_line(aes(y = Perú, color = "Perú"), size = 0.6, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Años", y = "Producción de coca (Hectáreas)")
scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                   labels = c("Bolivia", "Colombia", "Perú"))


###Ejercicio 2:

getwd()
setwd("/Users/Gamer/Documents/GitHub")

# usamos la base de datos economics de la libreria ggplot

library(readxl)
erradicacion_coca <- read_excel("/Users/Gamer/Documents/GitHub/1ECO35_2023_1/data/produccion_coca/6.1.2_-_Eradication_of_coca_bush.xlsx")

###Mantenemos lo que necesitaremos
erradicacion_coca2 <- erradicacion_coca[-c(1, 5:17),]
erradicacion_coca3 <- erradicacion_coca2[,-(1:3)]
erradicacion_coca4 <- erradicacion_coca3[,-13]

### Se crea una variable nueva que tenga los años
erradicacion_coca4[nrow(erradicacion_coca4) + 1,] <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

### Nombramos filas y columnas
row.names(erradicacion_coca4) <- c('Bolivia', 'Colombia', 'Perú', 'Año')

### Transponemos la matriz para poder graficarla
erradicacion_coca_T <- transpose(erradicacion_coca4)

### Generamos un dataframe a partir del nested list que ha producido transposición
erradicacion_coca_TDF <- as.data.frame(do.call(cbind, erradicacion_coca_T))

### Colocamos los nombres
colnames(erradicacion_coca_TDF) <- c('Bolivia', 'Colombia', 'Perú', 'Año')

### Formateamos las variables para que permita realizar los graficos

str(erradicacion_coca_TDF)
erradicacion_coca_TDF$Año <- as.Date(paste(erradicacion_coca_TDF$Año, "01-01", sep = "-"), format = "%Y-%m-%d")
erradicacion_coca_TDF$Bolivia <- as.numeric(erradicacion_coca_TDF$Bolivia)
erradicacion_coca_TDF$Perú <- as.numeric(erradicacion_coca_TDF$Perú)
erradicacion_coca_TDF$Colombia <- as.numeric(erradicacion_coca_TDF$Colombia)

###Finalmente, graficamos.
ggplot(erradicacion_coca_TDF) +
  aes(x = Año) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed") +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid") +
  geom_line(aes(y = Perú, color = "Perú"), size = 0.6, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Años", y = "Producción de coca (Hectáreas)")
scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                   labels = c("Bolivia", "Colombia", "Perú"))

## Ejercicio 3:

####Mantenemos lo que necesitaremos
produccion_coca_TDF_peru <- produccion_coca_TDF[,-(1:2)]
colnames(produccion_coca_TDF_peru)[1] ="producción"

###Hacemos lo mismo con el segundo dataframe
erradicacion_coca_TDF_peru <- erradicacion_coca_TDF[,-(1:2)]
colnames(erradicacion_coca_TDF_peru)[1] ="erradicacion"

# unimos ambos dataframes
Peru <- merge(produccion_coca_TDF_peru,erradicacion_coca_TDF_peru,by="Año")

## Formateamos las variables para que permita realizar los graficos
str(Peru)
Peru$Año <- as.Date(paste(erradicacion_coca_TDF$Año, "01-01", sep = "-"), format = "%Y-%m-%d")
Peru$erradicacion <- as.numeric(Peru$erradicacion)
Peru$producción <- as.numeric(Peru$producción)

## Colocamos los codigos para hacer los graficos
ggplot(Peru) +
  aes(x = Año) +
  geom_line(aes(y = erradicacion), size = 0.6, color = "darkolivegreen3", linetype = "dashed") +
  geom_line(aes(y = producción), size = 0.6, color = "firebrick2", linetype = "dashed") 
theme_minimal() +
  labs(x = "Años", y = "Producción/Erradicación de coca (Hectáreas)")
scale_color_manual(values = c("erradicació " = "darkolivegreen3", "produccion" ="firebrick2"),
                   labels = c("erradicacion", "produccion"))

#Regex

## Ejercicio 1

#abrimos nuestra base de datos
library(readxl)
data2 <- read_excel("../../data/metropolitano.xlsx")

#Creamos una función para convertir las coordenadas (grados, minutos y segundos) a latitud y longitud
library(stringr)
convert_gps <- function(x) {
  signo <- ifelse(str_detect(x, '[swSWoO-]'), -1, 1)
  regex <- "\\s?(\\d+)°\\s*(\\d+)'\\s*(\\d+(?:\\.\\d+)?)"
  match <- str_match(x, regex)
  
  if (!is.na(match[1, 1])) {
    grados <- as.numeric(match[1, 2])
    minutos <- as.numeric(match[1, 3])
    segundos <- as.numeric(match[1, 4])
    
    return(signo * (grados + minutos / 60 + segundos / 3600))
  } else { return(NULL) }}

#Aplicamos la función a nuestra data2
data2$latitud <- sapply(data2$sur_latitud, convert_gps)
data2$longitud <- sapply(data2$oeste_longitud, convert_gps)

#vemos los resultados
data2
