                                                          # Tarea 5 ----

## Preparando el ambiente de trabajo ----
# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library

install.packages("pacman")
library(pacman) 
# permite llamar a varias librerias de manera simultánea
# Si la librería no está instalada, entonces lo instala y llama para su uso

p_load(dplyr, readxl, tidyverse, foreign, datos) 

p_load(readxl, tidyverse, foreign, ggthemes, datos)

                                                      ### Ejercicio 1.1 ####


getwd()
setwd("/Users/lala/Documents/GitHub")

# usamos la base de datos economics de la libreria ggplot

library(readxl)
produccion_coca <- read_excel("Mariale_repositorio/Curso_Python_R/data/produccion_coca/6.1.1_-_Illicit_coca_bush_cultivation.xlsx")

# borramos todas las observaciones que no son necesarias
produccion_coca2 <- produccion_coca[-c(1:3, 7:12),]
produccion_coca3 <- produccion_coca2[,-1]

## Creamos una nueva variable con los años
produccion_coca3[nrow(produccion_coca3) + 1,] <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# Colacamos nombres a las columnas y filas
row.names(produccion_coca3) <- c('Bolivia', 'Colombia', 'Perú', 'Año')

## Creamos una nueva variable con los años
produccion_coca3[nrow(produccion_coca3) + 1,] <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

## Transponemos la matriz para poder graficarla
produccion_coca_T <- transpose(produccion_coca3)

## Generamos un dataframe a partir del nested list que ha producido transposición
produccion_coca_TDF <- as.data.frame(do.call(cbind, produccion_coca_T))

## Volvemos a colocar los nombres
colnames(produccion_coca_TDF) <- c('Bolivia', 'Colombia', 'Perú', 'Año')


## Formateamos las variables para que permita realizar los graficos

str(produccion_coca_TDF)
produccion_coca_TDF$Año <- as.Date(paste(produccion_coca_TDF$Año, "01-01", sep = "-"), format = "%Y-%m-%d")
produccion_coca_TDF$Bolivia <- as.numeric(produccion_coca_TDF$Bolivia)
produccion_coca_TDF$Perú <- as.numeric(produccion_coca_TDF$Perú)
produccion_coca_TDF$Colombia <- as.numeric(produccion_coca_TDF$Colombia)

## Colocamos los codigos para hacer los graficos
ggplot(produccion_coca_TDF) +
  aes(x = Año) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed") +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid") +
  geom_line(aes(y = Perú, color = "Perú"), size = 0.6, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Años", y = "Producción de coca (Hectáreas)")
  scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                    labels = c("Bolivia", "Colombia", "Perú"))
  

                                                   ### Ejercicio 1.2 ####
  
getwd()
setwd("/Users/lala/Documents/GitHub")
  
# usamos la base de datos economics de la libreria ggplot
  
library(readxl)
erradicacion_coca <- read_excel("Mariale_repositorio/Curso_Python_R/data/produccion_coca/6.1.2_-_Eradication_of_coca_bush.xlsx")
  
# borramos todas las observaciones que no son necesarias
erradicacion_coca2 <- erradicacion_coca[-c(1, 5:17),]
erradicacion_coca3 <- erradicacion_coca2[,-(1:3)]
erradicacion_coca4 <- erradicacion_coca3[,-13]

## Creamos una nueva variable con los años
erradicacion_coca4[nrow(erradicacion_coca4) + 1,] <- list(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

# Colacamos nombres a las columnas y filas
row.names(erradicacion_coca4) <- c('Bolivia', 'Colombia', 'Perú', 'Año')

## Transponemos la matriz para poder graficarla
erradicacion_coca_T <- transpose(erradicacion_coca4)

## Generamos un dataframe a partir del nested list que ha producido transposición
erradicacion_coca_TDF <- as.data.frame(do.call(cbind, erradicacion_coca_T))

## Volvemos a colocar los nombres
colnames(erradicacion_coca_TDF) <- c('Bolivia', 'Colombia', 'Perú', 'Año')

## Formateamos las variables para que permita realizar los graficos

str(erradicacion_coca_TDF)
erradicacion_coca_TDF$Año <- as.Date(paste(erradicacion_coca_TDF$Año, "01-01", sep = "-"), format = "%Y-%m-%d")
erradicacion_coca_TDF$Bolivia <- as.numeric(erradicacion_coca_TDF$Bolivia)
erradicacion_coca_TDF$Perú <- as.numeric(erradicacion_coca_TDF$Perú)
erradicacion_coca_TDF$Colombia <- as.numeric(erradicacion_coca_TDF$Colombia)

## Colocamos los codigos para hacer los graficos
ggplot(erradicacion_coca_TDF) +
  aes(x = Año) +
  geom_line(aes(y = Bolivia, color = "Bolivia" ), size = 0.6, linetype = "dashed") +
  geom_line(aes(y = Colombia, color = "Colombia"), size = 0.6, linetype = "solid") +
  geom_line(aes(y = Perú, color = "Perú"), size = 0.6, linetype = "dashed") +
  theme_minimal() +
  labs(x = "Años", y = "Producción de coca (Hectáreas)")
scale_color_manual(values = c("Bolivia" = "grey", "Colombia" = "darkolivegreen3", "Perú" ="firebrick2"),
                   labels = c("Bolivia", "Colombia", "Perú"))


                                                ### Ejercicio 1.3 ####

# Primero eliminamos las variables que no vamos a utilizar y optamos por reemplazar el nombre de la variable restante para no confundirla
produccion_coca_TDF_peru <- produccion_coca_TDF[,-(1:2)]
colnames(produccion_coca_TDF_peru)[1] ="producción"

# Hacemos lo mismo con el segundo dataframe
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
