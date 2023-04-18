################  laboratorio 2 ############################
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

library(pacman)  # permite llamar a varias librerias de manera simultánea

p_load(dplyr, readxl, tidyverse, datos) 

# Si la librería no está instalada, entonces lo instala y llama para su uso

# tidyverse es una recopilación de varias librerias (dply, ggplot, stringr, etc)

# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


bbdd <- paises # dataset from "datos" library

write.csv(bbdd, "../../data/paises.csv", row.names = F)


# Base de datos en formato tibble

str(bbdd) # tipo de variable 

View(bbdd) # visualización de la base de datos 

dim(bbdd) # dimensiones de la base de datos

sapply(bbdd, class)  # tipo de varaible 

summary(bbdd)   # principales estadisticas descriptivas

sum(is.na(pais))  # ningun missing en la base país

sapply(bbdd, function(x) sum(is.na(x))) # check missing all variables 

table(bbdd$continente) # tabular

table( bbdd$anio, bbdd$continente) # tabular cruzado

#------------------------------------------------#


paises.d <- as.data.frame(bbdd) # formato data.frame
str(paises.d)

# 1.1 Filtro ----------------------

filter(bbdd, anio == 1957)  # > >= < <=  & | == !=

bbdd_1957 <- filter(bbdd, anio == 1957) 

# guardar la base de datos filtrada

# usando pip y las funciones de la libreria dplyr 

bbdd %>% filter(anio == 1957)   # Es equivalente

filter(bbdd, anio != 1957)

#-------------------------------#

bbdd %>% filter(pais == "China" & anio == 2002)

bbdd %>% filter(pais == "China", anio == 2002) # Es equivalente

bbdd %>% filter(pais == "Chile" | pais == "Perú") %>% View

bbdd %>% filter(pais == "Perú", anio == 1997 | anio == 2002 | anio == 2007)

bbdd %>% filter(pais == "Perú", anio %in% c(1997, 2002, 2007))  # Es equivalente


# 1.2 Ordenando datos ------------------------------------------

#  Ordenando observaciones según la esperanza de vida

bbdd %>% arrange(esperanza_de_vida)  # Por defecto ascendente

bbdd %>% arrange(desc(esperanza_de_vida)) # ordenamiento descendente

bbdd %>% arrange(-esperanza_de_vida) # Equivalente al anterior


#  Filtrando y ordenando

bbdd %>% filter(anio == 1957) %>% 
  arrange(desc(esperanza_de_vida)) 


#      Ejercicio    #
# Muestre la información de Europa del año 1987 ordenada 
# según el PIB de manera descendente


# 1.3 Creación de variable --------

# 5.1. Usando mutate para cambiar o crear una columna

# esperanza de vida en meses

bbdd %>% mutate(esperanza_de_vida = 12*esperanza_de_vida)

bbdd %>% mutate(esperanza_de_vida_meses = 12*esperanza_de_vida)


#      Ejercicio    #


# población en millones, año 2007, países de áfrica


# 1.4 Selección variables  ----------------------

bbdd[ , c(1, 3, 5)]
bbdd[ , 1:3]
bbdd[ , c(1:3, 5)]

bbdd[c(1, 3, 5)]
bbdd[1:3]
bbdd[c(1:3, 5)]

bbdd[ , c("pais", "anio", "poblacion")]

# selecting rows #

bbdd[1:100, ] # 100 primeras observaciones

bbdd[c(1,100,50,25), ]

bbdd[ c(100:200), c("pais", "anio", "poblacion")]

indices <- split(seq(nrow(bbdd)), sort( seq(nrow(bbdd)) %% 3 ) )
# indexing de las filas agrupado en 3 
indices

# add names to each vector

names(indices) <- c('training', 'est', 'test') ## add labels 

indices$est
indices$test
indices$training

bbdd[indices$training,]  # observaciones de trainning 
bbdd[indices$test,]  # observaciones de test


# using dplyr for selecting 

bbdd %>% select(pais, anio, poblacion)

bbdd  %>% select(pais, poblacion, anio) %>% 
  slice(100:n())  # desde la observación hasta la finaln

bbdd  %>% select(pais, poblacion, anio) %>% 
  slice(100:150) 


# 1.5 Rename() ------------------------

paises2 <- bbdd %>% rename(Pais = pais, Año = anio, 
                             PBI = pib_per_capita) %>%
  select(Año, Pais, PBI)

paises2

# write permite guardar bases de datos

write.csv(paises2, "../../data/Paises_renombrados.csv")

write.csv(paises2, "../../data/Paises_renombrados.csv", row.names = F)


# borrar el indexing de las filas en el archivo csv

# 1.6 Drop variables -----------------

bbdd$var1 <- 1000
bbdd$var2 <- "Indicadores"


bbdd[, ! names(bbdd) %in% c('var1', 'var2')] -> bbdd


bbdd$var <- 1000
bbdd$var <- NULL  # borrar variable de forma rapida 


# Ordenar la posición de las variables 

bbdd %>% select(anio, continente, pais, everything()) %>% View()



#----------------------------#
# 1.7 Groupby (agrupar) -------------
#----------------------------#

# Tendencia Central: mean(), median()
# Dispersión: sd()
# Rango: min(), max(), quantile()
# Posición: first(), last()
# Conteo: n(), n_distinct()
# Lógica: any(), all()
# aritmetica: sum()

# 1.8 Resumiendo con summarise() y count() ---------------------

# Resumiendo la esperanza de vida

mean(esperanza_de_vida)

# la variable esperanza_de_vida no existe 


attach(bbdd) # cada columna es una variable 

mean(esperanza_de_vida)

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


# Resumiendo por año
bbdd %>% group_by(anio) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida))

bbdd %>% group_by(anio, continente) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida)) %>%
  View

bbdd %>% group_by(anio, continente) %>% 
  summarise(mean_esperanza_de_vida = mean(esperanza_de_vida), 
            sd_esperanza_vida = sd(esperanza_de_vida)) %>%
  View


# Número de países por continente en 2007
bbdd %>% filter(anio == 2007) %>% 
  group_by(continente) %>% summarise(n())

bbdd %>% filter(anio == 2007) %>% 
  group_by(continente) %>% count()    # Es equivalente

bbdd %>% filter(anio == 2007) %>% 
  group_by(continente) %>% summarise(Número_Paises = n()) 


# Añadiendo el summarise a la base de datos 

clean_data <- bbdd %>% group_by(pais) %>% 
  mutate(mean_pbipc_pais = mean(pib_per_capita)) %>% ungroup() %>% 
  group_by(continente) %>%  mutate(median_pob = median(poblacion)) %>% 
  as.data.frame()

# |> alterantiva de pip 

clean_data <- bbdd |> group_by(pais) |>
  mutate(mean_pbipc_pais = mean(pib_per_capita)) |> ungroup() |>
  group_by(continente) |> mutate(median_pob = median(poblacion)) |>
  as.data.frame()






















# References ####


browseURL("https://allisonhorst.shinyapps.io/edge-of-the-tidyverse/#section-welcome")














