# R script 1

rm(list = ls())

graphics.off()

cat("\014")

options(scipen = 999)      

library(pacman) 

p_load(dplyr, readxl, tidyverse, foreign, datos) 

setwd("C:/Users/Ademir/Documents/GitHub/1ECO35_2023_1/data")

getwd()

#Profesor, he intentado utilizar el siguiente comando para crear el directorio:
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()

#Sin embargo, al intentar correrlo me sale un error y no permite jalar datos
#de la carpeta que se solicita, así que lo dejo indicado.
#Entiendo que lo correcto es usar el código que nos indicó en clase, pero 
#no me permite avanzar, por lo que cree un directorio con la ruta de mi
#computadora al espacio de trabajo. Espero pueda comprender. 

#Primero cargamos la base

datosactos <- read.spss("actos_est.sav",
                      use.value.labels = F, 
                      to.data.frame = TRUE)

#Se muestran las etiquetas

str(datosactos)

#Filtramos por eventos de desaparición, secuestros, reclutamiento forzado y 
#muertes en atentados

d1 <- datosactos %>% filter(LDS_LDT == 1 | LSE == 1 | LRC == 1 | MAT == 1)

#Borramos los duplicados de la columna IDACTO 

d2 <- d1 %>% group_by(IDACTO) %>% filter(!duplicated(IDACTO))

#Seleccionamos las variables relevantes para el análisis

d3 <- d2 %>% select(IDACTO, LDS_LDT, LSE, LRC, MAT, DEPNA0, UBIDIST, PERIODO) 

#Se crea una base con el total de eventos de violencia según su tipo a 
#nivel de departamento. Se agregan las columnas solicitadas con los
#resultados

d4 <- d3 %>% dplyr::group_by(DEPNA0) %>% 
  mutate(desaparecidos_por.dpto = sum(LDS_LDT)) %>%
  mutate(secuestrados_por.dpto = sum(LSE)) %>%
  mutate(reclutados_pordpto = sum(LRC)) %>%
  mutate(muertoporatentado_por.dpto = sum(MAT)) %>%
  mutate(eventos_por.dpto = sum(LDS_LDT) + sum(LSE) + sum(LRC) + sum(MAT))

#Se crea una base con el total de eventos de violencia según su tipo a 
#nivel de distrito. Y se crea una variable con el logaritmo del total de eventos
#Se agregan las columnas solicitadas con los resultados

d5 <- d3 %>% dplyr::group_by(UBIDIST) %>% 
  mutate(desaparecidos_por.dist = sum(LDS_LDT)) %>%
  mutate(secuestrados_por.dist = sum(LSE)) %>%
  mutate(reclutados_por.dist = sum(LRC)) %>%
  mutate(muertoporatentado_por.dist = sum(MAT)) %>%
  mutate(log_eventos_por.dist = log(sum(LDS_LDT) + sum(LSE) + sum(LRC) + sum(MAT)))

#Se crea una base con el total de eventos de violencia según su tipo a 
#nivel de departamento y periodo. Se agregan las columnas solicitadas con los
#resultados

d6 <- d3 %>% dplyr::group_by(DEPNA0, PERIODO) %>% 
  mutate(desaparecidos = sum(LDS_LDT)) %>%
  mutate(secuestrados = sum(LSE)) %>%
  mutate(reclutados = sum(LRC)) %>%
  mutate(muertoporatentado = sum(MAT)) %>%
  mutate(eventos = sum(LDS_LDT) + sum(LSE) + sum(LRC) + sum(MAT))








