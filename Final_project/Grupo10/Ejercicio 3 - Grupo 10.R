#Ejercicio 3

#Cargamos las librerías necesarias
rm(list = ls())


graphics.off()


cat("\014")



options(scipen = 999)     

options(warn = -1) 

library(pacman) 


p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , sf
  , haven 
  ,viridis
)

#Creamos el directorio de trabajo

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Cargamos las bases necesarias para la primera figura
departamentos <- st_read('../../data/trabajo_final/MAPAS/department_peru.shp')
distritos <- st_read('../../data/trabajo_final/MAPAS/districts_1975_remake.shp')

#Se asignan características como color de relleno, color y ancho de la línea

plot <- ggplot() + 
  theme_void()  

plot + geom_sf(data = departamentos,
               fill = "grey",
               color = "black",
               size = 1.5) +
  geom_sf(data = distritos, 
          color = "black", 
          fill = NA, size = 0.1, alpha = 0.8) 

