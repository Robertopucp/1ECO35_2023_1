#Mapas (R studio) - Grupo 9

#Primero realizamos una limpieza general del environment:
rm(list = ls())
# clean plots
graphics.off()
# clean console
cat("\014")
# additional options
options(scipen = 999)      # No scientific notation
options(warn = -1) 

#Segundo, cargamos las siguientes librerías:
library(pacman) 

p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , sf # provides a set of tools for reading, writing, manipulating, and visualizing spatial data
  , units
)

#Cambiamos el directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#El ejercicio nos pide Replicar la figura 1 (p. 261) y figura 2 (p. 263) del paper Land Reform and Civil Conflict: Theory and Evidence from Peru de Michael Albertus
#Para replicar la figura 1, primero, abrimos la base de datos:
library(sf)
ruta_distritos <- "../../data/trabajo_final/MAPAS/districts_1975_remake.shp"
distritos <- st_read(dsn = ruta_distritos)
plot(distritos)

#Luego, abrimos la ruta de departamentos: 
ruta_departamentos <- "../../data/trabajo_final/MAPAS/department_peru.shp"
departamentos <- st_read(dsn = ruta_departamentos)
plot(departamentos)

#Abrimos la capa "agrozone core2 polygons"
ruta_geopandadatabase <- "../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb"
capa_agrozone <- st_layers(dsn = ruta_geopandadatabase)
agrozone_core2 <- st_read(dsn = ruta_geopandadatabase, layer = capa_agrozone$name[1])
plot(agrozone_core2)

#Abrimos la capa "agrarian zones polygons"
ruta_geopandadatabase <- "../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb"
capa_agrarian_zones <- st_layers(dsn = ruta_geopandadatabase)
agrarian_zones <- st_read(dsn = ruta_geopandadatabase, layer = capa_agrarian_zones$name[1])
plot(agrarian_zones)


