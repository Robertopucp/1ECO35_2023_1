
################  Trabajo Final ##########------------------
## Curso: Laboratorio de R y Python ###
## @author: Grupo 4


# clean environment variables
rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options

options(scipen = 999)      # No scientific notation

options(warn = -1) 

# Library ####

# Load libraries ----

library(pacman) 

p_load(
  tidyverse
  , raster
  , sf
  , rgdal
)


#Set working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------#


distritos_shp <- st_read(
  '../../data/trabajo_final/MAPAS/districts_1975_remake.shp'
)

# Centroide del distrito 
distritos_geo$Point_centroid <-st_centroid(data_geo)['geometry']


distritos_shp$Point_centroid <-st_centroid(distritos_shp)['geometry']

# coordenadas en columnas separadas

distritos_shp$longitude <- st_coordinates(distritos_shp$Point_centroid)[, 1]
distritos_shp$latitude <- st_coordinates(distritos_shp$Point_centroid)[, 2]


# ------------------------------------------

# Al estilo de Chat gpt

# Cargar el shapefile de distritos
distritos <- st_read('../../data/trabajo_final/MAPAS/districts_1975_remake.shp'
)

  
# Cargar el shapefile de los departamentos
departamentos <- st_read('../../data/trabajo_final/MAPAS/department_peru.shp'
)


# Cargar las capas de Agrarian Core Zone y Agrarian Zone desde el geodatabase

st_layers("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb")  # lista de layers 


# se extrae el layer particular.
# Distritos de interes alrededor del espacio MITA

agrarian <- st_read("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb", layer = "agrarian_zones_polygons")

st_crs(agrarian)

str(agrarian)

agrarian |> ggplot() +
  geom_sf()

## Se extrae el otro layer particular 
agrozone <- st_read("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb", layer = "agrozone_core2_polygons")

st_crs(agrozone)

str(agrozone)

agrozone |> ggplot() +
  geom_sf()

# grafuci

ggplot() +
  geom_sf(data = distritos, fill = "grey", color = "black") +
  geom_sf(data = departamentos, fill = "white", color = "black") +
  geom_sf(data = agrozone, fill = "red", alpha = 0.5) +
  geom_sf(data = agrarian, fill = "blue", alpha = 0.5) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "") +
  scale_fill_manual(values = c("grey", "white", "red", "blue"),
                    labels = c("Distritos", "Departamentos", "Agrarian Core Zone", "Agrarian Zone"))

# grafico


# Crear el gráfico
ggplot() +
  geom_sf(data = distritos, fill = "grey", color = "black") +
  geom_sf(data = departamentos, fill = "white", color = "black") +
  geom_sf(data = agrozone, fill = "grey", color = "black") +
  geom_sf(data = agrarian, fill = "white", color = "black") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(color = "black"),
        panel.background = element_rect(color = "black")) +
  labs(fill = "") +
  scale_fill_manual(values = c("grey", "white"),
                    labels = c("Agrarian Zone Core", "Agrarian Zone")) +
  guides(fill = guide_legend(override.aes = list(color = c("black", "black", "black", "black"),
                                                 linetype = c(0, 0, 1, 1),
                                                 shape = c(15, 15, NA, NA),
                                                 fill = c("white", "white", "grey", "white"))),
         title = "Legend",
         title.position = "top",
         title.theme = element_text(size = 12, face = "bold"),
         label.theme = element_text(size = 10))

