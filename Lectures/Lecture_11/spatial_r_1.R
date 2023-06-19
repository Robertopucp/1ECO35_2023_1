################  Laboratorio 11 ------------------------
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

#options(warn = -1) 

# Library ####


# Load libraries ----

library(pacman) 


p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , sf # provides a set of tools for reading, writing, manipulating, and visualizing spatial data,
)


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------------#


# Read in a shapefile


st_layers("../../data/Mita/mita.gdb")  # lista de layers 


# se extrae el layer particular.
# Distritos de interes alrededor del espacio MITA

dist_mita <- st_read("../../data/Mita/mita.gdb", layer = "StudyDistricts")

st_crs(dist_mita)

str(dist_mita)
              
# Transform the CRS of `dist_mita` to EPSG:4326

dist_mita <- st_transform(dist_mita, 4326)


# Se retorna al sistema de coordenadas iniciales


dist_mita <- st_transform(dist_mita, 32718)


# Plot the shapefile using `ggplot` and `geom_sf`

dist_mita |> ggplot() +
  geom_sf()

## Point ----------------------------


capitales <- st_read("../../data/Mita/mita.gdb", layer = "d2_pot")

str(capitales)

# drop geometry colum

capitales$Shape <- NULL

# Convert `capitales` to an `sf` object
capitales <- st_as_sf(capitales, coords = c("LON", "LAT"), crs = "EPSG:4326")

# Set the geometry column to "Point"

st_geometry(capitales) <- capitales$geometry

# Del objeto Geometry a columnas latitud y longitud por separado 

capitales$longitude <- st_coordinates(capitales)[, 1]
capitales$latitude <- st_coordinates(capitales)[, 2]

# Plot 

capitales |> ggplot() +
  geom_sf(color = "darkblue", size = 1)

## Linestring ----------------------------

# Load shapefile

mita_boundary <- st_read("../../data/Mita/MitaBoundary.shp")

# Plot

# Plot Mita boundary del estudio 

mita_boundary |> ggplot() +
  geom_sf(color = "darkblue")

st_crs(mita_boundary)

# Join geospatial dataframes

# distritos de interés del estudio 

distritos <- st_read("../../data/geopandas_data/LIMITE_DISTRITO/LIMITE_DIST.shp")

# Centroide del distrito 

distritos$Point_centroid <-st_centroid(distritos)['geometry']
  

# Supongamos que no tenemos algun identificador de distrito, 
# provincia o algun espacio geopolítico, etc

capitales$UBIGEO <- NULL

# Joins dataset using geometries as a keys

### Contains option ---------------------

merge <- st_join(distritos, capitales, join = st_contains, left = F)

# View the resulting object

View(merge)

### intersects option -----------------------

data_geo <- st_join(distritos, capitales, join = st_contains, left = F) 


# Add the capital city point layer
ggplot(data_geo) +
  geom_sf(color = "black", fill = "white", size = 0.5) +
  geom_point(aes(x = longitude, y = latitude), 
             color = "orange", size = 1) +
  coord_sf() +
  theme_void()











































