
# Trabajo Final
# Integrantes: Lisbeth Ccoyo, Mishell Delgado, Steven Atoche


# INSTALAMOS LAS LIBRER�AS
install.packages('sf')  
install.packages("tidyverse")  
install.packages("ggplot2")  
library(ggplot2)  
install.packages("viridis") 
library(viridis)  

# Cargamos las librer�as requeridas
library(sf)
library(ggplot2)

# Establecemos el directorio de trabajo para la carpeta que contiene los shapefiles y archivos de zona
setwd("/Users/Lisbeth/Documents/GitHub/1ECO35_2023_1/data/trabajo_final/MAPAS")

# Leemos los shapefiles
shapefile1 <- sf::st_read("districts_1975_remake.shp")
shapefile2 <- sf::st_read("department_peru.shp")

# Leemos los archivos de zona
zonefile1 <- sf::st_read("Figure1Geodatabase.gdb", layer = "agrozone_core2_polygons")
zonefile2 <- sf::st_read("Figure1Geodatabase.gdb", layer = "agrarian_zones_polygons")

## Mapa 3.1A)
# Graficamos los shapefiles y archivos de zona
ggplot() +
  geom_sf(data = shapefile1, fill = "white", color = "grey") +
  geom_sf(data = zonefile1, fill = "darkgrey", color = "black", alpha = 0.5) +
  geom_sf(data = zonefile2, fill = "white", color = "black", alpha = 0.5) +
  coord_sf() +
  scale_fill_manual(values = colors, guide = guide_legend(title = "Leyenda")) +
  labs(fill = "Capas") +
  geom_sf_text(data = shapefile2, aes(label = Department), size = 2) +
  theme_minimal()

## Mapa 3.1B)
# Graficamos los shapefiles y archivos de zona
ggplot() +
  geom_sf(data = shapefile1, fill = "white", color = "grey") +
  geom_sf(data = zonefile1, fill = "grey", color = "white", alpha = 0.5) +
  geom_sf(data = zonefile2, fill = "white", color = "grey", alpha = 0.5) +
  geom_sf(data = shapefile2, fill = "white", color = "black", lty = 3, alpha = 0.1) + 
  coord_sf() +
  scale_fill_manual(values = colors, guide = guide_legend(title = "Leyenda")) +
  labs(fill = "Capas") +
  geom_sf_text(data = shapefile2, aes(label = Department), size = 2) +
  theme_minimal()  

## Figura 2 Land Reform And Civil Conflict in Peru

datos <- read.csv("PeruLR_1975shapedata.csv")

datos$log_transformed_1 <- log(1 + 100 * datos$mEE_DR_13_1980_pcSupM_adj)
datos$log_transformed_2 <- log(0.01 + datos$totalevents)

head(datos)

# Hacemos merge del shapefile con el marco de datos 'datos'
merged_data <- merge(shapefile1, datos, by.x = "Name", by.y = "name", all.x = TRUE)

# Restablecemos el estado de gráficos
dev.off()

# Creamos el mapa de calor geográfico 3.2A)
ggplot() +
  geom_sf(data = merged_data, aes(fill = log_transformed_1)) +
  scale_fill_viridis(option = "C", direction = -1) +
  labs(title = "Log percent land areas expropiated by district, 1969-80") +
  theme_minimal()

# Creamos el mapa de calor geográfico 3.2B)
ggplot() +
  geom_sf(data = merged_data, aes(fill = log_transformed_2)) +
  scale_fill_viridis(option = "C", direction = -1) +
  labs(title = "Log total attacks by district, 1980-2000") +
  theme_minimal()
  
