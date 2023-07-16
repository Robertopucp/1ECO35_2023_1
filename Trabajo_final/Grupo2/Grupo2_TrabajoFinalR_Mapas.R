#Mapas----

#Cargamos las librerías necesarias
rm(list = ls())


graphics.off()


cat("\014")



options(scipen = 999)     

options(warn = -1) 

library(pacman) 
library(ggrepel)
library(gridExtra)

p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , sf
  , haven 
  ,viridis
)

#directorio de trabajo

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#

mapsdepartment <- st_read('../../data/trabajo_final/MAPAS/department_peru.shp')
mapsdistrict <- st_read('../../data/trabajo_final/MAPAS/districts_1975_remake.shp')

# Visualizar el gráfico
ggplot(data = mapsdepartment) +
        geom_sf() +
        labs(title = "Mapa de departamentos en Perú")
#Distritos
ggplot(data = mapsdistrict) +
  geom_sf() +
  labs(title = "Mapa de distritos en Perú")


st_layers("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb")  # lista de layers 

agrarianzones <- st_read("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb", layer = "agrarian_zones_polygons")
agrozonecore<- st_read("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb", layer = "agrozone_core2_polygons")

st_crs(agrarianzones)

str(agrarianzones)

agrarianzones |> ggplot() +
  geom_sf()


##Figura 1----

# Gráfico 1
plot1 <- ggplot() +
  geom_sf(data = mapsdepartment, fill = "white", alpha = 0.5, color = "black", linetype = "solid", size = 1) +
  geom_sf(data = agrozonecore, fill = "gray", color = "gray") +
  geom_sf(data = mapsdistrict, fill = NA, color = "black", linetype = "dashed", size = 0.5) +
  theme_void()

# Gráfico 2
plot2 <- ggplot() +
  geom_sf(data = agrarianzones, fill = "white", alpha = 0.5, color = "black", linetype = "solid", size = 1) +
  geom_sf(data = agrozonecore, fill = "gray", color = "gray") +
  geom_sf(data = mapsdistrict, fill = NA, color = "black", linetype = "dashed", size = 0.5) +
  theme_void()

grid.arrange(plot1, plot2, ncol = 2)
plot1 + plot2 + plot_layout(ncol = 2)

##Figura 2----

# Ruta al archivo CSV
ruta <- "../../data/trabajo_final/MAPAS/PeruLR_1975shapedata.csv"

# Leer el archivo CSV
datos <- read.csv(ruta)
# Renombrar la variable "ubi12" a "UBIGEO"
datos <- datos %>% rename(UBIGEO = ubi12)

# Convertir la variable "UBIGEO" a cadena de 6 dígitos
datos$UBIGEO <- sprintf("%06d", datos$UBIGEO)
  

# Calcular el logaritmo del porcentaje de tierras expropiadas
datos$ln_porcentaje_expropiadas <- log(1 + 100 * datos$mEE_DR_13_1980_pcSupM_adj)

# Calcular el logaritmo de totalevents + 0.01
datos$log_totalevents <- log(0.01 + datos$totalevents)

colnames(mapsdistrict)[colnames(mapsdistrict) == 'DI93'] <- 'UBIGEO'

dataset <- merge(mapsdistrict, datos, by = "UBIGEO", all.y = TRUE)



