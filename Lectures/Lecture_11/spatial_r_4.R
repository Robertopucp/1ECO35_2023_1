################  Laboratorio 11 parte 4 ------------------------
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


dpt_shp <- st_read(
  '../../data/geopandas_data/LIMITE_DEPARTAMENTO/LIMITE_DEP.shp'
)

dpt_shp$Point_centroid <-st_centroid(dpt_shp)['geometry']

# coordenadas en columnas separadas

dpt_shp$longitude <- st_coordinates(dpt_shp$Point_centroid)[, 1]
dpt_shp$latitude <- st_coordinates(dpt_shp$Point_centroid)[, 2]


#-----------------------------------------------------------#

# plot raster file 

tavg <- raster(
  "../../data/geopandas_data/tavg.tif"
) |>
  rasterToPoints() |>  # coordenadas por raster, se toma el promedio de la variable en el pixel
  data.frame() |>
  as_tibble() |>
  rename(fill1 = 3)

ggplot() +
  geom_raster(data = tavg, aes(x, y, fill = fill1)) 




#-----------------------------------------------------------#


tavg <- raster(
   "../../data/geopandas_data/tavg.tif"
) |>
  crop(dpt_shp) |>  # solo quedarme con la info de a nivel departamento
  mask(dpt_shp) |>  # Todo pixel fuera de los departamentos tiene NA
  rasterToPoints() |>  # coordenadas por raster, se toma el promedio de la variable en el pixel
  data.frame() |>
  as_tibble() |>
  rename(fill1 = 3)


prec <- raster(
  "../../data/geopandas_data/prec.tif"
) |> 
  crop(dpt_shp) |>  # solo quedarme con la info de a nivel departamento
  mask(dpt_shp) |>  # Todo pixel fuera de los departamentos tiene NA
  rasterToPoints() |>  # coordenadas por raster, se toma el promedio de la variable en el pixel
  data.frame() |>
  as_tibble() |>
  rename(fill1 = 3)



t_a <- colorRampPalette(c("#14e4e8", "#c61707"))


p <-
ggplot() +
geom_raster(data = tavg, aes(x, y, fill = fill1)) +
geom_sf(data = dpt_shp, fill = NA, color = "#15233f") +  # color borde, sin colo al interior
geom_text(
  data = dpt_shp, aes(longitude, latitude, label = str_to_sentence(NOMBDEP))
  , size = 2, color = "#15233f"  
  , nudge_x = .1
) +
theme_void() +
scale_fill_gradientn(colors = t_a(2)) +
guides(
  fill = guide_colorbar(
    barheight = unit(70, units = "mm"), # alto de la barra de la leyenda
    barwidth = unit(5, units = "mm"),  # ancho de la barra de la leyenda
    direction = "vertical", # dirección de la barra de la leyende
    ticks.colour = "black",
    title.position = "right", # ubicación del título
    title.hjust = 0.5
  )
)   +
    theme(
      legend.position = c(1.1, .5) # posición leyenda 
      , legend.text = element_text(color = "black", size = 10)
      , legend.title = element_text(color = "black",
                                  angle = 90, size = 12)
    ) +
  labs(fill = "Temperatura promedio") # titulo de barra

plot(p)



ggsave(
  plot = p  # nombre del objeto
  , filename = "../../output/plots/raster_temp_peru.png"    # path 
  , width = 15  # ancho
  , height = 15  # alto
  , dpi = 800    # calidad de imagen. A más dpi , más memoria consume
)



