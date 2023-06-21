################  Laboratorio 11 parte 3 ------------------------
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
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , sf
  , haven 
  ,viridis
)

#Set working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------#

# Border of Switzerland

ch_borders <- st_read(
  '../../data/geopandas_data/eu_maps/CHE_adm0.shp'
)

# Border of Germany

de_borders <- st_read(
  '../../data/geopandas_data/eu_maps/gadm36_DEU_0.shp'
)

# Switzerland lakes shape files

ch_lakes <- st_read(
  '../../data/geopandas_data/eu_maps/g2s15.shp'
)

# muni

Geo_muni <- st_read(
  "../../data/geopandas_data/eu_maps/VG250_GEM.shp"
)


# Set up the plot
plot <- ggplot() + 
  theme_void()  

# German Plot, switzerland border and Swiss lakes
plot + geom_sf(data = Geo_muni,
               fill = "#00BFFF",
               color = "black",
               size = 0.1 ,
               alpha = 0.8) +
  geom_sf(data = ch_borders, 
                       color = "black", 
                       fill = NA, size = 1.5) +
  geom_sf(data = ch_lakes, 
                       color = "black",  # edgecolor (color del borde) in python
                       fill = "lightblue", # color argument in python
                       size = 0.4)


ggsave("../../output/plots/map_geo.jpg"
       , height = 7  # alto
       , width = 9  # ancho
       , dpi = 600   # resolución (calidad de la imagen)
)


# shows CRS 

shp_files <- list(Geo_muni, ch_borders, ch_lakes, de_borders)

for (i in shp_files) { print(st_crs(i)$input) }

# geom_sf function can handle shapefiles with
# different coordinate reference systems (CRS). 
# When you plot multiple shapefiles with different CRSs,
# ggplot2 will automatically reproject the shapefiles to a common CRS


cv_data <- read.csv('../../data/geopandas_data/Datos_panel_distritos.csv')
head(cv_data)

cv_data <- cv_data |>
  rename(ubigeo = Código) |>
  mutate(ubigeo = str_pad(ubigeo, 6, pad ="0")) # completar ubigeo 6 digitos

# import csv file of public health centers: MINSA, ESSALUD and Regional Goverment
# coordinates information (NORTE: longitud) (ESTE: latitud)

health_centers <- read.csv('../../data/geopandas_data/centros_hospitalarios.csv')

# Convert dataframe to an `sf` object

health_centers <-  health_centers |> 
                   filter( !is.na(ESTE) & !is.na(NORTE))|> 
  st_as_sf(coords = c("NORTE", "ESTE"), 
                           crs = "EPSG:4326")


# code1 = 19 to postas médicas 

Public_health1 <- health_centers |>
  filter(code1 == "19")

# code1 = 12 & 24 to Hospitals

Public_health2 <- health_centers |>
  filter(code1 == "12" | code1 == "24")


### Shapefile a nivel distrito 

maps <- st_read(
  '../../data/geopandas_data/shape_file/DISTRITOS.shp'
)

st_crs(maps)$input  # sistema de coordenadas geográficas (lonitud, latitud)

## Merge: Covid Data & Shapefile distrito -----------------

dataset_cv <- maps |>
  right_join(cv_data, by = c("IDDIST"="ubigeo"))

# En cv_data, se cuenta con info de distrito por mes y año 

# Function to filter dataset_covid by year, month and administrative level (province)

subdata <- function(a,b,c){
  
  if (c=="All-Peru"){
    resultado <- dataset_cv[dataset_cv$Año == a & dataset_cv$Mes == b, ]
  } else{
    resultado <- dataset_cv[dataset_cv$Año == a & dataset_cv$Mes == b & dataset_cv$Provincia ==c, ]
  }
  
  return(resultado)
}

#Second wave

base_lima <- subdata(2021, 8, "LIMA")


## Covid & Lima Matropolitana -------------

ggplot() +
  geom_sf(data = base_lima, aes(fill = Casos), 
          color = "black") +
  scale_fill_gradient(
    low = "white", high = "#4B0082",
    breaks = c(200, 400,600, 800, 1000 , 1200)
  ) +
  theme_void() +
  theme(legend.position = "right",
        legend.title=element_blank(),
        legend.text = element_text(size = 7), ) 

# Get centroid's district

base_lima$centroid <-st_centroid(base_lima)['geometry']

base_lima$longitude <- st_coordinates(base_lima$centroid)[, 1]
base_lima$latitude <- st_coordinates(base_lima$centroid)[, 2]


# using geom_sf_label 

ggplot() + 
  geom_sf(data = base_lima, aes(fill = Casos), color = "black") + 
  scale_fill_viridis() + 
  theme_void() + 
  theme(legend.position = "right",
        legend.title=element_blank()) +
  geom_sf_label(data = base_lima, aes(label =Distrito),
                size = 2, # tamaño caja
                label.size = 0.1, # tamaño de la etiqueta (nombre de distritos)
                label.padding = unit(0.2, "lines")) 

# padding: espacio entre texto y la caja de la leyenda



ggsave("../../output/plots/lima_covid.png"
       , height = 15  # alto
       , width = 15  # ancho
       , dpi = 600   # resolución (calidad de la imagen)
)

# Postas Médicas and Hospitales públicos - LIMA


PH_lima_postas <- Public_health1[Public_health1$Provincia == "LIMA",]

PH_lima_hospitales <- Public_health2[Public_health2$Provincia == "LIMA",]

### Covid deaths and Postas médicas ---------------------

ggplot() +
  geom_sf(data = base_lima, 
          aes(fill = Muertes.Covid), 
          color = "black") + 
  scale_fill_viridis() + 
  theme_void() +
  geom_sf(data = PH_lima_postas, 
          color = "red",
          size = 2)+
  theme(legend.title=element_blank())

### Postas and Hospital ----------------


ggplot() +
  geom_sf(data = base_lima,
          color = "black", fill = NA) +
  theme_void() +
  geom_sf(data = PH_lima_postas,
          aes(color = "A"), # creamos la cateogia A
          show.legend = "point")+
  geom_sf(data = PH_lima_hospitales,
          aes(color = "B"), # creamos la cateogia B
          size = 3.5,
          show.legend = "point") +
  scale_color_manual(values = c("A" = "#008080", "B" = "red"), # asignamos color
                      labels = c("Postas Médicas", "Hospitales Públicos"), #labels
    guide = guide_legend(override.aes = list(linetype = c("blank", "blank"), 
                                    shape = c(20, 16))))  +  # shape circulo, numero indica el tipo de diseño 
  theme(legend.title=element_blank())




## Mapa de calor, Postas and Hospital

ggplot() +
  geom_sf(data = base_lima, 
          aes(fill = Muertes.Covid), 
          color = "black") + 
  scale_fill_viridis()  +
  theme_void() +
  geom_sf(data = PH_lima_postas,
          aes(color = "A"),
          show.legend = "point",
          size = 2) +
  geom_sf(data = PH_lima_hospitales,
          aes(color = "B"),
          show.legend = "point",
          size = 3.5) +
  scale_color_manual(values = c("A" = "lightblue", "B" = "red"), # asignamos color
                      labels = c("Postas Médicas", "Hospitales Públicos"), #labels
          guide = guide_legend(override.aes = list(linetype = c("blank", "blank"), 
          shape = c(20, 16))))  +  # shape circulo 
  theme(legend.title=element_blank())
  
  
  
  #theme(legend.title=element_blank())

### Relative Size of Markers ---------------

PH_lima_hospitales <- PH_lima_hospitales[!is.na(PH_lima_hospitales$CAMAS),]



ggplot() +
  geom_sf(data = base_lima, 
          aes(fill = Muertes.Covid), 
          color = "black") + 
  scale_fill_viridis()  +
  theme_void() +
  geom_sf(data = PH_lima_hospitales,
          aes(size = CAMAS, color = "A"), # se define el tamaño de cada point según los valores en la columna CAMAS
          show.legend = F
          )  +
  scale_color_manual(values = c("A" = "red"), # asignamos color
                      labels = c("Hospitales Públicos - camas"), #labels
                      guide = guide_legend(override.aes = list(linetype = "blank", 
                                                               shape = 16)))  +  # shape circulo 
  theme(legend.title=element_blank())




ggsave("../../output/plots/lima_covid_nucamas.png"
       , height = 15  # alto
       , width = 15  # ancho
       , dpi = 600   # resolución (calidad de la imagen)
)








































  
  