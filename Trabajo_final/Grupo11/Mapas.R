# INTEGRANTES ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)

#MAPAS

# clean environment variables
rm(list = ls())
# clean plots
graphics.off()
# clean console
cat("\014") #Ctrl + shift + c

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)      # Eliminar la notación científica
options(digits = 3)        # Número de decimales


library(pacman)
p_load(
  tidyverse
  ,sf
  ,haven
  ,viridis
)

#Instalamos las librerías
#install.packages("sf")
#install.packages("ggplot2")


library(sf)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)


# Especificar la ruta completa de los archivos shapefile
ruta_distritos <- "../../data/trabajo_final/MAPAS/districts_1975_remake.shp"
ruta_departamentos <- "../../data/trabajo_final/MAPAS/department_peru.shp"

# Cargar los shapefiles
distritos <- st_read(ruta_distritos)
departamentos <- st_read(ruta_departamentos)

# Especificar la ruta completa del archivo Geodatabase
ruta_geodb <- "../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb"

# Leer los datos desde el Geodatabase
agrarian_core <- st_read(dsn = ruta_geodb, layer = "agrozone_core2_polygons")
agrarian_zone <- st_read(dsn = ruta_geodb, layer = "agrarian_zones_polygons")

#Revisamos los sistemas de coordenadas
print(st_crs(distritos))
print(st_crs(departamentos))
print(st_crs(agrarian_core))
print(st_crs(agrarian_zone))

# Definir el código EPSG 4326 (WGS 84)
crs_4326 <- st_crs(4326)

# Transformar los objetos geoespaciales al sistema de coordenadas EPSG 4326
departamentos_crs <- st_transform(departamentos, crs_4326)
agrarian_core_crs <- st_transform(agrarian_core, crs_4326)
agrarian_zone_crs <- st_transform(agrarian_zone, crs_4326)

#Mapa1 figura 1

# Crear una figura y un eje
fig <- ggplot() +
  theme_minimal()

# Graficar agrarian zone cores
fig <- fig + geom_sf(data = agrarian_core_crs, fill = "lightgrey", show.legend = TRUE)

# Graficar distritos
fig <- fig + geom_sf(data = distritos, color = "grey", fill = NA, size = 0.4, show.legend = TRUE)

# Graficar boundaries de agrarian zones con líneas más gruesas (size = 1.5)
fig <- fig + geom_sf(data = agrarian_zone_crs, color = "black", fill = NA, size = 1.5, show.legend = TRUE)

# Añadir leyenda
fig <- fig + labs(title = "FIGURA 1: Mapa 1 Agrarian Zones and District Boundaries")

# Crear los elementos de la leyenda
legend_elements <- list(
  geom_line(aes(linetype = "District Boundaries"), color = "grey", size = 0.4),
  geom_line(aes(linetype = "Agrarian Zones"), color = "black", size = 1.5),
  geom_rect(aes(fill = "Agrarian Zone Core"), color = "black")
)

# Añadir la leyenda al gráfico
fig <- fig + scale_linetype_manual(name = "Legend", values = c("blank", "blank"),
                                   labels = c("District Boundaries", "Agrarian Zones")) +
  scale_fill_manual(name = "Legend", values = "lightgrey", labels = "Agrarian Zone Core") +
  guides(linetype = guide_legend(override.aes = legend_elements),
         fill = guide_legend(override.aes = legend_elements),
         title.position = "left", title.hjust = 0.5)

# Visualizar el gráfico con la leyenda
print(fig)

# Guardar el mapa como archivo PNG
ggsave(filename = "figura1_mapa1R.png")


#Mapa2 figura 1 - v1
# Crear un objeto ggplot vacío
p <- ggplot() +
  theme_minimal()

# Graficar agrarian zone cores
p <- p + geom_sf(data = agrarian_core_crs, color = "lightgrey", show.legend = TRUE)

# Graficar distritos
p <- p + geom_sf(data = distritos, color = "grey", fill = NA, size = 0.4, show.legend = TRUE)

# Graficar boundaries de departamentos
p <- p + geom_sf(data = departamentos_crs, color = "black", fill = NA, linetype = "dashed", size = 5, show.legend = TRUE)

# Añadir título
p <- p + labs(title = "FIGURE 1 Agrarian Reform Zones in Peru")

# Añadir leyenda en la parte inferior izquierda
p <- p +
  guides(
    color = guide_legend(
      title = "Legend",
      override.aes = list(size = c(0.4, 5, NA)),
      label.position = "left",
      label.hjust = 0,
      label.vjust = 1,
      nrow = 1
    )
  )

# Visualizar el gráfico
print(p)

# Guardar el mapa como archivo PNG
ggsave(filename = "figura1_mapa2R.png")




#FIGURA2

# Visualizar el contenido del archivo de distritos
print(distritos)

# Suponiendo que tienes un dataframe llamado "distritos"
# Utilizamos la función str() para obtener información sobre el tipo de contenido de cada columna
str(distritos)

# Ruta del archivo CSV
ruta <- "../../data/trabajo_final/MAPAS/PeruLR_1975shapedata.csv"

# Leer el archivo CSV como un objeto 'readr::read_lines'
base <- readr::read_lines(ruta, skip = 1)  # Salta la primera línea si es el encabezado

# Reconocer el formato de texto
det <- readr::guess_encoding(base)

# Obtener el conjunto de caracteres (encoding) detectado
charenc <- det$encoding

# Imprimir el conjunto de caracteres detectado
print(charenc)

# Leer el archivo CSV con el conjunto de caracteres especificado
data2 <- read.csv(file = ruta, encoding = charenc)

# Ver las primeras 6 filas del dataframe
head(data2)

# Crear las nuevas variables
data2 <- data2 %>%
  mutate(log_expropiadas = log(1 + 100 * mEE_DR_13_1980_pcSupM_adj),
         violencia_politica = log(0.01 + totalevents))

# Mostrar el nuevo dataframe
print(data2)

# Usando str() para mostrar el tipo de datos de las columnas
str(data2)

# Usando names() para cambiar los nombres de las columnas
names(data2)[names(data2) == "name"] <- "Name"
names(data2)[names(data2) == "province"] <- "Province"
names(data2)[names(data2) == "department"] <- "Department"

# Hacer el merge por las columnas "Name", "Province" y "Department" con left join
merged_data <- merge(distritos, data2, by = c("Name", "Province", "Department"), all.x = TRUE)

# Mostrar el resultado
print(merged_data)


#MAPA DE CALOR

#(a)
# Creamos el mapa
mapa_a <- ggplot() +
  # Agregamos los datos para el heatmap
  geom_sf(data = merged_data, aes(fill = log_expropiadas)) +
  # Configuramos el color
  scale_fill_gradient(low = "white", high = "black") +
  # Agregamos los límites de los departamentos
  geom_sf(data = departamentos_crs, color = "gray", fill = NA, size = 0.4) +
  theme_minimal() +
  labs(title = "(a)")

# Visualizamos el mapa
print(mapa_a)

# Guardar el mapa como archivo PNG
ggsave(filename = "figura2_mapa1R.png")

#(b)
# Crear el mapa de calor
mapa_b <-ggplot() +
  geom_sf(data = merged_data, aes(fill = violencia_politica)) +
  scale_fill_gradient(low = "white", high = "black") +
  geom_sf(data = departamentos_crs, color = "gray", fill = NA, size = 1.5) +
  labs(title = "(b)") +
  theme_minimal()

# Visualizamos el mapa
print(mapa_b)

# Guardar el mapa como archivo PNG
ggsave(filename = "figura2_mapa2R.png")
  