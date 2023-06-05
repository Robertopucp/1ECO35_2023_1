
rm(list = ls())

graphics.off()

cat("\014")

options(scipen = 999)

#Agregamos algunas librerías para realizsar este ejercicio

library(pacman) 
library(tidyr)
library(dplyr)
library(patchwork)
p_load(readxl, tidyverse, foreign, ggthemes, datos)

#El problema al utilizar la función setwd continúa, como lo mencioné
#en un trabajo anterior. De igual forma dejo debajo la opción
#utilizada en clase
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

setwd("C:/Users/Ademir/Documents/GitHub/1ECO35_2023_1/data/produccion_coca")

#Creamos dos rutas para hacer más práctico el llamar las bases
produccion <- "C:\\Users\\Ademir\\Documents\\GitHub\\1ECO35_2023_1\\data\\produccion_coca\\6.1.1_-_Illicit_coca_bush_cultivation.xlsx"
erradicacion <- "C:\\Users\\Ademir\\Documents\\GitHub\\1ECO35_2023_1\\data\\produccion_coca\\6.1.2_-_Eradication_of_coca_bush.xlsx"

#Primero limpiamos la base de erradicación, para que se puedan
#aplicar las funciones de forma más óptima

data1 <- read_excel(erradicacion,
                    range = "C2:Q5")
data2 <- data1[,-c(2:3)]
data3 <- data.frame(t(data2))
data4 <- rownames_to_column(data3, "Años")
data5 <- data4[-c(1),]
colnames(data5) <- c("Años", "Bolivia", "Colombia", "Perú")

#El comando patchwork nos permite unir plots, aunque
#no estén en un mismo gráfico, permite la comparación

ggplot(data5, aes(Años, Bolivia, group = 1)) +
  geom_line(size = 0.6, color = "azure4") +
  theme_minimal() +
  labs(x = "Year", y = "Coca Erradication",
       title = "Erradication of coca in Bolivia") +

ggplot(data5, aes(Años, Colombia, group = 1)) +
  geom_line(size = 0.6, color = "#56B4E9") +
  theme_minimal() +
  labs(x = "Year", y = "Coca Erradication",
       title = "Erradication of coca in Colombia") +

ggplot(data5, aes(Años, Perú, group = 1)) +
  geom_line(size = 0.6, color = "steelblue") +
  theme_minimal() +
  labs(x = "Year", y = "Coca Erradication",
       title = "Erradication of coca in Perú")

#Guardamos la imgane en la carpeta tal como se indicó

ggsave("../../output/plots/erradication_of_coca.jpg"
       , height = 7 
       , width = 9  
       , dpi = 320   
)

#Realizamos el mismo proceso de limpieza de la base para 
#cultivo de coca

cultivation <- read_excel(produccion,
                          range = "A3:M8")

cultivation1 <- na.omit(cultivation)
cult <- data.frame(t(cultivation1))
cult1 <- rownames_to_column(cult, "Años")

colnames(cult1) <- c("Años", "Bolivia", "Colombia", "Perú", "Total")
cult2 <- cult1[-c(1),]

#Al igual que el caso anterior se une la producción y la
#erradicación de la coca en Perú

ggplot(cult2, aes(Años, Perú, group = 1)) +
  geom_line(size = 0.6, color = "green") +
  theme_minimal() +
  labs(x = "Year", y = "Coca Production in Perú") +

ggplot(data5, aes(Años, Perú, group = 1)) +
  geom_line(size = 0.6, color = "steelblue") +
  theme_minimal() +
  labs(x = "Year", y = "Coca Erradication",
       title = "Erradication of coca in Perú")

#Se guarda el plot generado al igual que en el caso anterior

ggsave("../../output/plots/coca_Perú.jpg"
       , height = 7 
       , width = 9  
       , dpi = 320   
)







