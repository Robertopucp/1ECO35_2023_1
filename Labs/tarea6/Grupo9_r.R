#Tarea 6: reshape en R
rm(list = ls())
graphics.off()
cat("\014")
options(scipen = 999) 

#Cargamos las siguientes librerías:
library(pacman)
p_load(reshape, tidyverse, haven)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(haven)
data <- read_dta("~/GitHub/1ECO35_2023_1/data/Juntos_program/data.dta")
View(data)

colnames(data)
index =  grep("(numper)|(numpanh)|(mes)|(ubigeo)|(dominio)|(p400a3)|(p4022)", colnames(data))
print(colnames(data)[index])


data <- data[,index]
view(data)
ncol(data)
colnames(data)

data$ind <- seq(1,dim(data)[1])
data <- data  %>%
  select(numper, everything())

# Usando la libreria reshape:
new_data <- reshape(data = data, idvar = c("numper"), varying = 2:31 , 
                     sep="_", timevar = "time_var", 
                     times = c(15,16,17,18,19), direction = "long")
view(new_data)

#Haciendo el merge entre las bases de datos "new_data" y "unidos":
library(haven)
unidos <- read_dta("~/GitHub/1ECO35_2023_1/data/Juntos_program/unidos.dta")
View(unidos)

final_data <- merge(new_data, unidos, by = "ubigeo")


