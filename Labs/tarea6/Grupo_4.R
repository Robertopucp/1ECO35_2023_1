

# Grupo_4_r 

rm(list = ls())

library(readxl)
library(dplyr) 
library(rstudioapi)  
library(readr)
library(haven) 
library(pacman) 
library(stringr)
library(lubridate)
library(foreign)


# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

############################################################


ruta_archivo <- "C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/Juntos_program/data.dta"

data <- haven::read_dta(ruta_archivo)
colnames(data) <- tolower(colnames(data)) 

colnames(data)
library(tidyr)
library(dplyr)

data_long <- reshape(data, idvar=c("numper", "numpanh15"), varying = 7:ncol(data), sep = "_", direction = "long")
library(dplyr)
library(tidyverse)
library(foreign)

data_unidos <- read_dta("C:/Users/ASUS/Documents/GitHub/1ECO35_2023_1/data/Juntos_program/unidos.dta")

merged_data <- merge(data_long, data_unidos, by = "ubigeo")