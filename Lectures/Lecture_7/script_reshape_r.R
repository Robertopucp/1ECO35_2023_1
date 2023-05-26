################  laboratorio 7 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 
## Clean dataset


# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

library(pacman) 


p_load(reshape, tidyverse, haven)

# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



#------- Reshape -----------

# load panel dataset

panel <- read_dta("../../data/panel_2016_2018.dta", encoding = "latin1")




# nombre de las variables en minuscula

colnames(panel) <- tolower(colnames(panel)) 

# filtramos nuestras variables de interés 

colnames(panel)

# Filtramos algunas variables usando grepl (detecta patrones en cadena de caracteres)

index =  grep("(año)|(conglome)|(vivienda)|(hogar)|(estrato)|(mieperho)|(gashog2d)|
              (inghog1d)|(pobreza)|(factor07)",
              colnames(panel))

  
print(colnames(panel)[index])

# rename años

panel <- panel  |> dplyr::rename("year_16" = "año_16", "year_17" = "año_17", "year_18" = "año_18", "year_19" = "año_19",
                           "year_20" = "año_20", "cong"= "conglome", "viv" ="vivienda" )

# Filtramos las columnas 

panel <- panel[,index]

"Nos quedamos con 47 variables"

# identificador del hogar

panel$hog <- seq(1,dim(panel)[1])

# ordenando 

panel <- panel  %>%
  select(cong, viv, hog, everything())


# Usando la libreria reshape


new_panel <- reshape(data = panel, idvar = c("cong", "viv", "hog"), varying = 4:48, sep="_", timevar = "time_var", 
                     times = c(16,17,18,19,20), direction = "long")



new_panel$cong <- NULL  # borrar columnas
new_panel$viv <- NULL  # borrar columnas
new_panel$hog <- NULL  # borrar columnas
new_panel$time_var <- NULL  # borrar columnas
new_panel$sub_conglome_20 <- NULL  # borrar columnas
new_panel$pobrezav_20 <- NULL  # borrar columnas

# ordenando para inspección visual de panel de datos

new_panel <- new_panel[order(new_panel$conglome, new_panel$vivienda, new_panel$hogar, new_panel$year),]



