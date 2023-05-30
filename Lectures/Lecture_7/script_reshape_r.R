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

# Usaremos funciones pivot de la libreria tidyr que está dentro de tidyverse


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

# Pivot ---------------------


# create example dataframe

bdata <- data.frame(geo_id = c("01","01", "02", "02", "04", "04","05","05","06","06"),
                    name = c("Alabama","Alabama", "Alaska", "Alaska", 
                             "Arizona", "Arizona", "Arkansas","Arkansas",
                             "California","California"),
                    variable = c("income","rent","income","rent",
                                 "income","rent", "income","rent",
                                 "income","rent"),
                    estimate = c(24476,747, 32940, 1200, 27517, 972,
                                 23789, 709, 29454, 1358),
                    moe = c(136,3, 508,13, 148,4, 165, 5,109,3)
)

### Pivot_wider  --------

# Tantas columnas según cantidad de categorías

# variable: income (median yearly income), rent (median monthly rent)
# estimated value
# moe: margen de error 

  df1 <- bdata |> pivot_wider(id_cols = c(geo_id, name),
                              names_from = variable,
                              values_from = c(estimate, moe)
  )
  
  
  View(df1)

# prefix and separate

df2 <- bdata |> pivot_wider(id_cols = c(geo_id, name),
                            names_from = variable,
                            values_from = c(estimate, moe),
                            names_sep = ".",names_prefix = "category_"
)


View(df2)

### Pivot_longer  --------

## Tantas filas por unidad de observación según categorias

new_df <- df1 |> pivot_longer(cols = estimate_income:moe_rent,
                              names_to = c(".value", "category"), names_sep = "_")


View(new_df)







































