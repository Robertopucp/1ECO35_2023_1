################  laboratorio 7 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 
## Clean dataset


library(reshape)
library(haven)
library(dplyr)

#------- Reshape -----------

"1.0 Set Directorio"

user <- Sys.getenv("USERNAME")  # username

setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2022_2/Lab8") ) # set directorio


# load panel dataset

panel <- read_dta("../../../datos/panel/743-Modulo1478/sumaria-2016-2020-panelf.dta", encoding = "latin1")



# Filtro la variable hpanel1620 ==1, el hogar es entrevista seguidamente desde 2016-2020

panel <- panel[panel$hpanel1620 == 1,]

# nombre de las variables en minuscula

colnames(panel) <- tolower(colnames(panel)) 

# filtramos nuestras variables de interés 

colnames(panel)


index =  grep("(año)|(conglome)|(vivienda)|(hogar)|(estrato)|(mieperho)|(gashog2d)|
              (inghog1d)|(pobreza)|(factor07)",
              colnames(panel))

index =  grep("(año)|(^conglome)|(vivienda)|(hogar)|(estrato_)|(mieperho)|(gashog2d)|
              (inghog1d)|(pobreza_)|(factor07)",
              colnames(panel))
  
print(colnames(panel)[index])

# rename años

panel <- panel  %>% dplyr::rename("year_16" = "año_16", "year_17" = "año_17", "year_18" = "año_18", "year_19" = "año_19",
                           "year_20" = "año_20", "cong"= "conglome", "viv" ="vivienda" )

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


# ordenando para inspección visual de panel de datos

new_panel <- new_panel[order(new_panel$conglome, new_panel$vivienda, new_panel$hogar, new_panel$year),]



