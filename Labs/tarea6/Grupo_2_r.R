#Tarea 6 - Grupo2####
#Limpieza del environment e invocacion de librerias

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library 
library(pacman) 

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled,Hmisc) 

# Change working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##Reshape - Script R y Python (Jupiter notebook)####




###Formar una base de datos a partir de las bases data.dta y unidos.dta. Para ello, debe transformar la base data.dta a un formato long, luego hacer merge con la base unidos.dta. En el proceso de formato wide a long, use la variable numper como identificador####

####Transformar la base data.dta a un formato long####

#Se lee la base de datos data.dta

panelData <- read_dta("../../data/Juntos_program/data.dta")


#Se le añade un _ a los numpanh para poder utilizar luego el separador _
new_column_names <- gsub('numpanh', 'numpanh_', colnames(panelData))
colnames(panelData) <- new_column_names
panelData



# Se reordena las columnas en el dataframe
column_order <- c("numper")

for (period in 15:19) {
  column_order <- c(column_order,
                    paste0("numpanh_", period),
                    paste0("mes_", period),
                    paste0("ubigeo_", period),
                    paste0("dominio_", period),
                    paste0("p400a3_", period),
                    paste0("p4022_", period))
}

panelData <- panelData[, column_order]

print(colnames(panelData))


# Se transforma a formato long
longpanelData <- panelData %>%
  gather(key, value, -numper) %>%
  separate(key, into = c("var", "period"), sep = "_") %>%
  spread(var, value)

####Merge entre las dos bases####

#Se lee la base de datos unidos.dta
baseUnidos <- read_dta("../../data/Juntos_program/unidos.dta")

baseDatos <- merge(longpanelData, baseUnidos, by = "ubigeo", all.x = TRUE)


# Se ubica a numper como primera columna, period como segunda y se ordena segun numper y period
baseDatos <- baseDatos %>%
  select(numper, period, everything()) %>%
  arrange(numper, period)

# Se reestablece el index
baseDatos <- baseDatos %>%
  mutate(index = row.names(.)) %>%
  select(index, everything())

# Se elimina la columna index creada
baseDatos <- baseDatos %>%
  select(-index)

#Se muestra el dataframe baseDatos nacido del merge
baseDatos

####Etiquetas####

#Etiquetas de valores
val_labels(baseDatos$unidos) <- c("Programa Juntos was not applied in the district" = 0,
                                     "Programa Juntos applied in the district" = 1)
baseDatos$p4022 <- as.integer(baseDatos$p4022) #Se convierte a int
val_labels(baseDatos$p4022) <- c("The individual was not sick in the last 4 weeks" = 0,"The individual was sick in the last 4 weeks" = 1)

#Etiquetas
label(baseDatos$numper) <- "Unique individual identifier"
label(baseDatos$period) <- "The last 2 digits of the year"
label(baseDatos$ubigeo) <- "The official acronyms for Geographic Location Code"
label(baseDatos$dominio) <- "Subregions of peru"
label(baseDatos$mes) <- "Month of the survey in 2015"
label(baseDatos$numpanh) <- "Unique household identifier in 2015"
label(baseDatos$p400a3) <- "Year of birth of the person, reported in 2015"
label(baseDatos$p4022) <- "Were you sick in the last 4 weeks? Values: 1 (=Yes), 0 (=No)"
label(baseDatos$distrito) <- "District of Peru"
label(baseDatos$provincia) <- "Province of Peru"
label(baseDatos$region) <- "Region of Peru"
label(baseDatos$unidos) <- "Programa Juntos applied in the district? Values: 1 (=Yes), 0 (=No)"

#Se muestra las etiquetas
lapply(baseDatos, attr, 'label')

#Se muestra las etiquetas de valores
lapply(baseDatos, attr, 'labels')








