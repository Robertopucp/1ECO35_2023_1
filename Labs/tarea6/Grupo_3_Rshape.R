# se instalan las librerías 

install.packages("stringr")
install.packages("rebus")

library(stringr)
library(rebus)
library(haven)  
library(dplyr) 
library(tidyverse)
library(tidyr)

#se lee el archivo data.dta
panel <- read_dta("C:\\Users\\ALICIA\\Documents\\GitHub\\ultima tarea\\data\\data.dta")
panel

#se visualizan los nombres de las columnas del archivo
columnas <- colnames(panel)


#se renombran las columnas específicas en el dataframe 'panel'
panel <- panel %>%
  rename(numpanh_15 = numpanh15,  # Renombrar columna 'numpanh15' a 'numpanh_15'
         numpanh_16 = numpanh16,  # Renombrar columna 'numpanh16' a 'numpanh_16'
         numpanh_17 = numpanh17,  # Renombrar columna 'numpanh17' a 'numpanh_17'
         numpanh_18 = numpanh18,  # Renombrar columna 'numpanh18' a 'numpanh_18'
         numpanh_19 = numpanh19)  # Renombrar columna 'numpanh19' a 'numpanh_19'

#se imprime el dataframe 'panel' actualizado
panel

#se obtiene la lista de nombres de columnas del dataframe 'panel'
filter_list <- colnames(panel)

#se crea una nueva lista con valores únicos de la lista de nombres de columnas
new_list <- unique(filter_list)

#se imprime la nueva lista de nombres de columnas
new_list




#se obtiene la lista de columnas
columnas <- c('numper', 'numpanh_15', 'numpanh_16', 'numpanh_17', 'numpanh_18', 'numpanh_19', 'mes_15', 'ubigeo_15', 'dominio_15', 'p400a3_15', 'p4022_15', 'mes_16', 'ubigeo_16', 'dominio_16', 'p400a3_16', 'p4022_16', 'mes_17', 'ubigeo_17', 'dominio_17', 'p400a3_17', 'p4022_17', 'mes_18', 'ubigeo_18', 'dominio_18', 'p400a3_18', 'p4022_18', 'mes_19', 'ubigeo_19', 'dominio_19', 'p400a3_19', 'p4022_19')

#se obtiene el prefijo común en los nombres de las columnas
prefixo <- unique(sub("_.*", "", columnas[-1]))

#se utiliza reshape y gather
reshape_panel <- panel %>%
  gather(key, value, -numper) %>%
  separate(key, into = c("variable", "period"), sep = "_", remove = FALSE) %>%
  filter(variable %in% c("numpanh", "mes", "ubigeo", "dominio", "p400a3", "p4022")) %>%
  select(numper, period, variable, value) %>%
  spread(variable, value)

#se imprime el resultado
print(reshape_panel)




#se lee el archivo .dta
unidos <- haven::read_dta("C:\\Users\\ALICIA\\Documents\\GitHub\\ultima tarea\\data\\unidos.dta")
unidos

#se unen ambos archivos mediante lo común que es ubigeo
juntos <- merge(reshape_panel, unidos, by = "ubigeo", all = FALSE)
juntos 




# Asignar etiquetas a las variables en 'reshape_panel'
labels <- c("Número de persona", "Período", "Número de panh", "Mes", "Ubigeo", "Dominio", "P400a3", "P4022")
for (var in names(reshape_panel)) {
  attr(reshape_panel[[var]], "label") <- labels[var]
}

# Se imprime el resultado
print(reshape_panel)





#se instala el paquete sjlabelled 
if (!require(sjlabelled)) {
  install.packages("sjlabelled")
}

#se carga el paquete sjlabelled
library(sjlabelled)

#se añade etiquetas a todas las variables en 'juntos'
set_label(juntos$numper) <- "Número de persona"
set_label(juntos$period) <- "Período"
set_label(juntos$numpanh) <- "Número de panh"
set_label(juntos$mes) <- "Mes"
set_label(juntos$ubigeo) <- "Ubigeo"
set_label(juntos$dominio) <- "Dominio"
set_label(juntos$p400a3) <- "P400a3"
set_label(juntos$p4022) <- "P4022"
set_label(juntos$unidos) <- "Etiqueta de 'unidos'"

#se añaden etiquetas de valores a las columnas 'p4022' y 'unidos'
set_labels(juntos$p4022, labels = c("Valor1", "Valor2", "Valor3", "Valor4", "Valor5"))
set_labels(juntos$unidos, labels = c("Etiqueta1", "Etiqueta2", "Etiqueta3", "Etiqueta4", "Etiqueta5"))

#se imprime el resultado
print(juntos)



#se añaden etiquetas de valores a las columnas 'p4022' y 'unidos'
juntos$p4022 <- factor(juntos$p4022, levels = c(1, 2, 3, 4, 5), labels = c("Valor1", "Valor2", "Valor3", "Valor4", "Valor5"))
juntos$unidos <- factor(juntos$unidos, levels = c(1, 2, 3, 4, 5), labels = c("Etiqueta1", "Etiqueta2", "Etiqueta3", "Etiqueta4", "Etiqueta5"))

#se imprime el resultado
print(juntos)
