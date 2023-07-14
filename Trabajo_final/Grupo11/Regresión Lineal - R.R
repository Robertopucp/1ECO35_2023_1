# INTEGRANTES ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)

# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())

# Para limpiar el área de gráficos
graphics.off()

# Limpiar la consola
cat("\014")

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)      # Eliminar la notación científica
options(digits = 3)        # Número de decimales

# Paquetes
librarian::shelf( 
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , haven   # to read datset: .dta (stata), .spss, .dbf
  , fastDummies  # for dummies
  , stargazer  # for summary and econometrics tables
  , sandwich  # for linear models
  , lmtest # for robust standar error
  , estimatr # for iv, cluster, robust standar error (LM_robust)
  , lfe  # for fixed effects, cluster standar error
  , texreg # library for export table
  , caret
  , mfx # probit , logit model marginal effects
  , xtable
)

#------------------------------------------------------------#
##### CONSIDERACIONES PREVIAS #####
#------------------------------------------------------------#

# Leer data
datos <- read_dta("../../data/trabajo_final/replicacion/SerraPorterAEJ.dta")

#------------------------------------------------------------#
##### PREGUNTA 1: ESTADISTICAS DESCRIPTIVAS #####
#------------------------------------------------------------#
#Hacer tabla con las variables seleccionadas y convertirlas como data frame
table1 <- datos %>% dplyr::select(american, instate, freshman,
                                    ACumGPA, greek, econ_hs, varsity) %>% as.data.frame()


#Generar las etiquetas de las variables
list_vars <- c("Estudiante americano","Estudiante in-state","Estudiante es de primer año",
               "GPA acumulado", "Estudiante pertenece a una fraternidad o sororidad",
               "Estudiante llevó Economía en High School", "Estudiante es un atleta")

#Generar la tabla con opciones personalizadas en formato de latex
stargazer(table1, title = "Descriptive Statistics", digits = 2, # decimales con 2 digitos
          covariate.labels = list_vars,  # Lista de etiquetas
          notes = ""
          , notes.append = FALSE, # TRUE append the significance levels
          notes.align = 'r')



#------------------------------------------------------------#
##### PREGUNTA 2: REPLICANDO TABLAS #####
#------------------------------------------------------------#

#_____________________#
###### TABLA 3 ###### 
#_____________________#

#_____________________#
###### TABLA 4 ###### 
#_____________________#

#_____________________#
###### TABLA 5 ###### 
#_____________________#

#_____________________#
###### TABLA 6 ###### 
#_____________________#

#------------------------------------------------------------#
##### PREGUNTA 3: CREACIÓN DE UN COEFPLOT DE LA TABLA 4 #####
#------------------------------------------------------------#

