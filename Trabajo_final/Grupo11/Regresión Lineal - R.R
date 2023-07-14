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
##### PREGUNTA 1: ESTADISTICAS DESCRIPTIVAS #####
#------------------------------------------------------------#
#Leer data producción hoja de coca por hectárea
data <- read_excel("../../data/produccion_coca/6.1.1_-_Illicit_coca_bush_cultivation.xlsx")
data <- data[ c(4:6), c(2:13)]
data <- t(data)

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

