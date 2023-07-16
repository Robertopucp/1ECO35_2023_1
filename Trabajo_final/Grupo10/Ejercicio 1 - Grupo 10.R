#Ejercicio 1

#El primer paso es cargar las librerías relevantes para realizar la tabla

rm(list = ls())

graphics.off()

cat("\014")

options(scipen = 999)      

library(pacman) 


p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , haven   # to read datset: .dta (stata), .spss, .dbf
  , fastDummies  # for dummies
  , stargazer  # for summary and econometrics tables
  , sandwich  # for linear models
  , lmtest # for robust standar error
  , estimatr # for iv, cluster, robust standar error (LM_robust)
  , lfe  # for fixed effects, cluster standar error
  , caret # for easy machine learning workflow (mse, rmse)
  , texreg # library for export table
  , mfx # probit , logit model marginal effects
)

#Creamos el directorio de trabajo

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Cargamos la base de datos que utilizaremos, en este caso es un archivo dta
SerraPorter <- read_dta("../../data/trabajo_final/replicacion/SerraPorterAEJ.dta")


table1 <- SerraPorter %>% dplyr::select(american, instate, freshman,
                                    ACumGPA, greek, econ_hs, varsity) %>% as.data.frame()

stargazer(table1)


list_vars <- c("Estudiante americano",
               "Estudiante in-state",
               "Estudiante de primer año",
               "GPA acumulado",
               "Estudiante pertenece a una fraternidad",
               "Estudiante llevó economía en High School",
               "Estudiante es atleta")

stargazer(table1, title = "Estadísticas descriptivas", digits = 2, 
          covariate.labels = list_vars, 
          summary.stat = c("mean", "sd", "n", "min", "max"), 
          out = "latex_ejercicio1_tabla1.tex")






