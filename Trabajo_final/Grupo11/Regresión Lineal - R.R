# INTEGRANTES ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)

# Para limpiar el workspace, por si hubiera algún dataset o información cargada
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

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#
# NO consideramos variables de control
# Tenemos la fórmula
model1_3_formula <- as.formula(paste("took_year","~",
                                     paste("treat2016","yr_2016","treatment_class", sep = "+")))
# Realizando la estimación
ols_model1 <- lm_robust(model1_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1)


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
# SI consideramos variables de control)
# incuyendo las variables de control
control_vars <- c("female_prof", "instate", "freshman", "american",
                  "ACumGPA", "gradePrinciples","small_class")

# tenemos la fórmula
model2_3_formula <- as.formula(paste("took_year","~", paste("treat2016","yr_2016","treatment_class",
              paste(control_vars, collapse = "+"),sep="+")))

# Estimando los resultados
ols_model2 <- lm_robust(model2_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 


#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
# NO consideramos variables de control
# Tenemos la fórmula
model3_3_formula <- as.formula(paste("tookanother","~",
                                     paste("treat2016","yr_2016","treatment_class", sep = "+")))
# Realizando la estimación
ols_model3 <- lm_robust(model3_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3)


#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
# NO consideramos variables de control
# tenemos la fórmula
model4_3_formula <- as.formula(paste("tookanother","~", paste("treat2016","yr_2016","treatment_class",
                                                            paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model4 <- lm_robust(model4_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model4) 

#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#


#_____________________#
###### TABLA 4 ###### 
#_____________________#

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#
# NO consideramos variables de control
model1_4_formula <- as.formula(paste("numeconclass","~",
                                     paste("treat2016","yr_2016","treatment_class", sep = "+")))
# Realizando la estimación
ols_model1 <- lm_robust(model1_4_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1)


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
# NO consideramos variables de control
# tenemos la fórmula
model2_4_formula <- as.formula(paste("numeconclass","~", paste("treat2016","yr_2016","treatment_class",
                                                              paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model2 <- lm_robust(model2_4_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 


#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
# NO consideramos variables de control
model3_4_formula <- as.formula(paste("econmajor","~",
                                     paste("treat2016","yr_2016","treatment_class", sep = "+")))
# Realizando la estimación
ols_model3 <- lm_robust(model3_4_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3)


#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
# NO consideramos variables de control
# tenemos la fórmula
model4_4_formula <- as.formula(paste("econmajor","~", paste("treat2016","yr_2016","treatment_class",
                                                               paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model4 <- lm_robust(model4_4_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model4) 

#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#


#_____________________#
###### TABLA 5 ###### 
#_____________________#
# Todos los modelos presnetan presentan variables de control

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#
model1_5_formula <- as.formula(paste("Major_STEM","~", paste("treat2016","yr_2016","treatment_class",
                                                            paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model1 <- lm_robust(model1_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1) 


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
model2_5_formula <- as.formula(paste("Major_Business","~", paste("treat2016","yr_2016","treatment_class",
                                                             paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model2 <- lm_robust(model2_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 


#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
model3_5_formula <- as.formula(paste("Major_Finance","~", paste("treat2016","yr_2016","treatment_class",
                                                                 paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model3 <- lm_robust(model3_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3) 


#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
model4_5_formula <- as.formula(paste("Major_Marketing","~", paste("treat2016","yr_2016","treatment_class",
                                                                paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model4 <- lm_robust(model4_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model4) 


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#



#_____________________#
###### TABLA 6 ###### 
#_____________________#
# Todos los modelos presnetan presentan variables de control

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#
model1_6_formula <- as.formula(paste("Major_SocSc","~", paste("treat2016","yr_2016","treatment_class",
                                                             paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model1 <- lm_robust(model1_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1) 


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
model2_6_formula <- as.formula(paste("Major_Arts","~", paste("treat2016","yr_2016","treatment_class",
                                                                 paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model2 <- lm_robust(model2_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 


#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
model3_6_formula <- as.formula(paste("Major_Comm","~", paste("treat2016","yr_2016","treatment_class",
                                                                paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model3 <- lm_robust(model3_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3) 


#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
model4_6_formula <- as.formula(paste("Major_Hum","~", paste("treat2016","yr_2016","treatment_class",
                                                                  paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model4 <- lm_robust(model4_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model4) 


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#


#------------------------------------------------------------#
##### PREGUNTA 3: CREACIÓN DE UN COEFPLOT DE LA TABLA 4 #####
#------------------------------------------------------------#

