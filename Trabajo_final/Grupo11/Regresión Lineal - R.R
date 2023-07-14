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
          notes.align = 'r', out = "../../trabajo_final/Grupo11/Table_ED_R.tex")

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
model1_3_formula
# Realizando la estimación
ols_model1 <- lm_robust(model1_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1) 

# Otra forma de estimacion
m1 <- lm(model1_3_formula, data = datos)
m1

#Sacar errores estandar robustos
robust_model1_3 <- coeftest(m1,
                          vcov = vcovCL,
                          type = "HC1",
                          cluster = ~ class_fe2)

sd_robust_model1_3 <- robust_model1_3[,2] 
sd_robust_model1_3


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
# SI consideramos variables de control)
# incuyendo las variables de control
control_vars <- c("female_prof", "instate", "freshman", "american",
                  "ACumGPA", "gradePrinciples","small_class")

# Tenemos la fórmula
model2_3_formula <- as.formula(paste("took_year","~", paste("treat2016","yr_2016","treatment_class",
              paste(control_vars, collapse = "+"),sep="+")))
model2_3_formula
# Estimando los resultados
ols_model2 <- lm_robust(model2_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 

# Otra forma de estimación
m2 <- lm(model2_3_formula, data = datos)
m2


#Sacar errores estandar robustos
robust_model2_3 <- coeftest(m2,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model2_3 <- robust_model2_3[,2] 
sd_robust_model2_3


#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
# NO consideramos variables de control
# Tenemos la fórmula
model3_3_formula <- as.formula(paste("tookanother","~",
                                     paste("treat2016","yr_2016","treatment_class", sep = "+")))
model3_3_formula
# Realizando la estimación
ols_model3 <- lm_robust(model3_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3)

# Otra forma de estimación
m3 <- lm(model3_3_formula, data = datos)
m3

#Sacar errores estandar robustos
robust_model3_3 <- coeftest(m3,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model3_3 <- robust_model3_3[,2] 
sd_robust_model3_3


#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
# NO consideramos variables de control
# Tenemos la fórmula
model4_3_formula <- as.formula(paste("tookanother","~", paste("treat2016","yr_2016","treatment_class",
                                                            paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model4 <- lm_robust(model4_3_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model4) 


# Otra forma de estimación
m4 <- lm(model4_3_formula, data = datos)
m4

#Sacar errores estandar robustos
robust_model4_3 <- coeftest(m4,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_3 <- robust_model4_3[,2] 
sd_robust_model4_3


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1, m2, m3, m4,
           se=list(sd_robust_model1_3, sd_robust_model2_3, sd_robust_model3_3,
                   sd_robust_model4_3),
           dep.var.labels = c("Took Micro within year","Took another econclass"),
           title = "Treatment Effects on Intermediate Outcomes",
           keep = c("treat2016","yr_2016","treatment_class","(Intercept)"),
           covariate.labels=c("Treatment class x 2016","Year 2016",
                              "Treatment class (in 2015)","Intercepto"),
           align = T, no.space = T,
           add.lines=list(c("Controls", "No", "Yes","No","Yes")),
           keep.stat = c("n"),
           notes.append = FALSE, notes.align = "l",
           notes ="Huber robust standard errors are in parentheses", style = "qje"
           , out = "../../trabajo_final/Grupo11/Table_3R.tex")


#_____________________#
###### TABLA 4 ###### 
#_____________________#

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#
# NO consideramos variables de control
model1_4_formula <- as.formula(paste("numeconclass","~",
                                     paste("treat2016","yr_2016","treatment_class", sep = "+")))
model1_4_formula

# Realizando la estimación
ols_model1 <- lm_robust(model1_4_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1)

# Otra forma de estimacion
m1 <- lm(model1_4_formula, data = datos)
m1

#Sacar errores estandar robustos
robust_model1_4 <- coeftest(m1,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model1_4 <- robust_model1_4[,2] 
sd_robust_model1_4


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
# NO consideramos variables de control
# tenemos la fórmula
model2_4_formula <- as.formula(paste("numeconclass","~", paste("treat2016","yr_2016","treatment_class",
                                                              paste(control_vars, collapse = "+"),sep="+")))
model2_4_formula

# Estimando los resultados
ols_model2 <- lm_robust(model2_4_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 

# Otra forma de estimación
m2 <- lm(model2_4_formula, data = datos)
m2

#Sacar errores estandar robustos
robust_model2_4 <- coeftest(m2,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model2_4 <- robust_model2_4[,2] 
sd_robust_model2_4



#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
# NO consideramos variables de control
model3_4_formula <- as.formula(paste("econmajor","~",
                                     paste("treat2016","yr_2016","treatment_class", sep = "+")))
model3_4_formula

# Realizando la estimación
ols_model3 <- lm_robust(model3_4_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3)

# Otra forma de estimación
m3 <- lm(model3_4_formula, data = datos)
m3

#Sacar errores estandar robustos
robust_model3_4 <- coeftest(m3,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model3_4 <- robust_model3_4[,2] 
sd_robust_model3_4


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


# Otra forma de estimación
m4 <- lm(model4_4_formula, data = datos)
m4

#Sacar errores estandar robustos
robust_model4_4 <- coeftest(m4,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_4 <- robust_model4_4[,2] 
sd_robust_model4_4


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1, m2, m3, m4,
           se=list(sd_robust_model1_4, sd_robust_model2_4, sd_robust_model3_4,
                   sd_robust_model4_4),
           dep.var.labels = c("Number of econ classes taken", "Major in economics"),
           title = "Treatment Effects on Final Outcomes",
           keep = c("treat2016","yr_2016","treatment_class","(Intercept)"),
           covariate.labels=c("Treatment class x 2016","Year 2016",
                              "Treatment class (in 2015)","Intercepto"),
           align = T, no.space = T,
           add.lines=list(c("Controls", "No", "Yes","No","Yes")),
           keep.stat = c("n"),
           notes.append = FALSE, notes.align = "l",
           notes ="Huber robust standard errors are in parentheses", style = "qje"
           , out = "../../trabajo_final/Grupo11/Table_4R.tex")



#_____________________#
###### TABLA 5 ###### 
#_____________________#
# Todos los modelos presnetan presentan variables de control

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#
model1_5_formula <- as.formula(paste("Major_STEM","~", paste("treat2016","yr_2016","treatment_class",
                                                            paste(control_vars, collapse = "+"),sep="+")))
model1_5_formula

# Estmando los resultados
ols_model1 <- lm_robust(model1_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1) 


# Otra forma de estimacion
m1 <- lm(model1_5_formula, data = datos)
m1

#Sacar errores estandar robustos
robust_model1_5 <- coeftest(m1,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model1_5 <- robust_model1_5[,2] 
sd_robust_model1_5


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
model2_5_formula <- as.formula(paste("Major_Business","~", paste("treat2016","yr_2016","treatment_class",
                                                             paste(control_vars, collapse = "+"),sep="+")))
model2_5_formula

# Estimando los resultados
ols_model2 <- lm_robust(model2_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 

# Otra forma de estimación
m2 <- lm(model2_5_formula, data = datos)
m2

#Sacar errores estandar robustos
robust_model2_5 <- coeftest(m2,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model2_5 <- robust_model2_5[,2] 
sd_robust_model2_5



#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
model3_5_formula <- as.formula(paste("Major_Finance","~", paste("treat2016","yr_2016","treatment_class",
                                                                 paste(control_vars, collapse = "+"),sep="+")))
model3_5_formula

# Estimando los resultados
ols_model3 <- lm_robust(model3_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3) 

# Otra forma de estimación
m3 <- lm(model3_5_formula, data = datos)
m3

#Sacar errores estandar robustos
robust_model3_5 <- coeftest(m3,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model3_5 <- robust_model3_5[,2] 
sd_robust_model3_5



#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
model4_5_formula <- as.formula(paste("Major_Marketing","~", paste("treat2016","yr_2016","treatment_class",
                                                                paste(control_vars, collapse = "+"),sep="+")))
model4_5_formula

# Estimando los resultados
ols_model4 <- lm_robust(model4_5_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model4) 

# Otra forma de estimación
m4 <- lm(model4_5_formula, data = datos)
m4

#Sacar errores estandar robustos
robust_model4_5 <- coeftest(m4,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_5 <- robust_model4_5[,2] 
sd_robust_model4_5


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1, m2, m3, m4,
           se=list(sd_robust_model1_5, sd_robust_model2_5, sd_robust_model3_5,
                   sd_robust_model4_5),
           dep.var.labels = c("Major STEM","Major busines", "Major finance", "Major marketing"),
           title = "Treatment Effects on Other High-Earning Majors",
           keep = c("treat2016","yr_2016","treatment_class","(Intercept)"),
           covariate.labels=c("Treatment class x 2016","Year 2016",
                              "Treatment class (in 2015)","Intercepto"),
           align = T, no.space = T,
           add.lines=list(c("Controls", "Yes", "Yes","Yes","Yes")),
           keep.stat = c("n"),
           notes.append = FALSE, notes.align = "l",
           notes ="Huber robust standard errors are in parentheses", style = "qje"
           , out = "../../trabajo_final/Grupo11/Table_5R.tex")


#_____________________#
###### TABLA 6 ###### 
#_____________________#
# Todos los modelos presnetan presentan variables de control

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#
model1_6_formula <- as.formula(paste("Major_SocSc","~", paste("treat2016","yr_2016","treatment_class",
                                                             paste(control_vars, collapse = "+"),sep="+")))
model1_6_formula

# Estimando los resultados
ols_model1 <- lm_robust(model1_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model1) 

# Otra forma de estimacion
m1 <- lm(model1_6_formula, data = datos)
m1

#RMSE
lm_rmse1 <- round(RMSE(m1$fitted.values, datos$Major_SocSc ) ,2 )
lm_rmse1

#Sacar errores estandar robustos
robust_model1_6 <- coeftest(m1,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model1_6 <- robust_model1_6[,2] 
sd_robust_model1_6


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
model2_6_formula <- as.formula(paste("Major_Arts","~", paste("treat2016","yr_2016","treatment_class",
                                                                 paste(control_vars, collapse = "+"),sep="+")))
model2_6_formula

# Estimando los resultados
ols_model2 <- lm_robust(model2_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model2) 

# Otra forma de estimación
m2 <- lm(model2_6_formula, data = datos)
m2

#RMSE
lm_rmse2 <- round(RMSE(m2$fitted.values, datos$Major_Arts ) ,2 )
lm_rmse2

#Sacar errores estandar robustos
robust_model2_6 <- coeftest(m2,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model2_6 <- robust_model2_6[,2] 
sd_robust_model2_6


#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
model3_6_formula <- as.formula(paste("Major_Comm","~", paste("treat2016","yr_2016","treatment_class",
                                                                paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
ols_model3 <- lm_robust(model3_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model3) 

# Otra forma de estimación
m3 <- lm(model3_6_formula, data = datos)
m3

#RMSE
lm_rmse3 <- round(RMSE(m3$fitted.values, datos$Major_Comm) ,2 )
lm_rmse3

#Sacar errores estandar robustos
robust_model3_6 <- coeftest(m3,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model3_6 <- robust_model3_6[,2] 
sd_robust_model3_6



#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
model4_6_formula <- as.formula(paste("Major_Hum","~", paste("treat2016","yr_2016","treatment_class",
                                                                  paste(control_vars, collapse = "+"),sep="+")))
model4_6_formula

# Estimando los resultados
ols_model4 <- lm_robust(model4_6_formula, data = datos,
                        clusters = class_fe2, se_type = "stata")
# Viendo los resultados
summary(ols_model4) 

# Otra forma de estimación
m4 <- lm(model4_6_formula, data = datos)
m4

#RMSE
lm_rmse4 <- round(RMSE(m4$fitted.values, datos$Major_Hum ) ,2 )
lm_rmse4

#Sacar errores estandar robustos
robust_model4_6 <- coeftest(m4,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_6 <- robust_model4_6[,2] 
sd_robust_model4_6


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1, m2, m3, m4,
           se=list(sd_robust_model1_3, sd_robust_model2_3, sd_robust_model3_3,
                   sd_robust_model4_3),
           dep.var.labels = c("Major social sciences","Major arts",
                              "Major communication", "Major humanities"),
           title = "Treatment Effects on Low-Earning Majors",
           keep = c("treat2016","yr_2016","treatment_class","(Intercept)"),
           covariate.labels=c("Treatment class x 2016","Year 2016",
                              "Treatment class (in 2015)","Intercepto"),
           align = T, no.space = T,
           add.lines=list(c("Controls", "Yes", "Yes","Yes","Yes")),
           keep.stat = c("n"),
           notes.append = FALSE, notes.align = "l",
           notes ="Huber robust standard errors are in parentheses", style = "qje"
           , out = "../../trabajo_final/Grupo11/Table_6R.tex")


#------------------------------------------------------------#
##### PREGUNTA 3: CREACIÓN DE UN COEFPLOT DE LA TABLA 4 #####
#------------------------------------------------------------#




