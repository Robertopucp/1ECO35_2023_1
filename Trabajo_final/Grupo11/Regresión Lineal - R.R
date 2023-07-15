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

# Estimando
m1_3 <- lm(model1_3_formula, data = datos)
m1_3

#Sacar errores estandar robustos
robust_model1_3 <- coeftest(m1_3,
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
m2_3 <- lm(model2_3_formula, data = datos)
m2_3

#Sacar errores estandar robustos
robust_model2_3 <- coeftest(m2_3,
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
m3_3 <- lm(model3_3_formula, data = datos)
m3_3

#Sacar errores estandar robustos
robust_model3_3 <- coeftest(m3_3,
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
model4_3_formula

# Estimando los resultados

m4_3 <- lm(model4_3_formula, data = datos)
m4_3

#Sacar errores estandar robustos
robust_model4_3 <- coeftest(m4_3,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_3 <- robust_model4_3[,2] 
sd_robust_model4_3


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1_3, m2_3, m3_3, m4_3,
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
m1_4 <- lm(model1_4_formula, data = datos)
m1_4

#Sacar errores estandar robustos
robust_model1_4 <- coeftest(m1_4,
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
m2_4 <- lm(model2_4_formula, data = datos)
m2_4

#Sacar errores estandar robustos
robust_model2_4 <- coeftest(m2_4,
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
m3_4 <- lm(model3_4_formula, data = datos)
m3_4

#Sacar errores estandar robustos
robust_model3_4 <- coeftest(m3_4,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model3_4 <- robust_model3_4[,2] 
sd_robust_model3_4


#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
# NO consideramos variables de control
# Tenemos la fórmula
model4_4_formula <- as.formula(paste("econmajor","~", paste("treat2016","yr_2016","treatment_class",
                                                               paste(control_vars, collapse = "+"),sep="+")))
model4_4_formula

# Estimando los resultados

m4_4 <- lm(model4_4_formula, data = datos)
m4_4

#Sacar errores estandar robustos
robust_model4_4 <- coeftest(m4_4,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_4 <- robust_model4_4[,2] 
sd_robust_model4_4


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1_4, m2_4, m3_4, m4_4,
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
m1_5 <- lm(model1_5_formula, data = datos)
m1_5

#Sacar errores estandar robustos
robust_model1_5 <- coeftest(m1_5,
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
m2_5 <- lm(model2_5_formula, data = datos)
m2_5

#Sacar errores estandar robustos
robust_model2_5 <- coeftest(m2_5,
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
m3_5 <- lm(model3_5_formula, data = datos)
m3_5

#Sacar errores estandar robustos
robust_model3_5 <- coeftest(m3_5,
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
m4_5 <- lm(model4_5_formula, data = datos)
m4_5

#Sacar errores estandar robustos
robust_model4_5 <- coeftest(m4_5,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_5 <- robust_model4_5[,2] 
sd_robust_model4_5


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1_5, m2_5, m3_5, m4_5,
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
m1_6 <- lm(model1_6_formula, data = datos)
m1_6

#Sacar errores estandar robustos
robust_model1_6 <- coeftest(m1_6,
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
m2_6 <- lm(model2_6_formula, data = datos)
m2_6


#Sacar errores estandar robustos
robust_model2_6 <- coeftest(m2_6,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model2_6 <- robust_model2_6[,2] 
sd_robust_model2_6


#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
model3_6_formula <- as.formula(paste("Major_Comm","~", paste("treat2016","yr_2016","treatment_class",
                                                                paste(control_vars, collapse = "+"),sep="+")))
# Estimando los resultados
m3_6 <- lm(model3_6_formula, data = datos)
m3_6

#Sacar errores estandar robustos
robust_model3_6 <- coeftest(m3_6,
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
m4_6 <- lm(model4_6_formula, data = datos)
m4_6

#Sacar errores estandar robustos
robust_model4_6 <- coeftest(m4_6,
                            vcov = vcovCL,
                            type = "HC1",
                            cluster = ~ class_fe2)

sd_robust_model4_6 <- robust_model4_6[,2] 
sd_robust_model4_6


#- - - - - - - - - - EXPORTANDO A LATEX - - - - - - - - - -#

stargazer( m1_6, m2_6, m3_6, m4_6,
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

#- - - - - - - - - - - - MODELO 1 - - - - - - - - - - - -#

# Extrayendo coeficientes 

model1_4_coef <- robust_model1_4[2,1]
model1_4_coef

# Desviación estandar
model1_4_coef_se = robust_model1_4[2,2]
model1_4_coef_se

# Lower and upper bound
model1_4_lower = coefci(m1_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,1]
model1_4_lower
model1_4_upper = coefci(m1_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,2]
model1_4_upper


#- - - - - - - - - - - - MODELO 2 - - - - - - - - - - - -#
# Extrayendo coeficientes 
model2_4_coef <- robust_model2_4[2,1]
model2_4_coef

# Desviación estandar
model2_4_coef_se = robust_model2_4[2,2]
model2_4_coef_se

# Lower and upper bound
model2_4_lower = coefci(m2_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,1]
model2_4_lower
model2_4_upper = coefci(m2_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,2]
model2_4_upper

#- - - - - - - - - - - - MODELO 3 - - - - - - - - - - - -#
# Extrayendo coeficientes 
model3_4_coef <- robust_model3_4[2,1]
model3_4_coef

# Desviación estandar
model3_4_coef_se = robust_model3_4[2,2]
model3_4_coef_se

# Lower and upper bound
model3_4_lower = coefci(m3_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,1]
model3_4_lower
model3_4_upper = coefci(m3_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,2]
model3_4_upper

#- - - - - - - - - - - - MODELO 4 - - - - - - - - - - - -#
# Extrayendo coeficientes 
model4_4_coef <- robust_model4_4[2,1]
model4_4_coef

# Desviación estandar
model4_4_coef_se = robust_model4_4[2,2]
model4_4_coef_se

# Lower and upper bound
model4_4_lower = coefci(m4_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,1]
model4_4_lower
model4_4_upper = coefci(m4_4, df = Inf, vcov. = vcovCL, cluster = ~ class_fe2, type = "HC1")[2,2]
model4_4_upper

#- - - - - - - - Armanda tabla de resultados - - - - - - - -#

table<- matrix(0, 4, 4)
table[1,1]<- model1_4_coef
table[1,2]<- model1_4_coef_se
table[1,3]<- model1_4_lower
table[1,4]<- model1_4_upper

table[2,1]<- model2_4_coef
table[2,2]<- model2_4_coef_se
table[2,3]<- model2_4_lower
table[2,4]<- model2_4_upper

table[3,1]<- model3_4_coef
table[3,2]<- model3_4_coef_se
table[3,3]<- model3_4_lower
table[3,4]<- model3_4_upper

table[4,1]<- model4_4_coef
table[4,2]<- model4_4_coef_se
table[4,3]<- model4_4_lower
table[4,4]<- model4_4_upper

colnames(table)<- c("Estimate","Std. Error","Lower_bound","Upper_bound")
rownames(table)<- c("Model 1", "Model 2", "Model 3", "Model 4")

# Considerar que:
# Model 1: Number of econ classes taken (No Control)
# Model 2: Number of econ classes taken (Yes Control)
# Model 3: Major in economics (No Control)
# Model 4: Major in economics (Yes Control)

# Exportación a Latex
xtable(table)

# La tabla se convierte en un data frame 
tab <- as.data.frame(table)
tab

# Dimensiones del gráfico 
options(repr.plot.width = 8, repr.plot.height =5)


# Graficando 
tab  %>% ggplot(aes(x=rownames(tab), y=Estimate)) +
  geom_point(size=2, color = 'black') +
  geom_errorbar(aes(ymin=Lower_bound, ymax=Upper_bound) , width = 0.05,color="darkblue", linewidth = 0.8) +
  labs(x="", y="") + ggtitle("Treatment class x 2016")  +
  theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.8) +
  scale_x_discrete(limits = c("Model 1", "Model 2", "Model 3", "Model 4")) +
  scale_y_continuous(breaks = seq(-0.2,1.2,0.1) , limits = c(-0.2, 1.2)) +
  theme_classic(14)
