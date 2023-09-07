#TRABAJO FINAL

#REGRESIONES LINEALES
# Limpiamos la consola 
rm(list = ls())
graphics.off()
cat("\014")

# Opciones adicionales
options(scipen = 999)  

#Abrimos las librerias necesarias
library(pacman) 
p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc 
  , haven   # para leer la base de datos en formato .dta (stata), .spss, .dbf
  , sandwich  # sandwich: libreria de modelos lineales.
  , fastDummies  # para las dummies
  , stargazer  # para summary y tablas econométricas
  , lmtest # lmtest: errores estandar robustos,
  , estimatr # for iv, cluster, robust standar error (LM_robust)
  , lfe  # fpara efectos fijos, cluster standar error
  , caret # for easy machine learning workflow (mse, rmse)
  , texreg # ára exportar tabla
  , hdm # high dimention metric model
  , glmnet # linear machine learning models (lasso, elasticnet, ridge)
  , xtable # xtable: exportar matriz o dataframe, table a latex
)

#Cambiamos el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Abrimos la base de datos
replica <- read_dta("../../data/trabajo_final/replicacion/SerraPorterAEJ.dta")

#Exploramos la base de datos
str(replica)

lapply(replica, class)

names(replica)

attributes(replica)
##############################################################################################
#Estadísticas descriptivas
table1 <- replica %>% dplyr::select(american,instate, freshman, ACumGPA, greek, econ_hs, varsity) %>% as.data.frame()
#Generamos las etiquetas de las variables
list_vars <- c("Estudiante americano","Estudiante in-state","Estudiante es de primer año",
"GPA acumulado", "Estudiante pertenece a una fraternidad o sororidad", "Estudiante llevó Economía en High School",
"Estudiante es un atleta")

#Generar la tabla con opciones personalizadas en formato de latex
stargazer(table1, title = "Descriptive Statistics", digits = 2, # con dos decimales
          covariate.labels = list_vars,  # Lista de etiquetas
          notes = "Fuente: Porter y Serra (2020)"
          , notes.append = FALSE, # TRUE append the significance levels
          notes.align = 'l')
#####################################################################################
#Regresión
#Réplica de la tabla 3
#Modelo1

modelo1 <- lm(took_year ~ yr_2016 + treatment_class + treat2016 , data = replica)

attributes(modelo1)
modelo1$coefficients  # coeficientes
modelo1$fitted.values  # Y estimado

# summary table como en stata

#stata *** 1%, ** 5%, * 10%
#R de summary *** 0.1%, ** 1%, * 5%, . 10%
summary(modelo1)

summary(modelo1)$call

summary(modelo1)$coef
# Test de significancia individual
coeftest(modelo1, vcov = vcovHC(modelo1, "HC1"))
tabla_modelo1 <- stargazer(modelo1, title = "Model 1", align = TRUE, type = "text")

#-----------------------------------------------------
#Modelo2
# errores estandar robustas (Huber-white robust)
# Se añade variables de control
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples","small_class")
# Usando as.formula pues el modelo se hace extenso

model2_formula <- as.formula(
  paste("took_year ~ yr_2016 + treatment_class + treat2016 +", paste(control_vars, collapse = " + "))
)
#cluster para corregir por correlación entre residuos del mismo país (autocorrelación)
#se_type corrreción de errores estándar por heterocedasticidad
# Ajustar el modelo 2
modelo2 <- lm(model2_formula, data = replica)

# Obtener el resumen del modelo 2
summary_modelo2 <- summary(modelo2)

# Imprimir el resumen
print(summary_modelo2)

# Obtener los coeficientes y errores estándar robustos para el modelo 2
m2 <- lm(model2_formula, data = replica)
robust_modelo2 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

# Obtener los errores estándar robustos
sd_robust_modelo2 <- robust_modelo2[, 2]
coeftest(modelo2, vcov = vcovHC(modelo2, "HC1"))
tabla_modelo2 <- stargazer(modelo2, title = "Model 2", align = TRUE, type = "text")
#-------------------------------------------------------------------
#Modelo 3 
modelo3 <- lm(tookanother ~ yr_2016 + treatment_class + treat2016 , data = replica)

attributes(modelo3)
modelo3$coefficients  # coeficientes
modelo3$fitted.values  # Y estimado

# summary table como en stata

#stata *** 1%, ** 5%, * 10%
#R de summary *** 0.1%, ** 1%, * 5%, . 10%
summary(modelo3)

summary(modelo3)$call

summary(modelo3)$coef
# Test de significancia individual
coeftest(modelo3, vcov = vcovHC(modelo3, "HC1"))
tabla_modelo3 <- stargazer(modelo3, title = "Model 3", align = TRUE, type = "text")
#----------------------------------------------------------------
#Modelo 4
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples","small_class")
# Usando as.formula pues el modelo se hace extenso

model4_formula <- as.formula(
  paste("tookanother ~ yr_2016 + treatment_class + treat2016 +", paste(control_vars, collapse = " + "))
)
#cluster para corregir por correlación entre residuos del mismo país (autocorrelación)
#se_type corrreción de errores estándar por heterocedasticidad
# Ajustar el modelo 2
modelo4 <- lm(model4_formula, data = replica)

# Obtener el resumen del modelo 2
summary_modelo4 <- summary(modelo4)

# Imprimir el resumen
print(summary_modelo4)

# Obtener los coeficientes y errores estándar robustos para el modelo 2
m2 <- lm(model4_formula, data = replica)
robust_modelo4 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

# Obtener los errores estándar robustos
sd_robust_modelo4 <- robust_modelo4[, 2]
coeftest(modelo4, vcov = vcovHC(modelo4, "HC1"))
tabla_modelo4 <- stargazer(modelo4, title = "Model 4", align = TRUE, type = "text")
#--------------------------------------------------------------------------
stargazer::stargazer(modelo1, modelo2, modelo3, modelo4)
####################################################################################
#Replica de la tabla 4
#Modelo1

modelo41 <- lm(numeconclass ~ yr_2016 + treatment_class + treat2016 , data = replica)

attributes(modelo41)
modelo41$coefficients  # coeficientes
modelo41$fitted.values  # Y estimado

# summary table como en stata

#stata *** 1%, ** 5%, * 10%
#R de summary *** 0.1%, ** 1%, * 5%, . 10%
summary(modelo41)

summary(modelo41)$call

summary(modelo41)$coef
# Test de significancia individual
coeftest(modelo41, vcov = vcovHC(modelo41, "HC1"))
tabla_modelo41 <- stargazer(modelo41, title = "Model 1", align = TRUE, type = "text")
#-------------------------------------------------------------------------------------------
#Modelo2
# errores estandar robustas (Huber-white robust)
# Se añade variables de control
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples","small_class")
# Usando as.formula pues el modelo se hace extenso

model42_formula <- as.formula(
  paste("numeconclass ~ yr_2016 + treatment_class + treat2016 +", paste(control_vars, collapse = " + "))
)
#cluster para corregir por correlación entre residuos del mismo país (autocorrelación)
#se_type corrreción de errores estándar por heterocedasticidad
# Ajustar el modelo 2
modelo42 <- lm(model42_formula, data = replica)

# Obtener el resumen del modelo 2
summary_modelo42 <- summary(modelo42)

# Imprimir el resumen
print(summary_modelo42)

# Obtener los coeficientes y errores estándar robustos para el modelo 2
m2 <- lm(model42_formula, data = replica)
robust_modelo42 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

# Obtener los errores estándar robustos
sd_robust_modelo42 <- robust_modelo42[, 2]
coeftest(modelo42, vcov = vcovHC(modelo42, "HC1"))
tabla_modelo2 <- stargazer(modelo42, title = "Model 2", align = TRUE, type = "text")
#---------------------------------------------------------------------------------------
#Columna 3
#Modelo 3 
modelo43 <- lm(econmajor ~ yr_2016 + treatment_class + treat2016 , data = replica)

attributes(modelo43)
modelo43$coefficients  # coeficientes
modelo43$fitted.values  # Y estimado

# summary table como en stata

#stata *** 1%, ** 5%, * 10%
#R de summary *** 0.1%, ** 1%, * 5%, . 10%
summary(modelo43)

summary(modelo43)$call

summary(modelo43)$coef
# Test de significancia individual
coeftest(modelo43, vcov = vcovHC(modelo43, "HC1"))
tabla_modelo43 <- stargazer(modelo43, title = "Model 3", align = TRUE, type = "text")
#---------------------------------------------------------------------------------------
#Modelo 4
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples","small_class")
# Usando as.formula pues el modelo se hace extenso

model44_formula <- as.formula(
  paste("econmajor ~ yr_2016 + treatment_class + treat2016 +", paste(control_vars, collapse = " + "))
)
#cluster para corregir por correlación entre residuos del mismo país (autocorrelación)
#se_type corrreción de errores estándar por heterocedasticidad
# Ajustar el modelo 2
modelo44 <- lm(model44_formula, data = replica)

# Obtener el resumen del modelo 2
summary_modelo44 <- summary(modelo44)

# Imprimir el resumen
print(summary_modelo44)

# Obtener los coeficientes y errores estándar robustos para el modelo 2
m2 <- lm(model44_formula, data = replica)
robust_modelo44 <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

# Obtener los errores estándar robustos
sd_robust_modelo44 <- robust_modelo44[, 2]
coeftest(modelo44, vcov = vcovHC(modelo44, "HC1"))
tabla_modelo44 <- stargazer(modelo44, title = "Model 4", align = TRUE, type = "text")
#--------------------------------------------------------------------------
stargazer::stargazer(modelo41, modelo42, modelo43, modelo44)

######################################################################################
#Tabla 5
#MODELO1
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model51_formula <- as.formula(
  paste("Major_STEM ~ treat2016 + yr_2016 + treatment_class + ", paste(control_vars, collapse = " + "))
)

# Ajustar el modelo 1
modelo51 <- lm(model51_formula, data = replica)

# Obtener el resumen del modelo 1
summary_modelo51 <- summary(modelo51)

# Obtener los coeficientes y errores estándar robustos para el modelo 1
robust_modelo51 <- coeftest(modelo51, vcov = vcovHC(modelo51, type = "HC1"))

# Imprimir el resumen del modelo 1
print(summary_modelo51)

# Obtener los errores estándar robustos
sd_robust_modelo51 <- robust_modelo51[, 2]

# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo51, vcov = vcovHC(modelo51, "HC1"))

# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo51 <- stargazer(modelo51, title = "Model 1", align = TRUE, type = "text")
#-----------------------------------------------------------------------------------
#Modelo2
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model52_formula <- as.formula(
  paste("Major_Finance ~ treat2016 + yr_2016 + treatment_class + ", paste(control_vars, collapse = " + "))
)

# Ajustar el modelo 2
modelo52 <- lm(model52_formula, data = replica)

# Obtener el resumen del modelo 2
summary_modelo52 <- summary(modelo52)

# Obtener los coeficientes y errores estándar robustos para el modelo 1
robust_modelo52 <- coeftest(modelo52, vcov = vcovHC(modelo52, type = "HC1"))

# Imprimir el resumen del modelo 2
print(summary_modelo52)

# Obtener los errores estándar robustos
sd_robust_modelo52 <- robust_modelo52[, 2]

# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo52, vcov = vcovHC(modelo52, "HC1"))

# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo52 <- stargazer(modelo52, title = "Model 2", align = TRUE, type = "text")

#-----------------------------------------------------------------------------------------
#Modelo3
control_var <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model53_formula <- as.formula(
  paste("Major_Business ~ treat2016 + yr_2016 + treatment_class + ", paste(control_var, collapse = " + "))
)

# Ajustar el modelo 3
modelo53 <- lm(model53_formula, data = replica)

# Obtener el resumen del modelo 3
summary_modelo53 <- summary(modelo53)

# Obtener los coeficientes y errores estándar robustos para el modelo 3
robust_modelo53 <- coeftest(modelo53, vcov = vcovHC(modelo53, type = "HC1"))

# Imprimir el resumen del modelo 3
print(summary_modelo53)

# Obtener los errores estándar robustos
sd_robust_modelo53 <- robust_modelo53[, 2]

# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo53, vcov = vcovHC(modelo53, "HC1"))

# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo53 <- stargazer(modelo53, title = "Model 2", align = TRUE, type = "text")

#---------------------------------------------------------------------------------------
#Modelo 4
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model54_formula <- as.formula(
  paste("Major_Marketing ~ treat2016 + yr_2016 + treatment_class + ", paste(control_vars, collapse = " + "))
)

# Ajustar el modelo 4
modelo54 <- lm(model54_formula, data = replica)

# Obtener el resumen del modelo 4
summary_modelo54 <- summary(modelo54)

# Obtener los coeficientes y errores estándar robustos para el modelo 3
robust_modelo54 <- coeftest(modelo54, vcov = vcovHC(modelo54, type = "HC1"))

# Imprimir el resumen del modelo 4
print(summary_modelo54)

# Obtener los errores estándar robustos
sd_robust_modelo54 <- robust_modelo54[, 2]

# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo54, vcov = vcovHC(modelo54, "HC1"))

# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo54 <- stargazer(modelo54, title = "Model 4", align = TRUE, type = "text")
#-----------------------------------------------------------------------------------------
stargazer::stargazer(modelo51, modelo52, modelo53, modelo54)
################################################################################################
#Tabla6
# MODELO1
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model61_formula <- as.formula(
  paste("Major_SocSc ~ treat2016 + yr_2016 + treatment_class + ", paste(control_vars, collapse = " + "))
)

# Ajustar el modelo 1
modelo61 <- lm(model61_formula, data = replica)

# Obtener el resumen del modelo 1
summary_modelo61 <- summary(modelo61)

# Obtener los coeficientes y errores estándar robustos para el modelo 1
robust_modelo61 <- coeftest(modelo61, vcov = vcovHC(modelo61, type = "HC1"))

# Imprimir el resumen del modelo 1
print(summary_modelo61)

# Obtener los errores estándar robustos
sd_robust_modelo61 <- robust_modelo61[, 2]

# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo61, vcov = vcovHC(modelo61, "HC1"))

# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo61 <- stargazer(modelo61, title = "Model 1", align = TRUE, type = "text")
#-----------------------------------------------------------------------------------------------
#MODELO2
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model62_formula <- as.formula(
  paste("Major_Arts ~ treat2016 + yr_2016 + treatment_class + ", paste(control_vars, collapse = " + "))
)
# Ajustar el modelo 
modelo62 <- lm(model62_formula, data = replica)
# Obtener el resumen del modelo 
summary_modelo62 <- summary(modelo62)
# Obtener los coeficientes y errores estándar robustos para el modelo 1
robust_modelo62 <- coeftest(modelo62, vcov = vcovHC(modelo62, type = "HC1"))
# Imprimir el resumen del modelo 
print(summary_modelo62)
# Obtener los errores estándar robustos
sd_robust_modelo62 <- robust_modelo62[, 2]
# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo62, vcov = vcovHC(modelo62, "HC1"))
# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo62 <- stargazer(modelo62, title = "Model 1", align = TRUE, type = "text")
#-----------------------------------------------------------------------------------------------
#MODELO3
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model63_formula <- as.formula(
  paste("Major_Comm ~ treat2016 + yr_2016 + treatment_class + ", paste(control_vars, collapse = " + "))
)
# Ajustar el modelo 
modelo63 <- lm(model63_formula, data = replica)
# Obtener el resumen del modelo 
summary_modelo63 <- summary(modelo63)
# Obtener los coeficientes y errores estándar robustos para el modelo 1
robust_modelo63 <- coeftest(modelo63, vcov = vcovHC(modelo63, type = "HC1"))
# Imprimir el resumen del modelo 
print(summary_modelo63)
# Obtener los errores estándar robustos
sd_robust_modelo63 <- robust_modelo63[, 2]
# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo63, vcov = vcovHC(modelo63, "HC1"))
# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo63 <- stargazer(modelo63, title = "Model 1", align = TRUE, type = "text")
#-----------------------------------------------------------------------------------------------
#MODELO4
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
model64_formula <- as.formula(
  paste("Major_Hum ~ treat2016 + yr_2016 + treatment_class + ", paste(control_vars, collapse = " + "))
)
# Ajustar el modelo 
modelo64 <- lm(model64_formula, data = replica)
# Obtener el resumen del modelo 
summary_modelo64 <- summary(modelo64)
# Obtener los coeficientes y errores estándar robustos para el modelo 1
robust_modelo64 <- coeftest(modelo64, vcov = vcovHC(modelo64, type = "HC1"))
# Imprimir el resumen del modelo 
print(summary_modelo64)
# Obtener los errores estándar robustos
sd_robust_modelo64 <- robust_modelo64[, 2]
# Imprimir los coeficientes y errores estándar robustos
coeftest(modelo64, vcov = vcovHC(modelo64, "HC1"))
# Generar una tabla con los resultados del modelo 1 (usando la librería 'stargazer')
tabla_modelo64 <- stargazer(modelo64, title = "Model 1", align = TRUE, type = "text")
#-------------------------------------------------------------------------------------
stargazer::stargazer(modelo61, modelo62, modelo63, modelo64)
################################################################################################
#COEFPLOT
# lmtest: coeftest
#Modelo1
modelo41.tab <- coeftest(modelo41, vcov=vcovHC(modelo41, type='HC1'))
modelo41.tab
modelo41_coef <- modelo41.tab[2,1]
modelo41_coef_se = modelo41.tab[2,2]
modelo41_lower = coefci(modelo41, df = Inf, vcov. = vcovHC, type = "HC1")[2,1]
modelo41_upper = coefci(modelo41, df = Inf, vcov. = vcovHC, type = "HC1")[2,2]

# Modelo2
modelo42.tab <- coeftest(modelo42, vcov=vcovHC(modelo42, type='HC1'))
modelo42.tab
modelo42_coef <- modelo42.tab[2,1]
modelo42_coef_se = modelo42.tab[2,2]
modelo42_lower = coefci(modelo42, df = Inf, vcov. = vcovHC, type = "HC1")[2,1]
modelo42_upper = coefci(modelo42, df = Inf, vcov. = vcovHC, type = "HC1")[2,2]

# Modelo3
modelo43.tab <- coeftest(modelo43, vcov=vcovHC(modelo43, type='HC1'))
modelo43.tab
modelo43_coef <- modelo43.tab[2,1]
modelo43_coef_se = modelo43.tab[2,2]
modelo43_lower = coefci(modelo43, df = Inf, vcov. = vcovHC, type = "HC1")[2,1]
modelo43_upper = coefci(modelo43, df = Inf, vcov. = vcovHC, type = "HC1")[2,2]

# Modelo4
modelo44.tab <- coeftest(modelo44, vcov=vcovHC(modelo44, type='HC1'))
modelo44.tab
modelo44_coef <- modelo44.tab[2,1]
modelo44_coef_se = modelo44.tab[2,2]
modelo44_lower = coefci(modelo44, df = Inf, vcov. = vcovHC, type = "HC1")[2,1]
modelo44_upper = coefci(modelo44, df = Inf, vcov. = vcovHC, type = "HC1")[2,2]

# Tabla de resultaods
table<- matrix(0, 4, 4) # 3 filas y 4 columnas
table[1,1]<- modelo41_coef
table[1,2]<- modelo41_coef_se
table[1,3]<- modelo41_lower
table[1,4]<- modelo41_upper

table[2,1]<- modelo42_coef
table[2,2]<- modelo42_coef_se
table[2,3]<- modelo42_lower
table[2,4]<- modelo42_upper

table[3,1]<- modelo43_coef
table[3,2]<- modelo43_coef_se
table[3,3]<- modelo43_lower
table[3,4]<- modelo43_upper

table[4,1]<- modelo44_coef
table[4,2]<- modelo44_coef_se
table[4,3]<- modelo44_lower
table[4,4]<- modelo44_upper


colnames(table)<- c("Estimate","se","lower_bound","upper_bound")
rownames(table)<- c("OLS baseline", "OLS with controls", "OLS baseline2","OLS with controls II")

# Exportación a Latex
xtable(table)
tab <- as.data.frame(table)
# Coef-plot

options(repr.plot.width = 8, repr.plot.height =5)  # dimensiones del gráfico

# aes: ejes

tab  %>% ggplot(aes(x=rownames(tab), y=Estimate)) +
  geom_point(size=2, color = 'black') +
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound) , width = 0.05,color="darkblue", linewidth = 0.8) +
  labs(x="", y="") + ggtitle("Coeficiente de la variable Treatment class x 2016 (95% CI)")  +
  theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.8) +
  scale_x_discrete(limits = c("OLS baseline", "OLS with controls", "OLS baseline2","OLS with controls II")) +
  scale_y_continuous(breaks = seq(-0.2,0.2,0.1) , limits = c(-0.2, 0.2)) +
  theme_classic(14)


# geom_errorbar solicita el limite inferior y superior
# width  : ancho de la abrra superior
# scale_x_discrete: Nombre de modelos en eje inferior
# geom_hline: añadir lines horizontal

#Guardar coef plot
ggsave("Coef_plot.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)
    
   
