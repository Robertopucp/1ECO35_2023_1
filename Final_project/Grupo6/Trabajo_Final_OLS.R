################  TRABAJO FINAL ############################


# Limpiamos y cargamos todas las librerias 
rm(list = ls())

graphics.off()

cat("\014")

options(scipen = 999)      # No scientific notation

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

# Cambiamos el directorio

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Cargamos la base de datos que utilizaremos

SerraPorter <- read_dta("../../data/trabajo_final/replicacion/SerraPorterAEJ.dta")

str(SerraPorter)

lapply(SerraPorter, class)

names(SerraPorter)

attributes(SerraPorter)

summary(SerraPorter)


# TABLE 1: DESCRIPTIVE STATISTICS ----
table1 <- SerraPorter %>% dplyr::select(american, instate, freshman,
                                        ACumGPA, greek, econ_hs, varsity) %>% as.data.frame()

stargazer(table1)

#Cargamos la lista de variables que tendra nuestro cuadro

list_vars <- c("Estudiante americano",
               "Estudiante in-state",
               "Estudiante de primer año",
               "GPA acumulado",
               "Estudiante pertenece a una fraternidad",
               "Estudiante llevó economía en High School",
               "Estudiante es atleta")

#Usamos stargazer para poder exportarlo a latex
stargazer(table1, title = "Estadísticas descriptivas", digits = 2, 
          covariate.labels = list_vars, 
          summary.stat = c("mean", "sd", "min", "max", "n"), 
          out = "../../output/tables/Eje1_tabla1.tex")



#REPLICAMOS LAS TABLAS 3, 4, 5 Y 6
#Primera regresion de la Tabla 3 (sin variable de control)

ols_model1 <- lm(took_year ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model1)

ols_model1$coefficients  # coeficientes

ols_model1$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model1)

summary(ols_model1)$call

summary(ols_model1)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model1, vcov = vcovHC(ols_model1, "HC1")) # Huber-White robust (STATA)

m1 <- lm(took_year ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

#Segunda regresión de la Tabla 3 (con variables de control)
ols_model2 <- lm(took_year ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

attributes(ols_model2)

ols_model2$coefficients  # coeficientes

ols_model2$fitted.values

#en resumen tenemos:
summary(ols_model2)

summary(ols_model2)$call

summary(ols_model2)$coef


glance(ols_model2)

tidy(ols_model2)

tidy(ols_model2)

htmlreg(ols_model2)

coeftest(ols_model2, vcov = vcovHC(ols_model2, "HC1")) # Huber-White robust (STATA)


control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA",
                  "gradePrinciples", "small_class")

model2_formula <- as.formula(
  paste("took_year", "~",
        paste("treat2016 + yr_2016 + treatment_class",
              paste(control_vars, collapse = "+"),
              sep = " + "
        )
  )
)

ols_model2 <- lm_robust(model2_formula, data = SerraPorter,
                        se_type = "stata")

summary(ols_model2)

glance(ols_model2)

tidy(ols_model2)

m2 <- lm(model2_formula, data = SerraPorter)

#Tercera regresion de la Tabla 3 (sin variable de control)

ols_model3 <- lm(tookanother ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model3)

ols_model3$coefficients  # coeficientes

ols_model3$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model3)

summary(ols_model3)$call

summary(ols_model3)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model3, vcov = vcovHC(ols_model3, "HC1")) # Huber-White robust (STATA)

m3 <- lm(took_year ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

#Cuarta regresión de la Tabla 3 (con variables de control)
ols_model4 <- lm(tookanother ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

attributes(ols_model4)

ols_model4$coefficients  # coeficientes

ols_model4$fitted.values

#en resumen tenemos:
summary(ols_model4)

summary(ols_model4)$call

summary(ols_model4)$coef


glance(ols_model4)

tidy(ols_model4)

tidy(ols_model4)

htmlreg(ols_model4)

coeftest(ols_model4, vcov = vcovHC(ols_model4, "HC1")) # Huber-White robust (STATA)


control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA",
                  "gradePrinciples", "small_class")

model4_formula <- as.formula(
  paste("tookanother", "~",
        paste("treat2016 + yr_2016 + treatment_class",
              paste(control_vars, collapse = "+"),
              sep = " + "
        )
  )
)

ols_model4 <- lm_robust(model4_formula, data = SerraPorter,
                        se_type = "stata")

summary(ols_model4)



glance(ols_model4)

tidy(ols_model4)

m4 <- lm(model4_formula, data = SerraPorter)

texreg(list(ols_model1, ols_model2, ols_model3, ols_model4),
       custom.coef.map = list("treat2016"="Treatment class × 2016",
                              "yr_2016"="Year 2016, t",
                              "treatment_class" = "Treatment class (in 2015)",
                              "female prof" = "female profesor",
                              "instate" = "in - state student",
                              "freshman" = "Freshman",
                              "american" = "american student",
                              "ACumGPA" = "cumulative GPA",
                              "gradePrinciples" = "grade in principles",
                              "small class" = "small class")
       
       
       , digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Country fixed effects" = c("no", "no", "yes", "yes",),
                              "Country-specific time trends" = c("no", "yes", "yes", "yes", "yes"),
                              "RMSE" = c(rmse1,rmse2),
                              caption = "Dependent Variable: Economic Growth Rate, t")
       
# TABLE 4: 

#Primera regresion de la Tabla 4 (sin variable de control)

ols_model5 <- lm(numeconclass ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model5)

ols_model5$coefficients  # coeficientes

ols_model5$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model5)

summary(ols_model5)$call

summary(ols_model5)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model5, vcov = vcovHC(ols_model5, "HC1")) # Huber-White robust (STATA)

m5 <- lm(numeconclass ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

#Segunda regresión de la Tabla 4 (con variables de control)
ols_model6 <- lm(numeconclass ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

attributes(ols_model6)

ols_model6$coefficients  # coeficientes

ols_model6$fitted.values

#en resumen tenemos:
summary(ols_model6)

summary(ols_model6)$call

summary(ols_model6)$coef


tidy(ols_model6)

htmlreg(ols_model6)

coeftest(ols_model6, vcov = vcovHC(ols_model6, "HC1")) # Huber-White robust (STATA)


control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA",
                  "gradePrinciples", "small_class")

model6_formula <- as.formula(
  paste("numeconclass", "~",
        paste("treat2016 + yr_2016 + treatment_class",
              paste(control_vars, collapse = "+"),
              sep = " + "
        )
  )
)

ols_model6 <- lm_robust(model6_formula, data = SerraPorter,
                        se_type = "stata")

summary(ols_model6)


tidy(ols_model6)

m6 <- lm(model6_formula, data = SerraPorter)

#Tercera regresion de la Tabla 4 (sin variable de control)

ols_model7 <- lm(econmajor ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model7)

ols_model7$coefficients  # coeficientes

ols_model7$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model7)

summary(ols_model7)$call

summary(ols_model7)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model7, vcov = vcovHC(ols_model7, "HC1")) # Huber-White robust (STATA)

m7 <- lm(econmajor ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

#Cuarta regresión de la Tabla 4 (con variables de control)
ols_model8 <- lm(econmajor ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

attributes(ols_model8)

ols_model8$coefficients  # coeficientes

ols_model8$fitted.values

#en resumen tenemos:
summary(ols_model8)

summary(ols_model8)$call

summary(ols_model8)$coef


glance(ols_model8)

tidy(ols_model8)

tidy(ols_model8)

htmlreg(ols_model8)

coeftest(ols_model8, vcov = vcovHC(ols_model8, "HC1")) # Huber-White robust (STATA)


control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA",
                  "gradePrinciples", "small_class")

model8_formula <- as.formula(
  paste("econmajor", "~",
        paste("treat2016 + yr_2016 + treatment_class",
              paste(control_vars, collapse = "+"),
              sep = " + "
        )
  )
)

ols_model8 <- lm_robust(model8_formula, data = SerraPorter,
                        se_type = "stata")

summary(ols_model8)



glance(ols_model8)

tidy(ols_model8)

m8 <- lm(model8_formula, data = SerraPorter)


texreg(list(ols_model1, ols_model2, ols_model3, ols_model4),
       custom.coef.map = list("treat2016"="Treatment class × 2016",
                              "yr_2016"="Year 2016, t",
                              "treatment_class" = "Treatment class (in 2015)",
                              "female prof" = "female profesor",
                              "instate" = "in - state student",
                              "freshman" = "Freshman",
                              "american" = "american student",
                              "ACumGPA" = "cumulative GPA",
                              "gradePrinciples" = "grade in principles",
                              "small class" = "small class")
       
       
       , digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Country fixed effects" = c("no", "no", "yes", "yes",),
                              "Country-specific time trends" = c("no", "yes", "yes", "yes", "yes"),
                              "RMSE" = c(rmse1,rmse2),
                              caption = "Dependent Variable: Economic Growth Rate, t")
# Export tables 4----
       
       
# stargazer::stargazer(ols_model5, se = starprep(ols_model5))
       
       
texreg(list(ols_model1, ols_model2),
              custom.coef.map = list("treat2016"="Treatment class × 2016",
                                     "yr_2016"="Year 2016, t",
                                     "treatment_class" = "Treatment class (in 2015)",
                                     "female prof" = "female profesor",
                                     "instate" = "in - state student",
                                     "freshman" = "Freshman",
                                     "american" = "american student",
                                     "ACumGPA" = "cumulative GPA",
                                     "gradePrinciples" = "grade in principles",
                                     "small_class" = "small_class")
              
              
              , digits = 3,
stars = c(0.01, 0.05, 0.1),
custom.gof.rows = list("Country fixed effects" = c("no", "no", "yes", "yes", "yes"),
                                     "Country-specific time trends" = c("no", "yes", "yes", "yes", "yes"),
                                     "RMSE" = c(rmse1,rmse2)
              
#TABLE 5: 
#La primera regresion

ols_model9 <- lm(Major_STEM ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model9)

ols_model9$coefficients  # coeficientes

ols_model9$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model9)

summary(ols_model9)$call

summary(ols_model9)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model9, vcov = vcovHC(ols_model9, "HC1")) # Huber-White robust (STATA)

m9 <- lm(Major_STEM ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)



#La segunda regresion

ols_model10 <- lm(Major_Business ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model10)

ols_model10$coefficients  # coeficientes

ols_model10$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model10)

summary(ols_model10)$call

summary(ols_model10)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model10, vcov = vcovHC(ols_model10, "HC1")) # Huber-White robust (STATA)

m10 <- lm(Major_Business ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#La tercera regresion

ols_model11 <- lm(Major_Finance ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model11)

ols_model11$coefficients  # coeficientes

ols_model11$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model11)

summary(ols_model11)$call

summary(ols_model11)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model11, vcov = vcovHC(ols_model11, "HC1")) # Huber-White robust (STATA)

m11 <- lm(Major_Finance ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

#La cuarta regresion

ols_model12 <- lm(Major_Marketing ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model12)

ols_model12$coefficients  # coeficientes

ols_model12$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model12)

summary(ols_model12)$call

summary(ols_model12)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model12, vcov = vcovHC(ols_model12, "HC1")) # Huber-White robust (STATA)

m12 <- lm(Major_Finance ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)



# Export tables 5----


# stargazer::stargazer(ols_model5, se = starprep(ols_model5))


texreg(list(ols_model1, ols_model2),
       custom.coef.map = list("treat2016"="Treatment class × 2016",
                              "yr_2016"="Year 2016, t",
                              "treatment_class" = "Treatment class (in 2015)",
                              "female prof" = "female profesor",
                              "instate" = "in - state student",
                              "freshman" = "Freshman",
                              "american" = "american student",
                              "ACumGPA" = "cumulative GPA",
                              "gradePrinciples" = "grade in principles",
                              "small_class" = "small_class")
       
       
       , digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Country fixed effects" = c("no", "no", "yes", "yes", "yes"),
                              "Country-specific time trends" = c("no", "yes", "yes", "yes", "yes"),
                              "RMSE" = c(rmse1,rmse2)
                              

#TABLE 6: 
#La primera regresion

ols_model13 <- lm(Major_SocSc ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model13)

ols_model13$coefficients  # coeficientes

ols_model13$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model13)

summary(ols_model13)$call

summary(ols_model13)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model13, vcov = vcovHC(ols_model13, "HC1")) # Huber-White robust (STATA)

m13 <- lm(Major_SocSc ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)



#La segunda regresion

ols_model14 <- lm(Major_Arts ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model14)

ols_model14$coefficients  # coeficientes

ols_model14$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model14)

summary(ols_model14)$call

summary(ols_model14)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model14, vcov = vcovHC(ols_model14, "HC1")) # Huber-White robust (STATA)

m14 <- lm(Major_Business ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#La tercera regresion

ols_model15 <- lm(Major_Comm ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model15)

ols_model15$coefficients  # coeficientes

ols_model15$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model15)

summary(ols_model15)$call

summary(ols_model15)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model15, vcov = vcovHC(ols_model15, "HC1")) # Huber-White robust (STATA)

m15 <- lm(Major_Comm ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

#La cuarta regresion

ols_model16 <- lm(Major_Hum ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)


#– took year: dummy = 1 si la estudiante se matricula en cursos intermedios de
#Economía luego del curso Introductorio de Economía (Principios en Economía).

attributes(ols_model16)

ols_model16$coefficients  # coeficientes

ols_model16$fitted.values  # Y estimado

#en resumen tenemos:
summary(ols_model16)

summary(ols_model16)$call

summary(ols_model16)$coef

# Ajustamos el modelo de regresión lineal con error estándar robusto de Huber-White
coeftest(ols_model16, vcov = vcovHC(ols_model16, "HC1")) # Huber-White robust (STATA)

m12 <- lm(Major_Hum ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)




# Export tables 6----


# stargazer::stargazer(ols_model5, se = starprep(ols_model5))


texreg(list(ols_model1, ols_model2),
       custom.coef.map = list("treat2016"="Treatment class × 2016",
                              "yr_2016"="Year 2016, t",
                              "treatment_class" = "Treatment class (in 2015)",
                              "female prof" = "female profesor",
                              "instate" = "in - state student",
                              "freshman" = "Freshman",
                              "american" = "american student",
                              "ACumGPA" = "cumulative GPA",
                              "gradePrinciples" = "grade in principles",
                              "small_class" = "small_class")
       
       
       , digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Country fixed effects" = c("no", "no", "yes", "yes", "yes"),
                              "Country-specific time trends" = c("no", "yes", "yes", "yes", "yes"),
                              "RMSE" = c(rmse1,rmse2)
                              
                              
    
### 1.6 Crear un coefplot de la tabla 4
##    
    
    
as.list(sapply(SerraPorter, class))
    
SerraPorter$Dummy <- ifelse(SerraPorter$treat2016 > 0, 1, 0)
    
model1 <- lm(treat2016 ~ Dummy, data = SerraPorter)
    
attributes(model1)
    
    
model1.tab <- coeftest(model1, vcov=vcovHC(model1, type='HC1'))
model1.tab
    
# HC: heteerocedasticidad, HC1: matriz varianza y cov de Huber - White
    
model1_coef <- model1.tab[2,1]
    
model1_coef_se = model1.tab[2,2]
    
# HC1: standar error robust aginst heterocedasticity
    
# Intervalo de confianza
# intervalo de cofianza ajsutado por heterocedasticidad
    
model1_lower = coefci(model1, df = Inf, vcov. = vcovHC, type = "HC1")[2,1]
    
model1_upper = coefci(model1, df = Inf, vcov. = vcovHC, type = "HC1")[2,2]
    
    
    
# Tabla de resultaods
    
table<- matrix(0, 3, 4) # 3 filas y 4 columnas
    
    
table[1,1]<- model1_coef
table[1,2]<- model1_coef_se
    
table[1,3]<- model1_lower
table[1,4]<- model1_upper
    
    
    
colnames(table)<- c("Estimate","se","lower_bound","upper_bound")
rownames(table)<- c("OLS baseline", "OLS with controls", "OLS with controls II")
    
xtable(table)
    
    
tab <- as.data.frame(table)
    
# table de matriz a dataframe (tab)
    
# Coef-plot
    
options(repr.plot.width = 8, repr.plot.height =5)  # dimensiones del gráfico
    
# aes: ejes
    
tab  %>% ggplot(aes(x=rownames(tab), y=Estimate)) +
geom_point(size=2, color = 'black') +
geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound) , width = 0.05,color="darkblue", linewidth = 0.8) +
  labs(x="", y="") + ggtitle("coefplot de la tabla 4")  +
  theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.8) +
  scale_x_discrete(limits = c("OLS baseline",
                                  "OLS with controls", "OLS with controls II")) +
  scale_y_continuous(breaks = seq(-0.2,0.2,0.1) , limits = c(-0.2, 0.2)) +
  theme_classic(14)
    
    
# geom_errorbar solicita el limite inferior y superior
# width  : ancho de la abrra superior
# scale_x_discrete: Nombre de modelos en eje inferior
# geom_hline: añadir lines horizontal
# panel.grid.major = element_blank(), panel.grid.minor = element_blank() borra las cuadrículas en el fondo
    
ggsave("../../output/plots/Coef_plot1.png"
           , height = 8  # alto
           , width = 12  # ancho
           , dpi = 320   # resolución (calidad de la imagen)
 )
    
    
# Model Matrix -----------------
    
m <- lm(treat2016 ~ Dummy, data = SerraPorter)
X <- as.data.frame( model.matrix(m) )
    
    
    
                            