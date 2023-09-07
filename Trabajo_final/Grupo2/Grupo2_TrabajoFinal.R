
# Trabajo Final - Grupo 2

# Regresiones Lineales----

rm(list = ls()) # clean environment variables
graphics.off()  # clean plots
cat("\014")  # clean console
options(scipen = 999)  # additional options 

# Se carga las librerías necesarias

library(pacman) 

p_load(
  tidyverse  
  , haven   
  , fastDummies  
  , stargazer  
  , sandwich  
  , lmtest 
  , estimatr 
  , lfe  
  , caret 
  , texreg 
  , mfx 
)

# Directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Se carga la base

genderdata <- read_dta("../../data/trabajo_final/replicacion/SerraPorterAEJ.dta")

# Estadísticas descriptivas----

tab <- genderdata %>% dplyr::select(american, instate, freshman,
                                    ACumGPA, greek, econ_hs, varsity) %>% as.data.frame()

## Se selecciona solo las variables de nuestro interés
## Se convierte a dataframe porque la librería stargazer exige que la base de datos sea DataFrame

## Exportar tabla de Estadísticas descriptivas a Latex----

list_vars <- c("American student","In-state student","Freshman","Cumulative GPA",
               "Belongs to soroty","Took econ in high school","Athlete"
)


stargazer(tab, title = "Descriptive Statistics", digits = 2,   # decimales con 2 digitos
          covariate.labels = list_vars,  # Lista de etiquetas 
          summary.stat = c("mean", "sd", "min", "max" ,"n"), # se especifica el orden de los estadísticos
          notes.align = 'l'
          , out = "../../Trabajo_final/Grupo2/estat.tex")



# Réplica de tablas----
## Réplica de tablas del paper Gender Differences in the Choice of Major: The Importance of Female Role Models 
## de Chaterine Porter y Danila Serra:

## Se define las variables de controles
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA",
                  "gradePrinciples", "small_class")


# Tabla 3----

## Fórmulas de regresión----
## Se define las fórmulas a regresionar de cada columna de la tabla a replicar

formula_model1 <- as.formula(
  paste("took_year",        # se define el outcome
        "~",
        paste("yr_2016","treatment_class","treat2016",   #se define los regresores
              sep="+")
  )
)


formula_model2 <- as.formula(
  paste("took_year",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model3 <- as.formula(
  paste("tookanother",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              sep="+")
  )
)

formula_model4 <- as.formula(
  paste("tookanother",
        "~",
        paste("yr_2016","treatment_class","treat2016" ,
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

## Regresión de los modelos----

ols_model1 <- lm_robust(formula_model1, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model2 <- lm_robust(formula_model2, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model3 <- lm_robust(formula_model3, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model4 <- lm_robust(formula_model4, data = genderdata,
                        clusters = class_fe2, se_type = "stata")


## Ajuste del modelo con lm() y obtener errores estándar robustos


lm_model1 <- lm(formula_model1, data = genderdata)
robust_model1 <- lmtest::coeftest(lm_model1, vcov = sandwich::vcovHC(lm_model1, cluster = genderdata$class_fe2, type = "HC1"))

lm_model2 <- lm(formula_model2, data = genderdata)
robust_model2 <- lmtest::coeftest(lm_model2, vcov = sandwich::vcovHC(lm_model2, cluster = genderdata$class_fe2, type = "HC1"))

lm_model3 <- lm(formula_model3, data = genderdata)
robust_model3 <- lmtest::coeftest(lm_model3, vcov = sandwich::vcovHC(lm_model3, cluster = genderdata$class_fe2, type = "HC1"))

lm_model4 <- lm(formula_model4, data = genderdata)
robust_model4 <- lmtest::coeftest(lm_model4, vcov = sandwich::vcovHC(lm_model4, cluster = genderdata$class_fe2, type = "HC1"))


## Exportar tabla a latex----

stargazer(lm_model1, lm_model2, lm_model3, lm_model4,
          title = "TREATMENT EFFECTS ON INTERMEDIATE OUTCOMES",       ## título de la tabla
          align = TRUE,
          dep.var.labels = c("Took Micro within year", "Took Micro within year", "Took another econ class", "Took another econ class"),
          se = list(robust_model1[, "Std. Error"], robust_model2[, "Std. Error"],
                    robust_model3[, "Std. Error"], robust_model4[, "Std. Error"]),
          keep = c("treat2016", "yr_2016", "treatment_class", "Constant"),
          covariate.labels = c("Year 2016", "Treatment class (in 2015)", "Treatment class x 2016", "Constant"),
          type = "text",
          omit.stat = "f",
          no.space = TRUE,
          add.lines = list(c("Controls", "No", "Yes", "No", "Yes")),      ## se agrega una fila
          keep.stat = "n",
          notes.append = FALSE,
          notes.align = "l",
          notes = "Huber robust standard errors are in parentheses",
          style = "qje",
          out = "../../Trabajo_final/Grupo2/tabla3R.tex")


#Tabla 4----

## Fórmulas de regresión----
## Se define las fórmulas a regresionar de cada columna de la tabla a replicar

formula_model5 <- as.formula(
  paste("numeconclass",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              sep="+")
  )
)


formula_model6 <- as.formula(
  paste("numeconclass",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model7 <- as.formula(
  paste("econmajor",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              sep="+")
  )
)

formula_model8 <- as.formula(
  paste("econmajor",
        "~",
        paste("yr_2016","treatment_class","treat2016" ,
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

## Regresión de los modelos----

ols_model5 <- lm_robust(formula_model5, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model6 <- lm_robust(formula_model6, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model7 <- lm_robust(formula_model7, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model8 <- lm_robust(formula_model8, data = genderdata,
                        clusters = class_fe2, se_type = "stata")



## Ajuste del modelo con lm() y obtener errores estándar robustos

lm_model5 <- lm(formula_model5, data = genderdata)
robust_model5 <- lmtest::coeftest(lm_model5, vcov = sandwich::vcovHC(lm_model5, cluster = genderdata$class_fe2, type = "HC1"))

lm_model6 <- lm(formula_model6, data = genderdata)
robust_model6 <- lmtest::coeftest(lm_model6, vcov = sandwich::vcovHC(lm_model6, cluster = genderdata$class_fe2, type = "HC1"))

lm_model7 <- lm(formula_model7, data = genderdata)
robust_model7 <- lmtest::coeftest(lm_model7, vcov = sandwich::vcovHC(lm_model7, cluster = genderdata$class_fe2, type = "HC1"))

lm_model8 <- lm(formula_model8, data = genderdata)
robust_model8 <- lmtest::coeftest(lm_model8, vcov = sandwich::vcovHC(lm_model8, cluster = genderdata$class_fe2, type = "HC1"))


## Exportar tabla a Latex----

stargazer(lm_model5, lm_model6, lm_model7, lm_model8,
          title = "TREATMENT EFFECTS ON FINAL OUTCOMES",
          align = TRUE,
          dep.var.labels = c("Number of econ classes taken", "Number of econ classes taken", "Major in economics", "Major in economics"),
          se = list(robust_model5[, "Std. Error"], robust_model6[, "Std. Error"],
                    robust_model7[, "Std. Error"], robust_model8[, "Std. Error"]),
          keep = c("treat2016", "yr_2016", "treatment_class", "Constant"),
          covariate.labels = c("Year 2016", "Treatment class (in 2015)", "Treatment class x 2016", "Constant"),
          type = "text",
          omit.stat = "f",
          no.space = TRUE,
          add.lines = list(c("Controls", "No", "Yes", "No", "Yes")),
          keep.stat = "n",
          notes.append = FALSE,
          notes.align = "l",
          notes = "Huber robust standard errors are in parentheses",
          style = "qje",
          out = "../../Trabajo_final/Grupo2/tabla4R.tex")


#Tabla 5----

## Fórmulas de regresión----
## Se define las fórmulas a regresionar de cada columna de la tabla a replicar


formula_model9 <- as.formula(
  paste("Major_STEM",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model10 <- as.formula(
  paste("Major_Business",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model11 <- as.formula(
  paste("Major_Finance",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model12 <- as.formula(
  paste("Major_Marketing",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)


## Regresión de los modelos----

ols_model9 <- lm_robust(formula_model9, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model10 <- lm_robust(formula_model10, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model11 <- lm_robust(formula_model11, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model12 <- lm_robust(formula_model12, data = genderdata,
                        clusters = class_fe2, se_type = "stata")



## Ajuste del modelo con lm() y obtener errores estándar robustos

lm_model9 <- lm(formula_model9, data = genderdata)
robust_model9 <- coeftest(lm_model9, vcov = vcovCL(lm_model9, cluster = genderdata$class_fe2))

lm_model10 <- lm(formula_model10, data = genderdata)
robust_model10 <- coeftest(lm_model10, vcov = vcovCL(lm_model10, cluster = genderdata$class_fe2))

lm_model11 <- lm(formula_model11, data = genderdata)
robust_model11 <- coeftest(lm_model11, vcov = vcovCL(lm_model11, cluster = genderdata$class_fe2))

lm_model12 <- lm(formula_model12, data = genderdata)
robust_model12 <- coeftest(lm_model12, vcov = vcovCL(lm_model12, cluster = genderdata$class_fe2))

## Exportar tabla a Latex----

stargazer(lm_model9, lm_model10, lm_model11, lm_model12,
          title = "TREATMENT EFFECTS ON HIGH-EARNING MAJORS",
          align = TRUE,
          dep.var.labels = c("Major STEM", "Major Business", "Major Finance", "Major Marketing"),
          se = list(robust_model9[, "Std. Error"], robust_model10[, "Std. Error"],
                    robust_model11[, "Std. Error"], robust_model12[, "Std. Error"]),
          keep = c("treat2016", "yr_2016", "treatment_class", "Constant"),
          covariate.labels = c("Year 2016", "Treatment class (in 2015)", "Treatment class x 2016", "Constant"),
          type = "text",
          omit.stat = "f",
          no.space = TRUE,
          add.lines = list(c("Controls", "Yes", "Yes", "Yes", "Yes")),
          keep.stat = "n",
          notes.append = FALSE,
          notes.align = "l",
          notes = "Huber robust standard errors are in parentheses",
          style = "qje",
          out = "../../Trabajo_final/Grupo2/tabla5R.tex")



# Tabla 6----

## Fórmulas de regresión----
## Se define las fórmulas a regresionar de cada columna de la tabla a replicar


formula_model13 <- as.formula(
  paste("Major_SocSc",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model14 <- as.formula(
  paste("Major_Arts",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model15 <- as.formula(
  paste("Major_Comm",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)

formula_model16 <- as.formula(
  paste("Major_Hum",
        "~",
        paste("yr_2016","treatment_class","treat2016",
              paste(control_vars, collapse = "+"),
              sep="+")
  )
)


## Regresión de los modelos----

ols_model13 <- lm_robust(formula_model13, data = genderdata,
                        clusters = class_fe2, se_type = "stata")

ols_model14 <- lm_robust(formula_model14, data = genderdata,
                         clusters = class_fe2, se_type = "stata")

ols_model15 <- lm_robust(formula_model15, data = genderdata,
                         clusters = class_fe2, se_type = "stata")

ols_model16 <- lm_robust(formula_model16, data = genderdata,
                         clusters = class_fe2, se_type = "stata")


## Ajuste del modelo con lm() y obtener errores estándar robustos

lm_model13 <- lm(formula_model13, data = genderdata)
robust_model13 <- coeftest(lm_model13, vcov = vcovHC(lm_model13, cluster = ~ class_fe2, type = "HC1"))

lm_model14 <- lm(formula_model14, data = genderdata)
robust_model14 <- coeftest(lm_model14, vcov = vcovHC(lm_model14, cluster = ~ class_fe2, type = "HC1"))

lm_model15 <- lm(formula_model15, data = genderdata)
robust_model15 <- coeftest(lm_model15, vcov = vcovHC(lm_model15, cluster = ~ class_fe2, type = "HC1"))

lm_model16 <- lm(formula_model16, data = genderdata)
robust_model16 <- coeftest(lm_model16, vcov = vcovHC(lm_model16, cluster = ~ class_fe2, type = "HC1"))



## Exportar tabla a Latex----


stargazer::stargazer(lm_model13, lm_model14, lm_model15, lm_model16,
                     title = "TREATMENT EFFECTS ON LOW-EARNING MAJORS",
                     dep.var.labels = c("Major social sciences", "Major arts", "Major communication", "Major humanities"),
                     se = list(robust_model13[, "Std. Error"], robust_model14[, "Std. Error"],
                               robust_model15[, "Std. Error"], robust_model16[, "Std. Error"]),
                     keep = c("treat2016", "yr_2016", "treatment_class", "Constant"),
                     covariate.labels = c("Year 2016", "Treatment class (in 2015)", "Treatment class x 2016", "Constant"),
                     type = "text",
                     omit.stat = "f",
                     align = TRUE,
                     no.space = TRUE,
                     add.lines = list(c("Controls", "Yes", "Yes", "Yes", "Yes")),
                     keep.stat = c("n"),
                     notes.append = FALSE,
                     notes.align = "l",
                     notes = "Huber robust standard errors are in parentheses",
                     style = "qje",
                     out = "../../Trabajo_final/Grupo2/tabla6R.tex")


# Coefplot - Tabla 4----

# Modelo 5

ols_model5.tab <- coeftest(ols_model5)
ols_model5.tab

ols_model5_coef <- ols_model5.tab[4,1]  ##se extrae el coeficiente 
ols_model5_coef_se = ols_model5.tab[4,2]   ## se extrae el error estandar

## Intervalo de confianza

ols_model5_lower = coefci(ols_model5)[4,1] ##se extrae el límite inferior
ols_model5_upper = coefci(ols_model5)[4,2] ##se extrae el límite superior

# Modelo 6

ols_model6.tab <- coeftest(ols_model6)
ols_model6.tab

ols_model6_coef <- ols_model6.tab[4,1]  ##se extrae el coeficiente 
ols_model6_coef_se = ols_model6.tab[4,2]   ## se extrae el error estandar

## Intervalo de confianza

ols_model6_lower = coefci(ols_model6)[4,1] ##se extrae el límite inferior
ols_model6_upper = coefci(ols_model6)[4,2] ##se extrae el límite superior

# Modelo 7

ols_model7.tab <- coeftest(ols_model7)
ols_model7.tab

ols_model7_coef <- ols_model7.tab[4,1]  ##se extrae el coeficiente 
ols_model7_coef_se = ols_model7.tab[4,2]   ## se extrae el error estandar

## Intervalo de confianza

ols_model7_lower = coefci(ols_model7)[4,1] ##se extrae el límite inferior
ols_model7_upper = coefci(ols_model7)[4,2] ##se extrae el límite superior


# Modelo 8

ols_model8.tab <- coeftest(ols_model8)
ols_model8.tab

ols_model8_coef <- ols_model8.tab[4,1]  ##se extrae el coeficiente 
ols_model8_coef_se = ols_model8.tab[4,2]   ## se extrae el error estandar

## Intervalo de confianza

ols_model8_lower = coefci(ols_model8)[4,1] ##se extrae el límite inferior
ols_model8_upper = coefci(ols_model8)[4,2] ##se extrae el límite superior

## Tabla de resultados----

table<- matrix(0, 4, 4)   # se crea una tabla vacía de 4 filas y 4 columnas

table[1,1]<- ols_model5_coef 
table[1,2]<- ols_model5_coef_se

table[2,1]<- ols_model6_coef 
table[2,2]<- ols_model6_coef_se

table[3,1]<- ols_model7_coef 
table[3,2]<- ols_model7_coef_se

table[4,1]<- ols_model8_coef 
table[4,2]<- ols_model8_coef_se

table[1,3]<- ols_model5_lower
table[1,4]<- ols_model5_upper

table[2,3]<- ols_model6_lower
table[2,4]<- ols_model6_upper

table[3,3]<- ols_model7_lower
table[3,4]<- ols_model7_upper

table[4,3]<- ols_model8_lower
table[4,4]<- ols_model8_upper

colnames(table)<- c("Estimate","se","lower_bound","upper_bound")    ## nombre de las columnas de la tabla
rownames(table)<- c("Number of econ \n classes", "Number of econ \n classes using \n control variables", "Major in economics", 
                    "Major in economics \n using control variables")    ## nombre de las filas de la tabla

tab <- as.data.frame(table)  ## se convierte la tabla a un data frame

## Formato de la imagen----

options(repr.plot.width = 10, repr.plot.height =6)  # dimensiones del gráfico

# aes: ejes

tab  %>% ggplot(aes(x=rownames(tab), y=Estimate)) +
  geom_point(size=2, color = 'black') +
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound) , width = 0.05,color="darkblue", linewidth = 0.8) +
  labs(x="", y="") + ggtitle("Treatment class x 2016 coefficient (95% CI)")  +
  theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.8) +
  scale_x_discrete(limits = c("Number of econ \n classes",
                              "Number of econ \n classes using \n control variables", "Major in economics", 
                              "Major in economics \n using control variables")) +
  scale_y_continuous(breaks = seq(0.0,1.2,0.2) , limits = c(0.0, 1.3)) +
  theme_classic(14)


## geom_errorbar solicita el limite inferior y superior
## width  : ancho de la barra superior
## scale_x_discrete: Nombre de modelos en eje inferior
## geom_hline: añadir lines horizontal
## panel.grid.major = element_blank(), panel.grid.minor = element_blank() borra las cuadrículas en el fondo


# Guardar gráfico

ggsave("../../Trabajo_final/Grupo2/graph_treat2016_R.png",
       height = 8,  # alto
       width = 12,  # ancho
       dpi = 320   # resolución (calidad de la imagen)
)
