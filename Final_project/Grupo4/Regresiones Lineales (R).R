
# Trabajo Final
# Integrantes: Lisbeth Ccoyo, Mishell Delgado, Steven Atoche

rm(list = ls())

library(readxl)
library(dplyr) 
library(rstudioapi)  
library(readr)
install.packages("lmtest")
library(lmtest)
library(stargazer)
library(lmtest)
library(sandwich)
library(xtable)
library(sandwich)
library(estimatr)
library(haven)
install.packages("estimatr")
install.packages("haven")

library(estimatr)
install.packages("estimatr")
install.packages("stargazer")
install.packages("lmtest")
install.packages("xtable")
install.packages("sandwich")
install.packages("lmtest")

install.packages("estimatr")

# Cargar el paquete
library(estimatr)




library(lmtest)
#####################################################################

# Leer el archivo de datos
SerraPorter <- haven::read_dta("C:/Users/Lisbeth/Documents/Trabajo Final en R/SerraPorterAEJ.dta")

# Seleccionar las columnas de interés
SerraPorter <- SerraPorter[, c("american", "instate", "freshman", "ACumGPA", "greek", "econ_hs", "varsity")]

# Generar la tabla de estadísticas descriptivas
table_stats <- summarytools::descr(SerraPorter)

# Seleccionar los estadísticos de interés
selected_stats <- table_stats[c("Mean", "Std.Dev", "Min", "Max", "N.Valid"), ]

# Transponer la tabla
summary_table <- t(selected_stats)

# Renombrar las filas del dataframe
rownames(summary_table) <- c("Cumulative GPA", "American student", "Took econ in high school", "Freshman", "Belongs to sorority", "In-state student", "Athlete")

# Cambiar los nombres de las columnas del dataframe
colnames(summary_table) <- c("Mean", "Standard Deviation", "Min", "Max", "Observations")

# Crear el objeto xtable
xtable_obj <- xtable(summary_table)

# Imprimir la tabla en formato LaTeX
print(xtable_obj, type = "latex", file = "tabla.tex")


#######################################################################

#REPLICAMOS LA TABLA 3
SerraPorter <- haven::read_dta("C:/Users/Lisbeth/Documents/Trabajo Final en R/SerraPorterAEJ.dta")


# MODELO 1
# Definir la variable de respuesta y
y <- SerraPorter$took_year

# Agregar la constante a las variables explicativas
X <- SerraPorter %>% 
  select(treat2016, yr_2016, treatment_class) %>%
  mutate(constant = 1)

# Verificar el dataframe X
X

# Función Homocedastica 
ols_model <- lm(y ~ ., data = X)

ols_summary <- summary(ols_model)
coefficients_table <- coef(ols_summary)

ols_summary <- summary(ols_model)
residuals_table <- ols_summary$residuals

ols_summary <- summary(ols_model)
print(ols_summary)

# Ajustar el modelo de regresión lineal
model <- lm(took_year ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter)

# Calcular el error estándar robusto de Huber-White
robust_se <- coeftest(model, vcov. = sandwich::vcovHC, type = "HC1")

# Imprimir el resumen del modelo
summary(model)

# Imprimir el error estándar robusto
print(robust_se)

# Ajustar el modelo de regresión lineal con error estándar robusto

ols_model_rb <- lm_robust(took_year ~ treat2016 + yr_2016 + treatment_class, data = SerraPorter, clusters = SerraPorter$class_fe2)

# Imprimir el resumen del modelo
print(summary(ols_model_rb))

# Acceder a la información de la tabla resumen
summary_table <- summary(ols_model_rb)

# Imprimir la tabla resumen
print(summary_table)

# Acceder a la tabla específica
table_1 <- summary_table$tables[[1]]
print(table_1)

# MODELO 2 QUE INCLYE LA VARIABLE DE ONTROL


# Definir la variable de respuesta
y <- SerraPorter$took_year

# Definir las variables de control en el data frame X
X <- data.frame(
  treat2016 = SerraPorter$treat2016,
  yr_2016 = SerraPorter$yr_2016,
  treatment_class = SerraPorter$treatment_class,
  female_prof = SerraPorter$female_prof,
  instate = SerraPorter$instate,
  freshman = SerraPorter$freshman,
  american = SerraPorter$american,
  ACumGPA = SerraPorter$ACumGPA,
  gradePrinciples = SerraPorter$gradePrinciples,
  small_class = SerraPorter$small_class
)

# Ajustar el modelo de regresión
model <- lm(y ~ ., data = X)

# Ver el resumen del modelo
summary(model)


# TERCER MODELO 

# Establecer la variable dependiente y las variables independientes
y <- SerraPorter$tookanother
X <- data.frame(
  treat2016 = SerraPorter$treat2016,
  yr_2016 = SerraPorter$yr_2016,
  treatment_class = SerraPorter$treatment_class
)

# Estimar el modelo de regresión
ols_model2 <- lm(y ~ ., data = X)

# Imprimir el resumen completo del modelo de regresión
print(summary(ols_model2))


# Cuarto Modelo 

# Establecer la variable dependiente y las variables independientes
y <- SerraPorter$tookanother
X <- data.frame(
  treat2016 = SerraPorter$treat2016,
  yr_2016 = SerraPorter$yr_2016,
  treatment_class = SerraPorter$treatment_class,
  female_prof = SerraPorter$female_prof,
  instate = SerraPorter$instate,
  freshman = SerraPorter$freshman,
  american = SerraPorter$american,
  ACumGPA = SerraPorter$ACumGPA,
  gradePrinciples = SerraPorter$gradePrinciples,
  small_class = SerraPorter$small_class
)

# Estimar el modelo de regresión
ols_model3 <- lm(y ~ ., data = X)

# Imprimir el resumen completo del modelo de regresión
print(summary(ols_model3))

#GENERAR TABLA RESUMEN
# Instalar el paquete "stargazer" si aún no está instalado
install.packages("stargazer")

# Cargar el paquete "stargazer"
library(stargazer)

# Crear la tabla con los modelos
# Instalar el paquete "stargazer" si aún no está instalado
# install.packages("stargazer")

# Cargar el paquete "stargazer"
library(stargazer)

# Crear la tabla con los modelos
table_models <- stargazer(
  summary_table, model, ols_model2, ols_model3,
  title = "Table 3—Treatment Effects on Intermediate Outcomes",
  column.labels = c("Took Micro within year", "Took Micro within year", "Took another econ class", "Took another econ class"),
  column.separate = c(1, 1, 1),
  align = TRUE,
  omit.stat = c("LL", "ser"),
  dep.var.caption = "",
  dep.var.labels.include = FALSE,
  notes = c("Standard errors in parentheses\n[*] p<0.05, [**] p<0.01, [***] p<0.001"),
  add.lines = list(
    c("Observations", length(SerraPorter$took_year), length(SerraPorter$took_year), length(SerraPorter$tookanother), length(SerraPorter$tookanother))
  )
)

# Imprimir la tabla en formato de texto
print(table_models, type = "text")

### TABLA 4

library(foreign)
library(estimatr)

# Establecer el directorio de trabajo
setwd("C:/Users/Lisbeth/Documents/Trabajo Final en R")

# Cargar los datos en un data frame
SerraPorter <- haven::read_dta("C:/Users/Lisbeth/Documents/Trabajo Final en R/SerraPorterAEJ.dta")


# Definir las variables globales
nocontrols <- c("yr_2016", "treatment_class", "treat2016")
controls <- c("yr_2016", "treatment_class", "treat2016", "female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")
controlsfe <- c("yr_2016", "treat2016", "instate", "freshman", "american", "ACumGPA", "gradePrinciples")
controlsgrade <- c("yr_2016", "treatment_class", "treat2016", "female_prof", "instate", "freshman", "american", "ACumGPA", "small_class")
controlsfegrade <- c("yr_2016", "treat2016", "instate", "freshman", "american", "ACumGPA")

# Contar el número de observaciones
n_obs <- nrow(SerraPorter)


# Fijar la semilla aleatoria
set.seed(123456789)

str(SerraPorter)

# Modelo 1: numeconclass sin controles
m1 <- lm_robust(numeconclass ~ ., data = subset(SerraPorter, female == 1), clusters = SerraPorter$class_fe2)

install.packages("boot")
library(boot)


# Pruebas de bootstrap para los coeficientes y constante en m1
boot_m1 <- boot(m1, R = 10000, weights = webb, clusters = SerraPorter$class_fe2, type = "boot")
exists("webb")

# Obtener los p-valores de las pruebas de bootstrap para m1
p_m1 <- summary(boot_m1)$boot$t[, c(grep("^treat", colnames(coef(m1))), "_(Intercept)")]
names(p_m1) <- c("treat2016", "treatment_class", "yr_2016", "Constant")
p_m1

# Agregar los p-valores a los resultados de m1
m1 <- est_add(m1, "p-values" = p_m1)

# Modelo 2: numeconclass con controles
m2 <- lm_robust(numeconclass ~ ., data = subset(SerraPorter, female == 1), clusters = SerraPorter$class_fe2)

# Pruebas de bootstrap para los coeficientes y constante en m2
boot_m2 <- boot(m2, R = 10000, weights = webb, clusters = SerraPorter$class_fe2, type = "boot")

# Obtener los p-valores de las pruebas de bootstrap para m2
p_m2 <- summary(boot_m2)$boot$t[, c(grep("^treat", colnames(coef(m2))), grep("^f", colnames(coef(m2)))), "_(Intercept)"]
names(p_m2) <- c("treat2016", "treatment_class", "yr_2016", "female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class", "Constant")
p_m2

# Agregar los p-valores a los resultados de m2
m2 <- est_add(m2, "p-values" = p_m2)

# Modelo 3: econmajor sin controles
m3 <- lm_robust(econmajor ~ ., data = subset(SerraPorter, female == 1), clusters = SerraPorter$class_fe2)

# Pruebas de bootstrap para los coeficientes y constante en m3
boot_m3 <- boot(m3, R = 10000, weights = webb, clusters = SerraPorter$class_fe2, type = "boot")

# Obtener los p-valores de las pruebas de bootstrap para m3
p_m3 <- summary(boot_m3)$boot$t[, c(grep("^treat", colnames(coef(m3))), "_(Intercept)")]
names(p_m3) <- c("treat2016", "treatment_class", "yr_2016", "Constant")
p_m3

# Agregar los p-valores a los resultados de m3
m3 <- est_add(m3, "p-values" = p_m3)

# Modelo 4: econmajor con controles
m4 <- lm_rob


### TABLA 5


# Cargar la librería "lmtest" para calcular el error estándar robusto
library(lmtest)

# Cargar los datos
SerraPorter <- haven::read_dta("C:/Users/Lisbeth/Documents/Trabajo Final en R/SerraPorterAEJ.dta")

# Modelo 1: Major_STEM
model1 <- lm(Major_STEM ~ yr_2016 + treatment_class + treat2016, data = SerraPorter)
summary(model1)

# Modelo 2: Major_Business
model2 <- lm(Major_Business ~ yr_2016 + treatment_class + treat2016 + female_prof + instate + freshman + american + ACumGPA + gradePrinciples + small_class, data = SerraPorter)
summary(model2)

# Modelo 3: Major_Finance
model3 <- lm(Major_Finance ~ yr_2016 + treatment_class + treat2016 + female_prof + instate + freshman + american + ACumGPA + gradePrinciples + small_class, data = SerraPorter)
summary(model3)

# Modelo 4: Major_Marketing
model4 <- lm(Major_Marketing ~ yr_2016 + treatment_class + treat2016 + female_prof + instate + freshman + american + ACumGPA + gradePrinciples + small_class, data = SerraPorter)
summary(model4)

###  TABLA 6
library(boot)
library(lmtest)
library(sandwich)
library(xtable)

set.seed(123456789)

# Regresión para Major_SocSc
model1 <- lm(Major_SocSc ~ ., data = subset(SerraPorter, female == 1))
boot_results1 <- boot(model1, R = 10000, statistic = function(data, idx) {
  fit <- lm(Major_SocSc ~ ., data = data[idx, ])
  return(coef(fit))
}, sim = "parametric", ran.gen = set.seed, strata = SerraPorter$class_fe2)
p_values1 <- sapply(names(coef(model1)), function(x) boot.ci(boot_results1, type = "perc", index = which(colnames(coef(model1)) == x))$percent[4])
p_values1 <- p_values1[match(names(coef(model1)), colnames(p_values1))]

# Regresión para Major_Arts
model2 <- lm(Major_Arts ~ ., data = subset(SerraPorter, female == 1))
boot_results2 <- boot(model2, R = 10000, statistic = function(data, idx) {
  fit <- lm(Major_Arts ~ ., data = data[idx, ])
  return(coef(fit))
}, sim = "parametric", ran.gen = set.seed, strata = SerraPorter$class_fe2)
p_values2 <- sapply(names(coef(model2)), function(x) boot.ci(boot_results2, type = "perc", index = which(colnames(coef(model2)) == x))$percent[4])
p_values2 <- p_values2[match(names(coef(model2)), colnames(p_values2))]

# Regresión para Major_Comm
model3 <- lm(Major_Comm ~ ., data = subset(SerraPorter, female == 1))
boot_results3 <- boot(model3, R = 10000, statistic = function(data, idx) {
  fit <- lm(Major_Comm ~ ., data = data[idx, ])
  return(coef(fit))
}, sim = "parametric", ran.gen = set.seed, strata = SerraPorter$class_fe2)
p_values3 <- sapply(names(coef(model3)), function(x) boot.ci(boot_results3, type = "perc", index = which(colnames(coef(model3)) == x))$percent[4])
p_values3 <- p_values3[match(names(coef(model3)), colnames(p_values3))]

# Regresión para Major_Hum
model4 <- lm(Major_Hum ~ ., data = subset(SerraPorter, female == 1))
boot_results4 <- boot(model4, R = 10000, statistic = function(data, idx) {
  fit <- lm(Major_Hum ~ ., data = data[idx, ])
  return(coef(fit))
}, sim = "parametric", ran.gen = set.seed, strata = SerraPorter$class_fe2)
p_values4 <- sapply(names(coef(model4)), function(x) boot.ci(boot_results4, type = "perc", index = which(colnames(coef(model4)) == x))$percent[4])
p_values4 <- p_values4[match(names(coef(model4)), colnames(p_values4))]

# Guardar los resultados
results <- data.frame(
  Variables = colnames(coef(model1)),
  Major_SocSc = coef(model1),
  P_Value_SocSc = p_values1,
  Major_Arts = coef(model2),
  P_Value_Arts = p_values2,
  Major_Comm = coef(model3),
  P_Value_Comm = p_values3,
  Major_Hum = coef(model4),
  P_Value_Hum = p_values4
)

# Exportar la tabla en formato LaTeX
table_latex <- xtable(results, caption = "Resultados de las regresiones", label = "tab:regression_results")
print(table_latex, type = "latex", sanitize.text.function = identity)
