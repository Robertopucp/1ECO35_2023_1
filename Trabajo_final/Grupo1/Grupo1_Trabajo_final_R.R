#TRABAJO FINAL------------------------------------------------
#Grupo 1 - Keyth Hurtado, Melani Geng, Fatima Trujillo

# Clean y opciones adicionales

rm(list = ls())
graphics.off()

#REGRESIONES LINEALES-----------------------------------------

## Librerias, Base de Datos y Estadísticas Descriptivas-------

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
  , xtable # xtable: exportar matriz o dataframe, table a latex
)

# Directorio

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Importamos la base de datos y mostramos algunos atributos

data <- read_dta("../../data/trabajo_final/replicacion/SerraPorterAEJ.dta")

str(data)

lapply(data, class)

names(data)

attributes(data)

summary(data)

# Observamos los nombres de las variables

colnames(data)

# Tabla de Estadísticas Descriptivas

tabla_estadisticas <- data %>% dplyr::select('american', 'instate', 'freshman',
                                    'ACumGPA', 'greek', 'econ_hs',
                                    'varsity') %>% as.data.frame()

# Hacemos uso de Stargazer para poder adecuar la tabla y exportar en Latex

stargazer(tabla_estadisticas, title = "Descriptive Statistics", digits = 2, 
          covariate.labels = c("Estudiante americano","Estudiante in-state", "Estudiante de primer año",
                               "GPA acumulado", "Estudiante que pertenece a una fraternidad o sororidad",
                               "Estudiante que llevó Economía en High School", "Estudiante atleta"),
          min.max = T,
          summary.stat = c("n", "mean", "sd","min","max"), # Orden de estadísticos
          out = "../.../trabajo_final/Grupo1/tabla_estadisticas_R.tex")

# Se crea un vector de las variables de control para realizar las regresiones

control <- c('female_prof', 'instate', 'freshman', 'american',
               'ACumGPA', 'gradePrinciples','small_class')



## Regresiones Tabla 3----------------------------------------

# Modelo 1 - Tabla3

ols_model1_tabla3 <- lm_robust(took_year ~ treat2016 + yr_2016 + treatment_class, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model1_t3)

# Modelo 2 - Tabla3

model2_tabla3_formula <- as.formula(paste("took_year","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model2_tabla3 <- lm_robust(model2_t3_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model2_tabla3)

# Modelo 3 - Tabla3

ols_model3_tabla3 <- lm_robust(tookanother ~ treat2016 + yr_2016 + treatment_class, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model3_tabla3)

# Modelo 4 - Tabla3

model4_t3_formula <- as.formula(paste("tookanother","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model4_tabla3 <- lm_robust(model4_t3_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model4_tabla3)

# Creamos la tabla de resultados que agrupa los modelos de la tabla 3

texreg(list(ols_model1_tabla3, ols_model2_tabla3, ols_model3_tabla3, ols_model4_tabla3),
       caption = "Table 3 - Treatment Effects on Intermediate Outcomes",
       caption.above = T,
       custom.model.names = c("Took Micro within year","Took Micro within year",
                              "Took another econ class", "Took another econ class"),
       custom.coef.map = list("treat2016"="Treatment class x 2016",
                              "yr_2016"="Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"),
       digits = 3,
       ci.force = F,
       omit = c("rsq", "adjr2", "df", "resid.df", "p", "fstat"),
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Controls" = c("No","Yes", "No","Yes")),
       file = "../../trabajo_final/Grupo1/tabla_3_reg_R.tex")

## Regresiones Tabla 4----------------------------------------

# Modelo 1 - Tabla4

ols_model1_tabla4 <- lm_robust(numeconclass ~ treat2016 + yr_2016 + treatment_class, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model1_tabla4)

# Modelo 2 - Tabla4

model2_t4_formula <- as.formula(paste("numeconclass","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model2_tabla4 <- lm_robust(model2_t4_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model2_tabla4)

# Modelo 3 - Tabla4

ols_model3_tabla4 <- lm_robust(econmajor ~ treat2016 + yr_2016 + treatment_class, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model3_tabla4)

# Modelo 4 - Tabla4

model4_t4_formula <- as.formula(paste("econmajor","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model4_t4 <- lm_robust(model4_t4_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model4_tabla4)

# Creamos la tabla de resultados que agrupa los modelos de la tabla 4

texreg(list(ols_model1_tabla4, ols_model2_tabla4, ols_model3_tabla4, ols_model4_tabla4),
       caption = "Table 4 - Treatment Effects on Final Outcomes",
       caption.above = T,
       custom.model.names = c("Number of econ classes taken","Number of econ classes taken",
                              "Major in economics", "Major in economics"),
       custom.coef.map = list("treat2016"="Treatment class x 2016",
                              "yr_2016"="Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"),
       digits = 3,
       ci.force = F,
       omit = c("rsq", "adjr2", "df", "resid.df", "p", "fstat"),
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Controls" = c("No","Yes", "No","Yes")),
       file = "../../trabajo_final/Grupo1/tabla_4_reg_R.tex")

## Regresiones Tabla 5----------------------------------------

# Modelo 1 - Tabla5

model1_t5_formula <- as.formula(paste("Major_STEM","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model1_tabla5 <- lm_robust(model1_t5_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model1_tabla5)

# Modelo 2 - Tabla5

model2_t5_formula <- as.formula(paste("Major_Business","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model2_tabla5 <- lm_robust(model2_t5_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model2_tabla5)

# Modelo 3 - Tabla5

model3_t5_formula <- as.formula(paste("Major_Finance","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model3_tabla5 <- lm_robust(model3_t5_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model3_tabla5)

# Modelo 4 - Tabla5

model4_t5_formula <- as.formula(paste("Major_Marketing","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model4_tabla5 <- lm_robust(model4_t5_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model4_tabla5)

# Creamos la tabla de resultados que agrupa los modelos de la tabla 5

texreg(list(ols_model1_tabla5, ols_model2_tabla5, ols_model3_tabla5, ols_model4_tabla5),
       caption = "Table 5 - Treatment Effects on Other High-Earning Majors",
       caption.above = T,
       custom.model.names = c("Major STEM","Major finance",
                              "Major business", "Major marketing"),
       custom.coef.map = list("treat2016"="Treatment class x 2016",
                              "yr_2016"="Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"),
       digits = 3,
       ci.force = F,
       omit = c("rsq", "adjr2", "df", "resid.df", "p", "fstat"),
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Controls" = c("Yes","Yes", "Yes","Yes")),
       file = "../../trabajo_final/Grupo1/tabla_5_reg_R.tex")


## Regresiones Tabla 6----------------------------------------

# Modelo 1 - Tabla6

model1_t6_formula <- as.formula(paste("Major_SocSc","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model1_tabla6 <- lm_robust(model1_t6_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model1_tabla6)

# Modelo 2 - Tabla6

model2_t6_formula <- as.formula(paste("Major_Arts","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model2_t6 <- lm_robust(model2_t6_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model2_tabla6)

# Modelo 3 - Tabla6

model3_t6_formula <- as.formula(paste("Major_Comm","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model3_t6 <- lm_robust(model3_t6_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model3_tabla6)

# Modelo 4 - Tabla6

model4_t6_formula <- as.formula(paste("Major_Hum","~",
                                      paste("treat2016","yr_2016", "treatment_class",
                                            paste(control, collapse = "+"),  sep="+")))

ols_model4_tabla6 <- lm_robust(model4_t6_formula, data = data,
                           clusters = class_fe2, se_type = "stata")

summary(ols_model4_tabla6)

# Creamos la tabla de resultados que agrupa los modelos de la tabla 6

texreg(list(ols_model1_tabla6, ols_model2_tabla6, ols_model3_tabla6, ols_model4_tabla6),
       caption = "Table 6 - Treatment Effects on Low-Earning Majors",
       caption.above = T,
       custom.model.names = c("Major social sciences","Major arts",
                              "Major communication", "Major humanities"),
       custom.coef.map = list("treat2016"="Treatment class x 2016",
                              "yr_2016"="Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"),
       digits = 3,
       ci.force = F,
       omit = c("rsq", "adjr2", "df", "resid.df", "p", "fstat"),
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Controls" = c("Yes","Yes", "Yes","Yes")),
       file = "../../trabajo_final/Grupo1/tabla_6_reg_R.tex")


## Coefplot Tabla 4------------------------------------------

# Identificamos las tablas de coeficientes de todas las regresiones de la Tabla 4

sum_model1_tabla4 <- summary(ols_model1_tabla4)$coefficients
sum_model2_tabla4 <- summary(ols_model2_tabla4)$coefficients
sum_model3_tabla4 <- summary(ols_model3_tabla4)$coefficients
sum_model4_tabla4 <- summary(ols_model4_tabla4)$coefficients

# Ejemplo: primera regresión de la tabla 4 

sum_model1_tabla4 <- summary(ols_model1_tabla4)$coefficients
sum_model1_tabla4

# Extraemos los coeficientes para la variable Treatment class x 2016 (treat2016)

model1_tabla4_coef = sum_model1_t4[2,1]
model2_tabla4_coef = sum_model2_t4[2,1]
model3_tabla4_coef = sum_model3_t4[2,1]
model4_tabla4_coef = sum_model4_t4[2,1]


# Extraemos los intervalos de confianza

model1_lower = sum_model1_tabla4[2,5]
model1_upper = sum_model1_tabla4[2,6]

model2_lower = sum_model2_tabla4[2,5]
model2_upper = sum_model2_tabla4[2,6]

model3_lower = sum_model3_tabla4[2,5]
model3_upper = sum_model3_tabla4[2,6]

model4_lower = sum_model4_tabla4[2,5]
model4_upper = sum_model4_tabla4[2,6]


# Se crea una tabla que agrupe los estimados y los intervalos de confianza

tabla <- matrix(0, 4, 3) # 3 filas y 4 columnas


tabla[1,1]<- model1_tabla4_coef
tabla[2,1]<- model2_tabla4_coef
tabla[3,1]<- model3_tabla4_coef
tabla[4,1]<- model4_tabla4_coef

tabla[1,2]<- model1_lower
tabla[1,3]<- model1_upper

tabla[2,2]<- model2_lower
tabla[2,3]<- model2_upper

tabla[3,2]<- model3_lower
tabla[3,3]<- model3_upper

tabla[4,2]<- model4_lower
tabla[4,3]<- model4_upper

colnames(tabla)<- c("Estimate","Lower_bound","Upper_bound")
rownames(tabla)<- c("Modelo 1 - OLS", "Modelo 2 - OLS",
                    "Modelo 3 - LPM", "Modelo 4 - LPM")

# Se exporta en Latex y se convierte en formato DataFrame

tabla_est <- as.data.frame(tabla)
tabla_est_R <- xtable(tabla_est)
print(tabla_est_R, file = "../../trabajo_final/Grupo1/tabla_est_R.tex")


# Creamos el gráfico que muestre los coeficientes y los intervalos de confianza

options(repr.plot.width = 8, repr.plot.height = 5)  # Dimensiones del gráfico


tabla_est  %>% ggplot(aes(x=rownames(tabla_est), y=Estimate)) +
  geom_point(size=3, color = 'red') +
  geom_errorbar(aes(ymin=Lower_bound, ymax=Upper_bound) ,
                width = 0.05,color="red", linewidth = 0.8) +
  labs(x="", y="") + ggtitle("Treatment class and Year 2016 Interaction Coefficient (95% CI)")  +
  theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.8) +
  scale_x_discrete(limits = c("Modelo 1 - OLS", "Modelo 2 - OLS",
                              "Modelo 3 - LPM", "Modelo 4 - LPM")) +
  scale_y_continuous(breaks = seq(-1.5,0,1.5) , limits = c(-1.5,1.5)) +
  theme_classic(14)


# geom_errorbar solicita el limite inferior y superior
# width  : ancho de la abrra superior
# scale_x_discrete: Nombre de modelos en eje inferior
# geom_hline: añadir lines horizontal
# panel.grid.major = element_blank(), panel.grid.minor = element_blank() borra las cuadrículas en el fondo

# Exportamos el gráfico

ggsave("../../trabajo_final/Grupo1/Gráfico_Treat2016_R.png"
       , height = 8  
       , width = 12  
       , dpi = 320 )