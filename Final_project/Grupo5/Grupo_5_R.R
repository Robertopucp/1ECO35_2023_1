################  Trabajo Final ############################
## Curso: Laboratorio de R y Python ###########################


# clean environment variables
rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Abrimos la librería

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
# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------------------------------------------------------------------#
##pregunta 1----
repdata <- read_dta("../../data/trabajo_final/replicacion/SerraPorterAEJ.dta")

# Estadísticos descriptivos

table1 <- repdata %>% dplyr::select(american, instate, freshman,ACumGPA, greek, econ_hs,
                                    varsity) %>% as.data.frame()

#forma 2: tabla formato data frame
stargazer(table1)

list_vars <- c('American student',
               'In-state student',
               'Freshman',
               'Cumulative GPA',
               'Belongs to sorority',
               'Took econ in high school',
               'Athlete')


stargazer(table1, title = "Descriptive Statistics", digits = 2, # decimales con 2 digitos
          covariate.labels = list_vars,  # Lista de etiquetas
          summary.stat = c("mean", "sd", "min", "max", "n"), # se especifica el orden de los estadísticos
          notes.append = FALSE, # TRUE append the significance levels
          notes.align = 'l'
          , out = "../../output/tables/summary_r123.tex")

##Pregunta 2-----

###Recreación de la tabla 3----

#1
ols_model1 <- lm_robust(took_year ~ yr_2016 + treatment_class + treat2016, data= repdata,
                        clusters = class_fe2, se_type = "stata")
summary(ols_model1)

#3
ols_model3 <- lm_robust(tookanother ~ yr_2016 + treatment_class + treat2016, data= repdata,
                        clusters = class_fe2, se_type = "stata")
summary(ols_model3)

#Regresiones con variables de control

#2
control_vars <- c("female_prof", "instate", "freshman", "american", "ACumGPA", "gradePrinciples", "small_class")

model1_formula <- as.formula(
  
  paste("took_year",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols_model2 <- lm_robust(model1_formula, data = repdata,
                        clusters = class_fe2, se_type = "stata")
summary(ols_model2)

#4
model2_formula <- as.formula(
  paste("tookanother",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)
ols_model4 = lm_robust(model2_formula, data = repdata,
                       clusters = class_fe2, se_type = "stata")
summary(ols_model4)

#exportamos a latex
texreg(list(ols_model1, ols_model2, ols_model3, ols_model4),
       custom.coef.map = list("treat2016" = "Treatment class x 2016",
                              "yr_2016" = "Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"
       ), digits = 3,
       custom.gof.rows = list("Controls" = c("No", "Yes", "No", "Yes")),
       custom.note = c("Note: LPM regression.
                       $^{***}p<0.01$; $^{**}p<0.05$; $^{*}p<0.1$"),
       caption = "Treatment Effects on Intermediate Outcomes",
       file = "../../output/tables/regression_table_17_r.tex")
#*** significativo al 0.001
#** singificativo al 0.01
#* singificativo al 0.05
#* 
###recreacion de la tabla 4----
#1
ols2_model1 <- lm_robust(numeconclass ~ yr_2016 + treatment_class + treat2016, data= repdata,
                        clusters = class_fe2, se_type = "stata")
summary(ols2_model1)

#3
ols2_model3 <- lm_robust(econmajor ~ yr_2016 + treatment_class + treat2016, data= repdata,
                        clusters = class_fe2, se_type = "stata")
summary(ols2_model3)

#regresiones con variables de control

#2
model3_formula <- as.formula(
  
  paste("numeconclass",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols2_model2 <- lm_robust(model3_formula, data = repdata,
                        clusters = class_fe2, se_type = "stata")
summary(ols2_model2)

#4
model4_formula <- as.formula(
  paste("econmajor",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)
ols2_model4 = lm_robust(model4_formula, data = repdata,
                       clusters = class_fe2, se_type = "stata")
summary(ols2_model4)

#exportamos a latex
texreg(list(ols2_model1, ols2_model2, ols2_model3, ols2_model4),
       custom.coef.map = list("treat2016" = "Treatment class x 2016",
                              "yr_2016" = "Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"
       ), digits = 3,
       custom.gof.rows = list("Controls" = c("No", "Yes", "No", "Yes")),
       custom.note = c("Note.Column (1-2) OLS regressions, column (3-4) LPM regressions.
                       $^{***}p<0.01$; $^{**}p<0.05$; $^{*}p<0.1$"),
       caption = "Treatment Effects on Intermediate Outcomes",
       file = "../../output/tables/regression_table_21_r.tex")

###recreacion de la tabla 5----
#1
model5_formula <- as.formula(
  
  paste("Major_STEM",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols3_model1 <- lm_robust(model5_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols3_model1)

#2
model6_formula <- as.formula(
  
  paste("Major_Business",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols3_model2 <- lm_robust(model6_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols3_model2)

#3
model7_formula <- as.formula(
  
  paste("Major_Finance",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols3_model3 <- lm_robust(model7_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols3_model3)

#4
model8_formula <- as.formula(
  
  paste("Major_Marketing",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols3_model4 <- lm_robust(model8_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols3_model4)

#exportamos a latex
texreg(list(ols3_model1, ols3_model2, ols3_model3, ols3_model4),
       custom.coef.map = list("treat2016" = "Treatment class x 2016",
                              "yr_2016" = "Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"
       ), digits = 3,
       custom.gof.rows = list("Controls" = c("Yes", "Yes", "Yes", "Yes")),
       custom.note = c("Note.LPM regressions.
                       $^{***}p<0.01$; $^{**}p<0.05$; $^{*}p<0.1$"),
       caption = "Treatment Effects on Intermediate Outcomes",
       file = "../../output/tables/regression_table_32_r.tex")

###recreacion de la tabla 6----
#1
model9_formula <- as.formula(
  
  paste("Major_SocSc",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols4_model1 <- lm_robust(model9_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols4_model1)

#2
model10_formula <- as.formula(
  
  paste("Major_Arts",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols4_model2 <- lm_robust(model10_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols4_model2)

#3
model11_formula <- as.formula(
  
  paste("Major_Comm",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols4_model3 <- lm_robust(model11_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols4_model3)

#4
model12_formula <- as.formula(
  
  paste("Major_Hum",
        "~",
        paste("yr_2016", "treatment_class", "treat2016",paste(control_vars, collapse = "+")
              ,  sep="+"))
)

ols4_model4 <- lm_robust(model12_formula, data = repdata,
                         clusters = class_fe2, se_type = "stata")
summary(ols4_model4)

#exportamos a latex
texreg(list(ols4_model1, ols4_model2, ols4_model3, ols4_model4),
       custom.coef.map = list("treat2016" = "Treatment class x 2016",
                              "yr_2016" = "Year 2016",
                              "treatment_class" = "Treatment class (in 2015)",
                              "(Intercept)" = "Constant"
       ), digits = 3,
       custom.gof.rows = list("Controls" = c("Yes", "Yes", "Yes", "Yes")),
       custom.note = c("Note.LPM regressions.
                       $^{***}p<0.01$; $^{**}p<0.05$; $^{*}p<0.1$"),
       caption = "Treatment Effects on Intermediate Outcomes",
       file = "../../output/tables/regression_table_42_r.tex")
##pregunta 3-----
#1
model1 <- lm(numeconclass ~ yr_2016 + treatment_class + treat2016, data = repdata)
model1_cluster <- coeftest(model1, vcov = vcovCL, cluster = repdata$class_fe2)
model1_cluster_with_ci <- confint(model1_cluster, level = 0.95)

model1_coef <- model1_cluster[3, 1]
model1_coef_se <- model1_cluster[3, 2]
model1_lower <- model1_cluster_with_ci[3, "2.5 %"]
model1_upper <- model1_cluster_with_ci[3, "97.5 %"]

#se comprueba con
(ols2_model1)
model1_cluster 
model1_cluster_with_ci

#3
model3 <- lm(econmajor ~ yr_2016 + treatment_class + treat2016, data = repdata)
model3_cluster <- coeftest(model3, vcov = vcovCL, cluster = repdata$class_fe2)
model3_cluster_with_ci <- confint(model3_cluster, level = 0.95)

model3_coef <- model3_cluster[3, 1]
model3_coef_se <- model3_cluster[3, 2]
model3_lower <- model3_cluster_with_ci[3, "2.5 %"]
model3_upper <- model3_cluster_with_ci[3, "97.5 %"]

#se comprueba con
(ols2_model3)
model3_cluster 
model3_cluster_with_ci

#2

#2
model2 <- lm(numeconclass ~ yr_2016 + treatment_class + treat2016 + female_prof + instate + freshman + american + ACumGPA + gradePrinciples + small_class , data = repdata)
model2_cluster <- coeftest(model2, vcov = vcovCL, cluster = repdata$class_fe2)
model2_cluster_with_ci <- confint(model2_cluster, level = 0.95)

model2_coef <- model2_cluster[3, 1]
model2_coef_se <- model2_cluster[3, 2]
model2_lower <- model2_cluster_with_ci[3, "2.5 %"]
model2_upper <- model2_cluster_with_ci[3, "97.5 %"]

#se comprueba con
(ols2_model3)
model2_cluster 
model2_cluster_with_ci

#4
model4 <- lm(econmajor ~ yr_2016 + treatment_class + treat2016 + female_prof + instate + freshman + american + ACumGPA + gradePrinciples + small_class , data = repdata)
model4_cluster <- coeftest(model4, vcov = vcovCL, cluster = repdata$class_fe2)
model4_cluster_with_ci <- confint(model2_cluster, level = 0.95)

model4_coef <- model4_cluster[3, 1]
model4_coef_se <- model4_cluster[3, 2]
model4_lower <- model4_cluster_with_ci[3, "2.5 %"]
model4_upper <- model4_cluster_with_ci[3, "97.5 %"]



#se comprueba con
(ols2_model4)
model4_cluster 
model4_cluster_with_ci

#tabla de la variable treatment_class

table<- matrix(0, 4, 4) # 3 filas y 4 columnas

table[1,1]<- model1_coef
table[1,2]<- model1_coef_se
table[1,2]<- model1_lower
table[1,2]<- model1_upper

table[2,1]<- model2_coef
table[2,2]<- model2_coef_se
table[2,2]<- model2_lower
table[2,2]<- model2_upper

table[3,1]<- model3_coef
table[3,2]<- model3_coef_se
table[3,2]<- model3_lower
table[3,2]<- model3_upper

table[4,1]<- model4_coef
table[4,2]<- model4_coef_se
table[4,2]<- model4_lower
table[4,2]<- model4_upper
colnames(table)<- c("Estimate","se","lower_bound","upper_bound")
rownames(table)<- c("OLS Num Eco baseline","OLS Num Eco with controls", "OLS Major Eco baseline", "OLS Major Eco with control")


# table de matriz a dataframe (tab)
tab <- as.data.frame(table)



# Coef-plot de la variable treatment_class

options(repr.plot.width = 10, repr.plot.height =6)  # dimensiones del gráfico

# aes: ejes
tab  %>% ggplot(aes(x=rownames(tab), y=Estimate)) +
  geom_point(size=2, color = 'black') +
  geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound) , width = 0.05,color="darkblue", linewidth = 0.8) +
  labs(x="", y="") + ggtitle("Treatment effects on Final Outcomes (95% CI)")  +
  theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.8) +
  scale_x_discrete(limits = c("OLS Num Eco baseline","OLS Num Eco with controls", "OLS Major Eco baseline", "OLS Major Eco with control")) +
  scale_y_continuous(breaks = seq(-0.6,0.6,0.3) , limits = c(-0.6, 0.6)) +
  theme_classic(14)


ggsave("../../output/tables/Treatment_effects_on_Final_Outcomes_r.png", width = 10, height = 6)


## MAPAS (Parte en R)

### 1. Replicamos la figura 1

# Llamamos a las librerías
library(pacman)
library(ggplot2)
library(sf)

# Cargamos los shapes necesarios para la figura 1, que contienen los distritos y departamentos
distrito <- st_read("../../data/trabajo_final/MAPAS/districts_1975_remake.shp")
departamento <- st_read("../../data/trabajo_final/MAPAS/department_peru.shp")

# Cargamos Figure1Geodatabase para observar los layers
st_layers("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb")

# Seleccionamos los dos que necesitaremos
agc2 <- st_read("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb", layer = "agrozone_core2_polygons")
agcp <- st_read("../../data/trabajo_final/MAPAS/Figure1Geodatabase.gdb", layer = "agrarian_zones_polygons") 


# Figura 1_A (Mapa izquierda)

# Verificams los CRS de las capas
st_crs(distrito)
st_crs(departamento)

# Reasignar el CRS de la capa "distrito" al CRS de la capa "departamento"
distrito <- st_transform(distrito, crs = st_crs(departamento))

# Realizamos la unión espacial entre las capas con el mismo CRS
distrito_departamento <- st_join(distrito, departamento)
# Luego lo unimos a agc2
distrito_departamento_agc2 <- st_join(distrito_departamento,agc2)

# Ahora sí creamos el mapa

figura1_1<- ggplot() +
  geom_sf(data = distrito_departamento_agc2, fill = "white", color = "#6B6B6B", size = 0.2) +
  geom_sf(data = agc2, fill = "grey", color = "black", size = 0.8) +
  geom_sf_label(data = departamento, aes(label = DN93), color = "black", size = 3, fontface = "bold") +
  geom_sf(data = departamento, fill = NA, color = "black", size = 0.8) +
  geom_sf_label(data = departamento, aes (label= Agrozone)) +
  labs(title = "FIGURE 1  Agrarian Reform Zones in Peru",
       subtitle = "") +
  theme_minimal()
# Mostrar el mapa
print(figura1_1) 

# Guardamos el mapa
ggsave(filename = "../../1ECO35_2023_1/Trabajo_final/Grupo5/mapa1_a.png", plot = figura1_1)


# Creamos el mapa de la derecha
# Figura 1_b

#Ahora utilizamos agcp
distrito_departamento_agcp <- st_join(distrito_departamento, agcp)

#Creamos el mapa con ggplot
figura1_2<- ggplot() +
  geom_sf(data = distrito_departamento_agcp, fill = "red", color = "#6B6B6B", size = 0.2) +
  geom_sf(data = agcp, fill = "grey", color = "black", size = 0.5) +
  geom_sf_label(data = departamento, aes(label = DN93), color = "black", size = 3, fontface = "bold") +
  geom_sf(data = departamento, fill = NA, color = "red", size = 0.8) +
  geom_sf_label(data = departamento, aes (label= Agrozone)) +
  labs(title = "FIGURE 1  Agrarian Reform Zones in Peru",
       subtitle = "") +
  theme_minimal()

# Mostramos el mapa
print(figura1_2) 

# Guardamos el mapa
ggsave(filename = "../../1ECO35_2023_1/Trabajo_final/Grupo5/mapa1_b.png", plot = figura1_2)

#FIGURE 2

# Abrimos librería deplyr
library(dplyr)

# Cargamos el archivo PeruLR_1975shapedata
peru_1975 <- read.csv('../../data/trabajo_final/MAPAS/PeruLR_1975shapedata.csv')
head(peru_1975)

peru_1975 <- rename(peru_1975, tierras_expropi = mEE_DR_13_1980_pcSupM_adj) # aquí cambiamos el nombre para facilitar su ubicación

# Aplicamos los iguiente con función pipe

#∗ Log – porcentaje de tierras expropiadas:
#log ( 1 + 100 * mEE DR 13 1980 pcSupM adj)
#∗ Log – eventos de violencia pol´ıtica:
#  log (0.01 + totalevents)

peru_1975 <- peru_1975 %>%
  mutate(log_tierras_expropiadas = log10(1 + 100 * tierras_expropi),
         log_eventos_violencia = log10(0.01 + totalevents))

# Aquí tenemos nuestro data frame actualizado

head(peru_1975)

# Convertimos de data frame a sf para poder crear el mapa
peru_1975_sf <-  peru_1975 |> 
  filter( !is.na(log_tierras_expropiadas) & !is.na(log_eventos_violencia))|> 
  st_as_sf(coords = c("log_tierras_expropiadas", "log_eventos_violencia"), 
           crs = "EPSG:4326")

















