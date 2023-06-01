################  Clase 10 linear models ############################
## Curso: Laboratorio de R y Python #################################
## @author: Roberto Mendoza

# clear environment

rm(list=ls(all=TRUE))

# Load libraries ----


librarian::shelf(
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



user <- Sys.getenv("USERNAME")  # username

setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2022_2/Lab10") ) # set directorio


repdata <- read_dta("../data/dataverse_files/mss_repdata.dta")


str(repdata)

lapply(repdata, class)

names(repdata)

attributes(repdata)

summary(repdata)

# Observamos las etiquetas de variable de los valores de las variables

lapply(repdata, attr, 'label')

lapply(repdata, attr, 'labels')

repdata$ccode %>% attr('label') # var label

# dummy por país para efectos fijos


repdata  <- dummy_cols(repdata, select_columns = 'ccode')

#

index <- grep("ccode_", colnames(repdata))

# D*time_var 

repdata$time_year <- repdata$year - 1978 # creando la variable temporal

list_vars <- names(repdata)[index]

list_vars[1]

i = 1

while(i < 42){

    var <- paste0(list_vars[i],"_","time")
    repdata[var]  <- repdata[list_vars[i]]*repdata["time_year"]

    i = i + 1
}


# TABLE 1: DESCRIPTIVE STATISTICS ----


table1 <- repdata %>% dplyr::select(any_prio, any_prio_on, any_prio_off,
                             war_prio, war_prio_on, war_prio_off, war_col, war_inc, war)


stargazer(table1)

# No obtenemos resultado pues la libreria exige que la base de datos sea DataFrame

table1 <- repdata %>% dplyr::select(any_prio, any_prio_on, any_prio_off,
                             war_prio, war_prio_on, war_prio_off, war_col, war_inc, war,
                             GPCP, GPCP_g, GPCP_g_l,gdp_g, gdp_g_l,
        y_0, polity2l, polity2l_6, ethfrac, relfrac, Oil, lmtnest, lpopl1, tot_100_g) %>% as.data.frame()


stargazer(table1)



# Ajuste de algunos argumentos digits:



stargazer(table1, title = "Descriptive Statistics", digits = 2,
          covariate.labels = c("Civil conflict with ≥25 deaths: (PRIO/
Uppsala)","Onset","Offset","Civil conflict with ≥1,000 deaths:
PRIO/Uppsala","Onset","Offset","Collier and Hoeffler (2002)",
                               "Doyle and Sambanis (2000)","Fearon and Laitin (2003)"))


stargazer(table1, title = "Descriptive Statistics", digits = 2,
          covariate.labels = c("Civil conflict with ≥25 deaths: (PRIO/
Uppsala)","Onset","Offset","Civil conflict with ≥1,000 deaths:
PRIO/Uppsala","Onset","Offset","Collier and Hoeffler (2002)",
                               "Doyle and Sambanis (2000)","Fearon and Laitin (2003)"),
          min.max = F)



# summary.stat = c("n", "p75", "sd") ordenar las columnas de los estadisticos

list_vars <- c("Civil conflict with ≥25 deaths: (PRIO/
Uppsala)","Onset","Offset","Civil conflict with ≥1,000 deaths:
PRIO/Uppsala","Onset","Offset","Collier and Hoeffler (2002)",
               "Doyle and Sambanis (2000)","Fearon and Laitin (2003)",
"Annual rainfall (mm), GPCP measure",
"Annual growth in rainfall, time t",
"Annual growth in rainfall, time t-1",
"Annual economic growth rate, time t",
"Annual economic growth rate, time t-1",
"Log(GDP per capita), 1979",
"Democracy level (Polity IV score, -10 to 10), time t-1",
"Democracy indicator (Polity IV score > 15),
time t-1",
"Ethnolinguistic fractionalization (source:
Atlas Marodov Mira)",
"Religious fractionalization (source: CIA
Factbook)",
"Oil-exporting country (source: WDI)",
"Log(mountainous) (source: Fearon and
Laitin 2003)",
"Log(national population), time t-1
(source: WDI)",
"Growth in terms of trade, time t (source:
WDI)"
)

stargazer(table1, title = "Descriptive Statistics", digits = 2, # decimales con 2 digitos
          covariate.labels = list_vars,  # Lista de etiquetas
          summary.stat = c("mean", "sd", "n"), # se especifica el orden de los estadísticos
          min.max = F, # borrar el estadístico de maximo y minimo
          notes = "Note.—The source of most characteristics is the World Bank’s World Development Indicators (WDI)."
          , notes.append = FALSE, # TRUE append the significance levels
          notes.align = 'l')




# TABLE 2: First stage ----

# OLS simple

ols_model <- lm(gdp_g ~ GPCP_g + GPCP_g_l, data = repdata)

attributes(ols_model)

ols_model$coefficients  # coeficientes

ols_model$fitted.values  # Y estimado

# summary table como en stata

summary(ols_model)

summary(ols_model)$call

summary(ols_model)$coef

# Test de significancia individual usando Huber robust standard errors

coeftest(ols_model, vcov = vcovHC(ols_model, "HC")) # Clasical white robust
coeftest(ols_model, vcov = vcovHC(ols_model, "HC0"))

coeftest(ols_model, vcov = vcovHC(ols_model, "HC1")) # Huber-White robust (STATA)
coeftest(ols_model, vcov = vcovHC(ols_model, "HC2")) # Eicker-Huber-White robust
coeftest(ols_model, vcov = vcovHC(ols_model, "HC3"))
coeftest(ols_model, vcov = vcovHC(ols_model, "HC4"))


# vcovHC Matrix varianza-covarianza robusta ante heterocedasticidad

# robust se and cluster

robust_model <- coeftest(ols_model,
                          vcov = vcovCL,  # Matrix varianza-covarianza cluster (CL)
                          type = "HC1",
                          cluster = ~ ccode)

sd_robust <- robust_lm[,2]

#### Primer Modelo ----

# OLS, sin efectos fijos o country-time trend
# errores estandar robustas (Huber robust)
# Los residuos estan clusterizados (agrupados) a nivel país
# Using cluster and robust standar error

ols_model1 <- lm_robust(gdp_g ~ GPCP_g + GPCP_g_l, data = repdata,
              clusters = ccode, se_type = "stata")


summary(ols_model1)

attributes(ols_model1)

ols_model1$r.squared

rmse1 <- RMSE(ols_model1$fitted.values, repdata$gdp_g ) # root mean squeare error

RMSE(ols_model1$fitted.values, repdata$gdp_g )^2 # mean square error


# using tdyr functions


glance(ols_model1)

tidy(ols_model1)

tidy(ols_model1)

htmlreg(ols_model1)

tidy(ols_model1)[2,2] # coeficiente
tidy(ols_model1)[2,6] # limite inferior
tidy(ols_model1)[2,7] # limite superior 


"Usando LM y luego coestest para los error standar robusto"

m1 <- lm(gdp_g ~ GPCP_g + GPCP_g_l, data = repdata)

lm_rmse1 <- round(RMSE(m1$fitted.values, repdata$gdp_g ) ,2 )

robust_model1 <- coeftest(m1,
vcov = vcovCL,  # Matrix varianza-covarianza cluster (CL)
type = "HC1",
cluster = ~ ccode)

sd_robust_model1 <- robust_model1[,2]

# intervalo de confianza 

model1_lower = coefci(m1, df = Inf, 
                      vcov. = vcovCL, cluster = ~ ccode, type = "HC1")[2,1]

model1_upper = coefci(m1, df = Inf,
                      vcov. = vcovCL, cluster = ~ ccode, type = "HC1")[2,2]

tidy(m1, conf.int = TRUE)

#### Segundo Modelo ----
# No efectos fijos (country), Si country-time trends
# errores estandar robustas (Huber-white robust)
# termino de perturbación están clusterizados (agrupados) a nivel país
# Se añade variables de control

control_vars <- c("y_0", "polity2l", "ethfrac", "relfrac", "Oil", "lmtnest","lpopl1")

# Usando as.formula pues el modelo se hace extenso

names(repdata)

index_country_time <- grep("_time$", colnames(repdata))

country_time_trend <-names(repdata)[index_country_time]


model2_formula <- as.formula(
  
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l", paste(country_time_trend, collapse = "+"),
                paste(control_vars, collapse = "+")
                ,sep="+")
          )
    
    )

# Usando LM_robust 

ols_model2 <- lm_robust(model2_formula, data = repdata,
                        clusters = ccode, se_type = "stata")


summary(ols_model2)



glance(ols_model2)

tidy(ols_model2)

rmse2 <- RMSE(ols_model2$fitted.values, repdata$gdp_g ) # root mean squeare error


htmlreg(list(ols_model1,ols_model2))

"Usando LM y luego coestest para error standar robusto"


m2 <- lm(model2_formula, data = repdata)

lm_rmse2 <- round(RMSE(m2$fitted.values, repdata$gdp_g ) ,2 )

robust_model2 <- coeftest(m2,
                          vcov = vcovCL,
                          type = "HC1",
                          cluster = ~ ccode)

sd_robust_model2 <- robust_model2[,2]

coefci(m2, df = Inf, 
       vcov. = vcovCL, cluster = ~ ccode, type = "HC1")


#### Tercer Modelo ----
# Si efectos fijos (country), Si country-time trends
# errores estandar robustas (Huber robust)
# termino de perturbación están clusterizados (agrupados) a nivel país

# 1.0 Uso de lm_robust

index_country_time <- grep("_time$", colnames(repdata))

country_time_trend <-names(repdata)[index_country_time]

index_country <- grep("^ccode_\\d+$", colnames(repdata))

country_fe <-names(repdata)[index_country]

model3_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l", paste(country_time_trend, collapse = "+"),
                paste(country_fe, collapse = "+")
                , sep="+")
    )
)


ols_model3 <- lm_robust(model3_formula, data = repdata,
                        clusters = ccode, se_type = "stata")


summary(ols_model3)

glance(ols_model3)

tidy(ols_model3)

rmse3 <- RMSE(ols_model3$fitted.values, repdata$gdp_g ) # root mean squeare error



rmse3 <- RMSE(ols_model3$fitted.values, repdata$gdp_g ) # root mean squeare error

# 2. Incluyendo ccode como argumento de efectos fijos en LM_robust

model3_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l",
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)


ols_model3 <- lm_robust(model3_formula, data = repdata,
                        clusters = ccode, se_type = "stata", fixed_effects = ~ ccode)


summary(ols_model3)
tidy(ols_model3)
glance(ols_model3)

rmse3 <- RMSE(ols_model3$fitted.values, repdata$gdp_g ) # root mean squeare error

# !! Usar para el trabajo final 

# 3. Usando ccode como una variable tipo factor (variable categórica)

repdata$ccode_factor <- as.factor(repdata$ccode)

class(repdata$ccode_factor)

class(repdata$ccode)


model3_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l", "ccode_factor",
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)


ols_model3 <- lm_robust(model3_formula, data = repdata,
                        clusters = ccode, se_type = "stata")

summary(ols_model3)
tidy(ols_model3)
glance(ols_model3)

"Usando LM y luego coestest para los error standar robusto"


m3 <- lm(model3_formula, data = repdata)

lm_rmse3 <- round(RMSE(m3$fitted.values, repdata$gdp_g ) ,2 )

robust_model3 <- coeftest(m3 ,
                          vcov = vcovCL,
                          type = "HC1",
                          cluster = ~ ccode)



sd_robust_model3 <- robust_model3[,2]
#c.i : confidence interval 

coefci(m3, df = Inf, 
       vcov. = vcovCL, cluster = ~ ccode, type = "HC1")

#### Cuarto modelo ----
# Si efectos fijos (country), Si country-time trends
# errores estandar robustas (Huber robust)
# termino de perturbación están clusterizados (agrupados) a nivel país
# Se añade Growth in rainfall del siguiente año

model4_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l","GPCP_g_fl",
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)


ols_model4 <- lm_robust(model4_formula, data = repdata,
                        clusters = ccode, se_type = "stata", fixed_effects = ~ ccode)


summary(ols_model4)
tidy(ols_model4)
glance(ols_model4)

rmse4 <- RMSE(ols_model4$fitted.values, repdata$gdp_g ) # root mean squeare error


"Usando LM y luego coestest para los error standar robusto"

model4_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l","GPCP_g_fl","ccode_factor",
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)

m4 <- lm(model4_formula, data = repdata)

lm_rmse4 <- round(RMSE(m4$fitted.values, repdata$gdp_g ) ,2 )


robust_model4 <- coeftest(m4,
                          vcov = vcovCL,
                          type = "HC1",
                          cluster = ~ ccode)

sd_robust_model4 <- robust_model4[,2]

coefci(m4, df = Inf, 
       vcov. = vcovCL, cluster = ~ ccode, type = "HC1")

#### Quinto modelo ----
# Si efectos fijos (country), Si country-time trends
# errores estandar robustas (Huber robust)
# termino de perturbación están clusterizados (agrupados) a nivel país
# Se añade Growth in terms of trade

model5_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l","tot_100_g",
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)


ols_model5 <- lm_robust(model5_formula, data = repdata,
                        clusters = ccode, se_type = "stata", fixed_effects = ~ ccode)


summary(ols_model5)
tidy(ols_model5)
glance(ols_model5)

rmse5 <- RMSE(ols_model5$fitted.values, repdata$gdp_g ) # root mean squeare error


"Usando LM y luego coestest para los error standar robusto"

model5_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l","tot_100_g","ccode_factor",
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)

m5 <- lm(model5_formula, data = repdata)

robust_model5 <- coeftest(m5,
                          vcov = vcovCL,
                          type = "HC1",
                          cluster = ~ ccode)

sd_robust_model5 <- robust_model5[,2]

coefci(m5, df = Inf, 
       vcov. = vcovCL, cluster = ~ ccode, type = "HC1")

# RMSE manual

sq_resid <- m5$residuals**2
lm_rmse5 <- round( mean(sq_resid)^0.5, 2)

# Export tables ----

#
# stargazer::stargazer(ols_model1, ols_model2, ols_model3, ols_model4, ols_model5)
#
#
# stargazer::stargazer(ols_model5, se = starprep(ols_model5))

texreg(list(ols_model1, ols_model2, ols_model3, ols_model4, ols_model5),
       custom.coef.map = list("GPCP_g"="Growth in rainfall, t",
                              "GPCP_g_l"="Growth in rainfall, t-1",
                              "GPCP_g_fl" = "Growth in rainfall,t+1",
                              "tot_100_g" = "Growth in terms of trade, t" ,
                              "y_0" = "Log(GDP per capita), 1979",
                              "polity2l" = "Democracy (Polity IV), t-1",
                              "ethfrac" = "Ethnolinguistic fractionalization",
                              "relfrac" = "Religious fractionalization",
                              "Oil" = "Oil-exporting country",
                              "lmtnest" = "Log(mountainous)",
                              "lpopl1" = "Log(national population), t-1"
                              ), digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Country fixed effects" = c("no", "no", "yes", "yes", "yes"),
                              "Country-specific time trends" = c("no", "yes", "yes", "yes", "yes"),
                              "RMSE" = c(rmse1,rmse2,rmse3,rmse4,rmse5)),
       caption = "Dependent Variable: Economic Growth Rate, t")

# digits: 3 digitos
# stars = c(0.01, 0.05, 0.1), *** 1%, ** 5%, ** 10%

# Export using Stagezer


stargazer( m1, m2, m3, m4, m5,
           se=list(sd_robust_model1, sd_robust_model2, sd_robust_model3,
                   sd_robust_model4, sd_robust_model5),
           dep.var.labels = c(""),
           title = "Rainfall and Economic Growth (First-Stage)",
           keep = c("GPCP_g","GPCP_g_l","GPCP_g_fl","tot_100_g","y_0",
                    "polity2l","ethfrac","relfrac","Oil",
                    "lmtnest","lpopl1"),
           covariate.labels=c("Growth in rainfall, t","Growth in rainfall, t-1",
                              "Growth in rainfall, t+1",
                              "Growth in terms of trade, t","Log(GDP per capita), 1979",
                              "Democracy (Polity IV), t-1","Ethnolinguistic fractionalization",
                              "Religious fractionalization","Oil-exporting country",
                              "Log(mountainous)","Log(national population), t-1"),
           align = T, no.space = T,
           add.lines=list(c("Country fixed effects", "no", "no","yes","yes","yes"),
                c("Country-specific time trends","no", "yes","yes","yes","yes"),
                c("Root mean square error",lm_rmse1,lm_rmse2,lm_rmse3,lm_rmse4,lm_rmse5)),
           keep.stat = c("rsq","n"),
           notes.append = FALSE, notes.align = "l",
            notes ="Huber robust standard errors are in parentheses", style = "qje"
           )

# qje: quataerly journal of economics

# TABL4: OLS VERSUS 2SLS ----

prob_formula <- as.formula(
    paste("any_prio",
          "~",
          paste("gdp_g","gdp_g_l",
                paste(control_vars, collapse = "+"),
                "time_year"
                , sep="+")
    )
)


# Logit

logit <- glm(prob_formula, data = repdata, family = binomial)

attributes(logit)

# cluster terminos de perturbación y robust standar error

coeftest(logit, vcov. = vcovCL(logit, cluster = ~ccode, type = "HC"))

# Probit

probit <- glm(prob_formula, data = repdata, family = binomial(link = "probit"))

attributes(probit)

coeftest(probit, vcov. = vcovCL(probit, cluster = ~ccode, type = "HC"))


# Usar la libreria mfx base Econometrics Greene

# Logit model

###  Logit, Probit models ----

logit <- logitmfx(prob_formula, data = repdata, atmean = TRUE, robust = T)

logit <- logitmfx(prob_formula, data = repdata, atmean = TRUE, robust = T,
                    clustervar1 = "ccode")


# Probit model

probit <- probitmfx(prob_formula, data = repdata, atmean = TRUE, robust = T)

probit <- probitmfx(prob_formula, data = repdata, atmean = TRUE, robust = T,
                    clustervar1 = "ccode")


summary(probit)

# Export table

texreg(list(logit, probit))

htmlreg(list(logit, probit)) # export to html

###  Modelo 2 ----

model2_formula <- as.formula(
    paste("any_prio",
          "~",
          paste("gdp_g","gdp_g_l",
                paste(control_vars, collapse = "+"),
                "time_year"
                , sep="+")
    )
)


ols_model2 <- lm_robust(model2_formula, data = repdata,
                        clusters = ccode, se_type = "stata")

summary(ols_model2)

glance(ols_model2)

tidy(ols_model2)

rmse2 <- round( RMSE(ols_model2$fitted.values, repdata$any_prio ), 2)

###  Modelo 3 ----

model3_formula <- as.formula(
    paste("any_prio",
          "~",
          paste("gdp_g","gdp_g_l",
                paste(control_vars, collapse = "+"),
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)


ols_model3 <- lm_robust(model3_formula, data = repdata,
                        clusters = ccode, se_type = "stata")

summary(ols_model3)

glance(ols_model3)

tidy(ols_model3)

rmse3 <- round(RMSE(ols_model3$fitted.values, repdata$any_prio ), 2)

###  Modelo 4 ----

model4_formula <- as.formula(
    paste("any_prio",
          "~",
          paste("gdp_g","gdp_g_l",
                paste(country_time_trend, collapse = "+")
                , sep="+")
    )
)


ols_model4 <- lm_robust(model4_formula, data = repdata,
                        clusters = ccode, se_type = "stata", fixed_effects = ~ ccode)

summary(ols_model4)

glance(ols_model4)

tidy(ols_model4)

rmse4 <- round( RMSE(ols_model4$fitted.values, repdata$any_prio ), 2)


###  Modelo 5 IV ----

model5_formula <- as.formula(
    paste(
    paste("any_prio",
          "~",
          paste("gdp_g","gdp_g_l",
                paste(country_time_trend, collapse = "+")
                , paste(control_vars, collapse = "+")
                , sep="+")
    )
    , paste("GPCP_g", "GPCP_g_l",
            paste(country_time_trend, collapse = "+")
            , paste(control_vars, collapse = "+")
            , sep="+")
    , sep=" | "
    )
)



ols_model5 <- iv_robust(model5_formula, data = repdata,
                        clusters = ccode, se_type = "stata")

summary(ols_model5)

glance(ols_model5)

tidy(ols_model5)

rmse5 <- round(RMSE(ols_model5$fitted.values, repdata$any_prio ), 2)


### Modelo 6 IV ----

model6_formula <- as.formula(
    paste(
        paste("any_prio",
              "~",
              paste("gdp_g","gdp_g_l",
                    paste(country_time_trend, collapse = "+")
                    , sep="+")
        )
        , paste("GPCP_g", "GPCP_g_l",
                paste(country_time_trend, collapse = "+")
                , sep="+")
        , sep=" | "
    )
)


ols_model6 <- iv_robust(model6_formula, data = repdata,
                        clusters = ccode, se_type = "stata",
                        fixed_effects = ~ ccode)

summary(ols_model6)

glance(ols_model6)

tidy(ols_model6)

rmse6 <- round(RMSE(ols_model6$fitted.values, repdata$any_prio ), 2)


### Modelo 7 IV ----

model7_formula <- as.formula(
    paste(
        paste("war_prio",
              "~",
              paste("gdp_g","gdp_g_l",
                    paste(country_time_trend, collapse = "+")
                    , sep="+")
        )
        , paste("GPCP_g", "GPCP_g_l",
                paste(country_time_trend, collapse = "+")
                , sep="+")
        , sep=" | "
    )
)


ols_model7 <- iv_robust(model7_formula, data = repdata,
                        clusters = ccode, se_type = "stata",
                        fixed_effects = ~ ccode)

summary(ols_model7)

glance(ols_model7)

tidy(ols_model7)

rmse7 <- round(RMSE(ols_model7$fitted.values, repdata$war_prio ) ,2 )


# Export table

texreg(list(logit, probit, ols_model2, ols_model3, ols_model4,
            ols_model5, ols_model6, ols_model7),
       custom.coef.map = list("gdp_g"="Economic growth rate, t",
                              "gdp_g_l"="Economic growth rate, t-1",
                              "y_0" = "Log(GDP per capita), 1979",
                              "polity2l" = "Democracy (Polity IV), t-1",
                              "ethfrac" = "Ethnolinguistic fractionalization",
                              "relfrac" = "Religious fractionalization",
                              "Oil" = "Oil-exporting country",
                              "lmtnest" = "Log(mountainous)",
                              "lpopl1" = "Log(national population), t-1"

       ), digits = 3,
       stars = c(0.01, 0.05, 0.1),
       custom.gof.rows = list("Country fixed effects" = c("no","no", "no","no", "yes", "no","yes", "yes"),
                              "Country-specific time trends" = c("no","no", "no","no", "yes", "yes","yes", "yes"),
                              "RMSE" = c("","",rmse2,rmse3,rmse4,rmse5,rmse6,rmse7)),
       caption = "Economic Growth and Civil Conflict")



# Links referencias ----

## glmnet library para modelos de machine learning lineales

"https://glmnet.stanford.edu/articles/glmnet.html"

## Stagezer

"chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.princeton.edu/~otorres/NiceOutputR.pdf"

"chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/stargazer/stargazer.pdf"


# Usando fle library -----------------------------------------

# Modelo 3 de TABLE 2

model3_formula <- as.formula(
    paste("gdp_g",
          "~",
          paste("GPCP_g","GPCP_g_l",
                paste(country_time_trend, collapse = "+")
                , sep="+")," |ccode|0| ccode"
    )
)



# | ccode | 0 | ccode, primer ccode: efectos fijos
# segundo ccode: cluster el termino de perturbación a nivel país

ols_model3 <- felm( model3_formula, data = repdata )
attributes(ols_model3)

rmse3 <- RMSE(ols_model3$fitted.values, repdata$gdp_g ) # root mean squeare error

summary(ols_model3)

tidy(ols_model3)

glance(ols_model3)

ols_model3$rse  # just robust standar error no cluster terminos de perturbación


# Modelo 5 de TABLE 4

model5_formula <- as.formula(
    paste(
        paste("any_prio",
              "~",
              paste("gdp_g","gdp_g_l",
                    paste(country_time_trend, collapse = "+")
                    , paste(control_vars, collapse = "+")
                    , sep="+")
        )
        , "|0|(gdp_g|gdp_g_l ~ GPCP_g + GPCP_g_l)| ccode"
    )
)

ols_model5_fle <- felm( model5_formula, data = repdata )

summary(ols_model5_fle)

glance(ols_model5_fle)

tidy(ols_model5_fle)

rmse5_fle <- RMSE(ols_model5_fle$fitted.values, repdata$any_prio )

# Modelo 6 de TABLE 4

model6_formula <- as.formula(
    paste(
        paste("any_prio",
              "~",
              paste("gdp_g","gdp_g_l",
                    paste(country_time_trend, collapse = "+")
                    , sep="+")
        )
        , "|ccode|(gdp_g|gdp_g_l ~ GPCP_g + GPCP_g_l)| ccode"
    )
)

ols_model6_fle <- felm( model6_formula, data = repdata )

summary(ols_model6_fle)

glance(ols_model6_fle)

tidy(ols_model6_fle)

rmse6_fle <- round( RMSE(ols_model6_fle$fitted.values, repdata$any_prio ), 2)











