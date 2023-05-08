################  Workgroup 4 ############################
#Curso: Laboratorio de R y Python 
## @author: Grupo6

#Datos de la Comisión de la Verdad y Reconciliación ###########################
# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library 

library(pacman)
library(haven)
library(dplyr)
library(tidyr)
library(magrittr)

# permite llamar a varias librerias de manera simultÃ¡nea
# Si la librerÃ­a no estÃ¡ instalada, entonces lo instala y llama para su uso

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

# tidyverse es una recopilaciÃ³n de varias librerias (dply, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones
# haven tambien permite la lectura de base de datos de diferentes extensiones (i.e stata)


# Change working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Cargamos la base de datos
df <- read_sav("../../data/actos_est.sav")

##Etiquetas de valor y las etiquetas de las variables-------------------------------------

#Observamos las etiquetas de valor y las etiquetas de las variables
labels <- lapply(names(df), function(x) attr(df[[x]], "label"))
names(labels) <- names(df)
print(labels)

##Visualizamos las etiquetas de valor
value_df <- val_labels(df)
value <- as.list(value_df)
print(value)

##Filtrar la base de datos-------------------------------------


#Filtramos la base de datos según los siguientes tipos de eventos: 
#desaparición
new_df <- subset(df, IDTIPOAC == "LDS")
print(new_df)

#secuestros
new_df2 <- filter(df, IDTIPOAC == "LSE")
print(new_df2)

#reclutamiento forzado
new_df3 <- subset(df, IDTIPOAC=="LRC")
print(new_df3)

#muertes en atentados
new_df4 <- filter(df, IDTIPOAC == "MAT")
print(new_df4)

##Base de datos con el total de eventos de violencia, según DEPNAO-------------------------------------

#¿Qué tipo de eventos de violencia hay en la base de datos?
dfh <- df %>% group_by(IDACTO) %>% slice(1) %>% ungroup()
print(dfh)

act_violen_DEP <- dfh %>%
  group_by(DEPNA0) %>%
  summarise(
    across(c("LDS_LDT", "LDT", "LRC", "LSE", "MAE", "MAT", "MEF", "TLA", "TTR", "TVS"), sum)
  )
colnames(act_violen_DEP) <- toupper(colnames(act_violen_DEP))
print(act_violen_DEP )

##Base de datos con el total de eventos de violencia, según UBIDIST-------------------------------------

act_violen_UBIDIST <- dfh %>%
  group_by(UBIDIST) %>%
  summarise(
    across(c("LDS_LDT", "LDT", "LRC", "LSE", "MAE", "MAT", "MEF", "TLA", "TTR", "TVS"), sum)
  )
print(act_violen_UBIDIST )

##Base de datos con el total de eventos de violencia según DEPNAO y PERIODo-------------------------------------

UBIDIST_PERDI <- dfh %>%
  group_by(DEPNA0, PERIODO) %>%
  summarise(across(c("LDS_LDT", "LDT", "LRC", "LSE", "MAE", "MAT", "MEF", "TLA", "TTR", "TVS"), sum))
print(UBIDIST_PERDI)

#ENAHO###########################

library(pacman) 
p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled, haven) 

#importamos las bases de datos
enaho2019 <- haven::read_dta("../../data/enaho/enaho01-2019-200.dta")

enaho2019 %>% filter((p204==1 & p205==2) | (p204==2 & p206==1))


enahoEDUCACION <- haven::read_dta("../../data/enaho/enaho01a-2019-300.dta")

#creamos la variable de años de educacion
enahoEDUCACION <- enahoEDUCACION %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  )
) %>% rowwise() %>%  
  mutate(
    educ2 = sum(p301b, p301c, na.rm = T),  
    years_educ = sum(educ1, educ2, na.rm = T)
  ) %>% 
  ungroup()

#creamos la variable con el MAX años de educacion alcanzados de algun miembro del hogar
enahoEDUCACION <- enahoEDUCACION  %>%  group_by(vivienda) %>% 
  mutate(
    max_educ = max(years_educ, na.rm = T)) %>% as.data.frame()

#definimos la variable dpto

enaho19 <- enaho19 %>% 
  mutate(dpto_code = substr(ubigeo, 1, 2), 
         ubigeo3 = str_pad(ubigeo2, 6, pad ="0"),
         dpto = case_when(
           dpto_code == "01" ~ "Amazonas", dpto_code == "02" ~ "Ancash",
           dpto_code == "03" ~ "Apurimac", dpto_code == "04" ~ "Arequipa",
           dpto_code == "05" ~ "Ayacucho", dpto_code == "06" ~ "Cajamarca",
           dpto_code == "07" ~ "Callao"  , dpto_code == "08" ~ "Cusco",
           dpto_code == "09" ~ "Huancavelica",dpto_code == "10" ~ "Huanuco",
           dpto_code == "11" ~ "Ica"     , dpto_code == "12" ~ "Junin",
           dpto_code == "13" ~ "La Libertad",dpto_code == "14" ~ "Lambayaque",
           dpto_code == "15" ~ "Lima"    , dpto_code == "16" ~ "Loreto",
           dpto_code == "17" ~ "Madre de Dios",dpto_code == "18" ~ "Moquegua",
           dpto_code == "19" ~ "Pasco"   ,dpto_code == "20" ~ "Piura",
           dpto_code == "21" ~ "Puno"    ,dpto_code == "22" ~ "San Martin",
           dpto_code == "23" ~ "Tacna"   ,dpto_code == "24" ~ "Tumbes",
           dpto_code == "25" ~ "Ucayali"
         ),
         gmensual_pc = gashog2d/(mieperho*12),  
         imensual_pc = inghog1d/(mieperho*12)   
  )

#diseño muestral a nivel de departamento estandarizado a nivel hogar


design <- svydesign(
  data = enaho19,
  ids = ~ conglome,
  strata = ~estrato,
  weights = ~ factor07,
  nest = TRUE
)

#grafico en barras

prop.table(svytable(~ dpto + nbi, design = design), 1) %>%
  as.data.frame() %>% 
  filter(nbi == 1) %>% 
  mutate(ratenbi = Freq*100) %>% 
  ggplot(aes(y = reorder( dpto, -ratenbi) , x = ratenbi   )) +
  geom_col() +
  scale_fill_identity(guide = "none") +
  theme_minimal() +  
  xlab("NBI %") +
  ylab("Departamento")


#importar bases de datos
sumaria_15 <- read_dta("../../data/enaho/sumaria-2015.dta")
sumaria_16 <- read_dta("../../data/enaho/sumaria-2016.dta")
sumaria_17 <- read_dta("../../data/enaho/sumaria-2017.dta")
sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
base_deflactores <- read_dta("../../data/enaho/deflactores_base2020_new.dta")

#juntar las bases
append_enaho <- bind_rows(sumaria_15, sumaria_16, sumaria_17, sumaria_18, sumaria_19, sumaria_20) %>% 
  mutate(  dpto = as.numeric( substr(ubigeo,1,2) ),
           año = as.numeric(año)) 

append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15 

append_enaho <- left_join(append_enaho,
                          base_deflactores, by = c("dpto", "año" = "aniorec"))

#agrupacion por Área

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    gpc = gashog2d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )
#formato encuesta por persona
gasto_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   nest = TRUE,
                   weight = factorpob) %>%   
  group_by(año, area) %>% 
  summarise(
    gpc = survey_mean(gpc, na.rm = T)
  )

#gráfico
gasto_years %>% ggplot( aes(x = año, y = gpc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + 
  scale_x_continuous(breaks = c(2018, 2019 , 2020) ) +
  ggtitle("Gasto medio mensual per cápita por area")+
  xlab("año")+
  ylab("gasto per capita")


# ENDES###########################


#Unir los módulos RECH84DV, RECH0, RECH1 y RECH23. Crear una variable dummy de violencia física y sexual respectivamente.

# Cargamos los datos de los módulos RECH84DV, RECH0, RECH1 y RECH23
rech84dv <- read_dta("../../data/endes/REC84DV.dta")
rech0 <- read_dta("../../data/endes/RECH0.dta")
rech1 <- read_dta("../../data/endes/RECH1.dta")
rech23 <- read_dta("../../data/endes/RECH23.dta")

names(rech84dv)
names(rech0)

names(rech84dv)[names(rech84dv) == "caseid"] <- "HHID"

# Unimos los data frames
datos_endes <- merge(rech84dv, rech0, by = "HHID")
datos_endes <- merge(datos_endes, rech1, by = "HHID")
datos_endes <- merge(datos_endes, rech23, by = "HHID")


# Creamos variables dummy para cada tipo de violencia física
datos_endes$d105a_dummy <- ifelse(datos_endes$d105a %in% c(1, 2), 1, 0)
datos_endes$d105b_dummy <- ifelse(datos_endes$d105b %in% c(1, 2), 1, 0)
datos_endes$d105c_dummy <- ifelse(datos_endes$d105c %in% c(1, 2), 1, 0)
datos_endes$d105d_dummy <- ifelse(datos_endes$d105d %in% c(1, 2), 1, 0)
datos_endes$d105e_dummy <- ifelse(datos_endes$d105e %in% c(1, 2), 1, 0)

# Creamos una variable dummy para violencia física en general
datos_endes$physical <- ifelse(rowSums(datos_endes[, c("d105a_dummy", "d105b_dummy", "d105c_dummy", "d105d_dummy", "d105e_dummy")]) > 0, 1, 0)

#Crear una variable dummy por cada variable. La Dummy toma el valor de 1 si la variable de violencia toma los valores 1 y 2, mientras el valor 0, para los valores 0 y 2. Finalmente, crear la variable dummy llamada physical = 1 si alguna de las variables dummies toma el valor de 1, mientras toma el valor de cero si cada variable dummy toma el valor de cero.

# Creamos una variables dummy de violencia física
rech84dv <- rech84dv %>%
  mutate(
    d105a_dummy = ifelse(d105a %in% c(1,2), 1, 0),
    d105b_dummy = ifelse(d105b %in% c(1,2), 1, 0),
    d105c_dummy = ifelse(d105c %in% c(1,2), 1, 0),
    d105d_dummy = ifelse(d105d %in% c(1,2), 1, 0),
    d105e_dummy = ifelse(d105e %in% c(1,2), 1, 0)
  )

# Creammos una variable dummy physical
rech84dv <- rech84dv %>%
  mutate(
    physical = ifelse(d105a_dummy == 1 | d105b_dummy == 1 | d105c_dummy == 1 | d105d_dummy == 1 | d105e_dummy == 1, 1, 0)
  )