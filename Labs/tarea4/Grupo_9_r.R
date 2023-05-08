#TAREA 4
#PARTE EN R Y PYTHON
#Primero instalamos todos los paquetes necesarios para desarrollar el ejercicio:
rm(list = ls())
graphics.off()
cat("\014")
library(pacman)
p_load(dplyr, readxl, tidyverse, foreign, datos)

#Segundo, cargamos la base de datos:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(haven)
data <- read_sav ("../../data/actos_est.sav")


#Mostrar las etiquetas de valor y las etiquetas de las variables:
str(data)
sapply(data, attr, 'label')

#Filtrar la base de datos según los siguientes tipos de eventos: desaparición, secuestros, reclutamiento forzado y muertes en atentados.
data %>% filter(LDS_LDT == 1 & LRC == 1 & LSE == 1 & MAT == 1) 
view(data)

#Borrar los duplicados de la columna (IDACTO). Esto pues la base de datos está a nivel individuo. El IDACTO se repite por cada persona afectada, pero a nosotros nos interesa una contabilidad agregada de los eventos.
data_sin_duplicados_IDACTO <- data[!duplicated(data$IDACTO), ]
view(data_sin_duplicados_IDACTO)

#Crear una base de datos con el total de eventos de violencia según su tipo a nivel departamento (DEPNAO). La base de datos debe contar con columnas según el tipo de evento. Apoyese de las variables dummy por cada tipo de evento que está en la base de datos.
data %>% dplyr:: group_by(DEPNA0)

#Crear una base de datos con el total de eventos de violencia según su tipo a nivel distrito (UBIDIST).
data %>% dplyr:: group_by(UBIDIST)

#Crear una base de datos con el total de eventos de violencia según su tipo a nivel departamento (DEPNAO) y periodo (PERIODO).
data %>% dplyr:: group_by(DEPNA0) %>% dplyr:: group_by(PERIODO)
#PARTE SOLO EN R
############################################################################
#ENAHO
#datos 
enaho100_19 <- read_dta("../../data/enaho/enaho01-2019-100.dta")
enaho200_19 <- read_dta("../../../../datos/enaho01-2019-200.dta")
enaho300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")
sumaria_15 <- read_dta("../../data/enaho/sumaria-2015.dta")
sumaria_16 <- read_dta("../../data/enaho/sumaria-2016.dta")
sumaria_17 <- read_dta("../../data/enaho/sumaria-2017.dta")
sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
base_deflactores <- read_dta("../../data/enaho/deflactores_base2020_new.dta")


options(scipen = 999)   
library(pacman) 

# permite llamar a varias librerias de manera simultánea
p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

#Unimos las bases de datos
#Módulos 200  y 300 del año 2019

#Utilizamos left join
enaho19 <- enaho200_19 %>% 
  left_join(enaho300_19, by = c("conglome","vivienda","hogar","codperso"))

nrow(enaho19)
#--------------------------------------------------------------------- 
#Filtramos la base de datos según residentes permanentes en el hogar 
#((p204==1 & p205==2) | (p204==2 & p206==1)) y reemplazamos
enaho19 <- enaho19 %>%
  filter((p204 == 1 & p205 == 2) | (p204 == 2 & p206 == 1))
#----------------------------------------------------------------------
#trabajamos con la base ahora filtrada

#Creamos los años de educación de cada miembro del hogar. Luego creamos una variable 
#con la máxima cantidad de años de educación alcanzado por algún miembro del hogar. 

attr(enaho19$p301a, 'label')
attr(enaho19$p301a, 'labels')
attr(enaho19$p301b, 'label')
attr(enaho19$p301c, 'label')

# años de educación
# años de estudio acumulado hasta el nivel educativo alcanzado

enaho19 <- enaho19 %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  ))

# Años de estudio en el nivel educativo actual
# Sumamos el grado o año de estudios. Grado para secundaria o primaria
# años de estudio para educación superior

enaho19$educ2 <- apply(enaho19[,c("p301b","p301c")], 1 , sum , na.rm = T)
enaho19$years_educ <- apply(enaho19[,c("educ1","educ2")], 1 , sum , na.rm = T)

#Ahora vemos el máximo de años de estudio por hogar
enaho19 %>% group_by(hogar) %>%
  mutate(máx_cant_años_educ=max(years_educ)) %>% View()
#-------------------------------------------------------------------------------
#Usamos el módulo 100 del año 2019 

Enaho19 <- enaho100_19 %>% 
  inner_join(sumaria_19, by = c("conglome","vivienda","hogar"))
#primero sumamos las nbis
Enaho19$nb_insatisf <- apply(Enaho19[,c("nbi1","nbi2","nbi3","nbi4","nbi5")], 1 , sum , na.rm = T)

#ahora creamos una variable dummy que indique si el hogar presenta 
#al menos una necesidad básica insatisfecha
# 0 no carece 1 sí carece
Enaho19$dummy_nb_insatisf <- ifelse(Enaho19$nb_insatisf > 0, 1, 0)

#le ponemos nombre a la variable
var_label(Enaho19) <- list(dummy_nb_insatisf = "Dummy de necesidades básicas insatisfechas")

#le damos nombre a los labels
val_labels(Enaho19$dummy_nb_insatisf) <- c("Hogar con nb cubiertas" = 0,
                                     "Hogar sin nb cubiertas" = 1)
attr(Enaho19$dummy_nb_insatisf, "labels")

#Vemos los departamentos
Enaho19 <- Enaho19 %>% 
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
         )
  )

#especificamos el diseño

design <- svydesign(
  data = Enaho19,
  ids = ~ conglome,
  strata = ~ estrato,
  weights = ~ factor07,
  nest = TRUE,
  na.rm = TRUE
)

#Hacemos el gráfico de barras

prop.table(svytable(~ dpto + dummy_nb_insatisf, design = design), 1) %>%
  as.data.frame() %>% 
  filter(dummy_nb_insatisf == 1) %>% 
  mutate(ratenbsatisf = Freq*100) %>% 
  ggplot(aes(y = reorder( dpto, -ratenbsatisf) , x = ratenbsatisf   )) +
  geom_col() +
  scale_fill_identity(guide = "none") +
  theme_minimal()+
  xlab("")+
  ylab("Department")

#-------------------------------------------------------------------------------
#Realizamos el Append de las módulos de sumaria desde 2015 hasta 2020

append_enaho <- bind_rows(sumaria_15, sumaria_16, sumaria_17, sumaria_18, sumaria_19, sumaria_20) %>% 
  mutate(  dpto = as.numeric( substr(ubigeo,1,2) ),
           año = as.numeric(año)) 
    append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15 

    append_enaho <- left_join(append_enaho,
                          base_deflactores, by = c("dpto", "año" = "aniorec"))
#INGRESO

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    ingmpc = inghog1d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )


income_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   nest = TRUE,
                   weight = factorpob) %>%   
  group_by(año, area) %>% 
  summarise(
    ingmpc = survey_mean(ingmpc, na.rm = T)
  )

income_years %>% ggplot( aes(x = año, y = ingmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point() +
  geom_errorbar(aes(ymin=ingmpc -ingmpc_se, ymax = ingmpc + ingmpc_se),
                width=.04) +
  theme_bw() +
  ggtitle("Average monthly income percapita by area")+
  xlab("")+
  ylab("")

#GASTO
append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    gastmpc = gashog2d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )


gast_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   nest = TRUE,
                   weight = factorpob) %>%   
  group_by(año, area) %>% 
  summarise(
    gastmpc = survey_mean(gastmpc, na.rm = T)
  )

gast_years %>% ggplot( aes(x = año, y = gastmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point() +
  geom_errorbar(aes(ymin=gastmpc -gastmpc_se, ymax = gastmpc + gastmpc_se),
                width=.04) +
  theme_bw() +
  ggtitle("Average monthly expenditure percapita by area")+
  xlab("")+
  ylab("")

#------------------------------------------------------------------------
#ENDES
#PREGUNTA ENDES
# Lo primero que vamos a hacer es importar los datos de los diferentes módulos
#Empezamos con rech0
rech0 <- read_dta("../../data/endes/RECH0.dta") %>% 
  # Eliminamos espacios en blanco al principio y al final del ID del hogar
  mutate(HHID = str_trim(HHID))
#Seguimos con rech1
rech1 <- read_dta("../../data/endes/RECH1.dta") %>% 
  # Eliminamos espacios en blanco al principio y al final del ID del hogar
  mutate(HHID = str_trim(HHID))
#Ahora rech23
rech23 <- read_dta("../../data/endes/RECH23.dta") %>% 
  # Eliminamos espacios en blanco al principio y al final del ID del hogar
  mutate(HHID = str_trim(HHID))
#Lo propio para rec84dv
dv <- read_dta("../../data/endes/REC84DV.dta") %>% 
  # Vamos a eliminar los espacios en blanco al principio y al final del ID del caso
  mutate(caseid = str_trim(caseid))

# Unimos los datos de los diferentes módulos usando la variable HHID (o caseid en el caso de dv)
endes_health <- rech1 %>% 
  left_join(rech0, by = "HHID") %>% 
  left_join(rech23, by = "HHID") %>% 
  left_join(dv, by = "caseid")

# Ahora vamos a crear  variables dummy para la violencia física y sexual respectivamente
#En la tarea se especifica 103, pero no es compatible... es probable que sea 105, por  ello el  coding se realizara de acuerdo a lo indicado
endes_health <- endes_health %>% 
  mutate(viol_fisica = case_when(
    d105a %in% c(1,2) | 
      d105b %in% c(1,2) |
      d105c %in% c(1,2) |
      d105d %in% c(1,2) |
      d105e %in% c(1,2) ~ 1,
    TRUE ~ 0
  ),
  viol_sexual = case_when(
    d90 %in% c(1,2) |
      d91 %in% c(1,2) |
      d92 %in% c(1,2) |
      d93 %in% c(1,2) |
      d94 %in% c(1,2) |
      d95 %in% c(1,2) |
      d96 %in% c(1,2) |
      d97 %in% c(1,2) |
      d98 %in% c(1,2) |
      d99 %in% c(1,2) ~ 1,
    TRUE ~ 0
  ))

# Creamos una variable dummy por tipo de violencia física
endes_health <- endes_health %>% 
  mutate(empujon = case_when(
    d105a %in% c(1,2) ~ 1,
    TRUE ~ 0
  ),
  abofeton = case_when(
    d105b %in% c(1,2) ~ 1,
    TRUE ~ 0
  ),
  golpe = case_when(
    d105c %in% c(1,2) ~ 1,
    TRUE ~ 0
  ),
  patear = case_when(
    d105d %in% c(1,2) ~ 1,
    TRUE ~ 0
  ),
  estrangular = case_when(
    d105e %

