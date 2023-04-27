################  laboratorio 2 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 

# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

library(pacman) 


# permite llamar a varias librerias de manera simultánea
# Si la librería no está instalada, entonces lo instala y llama para su uso

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

# tidyverse es una recopilación de varias librerias (dply, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones
# haven tambien permite la lectura de base de datos de diferentes extensiones (i.e stata)


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

enaho100_19 <- read_dta("../../data/enaho/enaho01-2020-100.dta")
enaho200_19 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
enaho300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")
enaho400_19 <- read_dta("../../data/enaho/enaho01a-2019-400.dta")
enaho500_19 <- read_dta("../../data/enaho/enaho01a-2019-500.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")


# Join datasets 

#data1 <- 

df <- data.frame(
  id = 1:5,
  color = c("red", "blue", "green", NA, "red"),
  size = c("small", "large", "medium", "medium", "small"),
  type = c("A", "B", "C", NA, "A")
)



# Elegimos nuestro Master data


enaho19 <- enaho200_19 %>% 
  full_join(enaho500_19, by = c("conglome","vivienda","hogar","codperso"))

enaho19 <- enaho200_19 %>% 
  inner_join(enaho500_19, by = c("conglome","vivienda","hogar","codperso"))

enaho19 <- enaho200_19 %>% 
  left_join(enaho500_19, by = c("conglome","vivienda","hogar","codperso"))


enaho19 <- enaho200_19 %>% 
  left_join(enaho300_19, by = c("conglome","vivienda","hogar","codperso")) %>% 
  left_join(enaho400_19, by = c("conglome","vivienda","hogar","codperso")) %>% 
  left_join(enaho500_19, by = c("conglome","vivienda","hogar","codperso")) %>% 
  left_join(enaho100_19, by = c("conglome","vivienda","hogar")) %>% 
  left_join(sumaria_19, by = c("conglome","vivienda","hogar"))


table(enaho19$p300a)

names(enaho19)

attr(enaho19$p300a, 'labels')
attr(enaho19$p301a, 'label')
attr(enaho19$pobreza, 'labels')


lapply(enaho19, attr, 'labels')

lapply(enaho19, attr, 'label')

#-----------------------------------------------#

enaho19$lengua <- case_when(
  enaho19$p300a == 4 ~ 1,
  enaho19$p300a < 4 ~ 2,
  enaho19$p300a > 5 ~ 3
)


# siempre hacer crucce de variables para verificar que el proceso sea el correcto

# frecuencia relativa por columna

table(enaho19$p300a, enaho19$lengua)

prop.table(table(enaho19$p300a, enaho19$lengua), margin = 2)
  

enaho19$area <- case_when(
  enaho19$estrato <= 5 ~ 1,
  enaho19$estrato > 5 ~ 2
  
)

# años de educación #

enaho19 <- enaho19 %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  ))


enaho19$educ2 <- apply(enaho19[,c("p301b","p301c")], 1 , sum , na.rm = T)
enaho19$years_educ <- apply(enaho19[,c("educ1","educ2")], 1 , sum , na.rm = T)



enaho19 <- enaho19 %>% mutate(
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

# Crear dummies por cad anivel educativo

enaho19 <- dummy_cols(enaho19, select_columns = 'p301a')

enaho19$p301a_NA <- NULL

# Variables de pobreza #

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
         gmensual_pc = gashog2d/(mieperho*12),  # gasto del hogar mensual percapita
         imensual_pc = inghog1d/(mieperho*12)   # ingreso del hogar percapita
         )

# Variable binaria de pobreza: pobre y no pobre #

enaho19$dummy_pobre <- ifelse(enaho19$pobreza %in% c(1,2), 1, 0)

# var label

var_label(enaho19) <- list(dummy_pobre = "Dummy de pobreza")


# value labels


val_labels(enaho19$dummy_pobre) <- c("Hogar pobre" = 0,
                                    "Hogar no pobre" = 1)

val_labels(enaho19$area) <- c("Urbano" = 0,
                              "Rural" = 1)

# Replace 

enaho19$dummy_pobre <- replace( enaho19$dummy_pobre , 
                                which( enaho19$pobreza ==1 ), 1 )

enaho19$dummy_pobre <- replace( enaho19$dummy_pobre , 
                                which( enaho19$pobreza ==2 ), 2 )

enaho19$dummy_pobre <- replace( enaho19$dummy_pobre , 
                                which( enaho19$pobreza ==3 ), 3 )

table(enaho19$pobreza, enaho19$dummy_pobre)

enaho19$dummy_pobre <- ifelse(enaho19$pobreza %in% c(1,2), 1, 0)


# usando pip %>% 

enaho19 <- enaho19 %>% 
  mutate(
    dummy_pobre2 = 0,
    dummy_pobre2 = replace(dummy_pobre2, which(pobreza == 1), 1),
    dummy_pobre2 = replace(dummy_pobre2, which(pobreza == 2), 2),
    dummy_pobre2 = replace(dummy_pobre2, which(pobreza == 3), 3),
  )


table(enaho19$pobreza, enaho19$dummy_pobre2)

# Tasa de pobreza a nivel región usando la libreru survey

# Survey ----

# primero se declara el diseño muestral de la encuesta

enaho19$factorpob <- round(enaho19$factor07*enaho19$mieperho, 1)

data_ind <- enaho19  %>% 
  as_survey_design(ids = conglome, 
                  strata = estrato,
                  weight = factorpob) %>% 
                    dplyr::group_by(dpto) %>% 
                    summarise(
                    poverty_rate = survey_mean(dummy_pobre, na.rm = T)*100
                    )

# Uso de groupby
          
enaho19  %>%  group_by(dpto) %>% 
  summarise(
    poverty_rate = mean(dummy_pobre, na.rm = T)*100
  ) %>% as.data.frame()


# Uso de libreria survey #

# tabla de frecuencia relativa 

# Diseño de la encuesta 

design <- svydesign(
  data = enaho19,
  ids = ~ conglome,
  strata = ~estrato,
  weights = ~ factorpob,
  nest = TRUE
)

# tabla

prop.table(svytable(~ dpto + dummy_pobre, design = design), 1)

# Trabajo infantil y adolescente --------------------------------------

# ifelse coloca missing si alguna de las varaibles de la condicional es NA
# Esto es diferente en Python

enaho19 <- enaho19 %>% 
  mutate(
    dchildwork = ifelse( between(p208a,5,17), 0 , NA),
    dchildwork = replace(dchildwork, which(
      between(p208a,5,17) & (p210 == 1 | (p210 == 2 & (! t211 %in% c(9,11))) )
    ), 1),
    dmujer = ifelse(p207== 1, 0, 1)
  )

# label de la variable dummy di el menor de edad labora

val_labels(enaho19$dchildwork) <- c("No trabajo" = 0,
                                    "Trabaja" = 1)


design <- svydesign(
  data = enaho19,
  ids = ~ conglome,
  strata = ~estrato,
  weights = ~ factorpob,
  nest = TRUE
)

# area 1 (urbano), 2 (rural)

prop.table(svytable(~ area + dchildwork, design = design), 1)

prop.table(svytable(~ dmujer + dchildwork, design = design), 1)

prop.table(svytable(~ dpto + dchildwork, design = design), 1) %>%
  as.data.frame() %>% 
  filter(dchildwork == 1) %>% 
  mutate(ratechildw = Freq*100) %>% 
  ggplot(aes(y = reorder( dpto, -ratechildw) , x = ratechildw   )) +
  geom_col() +
  scale_fill_identity(guide = "none") +
  theme_minimal()+
  xlab("")+
  ylab("Department")

  



# Append --------------------------------

# Ingreso mensual per cápital del hogar promedio 2018-2020

append_enaho <- bind_rows(sumaria_18, sumaria_19, sumaria_20)

append_enaho$factorpob <-  round(append_enaho$factor07*append_enaho$mieperho, 1) 

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    ingmpc = inghog1d/(mieperho*12),
    factorpob = round(factor07*mieperho, 1)
  )


income_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob) %>%   
  group_by(año, area) %>% 
  summarise(
    ingmpc = survey_mean(ingmpc, na.rm = T),
    ingmpc_sd = survey_sd(ingmpc, na.rm = T)
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


ggsave("../../output/plots/line_income.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)



# inghog1d/(mieperho*12*i00)  próxima tarea


# ld: deflactor espacial
# i00: deflactor temporal

# ENDES ---------------------------------------------------

# Base a nivel hogar

read_dta("../../data/endes/RECH0.dta")[1,2]

rech0 <- read_dta("../../data/endes/RECH0.dta") %>% 
  mutate(
    HHID = str_trim(HHID)
  )

names(rech0) <- tolower(names(rech0))

sapply(rech0,attr ,'labels')
sapply(rech0,attr ,'label')
sapply(rech0, class)

# Mas información a nivel hogar

rech23 <- read_dta("../../data/endes/RECH23.dta") %>% 
  mutate( HHID = str_trim(HHID) )

names(rech23) <- tolower(names(rech23))

sapply(rech23,attr ,'labels')
sapply(rech23,attr ,'label')
sapply(rech23, class)

# Información a nivel individuo

rech1 <- read_dta("../../data/endes/RECH1.dta")


rech1 <- read_dta("../../data/endes/RECH1.dta") %>% 
  mutate(
    HHID = str_trim(HHID)
  )

names(rech1) <- tolower(names(rech1))

sapply(rech1,attr ,'labels')
sapply(rech1,attr ,'label')

sapply(rech1, class)

# Información de desnutrición 

rech6 <- read_dta("../../data/endes/RECH6.dta") %>% 
  mutate(
    HHID = str_trim(HHID)
  )

names(rech6) <- tolower(names(rech6))

sapply(rech6, class)

# Salud mental

salud <- read_dta("../../data/endes/CSALUD01.dta") %>% 
  mutate(
    HHID = str_trim(HHID)
  )

names(salud) <- tolower(names(salud))

sapply(salud, class)

#----------------------- Merge ---------------------------------#

endes_health_child <- rech6 %>% 
  left_join(rech0, by = "hhid") %>% 
  left_join(rech23, by = "hhid") %>% 
  left_join(rech1, by = c("hhid", "hc0"="hvidx"))

 
  # left_join(salud, by = c("hhid", "hvidx"="qsnumero"))

# Trabajaremos las variables 

# Dummies de anemia #

# El problema no es la presencia de missing tal cual
# Sino cuando la falta de información es representada por valores: 9, 999, 9998, 99888

endes_health_child <- endes_health_child %>% 
  mutate(
    anemia_sev = case_when(
      hc57 == 1 ~ 1,
      hc57 %in% c(2,3,4) ~ 0,
      hc57 == 9 ~ NA
    ),
    anemia_mildmod = case_when(
      hc57 %in% c(1,4) ~ 0,
      hc57 %in% c(2,3) ~ 1,
      hc57 == 9 ~ NA
    )
    
  )

# Dummy por desnutrición crónica #
# Niñas y niños que están por debajo de -3 DE de la media #

endes_health_child <- endes_health_child  |>
  mutate(
    hc70 = replace(hc70, which(hc70 %in% c(996,9998,9999)), NA),
    desncro = ifelse(hc70 < -300 & hv103 == 1, 1, NA),
    desncro = replace(desncro, which(hc70 >= -300 & hc70 < 601 & hv103 == 1), 0 ),
    peso = hv005a/1000000,
    region = factor(hv024, 
                    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
                               17,18,19,20,21,22,23,24,25),
                    labels = c("Amazonas", "Ancash", "Apurimac","Arequipa",
                          "Ayacuho","Cajamarca","Callao","Cusco","Huancavelica",
                          "Huanuco","Ica","Junin", "La Libertad", "Lambayeque",
                          "Lima", "Loreto", "Madre de Dios", "Moquegua",
                          "Pasco", "Piura" ,"Puno", "San Martín", "Tacna", "Tumbes",
                          "Ucayali"))
  )

# hv103 : la persona pasó la noche en el hogar


design <- svydesign(
  data = endes_health_child,
  ids = ~ hv001,
  strata = ~ hv022,
  weights = ~ peso
)

# area 1 (urbano), 2 (rural)


prop.table(svytable(~ region + desncro, design = design), 1) %>%
  as.data.frame() %>% 
  filter(desncro == 1) %>% 
  mutate(ratechildcro = Freq*100) %>% 
  ggplot(aes(y = reorder( region , -ratechildcro ) , x = ratechildcro    )) +
  geom_col() +
  scale_fill_identity(guide = "none") +
  theme_minimal()+
  xlab("")+
  ylab("Department")










# https://github.com/DHSProgram/DHS-Indicators-Stata

# https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/














