################  laboratorio 5 ############################
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

# tidyverse es una recopilación de varias librerias (dplyr, ggplot, stringr, etc)
# foreign, libreria que permite leer base de datos de diferentes extensiones
# haven tambien permite la lectura de base de datos de diferentes extensiones (i.e stata)


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Merge datasets ----------------------------

df1 <- data.frame(
  id = 1:5,
  color = c("red", "blue", "green", NA, "red"),
  size = c("small", "large", "medium", "medium", "small"),
  type = c("A", "B", "C", NA, "A")
)

df2 <- data.frame(
  id = c(2,3,4,10,20,30),
  value = c(1000, 2000, 3000,4000,5000,6000)
  
)

# Elijo mi MASTER data

df1 |>
  full_join(df2, by = "id")


df1 |>
  inner_join(df2, by = "id")


data <- df1 |>
  left_join(df2, by = "id")


### Dummies -------------------------------------

# creación de Dummies y missing #


data$dummy <- ifelse(data$value > 1500, 1, 0)


# Cuando alguna de las variables del condicional es missing
# la dummy toma el valor NA  ( Esto es diferente en python y stata )

### Merge ENAHO ------------------------------

enaho100_19 <- read_dta("../../data/enaho/enaho01-2019-100.dta")
enaho200_19 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
enaho300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")
enaho400_19 <- read_dta("../../data/enaho/enaho01a-2019-400.dta")
enaho500_19 <- read_dta("../../data/enaho/enaho01a-2019-500.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")
base_deflactores <- read_dta("../../data/enaho/deflactores_base2020_new.dta")


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

### Uso de case_when -------------------------------------------

enaho19$lengua <- case_when(
  enaho19$p300a == 4 ~ 1,
  enaho19$p300a < 4 ~ 2,
  enaho19$p300a > 5 ~ 3
)

# ~ ALT + 126

# recode 

# siempre hacer cruce de variables para verificar que el proceso sea el correcto

# frecuencia relativa por columna

table(enaho19$p300a, enaho19$lengua)

prop.table(table(enaho19$p300a, enaho19$lengua), margin = 2)
  
# Creación de la variable area 1 : urbano y 2 para rural

enaho19$area <- case_when(
  enaho19$estrato <= 5 ~ 1,  # urbano
  enaho19$estrato > 5 ~ 2    #rural 
  
)

# años de educación #

# años de estudio acumulado hasta el nivel educativo alcanzado



enaho19 <- enaho19 %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  ))

# p301a 1,2,3,4 -> educ1 = 0
# p301a 5,6 -> educ1 = 6
# p301a 7,10 ->  educa1 = 11 
# p301a 11 -> 16 

# Años de estudio en el nivel educativo actual

# Sumamos el grado o año de estudios. Grado para secundaria o primaria
# año de estudio para educación superior

enaho19$educ2 <- apply(enaho19[,c("p301b","p301c")], 1 , sum , na.rm = T)
enaho19$years_educ <- apply(enaho19[,c("educ1","educ2")], 1 , sum , na.rm = T)

# rowtotal()

# margin = 1 para sumar fila por fila (suma horizontal)

# na.rm ignora los missing

# otra alternatica

enaho19 <- enaho19 %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  )
  ) %>% rowwise() %>%  # row (fila), se realizará operaciones fila por fila
  mutate(
  educ2 = sum(p301b, p301c, na.rm = T),  # suma horizontal
  years_educ = sum(educ1, educ2, na.rm = T)
) %>% 
  ungroup()  # resactivar rowwise()

### Dummies de educación ----------------------------------------------

# Crear dummies por cada anivel educativo

enaho19 <- dummy_cols(enaho19, select_columns = 'p301a')



enaho19$p301a_NA <- NULL



# Variables de pobreza #

enaho19 <- enaho19 %>% 
  mutate(dpto_code = substr(ubigeo, 1, 2), # posicicón inicial, 2 : posición final
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

# mierperho : cantidad de miembros del hogar
# gashog2d

# Variable binaria de pobreza: pobre y no pobre #

enaho19$dummy_pobre <- ifelse(enaho19$pobreza %in% c(1,2), 1, 0)

# var label

var_label(enaho19) <- list(dummy_pobre = "Dummy de pobreza")


# value labels


val_labels(enaho19$dummy_pobre) <- c("Hogar pobre" = 0,
                                    "Hogar no pobre" = 1)

val_labels(enaho19$area) <- c("Urbano" = 0,
                              "Rural" = 1)


enaho19$dummy_pobre <- ifelse(enaho19$pobreza %in% c(1,2), 1, 0)


# Tasa de pobreza a nivel región usando la libreria survey

# Survey design --------------------------------

# primero se declara el diseño muestral de la encuesta

enaho19$factorpob <- round(enaho19$factor07*enaho19$mieperho, 1)


# factor07 es de modulo sumaria (factor de expansión a nivel hogar)

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

### tabla de frecuencia -------------------------------------

prop.table(svytable(~ dpto + dummy_pobre, design = design), 1)

### Trabajo infantil y adolescente --------------------------------------

# ifelse coloca missing si alguna de las variables de la condicional es NA
# Esto es diferente en Python

enaho19 <- enaho19 %>% 
  mutate(
    dchildwork = ifelse(  
      between(p208a,5,17) & (p210 == 1 | (p210 == 2 & (! t211 %in% c(9,11)))),
      1, ifelse( between(p208a,5,17), 0 , NA) ) ,
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

# Append sumaria y merge con los deflactores anuales. 
# El año base es 2020

append_enaho <- bind_rows(sumaria_18, sumaria_19, sumaria_20) %>% 
  mutate(  dpto = as.numeric( substr(ubigeo,1,2) ),
           año = as.numeric(año)
           ) 

# Asignamos el código de Lima metropolitana al Callao


append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15


append_enaho <- left_join(append_enaho,
                          base_deflactores, by = c("dpto", "año" = "aniorec"))

# Factor de expansión a nivel persona 

append_enaho$factorpob <-  round(append_enaho$factor07*append_enaho$mieperho, 1) 

# Uniendo con la base deflactores 

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
                   weight = factorpob) %>%   
  group_by(año, area) %>% 
  summarise(
    ingmpc = survey_mean(ingmpc, na.rm = T)
  )

  
income_years %>% ggplot( aes(x = año, y = ingmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks = c(2018, 2019 , 2020) ) +
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

dv <- read_dta("../../data/endes/REC84DV.dta") %>% 
  mutate(
    caseid = str_trim(caseid)
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

### Dummies de anemia-------------------

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

### Dummy por desnutrición crónica ----------------------------
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

### Mental health -----------------------------

# Este módulo es respondido por el jefe o jefa del hogar

endes_mental <- salud %>% 
  left_join(rech0, by = "hhid") %>% 
  left_join(rech23, by = "hhid") %>% 
  left_join(rech1, by = c("hhid", "qsnumero"="hvidx"))


# Dummies de depression #

" Preguntas respecto a los últimos 14 días"

col_repl <- c("qs700a", "qs700b", "qs700c", "qs700d","qs700e", "qs700f",
              "qs700g", "qs700h","qs700i")

attr(endes_mental$qs700a, "labels")


# reemplazamos los valores 9 por missing

endes_mental[col_repl] <- sapply(endes_mental[col_repl],  
                              function(x) replace(x, x == 9, NA))

endes_mental <- endes_mental |>
  rename(low_interest = qs700a,
         depressed = qs700b,
         not_sleep = qs700c,
         tired = qs700d,
         poor_appetite = qs700e,
         pay_attention = qs700f,
         difficult_move = qs700g,
         suicide =  qs700h,
         feel_bad = qs700i) |> rowwise() |>
        mutate(
                 phq9_score = sum(
                   low_interest,depressed, not_sleep, tired, poor_appetite,
                   pay_attention, difficult_move, suicide, feel_bad
                   , na.rm = T)
               ) |> ungroup() 

attach(endes_mental) # cada variable es un objeto independiente

endes_mental$mild_depression <- ifelse(phq9_score<5 | phq9_score >9, 0, 
                              ifelse(phq9_score>=5 | phq9_score <=9, 1, NA))

endes_mental$moderate_depression <- ifelse(phq9_score<10 | phq9_score >14, 0,
                                 ifelse(phq9_score>=10 | phq9_score <=14, 1, NA))

endes_mental$severe_depression <- ifelse(phq9_score<15, 0 ,
                                ifelse(phq9_score>=15, 1, NA))


# Violencia Doméstica -----------------------------------------------------

# unir bases de datos 
# Master Data es la base RECH84DV
# Uniré la bases de información socioeconómica personal rech1 e 
# información a nivel hogar (rech0 y rech24)

# En este caso, el identificador de la mujer es el caseid
# En esta base de datos se entrevistó a mujeres casadas o en convivencia.

# A partir de la variable caseid, creamos el HHID (id hogar) y HVIDX (id persona)

dv[c('hhid','hvidx')] <- str_split_fixed(dv$caseid, " ", 2) # split del espacio vacío

# 2 significa la cantidad de palabras separadas

dv$hvidx <- as.numeric( dv$hvidx )

dv_endes <- dv %>% 
  left_join(rech1, by = c("hhid","hvidx")) %>% 
left_join(rech0, by = "hhid") %>% 
  left_join(rech23, by = "hhid") 

# etiquetas

sapply(dv_endes, attr, 'label')

sapply(dv_endes, attr, 'labels')

# Violencia psicológica #

dv_endes <- dv_endes %>% 
  mutate(
    humiliated = ifelse(d103a %in% c(1,2), 1 , 
                         ifelse(d103a %in% c(0,3), 0, NA)),
    threatened = ifelse(d103b %in% c(1,2), 1 , 
                        ifelse(d103b %in% c(0,3), 0, NA)),
    psycho = ifelse(humiliated==1 | threatened== 1, 1 , 
                    ifelse(humiliated==0 & threatened== 0, 0, NA))
  )


table(dv_endes$psycho)


# References -----------------------------------

browseURL("https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/")
browseURL("https://github.com/DHSProgram/DHS-Indicators-Stata")













