
############       Workgroup 4       ################

## Curso: Laboratorio de R y Python ####
## Profesor: Roberto Mendoza  ####
## Autor: Grupo 4 ####

# clean environment variables
rm(list = ls())

# clean console
cat("\014")

# Library 
library(pacman)  # me permite llamar a varias librerias a la vez

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

# Change working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# 1.1 Importar los módulos 200 y 300 ----
datos200_19 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
datos300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")

names(datos200_19) # no existe la variable p204, p205 ni p206

# 1.2 Merge de los módulos 200 y 300 ----
enaho_2019 <- datos200_19 %>% 
  inner_join(datos300_19, by = c("conglome","vivienda","hogar","codperso"))

names(enaho_2019)

# 1.3 Filtrar la base ----
#enaho_2019 <- subset(enaho_2019, (p204==1 & p205==2) | (p204==2 & p206==1))

# no se puede porque no existen las variables de filtrado

# 1.4 Crear la variable años de educación ----
# 1.5 y la var. con la máxima cant. de años de educ. por hogar ----

enaho_2019 <- enaho_2019 %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  )
) %>% rowwise() %>%  # row (fila), se realizarÃ¡ operaciones en fila
  mutate(
    educ2 = sum(p301b, p301c, na.rm = T),  # suma horizontal
    years_educ = sum(educ1, educ2, na.rm = T)
    ) %>% 
  group_by(hogar) %>%  # por hogar
  mutate(max_educ_hogar = max(years_educ)) %>%  # maxima cant. de años
  ungroup()  # resactivar rowwise()

# 1.6 Cruzamos variables para comprobar ----
table(enaho_2019$p301a, enaho_2019$educ1)

table(enaho_2019$max_educ_hogar, enaho_2019$hogar)

table(enaho_2019$years_educ, enaho_2019$hogar)

### Parte 2 ----------------------------------------------------

## 2.1 Importando el módulo 100 y la sumaria ----
datos100_19 <- read_dta("../../data/enaho/enaho01-2019-100.dta")
datos_sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")


## 2.2 Merge del módulo 100 y la sumaria ----
enaho1_2019 <- datos100_19 %>% 
  inner_join(datos_sumaria_19, by = c("conglome","vivienda","hogar"))

names(enaho1_2019)

#View(datos_sumaria_19)
#View(datos100_19)

# attr(enaho1_2019$nbi1, 'labels')
#attr(enaho1_2019$nbi2, 'labels')
#attr(enaho1_2019$nbi3, 'labels')
#attr(enaho1_2019$nbi4, 'labels')
#attr(enaho1_2019$nbi5, 'labels')

#Cada variable toma el valor de 1 si el hogar
# carece de la necesidad básica.

## 2.3 Crear una variable dummy que indique si el hogar ---- 
## presenta al menos una necesidad básica insatisfecha

enaho1_2019$nbi_total <-apply(enaho1_2019[,c("nbi1","nbi2","nbi3","nbi4","nbi5")], 1,  sum, na.rm = T)

enaho1_2019 <- enaho1_2019 %>%
  mutate(neces_bas_insat = ifelse(nbi_total > 0, 1, 0))

# Hacemos un cruce para comprobar si está bien la función
table(enaho1_2019$nbi_total, enaho1_2019$neces_bas_insat)

# Si neces_bas_insat es :  
# .... = 1, el hogar carece de algunas necesidades básicas
# .... = 0,  el hogar no carece de alguna necesidad básica


# Variable binaria de alguna necesidad básica insatisfecha
val_labels(enaho1_2019$neces_bas_insat) <- c("El hogar no carece de alguna necesidad básica " = 0,
                                             "El hogar carece de alguna necesidad básica" = 1)

# Variable de necesidades básicas insatisfechas
attr(enaho1_2019$neces_bas_insat, "labels")


# Variable departamentos

enaho1_2019 <- enaho1_2019 %>% 
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
         ))

## 2.4 Crear el diseño muestral ---------

# primero se crea el factor de expansión
enaho1_2019$factorpob <- round(enaho1_2019$factor07*enaho1_2019$mieperho, 1)

# factor07 : factor de expansión a nivel hogar
# factor07 es de modulo sumaria (factor de expansión a nivel hogar)

# Forma 1

 data_ind <- enaho1_2019  %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob) %>% 
  dplyr::group_by(dpto) %>% 
  summarise(
    neces_bas_insat_rate = survey_mean(neces_bas_insat, na.rm = T)*100
  )

 
# Diseño de la encuesta
design <- svydesign(
  data = enaho1_2019,
  ids = ~ conglome,
  strata = ~estrato,
  weights = ~ factorpob,
  nest = TRUE
)

# 2.5. Crear un gráfico de barras ------

prop.table(svytable(~ dpto + neces_bas_insat, design = design), 1) %>%
  as.data.frame() %>% 
  filter(neces_bas_insat == 1) %>% 
  mutate(neces_bas_insat_rate = Freq*100) %>% 
  ggplot(aes(y = reorder( dpto, - neces_bas_insat_rate) , x =  neces_bas_insat_rate)) +
  geom_col() +
  scale_fill_identity(guide = "none") +
  theme_minimal() +  #diseÃ±o 
  xlab("") +
  ylab("Department")

### Parte 3 ----

# Realice un append de los módulos sumaria desde 2015 - 2020 ----

sumaria_15 <- read_dta("../../data/enaho/sumaria-2015.dta")
sumaria_16 <- read_dta("../../data/enaho/sumaria-2016.dta")
sumaria_17 <- read_dta("../../data/enaho/sumaria-2017.dta")
sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
base_deflactores <- read_dta("../../data/enaho/deflactores_base2020_new.dta")

# Append sumaria y merge con los deflactores anuales ----

# El año base es 2020 

append_enaho <- bind_rows(sumaria_15, sumaria_16, sumaria_17, sumaria_18, sumaria_19, sumaria_20) %>% 
  mutate(  dpto = as.numeric( substr(ubigeo,1,2) ),
           año = as.numeric(año)
  ) 


# Asignamos el código de Lima region  al Callao ----


append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15


append_enaho <- left_join(append_enaho,
                          base_deflactores, by = c("dpto", "año" = "aniorec"))


# Factor de expansión a nivel persona ----

append_enaho$factorpob <-  round(append_enaho$factor07*append_enaho$mieperho, 1) 

# Uniendo con la base deflactores  ------

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
                   weight = factorpob,
                   nest=TRUE) %>%   
  group_by(año, area) %>% 
  summarise(
    ingmpc = survey_mean(ingmpc, na.rm = T)
  )

# Evolución del ingreso percápita mensual del hogar 2015-2020 -------

income_years %>% ggplot( aes(x = año, y = ingmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + # diseÃ±o de fondo
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020) ) +
  ggtitle("Average monthly income percapita by area")+
  xlab("")+
  ylab("")

## Evolución del gasto per capita mensual ---------------

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    gasmpc = gashog2d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )

income_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob,
                   nest=TRUE) %>%   
  group_by(año, area) %>% 
  summarise(
    gasmpc = survey_mean(gasmpc, na.rm = T)
  )

income_years %>% ggplot( aes(x = año, y = gasmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + # diseño de fondo
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020) ) +
  ggtitle("Evolución del gasto per capita mensual por area")+
  xlab("")+
  ylab("")


## Gracias -------------
