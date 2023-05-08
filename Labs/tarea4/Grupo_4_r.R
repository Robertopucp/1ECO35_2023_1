
############       Workgroup 4       ################

## Curso: Laboratorio de R y Python ####
## Profesor: Roberto Mendoza  ####
## Autor: Grupo 4 ####
## Integrantes: Mishell Delgado, Lisbeth Ccoyo y Steven Atoche

# clean environment variables
rm(list = ls())

# clean console
cat("\014")

#Datos de la Comisión de la Verdad y Reconciliación

##Instalamos pacman

##install.packages("pacman")
##install.packages("Hmisc")

##Llamamos a la librería pacman

library(pacman) 
library(Hmisc)
library(labelled)


##Lo llamamos para su uso
p_load(dplyr, readxl, tidyverse, foreign, datos) 
p_load(haven, labelled)

# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

##Accedemos a la base de datos actos_est.sav

datoscvr <- read.spss("../../data/actos_est.sav", to.data.frame = TRUE)

#Revisamos las variables y en qué formato están

str(datoscvr)

## 1) Etiquetas de valor y etiquetas de las variables
# Extraemos las etiquetas de variable en var_labels

var_labels <- attr(datoscvr, "variable.labels")
var_labels

# Extaemos las etiquetas de valor

attributes(value_label)$label.table

## 2) Filtrar la base de datos 
#Desaparición

datoscvr[datoscvr$IDTIPOAC == "LDS",]

#Secuestros

datoscvr[datoscvr$IDTIPOAC == "LSE", ]

#Reclutamiento forzado

datoscvr[datoscvr$IDTIPOAC == "LRC", ]

#Muertes en atentados

datoscvr[datoscvr$IDTIPOAC=="MAT",]

## 3) Borrar duplicados
#Solo dejamos la primera observacion de cada persona

datoscvr2 <- distinct(datoscvr, IDACTO, .keep_all = TRUE)

##4) Crear distintas base de datos
###4.1) Total de eventos de violencia según su tipo a nivel departamento

actos <- var_values[['IDTIPOAC']]
actos

# Después, buscamos los departamentos registrados

departamentos <- var_values[['DEPNA0']]
departamentos

#Parte de la ENAHO

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

#ENDES

rm(list = ls())

library(readxl)
library(dplyr) 
library(rstudioapi)  
library(readr)
library(haven) # para leer archivos .dta


# cargar datos 
rech84dv <- read_dta("REC84DV.dta")
rech0 <- read_dta("RECH0.dta")
rech1 <- read_dta("RECH1.dta")
rech23 <- read_dta("RECH23.dta")

str(rech84dv)

# para ver los nombres de las columnas de las bases de datos
head(rech84dv, n = 13)
head(rech0, n = 13)
head(rech1, n = 13)
head(rech23, n = 13)

#identifiicar las columnas repetidas de las bases de datos
intersect(colnames(rech1), colnames(rech23))
intersect(colnames(rech0), colnames(rech1))
intersect(colnames(rech0), colnames(rech84dv))

# Combina rech0, rech1 y rech23 en una sola tabla utilizando la columna comÃºn HHID
datos_combinados <- merge(rech0, rech1, by = "HHID")

datos_combinados <- merge(datos_combinados, rech23, by = "HHID")

# no se puede unir datos_combinados con rech84dv porque no comparten una columna en comÃºn
names(rech84dv)

names(datos_combinados)

# crear una columna nueva para las dos bases de datos y luego unirlas 
datos_combinados$clave <- 1:nrow(datos_combinados)
rech84dv$clave <- 1:nrow(rech84dv)

datos_combinados_con_rech84dv <- merge(datos_combinados, rech84dv, by = "clave")



# variable dummy de violencia fisica: identificamos las variables que evidencien algun tipo de violencia fisica

datos_combinados_con_rech84dv$violencia_fisica <- ifelse(datos_combinados_con_rech84dv$d103b == 1 |
                                                           datos_combinados_con_rech84dv$d105a == 1 |
                                                           datos_combinados_con_rech84dv$d105b == 1 |
                                                           datos_combinados_con_rech84dv$d105c == 1 |
                                                           datos_combinados_con_rech84dv$d105d == 1 |
                                                           datos_combinados_con_rech84dv$d105e == 1 |
                                                           datos_combinados_con_rech84dv$d105f == 1 |
                                                           datos_combinados_con_rech84dv$d105j == 1 |
                                                           datos_combinados_con_rech84dv$d105h == 1,
                                                         1, 0)


# dummy de violencia sexual 
datos_combinados_con_rech84dv$violencia_sexual <- ifelse(datos_combinados_con_rech84dv$d105i == 1, 1, 0)


#crear una variable ficticia por cada tipo de violencia fisica


datos_combinados_con_rech84dv$ficticia_fisica <- ifelse(datos_combinados_con_rech84dv$d105a == 0 & datos_combinados_con_rech84dv$d105b == 0 & datos_combinados_con_rech84dv$d105c == 0 & datos_combinados_con_rech84dv$d105d == 0 & datos_combinados_con_rech84dv$d105e == 0, 0,   # Si todas las respuestas son No
                                                        ifelse(datos_combinados_con_rech84dv$d105a == 1 | datos_combinados_con_rech84dv$d105b == 1 | datos_combinados_con_rech84dv$d105c == 1 | datos_combinados_con_rech84dv$d105d == 1 | datos_combinados_con_rech84dv$d105e == 1, 1,  # Si alguna respuesta es Frecuentemente
                                                               ifelse(datos_combinados_con_rech84dv$d105a == 2 | datos_combinados_con_rech84dv$d105b == 2 | datos_combinados_con_rech84dv$d105c == 2 | datos_combinados_con_rech84dv$d105d == 2 | datos_combinados_con_rech84dv$d105e == 2, 2,  # Si alguna respuesta es Algunas veces
                                                                      3)))  # Si todas las respuestas son Nunca

# Ver los primeros registros del nuevo data frame con la variable ficticia
head(datos_combinados_con_rech84dv)


#crear dummys por cada variable ficticia creada

datos_combinados_con_rech84dv$d105a_dummy <- ifelse(datos_combinados_con_rech84dv$ficticia_fisica %in% c(1, 2), ifelse(datos_combinados_con_rech84dv$d105a %in% c(1, 2), 1, 0), 0)
datos_combinados_con_rech84dv$d105b_dummy <- ifelse(datos_combinados_con_rech84dv$ficticia_fisica %in% c(1, 2), ifelse(datos_combinados_con_rech84dv$d105b %in% c(1, 2), 1, 0), 0)
datos_combinados_con_rech84dv$d105c_dummy <- ifelse(datos_combinados_con_rech84dv$ficticia_fisica %in% c(1, 2), ifelse(datos_combinados_con_rech84dv$d105c %in% c(1, 2), 1, 0), 0)
datos_combinados_con_rech84dv$d105d_dummy <- ifelse(datos_combinados_con_rech84dv$ficticia_fisica %in% c(1, 2), ifelse(datos_combinados_con_rech84dv$d105d %in% c(1, 2), 1, 0), 0)
datos_combinados_con_rech84dv$d105e_dummy <- ifelse(datos_combinados_con_rech84dv$ficticia_fisica %in% c(1, 2), ifelse(datos_combinados_con_rech84dv$d105e %in% c(1, 2), 1, 0), 0)

# Crear variable dummy "physical"
datos_combinados_con_rech84dv$physical <- ifelse(datos_combinados_con_rech84dv$d105a_dummy == 1 | datos_combinados_con_rech84dv$d105b_dummy == 1 | datos_combinados_con_rech84dv$d105c_dummy == 1 | datos_combinados_con_rech84dv$d105d_dummy == 1 | datos_combinados_con_rech84dv$d105e_dummy == 1, 1, 0)

# Ver los primeros registros del nuevo data frame con las variables dummy y la variable "physical"
head(datos)


## Gracias -------------
