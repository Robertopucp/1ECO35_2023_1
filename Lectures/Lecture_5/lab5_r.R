################  laboratorio 7 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza
## Clean dataset

#install.packages("srvyr")  para declarar encuestas en R (similar al svyset en stata)


#install.packages("fastDummies")

#Librerias de limpieza de datos

#pacman::p_load(haven,dplyr, stringr, fastDummies)


library(haven)  # leer archivos spss, stata, dbf, etc
library(dplyr)  # limpieza de datos
library(stringr)   # grep for regular expression
library(fastDummies) # crear dummy
library(srvyr)  # libreria para declarar el diseño muestral de una encuesta
library(survey)


# tydiverse: ggplot , dplyr other libraries

# Conglome : 1235 , vivienda: 10, hogar: 11 , codperso : 4 año 2019

# Conglome : 1235 , vivienda: 10, hogar: 11, codperso : 4 año 2015



"1.0 Set Directorio"

user <- Sys.getenv("USERNAME")  # username

setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2022_2/Lab7") ) # set directorio


"2.0 Load dataset de ENAHO"

enaho01 <- read_dta("../../../enaho/2020/737-Modulo01/737-Modulo01/enaho01-2020-100.dta")

# tibble dataset

enaho01$dominio

enaho01 <- data.frame(

  read_dta("../../../enaho/2020/737-Modulo01/737-Modulo01/enaho01-2020-100.dta")

)

#data.frame dataset

enaho01
enaho01$dominio

# Check labels

# %>% Ctrl + shift + m

enaho01$estrato  %>% attr('labels') # value labels


enaho01$factor07 %>% attr('label') # var label

names(enaho01)

# Weight sampling

enaho02 = data.frame(
  read_dta("../../../enaho/2020/737-Modulo02/737-Modulo02/enaho01-2020-200.dta")
)

names(enaho02)

length( unique(enaho02$facpob07) )

length( unique(enaho02$conglome) )


length( unique(enaho01$factor07) )

length( unique(enaho01$conglome) )

sum(enaho02$facpob07)

unique(enaho01$conglome) # La suma resulta en total de la población 2020 proyectada?

"Módulo02"

enaho02 = data.frame(
  read_dta("../../../enaho/2020/737-Modulo02/737-Modulo02/enaho01-2020-200.dta")
)

enaho03 = data.frame(
  read_dta("../../../enaho/2020/737-Modulo03/737-Modulo03/enaho01a-2020-300.dta"))

enaho04 = data.frame(
  read_dta("../../../enaho/2020/737-Modulo04/737-Modulo04/enaho01a-2020-400.dta")
)

enaho05 = data.frame(
  read_dta("../../../enaho/2020/737-Modulo05/737-Modulo05/enaho01a-2020-500.dta")
)

enaho34 = data.frame(
  read_dta("../../../enaho/2020/737-Modulo34/737-Modulo34/sumaria-2020.dta")
)

enaho37 = data.frame(
  read_dta("../../../enaho/2020/737-Modulo37/737-Modulo37/enaho01-2020-700.dta")
)




# Seleccionar variables



enaho02 <- enaho02[ , c("conglome", "vivienda", "hogar" , "codperso",
                       "ubigeo", "dominio" ,"estrato" ,"p208a", "p209",
                       "p207", "p203", "p201p" , "p204",  "facpob07") ]


enaho03 <- enaho03[ , c("conglome", "vivienda", "hogar" , "codperso",
                        "p301a", "p301b", "p301c" , "p300a","p301b","p301c")]

enaho05 <- enaho05[ , c("conglome", "vivienda", "hogar" , "codperso",
                        "i524e1", "i538e1", "p558a5" , "i513t", "i518",
                        "p507", "p511a", "p512b", "p513a1", "p505" , "p506", "d544t", "d556t1",
                        "d556t2" , "d557t" , "d558t" , "ocu500" , "i530a" , "i541a")]

# 3.0 Merge section #

# Merge identifica automaticamente los casos de merge m:1, 1:1, 1:m



# _merge3 == 1,3

"Left merge"

#enaho02: master data
# enaho01: using data

enaho_merge <- merge(enaho02, enaho01,
                   by = c("conglome", "vivienda", "hogar"),
                   all.x = T
                   )

enaho_02_05 <- merge(enaho02, enaho05,
                     by = c("conglome", "vivienda", "hogar","codperso"),
                     all.x = T
)


# by: variable que permite identificar las observaciones en común en las bases de datos
# all.x : La base de datos preservará todas las observaciones de left data (enaho02)
# all.x tiene como valor predeterminado a False.


# all.x = False, all.y = False

# _merge3 == 2,3


enaho_02_05 <- merge(enaho02, enaho05,
                     by = c("conglome", "vivienda", "hogar","codperso"),
                     all.y = TRUE
                      )


# _merge3 == 3 (match inner)

enaho_merge_inner <- merge(enaho02, enaho01,
                     by = c("conglome", "vivienda", "hogar"),
                     all.x = F, all.y = F
                       )



enaho_merge_inner <- merge(enaho02, enaho01,
                     by = c("conglome", "vivienda", "hogar")
                  )

enaho_merge_02_05 <- merge(enaho02, enaho05,
                           by = c("conglome", "vivienda", "hogar","codperso")
)


# Match outer


enaho_merge_outer <- merge(enaho02, enaho05,
                           by = c("conglome", "vivienda", "hogar","codperso"),
                           all.x = T, all.y = T
)


enaho_merge_outer_2 <- merge(enaho02, enaho05,
                           by = c("conglome", "vivienda", "hogar","codperso"),
                           all=  T
)



# suffixes

enaho_merge <- merge(enaho02, enaho01,
                     by = c("conglome", "vivienda", "hogar"),
                     all.x = T
)


names(enaho_merge)



enaho_merge <- merge(enaho02, enaho01,
                     by = c("conglome", "vivienda", "hogar"),
                     all.x = T, suffixes = c("","")
                      )

enaho_merge <- merge(enaho02, enaho01,
                     by = c("conglome", "vivienda", "hogar"),
                     all.x = T, suffixes = c("",".y")
)


names(enaho_merge)

#------------------------- Match with different keyword (variable llave)  -------------------------


# rename variables que identifican de manera unica a cada hogar

enaho05 <- enaho05 %>% dplyr::rename(Conglo = conglome, viv = vivienda,
                              hog = hogar, cod = codperso)




enaho_02_05 <- merge(enaho02, enaho05,
                     by.x = c("conglome", "vivienda", "hogar","codperso"),
                     by.y = c("Conglo", "viv", "hog","cod"),
                     all = TRUE
)



# reset los nombre correctos de la variables que identificar cada hogar

enaho05 <- enaho05 %>% dplyr::rename(conglome = Conglo, vivienda = viv,
                              hogar = hog, codperso = cod)



#---------------------- Merge in Loop ------------------------------------

# <-  shortcut Alt + -

num = list(enaho34 , enaho37) # lista de data.frames

merge_hog = enaho01 # Master Data

for (i in num){

  merge_hog <- merge(merge_hog, i,
                     by = c("conglome", "vivienda", "hogar"),
                     all.x = T, suffixes = c("",".y")
                     )
}

names(merge_hog)



# Individual dataset

num = list(enaho03 , enaho04, enaho05 ) # lista de data.frames

merge_ind = enaho02 # Master Data

for (i in num){

  merge_ind <- merge(merge_ind, i,
                     by = c("conglome", "vivienda", "hogar","codperso"),
                     all.x = T, suffixes = c("",".y")
  )
}

names(merge_ind)


#----------------------- Merge Indivual and Hohar datasets -----------------------------------

# merge merge_hog and merge_ind
# mwrge_ind : master data

merge_base <- merge(merge_ind, merge_hog,
                   by = c("conglome", "vivienda", "hogar"),
                   all.x = T, suffixes = c("",".y"))


colnames(merge_base)

index <- grep(".y$", colnames(merge_base))  # Regular regular

# $ el texto finaliza con .y

merge_base_2020 <- merge_base[, - index]

colnames(merge_base_2020)


### Ubigeo de departamento

#ubigeo: 12 (ubigeo region junin, 1206 (provincia de satipo)
# 120601 (distrito de la provincia de satipo, region junin)

# sibstr permite sustraer digitos de un string, texto, caracter

merge_base_2020['ubigeo_dep'] = substr(merge_base_2020$ubigeo, 1, 2)

# a aprtir de  la posición inicial, extraer los dos primeros digitos

merge_base_2020['ubigeo_dep_2'] = paste(substr(merge_base_2020$ubigeo,1,2),
                                        "0000", sep = "")


### filtrado para algunos departamentos

merge_base_2020 <- merge_base_2020 %>%  filter(
  merge_base_2020$ubigeo_dep  %in% c("15","03","04","12") )

#library(dplyr)

merge_base_2020 <- merge_base_2020 %>%
  mutate(region = case_when(ubigeo_dep == "04" ~ "Arequipa",
                          ubigeo_dep == "03" ~ "Apurimac",
                          ubigeo_dep == "12" ~ "Junin",
                          ubigeo_dep == "15" ~ "Lima") )




"ENAHO 2019"

enaho01 <- data.frame(
  read_dta("../../../datos/2019/687-Modulo01/687-Modulo01/enaho01-2019-100.dta")
)

enaho02 = data.frame(
  read_dta("../../../datos/2019/687-Modulo02/687-Modulo02/enaho01-2019-200.dta")
)

enaho03 = data.frame(
  read_dta("../../../datos/2019/687-Modulo03/687-Modulo03/enaho01a-2019-300.dta"))

enaho04 = data.frame(
  read_dta("../../../datos/2019/687-Modulo04/687-Modulo04/enaho01a-2019-400.dta")
)

enaho05 = data.frame(
  read_dta("../../../datos/2019/687-Modulo05/687-Modulo05/enaho01a-2019-500.dta")
)

enaho34 = data.frame(
  read_dta("../../../datos/2019/687-Modulo34/687-Modulo34/sumaria-2019.dta")
)

enaho37 = data.frame(
  read_dta("../../../datos/2019/687-Modulo37/687-Modulo37/enaho01-2019-700.dta")
)


# Seleccionar variables


enaho02 <- enaho02[ , c("conglome", "vivienda", "hogar" , "codperso",
                        "ubigeo", "dominio" ,"estrato" ,"p208a", "p209",
                        "p207", "p203", "p201p" , "p204",  "facpob07")]

enaho03 <- enaho03[ , c("conglome", "vivienda", "hogar" , "codperso",
                        "p301a", "p301b", "p301c" , "p300a","p301b","p301c")]

enaho05 <- enaho05[ , c("conglome", "vivienda", "hogar" , "codperso",
                        "i524e1", "i538e1", "p558a5" , "i513t", "i518",
                        "p507", "p511a", "p512b", "p513a1", "p505" , "p506", "d544t",
                        "d556t1",
                        "d556t2" , "d557t" , "d558t" , "ocu500" , "i530a" , "i541a")]




num = list(enaho34 , enaho37) # lista de data.frames

merge_hog = enaho01 # Master Data

for (i in num){

  merge_hog <- merge(merge_hog, i,
                     by = c("conglome", "vivienda", "hogar"),
                     all.x = T, suffixes = c("",".y")
  )
}

# Individual dataset

num = list(enaho03 , enaho04, enaho05 ) # lista de data.frames

merge_ind = enaho02 # Master Data

for (i in num){

  merge_ind <- merge(merge_ind, i,
                     by = c("conglome", "vivienda", "hogar","codperso"),
                     all.x = T, suffixes = c("",".y")
  )
}



merge_base <- merge(merge_ind, merge_hog,
                    by = c("conglome", "vivienda", "hogar"),
                    all.x = T, suffixes = c("",".y"))

index <- grep(".y$", colnames(merge_base))


merge_base_2019 <- merge_base[, - index]



#----------------------- Append -----------------------------------


merge_append <-  bind_rows(merge_base_2020, merge_base_2019) # bind_rows from dyplr


unique(merge_append$aÑo)

# bind_rows from dplyr library


write_dta(merge_append, "../data/append_enaho_r.dta")


#------------------------ Poverty and dummies -------------------------------

#Ingreso nominal percapita mensual y gasto nominal mensual percapital del hogar

# inghog1d: ingreso anual bruto del hogar  (incluye ingresos en forma de bienes)
# gashog2d: gasto anual bruto hogar

# Estas variables provienen del módulo 34 - sumaria (módulo de variables calculadas)
# Linea de pobreza
# mieperho: miembros del hogar
# Excluye a los trabajadores domésticos y a las personas que subarriendan una habitación en el hogar


merge_base_2020 <- merge_base_2020 %>%
  dplyr::mutate(ingreso_month_pc = inghog1d/(12*mieperho),
         gasto_month_pc = gashog2d/(12*mieperho)
         ) %>%
  dplyr::mutate(dummy_pobre = ifelse( gasto_month_pc < linea ,
                        1 ,
                        0 ) ) %>%
  dplyr::mutate(pobre = ifelse( gasto_month_pc < linea ,
                               "pobre" ,
                               "No pobre") )   %>%
  dplyr::mutate(pc_pobre = case_when(pobreza == 1 ~ "Pobre extremo",
                             pobreza == 2 ~ "Pobre",
                             pobreza == 3 ~ "No pobre"))

# Si existe missing values en las variables usadas en el condicional
# entonces R colocará missing, esto no es directo en python


# Ejemplo adicional

var1 <- c(NA,2,3)

var2 <- c(400,1,5)

# creamos una base de datos en formato tibble similar a un data.frame

base <- tibble(
  var1, var2

)


# aplicamos mutate para crear la dummy

base %>%  mutate(

  Dummy = ifelse(var1 < var2, 1,0)

)


sum(is.na(merge_base_2020$gashog2d)) # no hay missing en la variables gasto anual del hogar


#creando dummies usando la variabe de nivel educativo alcanzado p301a

merge_base_2020 <- dummy_cols(merge_base_2020, select_columns = 'p301a')


View(merge_base_2020[, c("p301a","p301a_1","p301a_2","p301a_3","p301a_4","p301a_5")])



################ Colappse #############################################


# Tab in R from dplyr library

count(merge_base_2020, pobreza, sort = TRUE)

count(merge_base_2020, pc_pobre, sort = F)



#Alternativa de tab (STATA) en R

table(merge_base_2020$pc_pobre)

table(merge_base_2020$p301a)

merge_base_2020 %>% dplyr::filter(!is.na(p301a)) %>%  group_by(p301a) %>% summarise(Freq.abs = n()) %>%
  mutate(Freq.relative = (Freq.abs/sum(Freq.abs))*100) %>% arrange(desc(Freq.relative))

# arrange de la libreria dplyr permite ordenar una variable
# dplyr::filter pues al instalar las librerias, R indica de conflicto en el nombre de funciones
# en librerias diferentes. El código significa que que usará la función o método filter de la librearia dplyr


df1 <- merge_base_2020 %>% group_by(conglome, vivienda, hogar ) %>%
  summarise(
    edu_min = min(p301a),
    sup_educ = sum(p301a_10), total_miembros = n(),
    edu_max = max(p301a), .groups = "keep"
  )



# sin considerar los missing

df1_no_missing <- merge_base_2020 %>% group_by(conglome, vivienda, hogar ) %>%
  summarise(
    edu_min = min(p301a, na.rm = TRUE),
    sup_educ = sum(p301a_10, na.rm = T), total_miembros = n(),
    edu_max = max(p301a, na.rm = T),
  )



# La advertencia surge por que se están agrupando por varias variables.
# Para evitar el mensaje, debemos incluir el argumento .groups = "keep"

df2 <- merge_base_2020 %>% group_by(conglome, vivienda, hogar ) %>%
  summarise(
    edu_min = min(p301a, na.rm = TRUE),
    sup_educ = sum(p301a_10, na.rm = T), total_miembros = n(),
    edu_max = max(p301a, na.rm = T), .groups = "keep"
  )


#  max(p301a, na.rm = T), na.rm = T causa que R no tome en cuenta a los missings


# na.rm permite ignorar los missing en las operaciones mean, sum, max


df3 <- merge_base_2020 %>% group_by(ubigeo_dep, region) %>%
  summarise(index_poverty = mean(dummy_pobre, na.rm = T), .groups = "keep" )


class(merge_base_2020$p505)


#----------------- Indicadores socieconómicos ------------------------

# Primero indicamos a R que nuestra base de datos es una encuesta
# Para ello demebos declarar el diseño de la encuesta
# ids: conglomerado, strato: estrato y wieght : factor de expansión

survey_enaho <- merge_base_2020  %>% as_survey_design(ids = conglome, strata = estrato,
                                                      weight = facpob07)

#facpob07: factor de expansión a nivel población. Esto se constriye a partir de información Censo 2017


names(merge_base_2020)

ind1 <- survey_enaho %>%  dplyr::filter(p208a >=  10 & p208a<= 65) %>%  # me quedo con personas de 10 a 65 años
  mutate(
    g1 = ifelse(p208a>=10 & p208a <=20,1,0),  # dummies por grupos de edad
    g2 = ifelse(p208a>20 & p208a <=30,1,0),
    g3 = ifelse(p208a >30 & p208a <=40,1,0),
    g4 = ifelse(p208a >40 & p208a <=65,1,0),

  )  %>%  group_by(region) %>%   # indicadores de grupo de edad y nivel educativo
                                 # indicadores a nivel regional

  summarise(

    gp1 = survey_mean(g1), gp2 = survey_mean(g2), gp3 = survey_mean(g3),
    gp4 = survey_mean(g4),
    g_sec = survey_mean(p301a_6, na.rm = T), g_uni_co = survey_mean(p301a_10, na.rm = T)

  )



merge_base_2020$estrato

merge_base_2020$dominio


#------- Libreria Survey -------

# Se declara el diseño muestral


survey_enaho <- svydesign(id=~conglome, weights=~facpob07,strata=~estrato, data=merge_base_2020)




