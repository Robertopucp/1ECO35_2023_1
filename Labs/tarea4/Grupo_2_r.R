# Tarea 4 ####

# Limpia el environment

rm(list = ls())

# Limpia gráficos
graphics.off()

# Limpia consolas

cat("\014")
#Abrir librerias

library(readxl)
library(openxlsx)
library(pacman) 
p_load(dplyr, readxl, tidyverse, foreign, datos) 

## Script R y Python - Datos de la Comisión de la Verdad y Reconciliación----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Se selecciona el directorio

datossav <- read.spss("../../data/actos_est.sav", #Se extrae la base de datos
                      use.value.labels = F, 
                      to.data.frame = TRUE)


###Mostrar las etiquetas de valor y las etiquetas de las variables----


attributes(datossav)$variable.labels #Etiqueta de variables
datosValores <- read.spss("../../data/actos_est.sav") 
attributes(datosValores)$label.table #Etiqueta de valores


###Filtrar la base de datos según los siguientes tipos de eventos: desaparición, secuestros, reclutamiento forzado y muertes en atentados----


datossavDesaparicion <- filter(datossav, LDS_LDT == 1)  # Desaparición
datossavSecuestros <- filter(datossav, LSE == 1)  # Secuestros
datossavReclutamientoForzado <- filter(datossav, LRC == 1)  # Reclutamiento forzado
datossavMuertesAtentados <- filter(datossav, MAT == 1)  # Muertes en atentados


###Borrar los duplicados de la columna (IDACTO)----

datossav <- distinct(datossav, IDACTO, .keep_all = TRUE)

###Crear una base de datos con el total de eventos de violencia según su tipo a nivel departamento----

#Generar variable DEPNAO
datossav <- datossav %>% 
  mutate(DEPNAO = substr(UBIDIST, 1, 2), 
         DEPNAO = case_when(
           DEPNAO == "01" ~ "Amazonas", DEPNAO == "02" ~ "Ancash",
           DEPNAO == "03" ~ "Apurimac", DEPNAO == "04" ~ "Arequipa",
           DEPNAO == "05" ~ "Ayacucho", DEPNAO == "06" ~ "Cajamarca",
           DEPNAO == "07" ~ "Callao"  , DEPNAO == "08" ~ "Cusco",
           DEPNAO == "09" ~ "Huancavelica",DEPNAO == "10" ~ "Huanuco",
           DEPNAO == "11" ~ "Ica"     , DEPNAO == "12" ~ "Junin",
           DEPNAO == "13" ~ "La Libertad",DEPNAO == "14" ~ "Lambayaque",
           DEPNAO == "15" ~ "Lima"    , DEPNAO == "16" ~ "Loreto",
           DEPNAO == "17" ~ "Madre de Dios",DEPNAO == "18" ~ "Moquegua",
           DEPNAO == "19" ~ "Pasco"   ,DEPNAO == "20" ~ "Piura",
           DEPNAO == "21" ~ "Puno"    ,DEPNAO == "22" ~ "San Martin",
           DEPNAO == "23" ~ "Tacna"   ,DEPNAO == "24" ~ "Tumbes",
           DEPNAO == "25" ~ "Ucayali"
         ),
         
  )
#Se genera la tabla que presenta, por departamento, el total de eventos de violencia según tipo
datosViolenciaDEP<- datossav %>% 
  group_by(DEPNAO) %>% 
  summarise(desapForz = sum(LDS_LDT),
            detencion = sum(LDT),
            reclutamientoForz= sum(LRC),
            secuestro = sum(LSE),
            asesinato = sum(MAE),
            muerteAtentado = sum(MAT),
            muerteEnfrentamiento = sum(MEF),
            lesiones = sum(TLA),
            tortura = sum(TTR),
            violacionSexual = sum(TVS))


###Crear una base de datos con el total de eventos de violencia según su tipo a nivel distrito---- 


#Se genera la tabla que presenta, por distrito, el total de eventos de violencia según tipo
datosViolenciaDIS<- datossav %>% 
  group_by(UBIDIST) %>% 
  summarise(desapForz = sum(LDS_LDT),
            detencion = sum(LDT),
            reclutamientoForz= sum(LRC),
            secuestro = sum(LSE),
            asesinato = sum(MAE),
            muerteAtentado = sum(MAT),
            muerteEnfrentamiento = sum(MEF),
            lesiones = sum(TLA),
            tortura = sum(TTR),
            violacionSexual = sum(TVS))

#Se agrega a la tabla anterior los logaritmos de las respectivas variables. Se coloca 0 si el total de eventos es 0, pues no existe el ln(0)
datosViolenciaDIS <- datosViolenciaDIS %>% 
  mutate(log_desapForz = ifelse(desapForz == 0, 0, log(desapForz)),
         log_detencion = ifelse(detencion == 0, 0, log(detencion)),
         log_reclutamientoForz = ifelse(reclutamientoForz == 0, 0, log(reclutamientoForz)),
         log_secuestro = ifelse(secuestro == 0, 0, log(secuestro)),
         log_asesinato = ifelse(asesinato == 0, 0, log(asesinato)),
         log_muerteAtentado = ifelse(muerteAtentado == 0, 0, log(muerteAtentado)),
         log_muerteEnfrentamiento = ifelse(muerteEnfrentamiento == 0, 0, log(muerteEnfrentamiento)),
         log_lesiones = ifelse(lesiones == 0, 0, log(lesiones)),
         log_tortura = ifelse(tortura == 0, 0, log(tortura)),
         log_violacionSexual = ifelse(violacionSexual == 0, 0, log(violacionSexual)))


###Crear una base de datos con el total de eventos de violencia según su tipo a nivel departamento y periodo----

datosViolenciaDEP_PERIOD <- datossav %>% 
  group_by(DEPNAO, PERIODO) %>% 
  summarise(desapForz = sum(LDS_LDT),
            detencion = sum(LDT),
            reclutamientoForz = sum(LRC),
            secuestro = sum(LSE),
            asesinato = sum(MAE),
            muerteAtentado = sum(MAT),
            muerteEnfrentamiento = sum(MEF),
            lesiones = sum(TLA),
            tortura = sum(TTR),
            violacionSexual = sum(TVS)) 

## Script solo en R----


### Bases del modulo 200 y 300 ####

cat("\014")
rm(list = ls())
library(foreign)
library(dplyr)
library(haven)
library(pacman)
library(ggplot2)

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

enaho_200 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
enaho_300 <- read_dta("../..//data/enaho/enaho01a-2019-300.dta")


enaho_unidos <- inner_join(enaho_200, enaho_300, by = c("conglome", "vivienda", "hogar", "codperso"))

# Filtro de datos

enaho200_filtrado <- subset(enaho_200, (p204 == 1 & p205 == 2) | (p204 == 2 & p206 == 1))

# Creacion de a?os de educacion

enaho_unidos <- enaho_unidos %>% 
  mutate(
    educ1 = case_when(
      between(p301a,1,4) ~ 0,
      between(p301a,5,6) ~ 6,
      between(p301a,7,10) ~ 11,
      p301a == 11 ~ 16
    ),
    educ2 = ifelse(p301b == 0 & p301c == 0, NA_real_, p301b + p301c)
  ) %>% 
  group_by(conglome, vivienda, hogar, codperso) %>% 
  mutate(
    years_educ = sum(educ1, educ2, na.rm = TRUE)
  ) %>% 
  ungroup()

#Se halla el maximo de a?os de educacion de algun miembro del hogar y se agrega al dataframe enaho_unidos. Por ejemplo, si en un hogar uno tiene 11 y otro tiene 6 a?os de educacion, 11 se le asigna al hogar como el maximo. 

enaho_unidos_maxeduc <- enaho_unidos %>% 
  group_by(conglome, vivienda, hogar) %>% 
  summarise(max_educ = max(years_educ, na.rm = TRUE)) %>% 
  ungroup()

enaho_unidos <- enaho_unidos %>% 
  left_join(enaho_unidos_maxeduc, by = c("conglome", "vivienda", "hogar"))




### Uso del modulo 100 ####

enaho_100 <- read_dta("../..//data/enaho/enaho01-2019-100.dta")
sumaria <- read_dta("../..//data/enaho/sumaria-2019.dta")
union_all <- inner_join(enaho_100, sumaria, by = c("conglome", "vivienda", "hogar"))

# Creacion de la variable dummy. nbi_dummy toma el valor de 1 si es que existe al menos una necesidad basica insatisfecha.

union_all <- union_all %>%
  mutate(nbi_total = nbi1 + nbi2 + nbi3 + nbi4 + nbi5,
         nbi_dummy = ifelse(nbi_total > 0, 1, 0))

# Gr?fico de barras



#
union_all <- union_all %>% 
  mutate(dpto_code = substr(ubigeo, 1, 2), # posicic?n inicial, 2 : posici?n final
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
         
  )
#


# Se crea un dise?o de encuesta donde se usa factor07
dise?o <- union_all %>% 
  as_survey_design(ids = conglome, strata = estrato, weight = factor07)

# Se halla el porcentaje de hogares con NBI por departamento
nbi_departamentos <- dise?o %>% 
  group_by(dpto) %>% 
  summarise(pct_hogares_nbi = survey_mean(nbi_dummy) * 100)

# Se realiza el gr?fico de barras por departamento
ggplot(nbi_departamentos, aes(x = dpto, y = pct_hogares_nbi)) +
  geom_col(fill = "steelblue") +
  labs(x = "Departamento", y = "Porcentaje de hogares con NBI", 
       title = "Porcentaje de hogares con NBI por departamento - 2019") +
  coord_flip()


### Append de las modulos de sumaria desde 2015 hasta 2020 ####

sumaria_2015 <- read_dta("../..//data/enaho/sumaria-2015.dta")
sumaria_2016 <- read_dta("../..//data/enaho/sumaria-2016.dta")
sumaria_2017 <- read_dta("../..//data/enaho/sumaria-2017.dta")
sumaria_2018 <- read_dta("../..//data/enaho/sumaria-2018.dta")
sumaria_2019 <- read_dta("../..//data/enaho/sumaria-2019.dta")
sumaria_2020 <- read_dta("../..//data/enaho/sumaria-2020.dta")
base_deflactores <- read_dta("../../data/enaho/deflactores_base2020_new.dta")
#Se juntan las bases de datos


appendSumaria <- bind_rows(sumaria_2015, sumaria_2016, sumaria_2017, sumaria_2018, sumaria_2019, sumaria_2020) %>% 
  rename(anio = a?o) %>% 
  mutate(  dpto = as.numeric(substr(ubigeo, 1, 2)),
           anio = as.numeric(anio)
  )

# Se asigna el c?digo de Lima region  al Callao


appendSumaria$dpto[ appendSumaria$dpto == 7 ] <- 15
appendSumaria <- left_join(appendSumaria,
                           base_deflactores, by = c("dpto", "anio" = "aniorec"))

# Se obtiene el factor de expansi?n a nivel persona 

appendSumaria$factorpob <-  round(appendSumaria$factor07*appendSumaria$mieperho, 1) 

# Se obtiene la variable area (rural o urbana) y el gasto mensual per capita asi como el factor de expansion
appendSumaria <- appendSumaria %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    gasmpc = gashog2d/(mieperho*12*i00), #Se deflacta
    factorpob = round(factor07*mieperho, 1)
  )

#Se crea un dise?o de encuesta
gasto_years <- appendSumaria %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   nest = TRUE,
                   weight = factorpob) %>%   
  group_by(anio, area) %>% 
  summarise(
    gasmpc = survey_mean(gasmpc, na.rm = T)
  )

#Se realiza el gr?fico
gasto_years %>% ggplot( aes(x = anio, y = gasmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + # dise?o de fondo
  scale_x_continuous(breaks = c(2015,2016,2017,2018, 2019 , 2020) ) +
  ggtitle("Average monthly expenditure percapita by area")+
  xlab("")+
  ylab("")


##ENDES----

library(pacman) 
p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 



## Se importa la base RECHO y se define como rech0

rech0 <- read_dta("../../data/endes/RECH0.dta") %>%  ## se importa la base RECHO y se define como rech0
  mutate(
    HHID = str_trim(HHID)    ## con str_trim se elimina los espacios en blanco
  )

names(rech0) <- tolower(names(rech0))  ## Se pasa los nombres de las variables de mayúscula a minúscula

## se importa la base RECH23 y se define como rech23

rech23 <- read_dta("../../data/endes/RECH23.dta") %>%    
  mutate( HHID = str_trim(HHID) )

names(rech23) <- tolower(names(rech23))   ## Se pasa los nombres de las variables de mayúscula a minúscula

## se importa la base RECH1 y se define como rech1

rech1 <- read_dta("../../data/endes/RECH1.dta") %>%     
  mutate(
    HHID = str_trim(HHID)
  )

names(rech1) <- tolower(names(rech1))   ## Se pasa los nombres de las variables de mayúscula a minúscula

## se importa la base REC84DV y se define como dv

dv <- read_dta("../../data/endes/REC84DV.dta") %>% 
  mutate(
    caseid = str_trim(caseid)
  )


## Se crea HHID (id hogar) y HVIDX (id persona)

dv[c('hhid','hvidx')] <- str_split_fixed(dv$caseid, " ", 2) 


dv$hvidx <- as.numeric( dv$hvidx )  ## se convierte los elementos a números


# Se une las 4 bases, se toma como master data a la base REC84DV, definida como dv

endes_violencia_física <- dv %>% 
  left_join(rech1, by = c("hhid","hvidx")) %>% 
  left_join(rech0, by = "hhid") %>% 
  left_join(rech23, by = "hhid") 


# Se crea una dummy que va a tomar el valor de 1, si la variable de violencia toma los valores de 1 y 2,
# mientras que tomará el valor de 0, si la variable de violencia toma los valores 0 y 3

endes_violencia_física <- endes_violencia_física %>% 
  mutate(
    dummy_d105a = case_when(
      d105a %in% c(0,3) ~ 0,
      d105a %in% c(1,2) ~ 1,   
      d105a == 9 ~ NA
    ),
    dummy_d105b = case_when(
      d105b %in% c(0,3) ~ 0,
      d105b %in% c(1,2) ~ 1,  
      d105b == 9 ~ NA
    ),
    dummy_d105c = case_when(
      d105c %in% c(0,3) ~ 0,
      d105c %in% c(1,2) ~ 1,   
      d105c == 9 ~ NA
    ),
    dummy_d105d = case_when(
      d105d %in% c(0,3) ~ 0,
      d105d %in% c(1,2) ~ 1,   
      d105d == 9 ~ NA
    ),
    dummy_d105e = case_when(
      d105e %in% c(0,3) ~ 0,
      d105e %in% c(1,2) ~ 1,   
      d105e == 9 ~ NA
    )
    
  )



# Se crea una dummy llamada physical que tomará el valor de 1 si alguna de las dummys toma el valor de 1 y tomará
# el valor de 0 si cada dummy toma el valor de 0

endes_violencia_física <- endes_violencia_física %>% 
  mutate(
    physical = ifelse(dummy_d105a==1 | dummy_d105b == 1 | dummy_d105c == 1 | dummy_d105d == 1 | dummy_d105e == 1, 1 , 
                      ifelse(dummy_d105a==0 & dummy_d105b== 0 & dummy_d105c== 0 & dummy_d105d== 0 & dummy_d105e== 0, 0, NA))
  )





