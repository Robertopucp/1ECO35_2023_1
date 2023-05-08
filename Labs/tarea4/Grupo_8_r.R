#TAREA 4------------------------------------------------
#Grupo 8
# Integrantes:
#- Renzo Mosquera (20181960)
#- Yenner Huancahuire (20173340)
#- Pamela Obregón (20173040)

# Instalamos librerias

install.packages("dplyr")
install.packages("readxl")
install.packages("pacman")
install.packages("stringr")


<<<<<<< Updated upstream
# Limpiamos la consola

cat("\014")
=======
# Limpiamos la consola, variables y gráficos

cat("\014")
rm(list = ls())
graphics.off()
>>>>>>> Stashed changes

# Library y otras opciones

library(pacman) 
<<<<<<< Updated upstream
p_load(dplyr, readxl, rstudioapi, tidyverse, foreign, datos)
=======
p_load(dplyr, readxl, rstudioapi, tidyverse, foreign, datos,
       fastDummies, haven, survey, srvyr, labelled)

>>>>>>> Stashed changes


options(scipen = 999) 

# Determinamos directorio de trabajo

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# Ejercicio sobre Datos de la CVR-----------------------

# Importamos la base de datos (haciendo los ajustes necesarios).

# Con etiquetas como datos

datoscvr <- read.spss("../../data/actos_est.sav",
                      use.value.labels = F, 
                      to.data.frame = TRUE,
                      reencode = "UTF-8")
                      
str(datoscvr)
sapply(datoscvr, class)

# Mostramos las etiquetas de valores y variables:

attributes(datoscvr)$variable.labels
attributes(datoscvr)$value.labels


# Filtramos la base de datos por eventos
# (desaparición, secuestros, reclutamiento forzado y muertes
# en atentados):

table(datoscvr$IDTIPOAC)

datoscvr <- filter(datoscvr, IDTIPOAC == 
                c("LDS", "LSE", "LRC", "MAT"))

# Eliminamos duplicados de la variable "IDACTO"

datoscvr <- datoscvr[!duplicated(datoscvr$IDACTO), ]

# Creamos las bases de datos solicitadas:

# Renombramos variables

datoscvr <-rename(datoscvr, DEPARTAMENTO = DEPNA0,
       DISTRITO = UBIDIST)

# Agregamos etiquetas necesarias

datoscvr$DEPARTAMENTO <- factor(datoscvr$DEPARTAMENTO, 
                             levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 90),
                             labels = c("Ayacucho", "Apurímac", "Huancavelica",
                                        "Cusco", "Huánuco", "Ucayali",
                                        "San Martín", "Puno", "Junín",
                                        "Lima-Callao", "Otros"))

datoscvr$PERIODO <- factor(datoscvr$PERIODO, 
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("1980-1982", "1983-1985",
                                           "1986-1988", "1989-1992",
                                           "1993-2000"))
                                          

# Base de datos que se agrupa por departamento y obtenemos el total de eventos
# de violencia

EventosxDpto <- datoscvr %>% group_by(DEPARTAMENTO) %>% 
  summarise(Total_Desaparición = sum(LDS_LDT),
            Total_Secuestros = sum(LSE),
            Total_RecluForza = sum(LRC),
            Total_MuAtentados = sum(MAT))

# Base de datos que se agrupa por distrito y obtenemos el total de eventos
# de violencia

<<<<<<< Updated upstream
=======
# Creamos la variable Total_Eventos que representa la suma total
# de cada evento.

 

>>>>>>> Stashed changes
EventosxDistr <- datoscvr %>% group_by(DISTRITO) %>% 
  summarise(Total_Desaparición = sum(LDS_LDT),
            Total_Secuestros = sum(LSE),
            Total_RecluForza = sum(LRC),
<<<<<<< Updated upstream
            Total_MuAtentados = sum(MAT))

=======
            Total_MuAtentados = sum(MAT),
            Log_T_Desaparición = log(Total_Desaparición),
            Log_T_Secuestros = log(Total_Secuestros),
            Log_T_RecluForza = log(Total_RecluForza),
            Log_T_MuAtentados = log(Total_MuAtentados))
            
            
>>>>>>> Stashed changes
# Base de datos que se agrupa por departamento y periodo, y luego obtenemos el total de eventos
# obtenemos el total de eventos de violencia

EventosDyP <- datoscvr %>% group_by(DEPARTAMENTO, PERIODO) %>% 
  summarise(Total_Desaparición = sum(LDS_LDT),
            Total_Secuestros = sum(LSE),
            Total_RecluForza = sum(LRC),
            Total_MuAtentados = sum(MAT))

<<<<<<< Updated upstream
=======
# ENAHO------------------------------------------------------

#-----------------------------------
# MODULO 200 Y MODULO 300-----------
#-----------------------------------

#Cargamos las bases de batos

enaho200_19 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
enaho300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")

#Unimos las bases de datos

enaho19 <- enaho200_19 %>% 
  inner_join(enaho300_19, by = c("conglome","vivienda","hogar","codperso"))



#PREGUNTA 1
#-----------

#Filtramos la base de datos con la siguiente condicion:
#La persona entrevistada en el hogar es miembro del hogar (p204 == 1) 
#que no ha estado ausente en el hogar en los últimos 30 días (p205 == 2) o 
#que la persona entrevsitada no es miembro del hogar (p204 == 2) 
#pero si estado presente en el hogar los ultimos 30 días (p206 == 1)

enaho_19_1 <- filter(enaho19, (p204==1 & p205==2) | (p204==2 & p206==1))


#PREGUNTA 2
#-----------

# Años de educación #

# Creamos una variable que indica los años de estudio acumulado hasta el NIVEL educativo alcanzado

enaho19$p301a

enaho19 <- enaho19 %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0, # hasta primaria completa indica 0 años
    between(p301a,5,6) ~ 6, # hasta secundaria completa indica 6 años
    between(p301a,7,10) ~ 11, # hasta superior universitaria completa indica 11 años
    p301a == 11 ~ 16 # si tiene maestria/doctorado completa indica 16 años
  ))


# Creamos una variable que indica los Años de estudio en el nivel educativo actual
# En dicha variable, Sumamos el grado,para secundaria o primaria, o 
# años de estudio, para educación superior

enaho19$educ2 <- apply(enaho19[,c("p301b","p301c")], 1 , sum , na.rm = T)

# Finalmente, creamos una variable que es la suma de los años acumulados con los años actuales
# Esta variable vendria a ser un indicador de 
# la máxima cantidad de años de educación alcanzado por algún miembro del hogar

enaho19$years_educ <- apply(enaho19[,c("educ1","educ2")], 1 , sum , na.rm = T)


#-----------------------------------
# MODULO 100 Y SUMARIA--------------
#-----------------------------------

#Cargamos las bases de batos

enaho100_19 <- read_dta("../../data/enaho/enaho01-2019-100.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")

#Unimos las bases de datos

enaho19 <- enaho100_19 %>% 
  inner_join(sumaria_19, by = c("conglome","vivienda","hogar"))


#PREGUNTA 1
#-----------

#Creamos una variable que indica la cantidad de necesidades basicas, sumando los valores 
# de 5 variables que indican diferentes tipos de necesidades basicas

enaho19$num_nec <- apply(enaho19[,c("nbi1","nbi2","nbi3","nbi4","nbi5")], 1 , sum , na.rm = T)

#Creamos una variable dicotomica, la cual indica 1 de tener al menos una necesidad basica

enaho19$nec_bas <- ifelse(enaho19$num_nec > 0, 1, 0)



#PREGUNTA 2
#-----------

#Creamos la variable departamento (dpto)

dpto_code = substr(enaho19$ubigeo, 1, 2)
ubigeo3 = str_pad(enaho19$ubigeo2, 6, pad ="0")
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


# Factor de expansion a nivel hogar
enaho19$factorpob <- round(enaho19$factor07, 1)

# Diseño de la encuesta 

design <- svydesign(
  data = enaho19,
  ids = ~ conglome,
  strata = ~estrato,
  weights = ~ factorpob,
  nest = TRUE
)

# Creamos la grafica de barras que indica el porcentaje de los hogares
#que poseen al menos una necesidad basica

prop.table(svytable(~ dpto + nec_bas, design = design), 1) %>%
  as.data.frame() %>% 
  filter(nec_bas == 1) %>% 
  mutate(nec_bas = Freq*100) %>% 
  ggplot(aes(y = reorder( dpto, -nec_bas) , x = nec_bas   )) +
  geom_col() +
  scale_fill_identity(guide = "none") +
  theme_minimal() +  #diseño 
  xlab("") +
  ylab("Departmento")


#-----------------------------------
# APPEND SUMARIA (2015 -2020)-------
#-----------------------------------
sumaria_15 <- read_dta("../../data/enaho/sumaria-2015.dta")
sumaria_16 <- read_dta("../../data/enaho/sumaria-2016.dta")
sumaria_17 <- read_dta("../../data/enaho/sumaria-2017.dta")
sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
base_deflactores <- read_dta("../../data/enaho/deflactores_base2020_new.dta")

# Unimos sumaria con la base de datos de deflactores

append_enaho <- bind_rows(sumaria_15, sumaria_16, sumaria_17, sumaria_18, sumaria_19, sumaria_20) %>% 
  mutate(  dpto = as.numeric( substr(ubigeo,1,2) ),
           año = as.numeric(año)
  ) 


# Asignamos el código de Lima region  al Callao


append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15

# Unimos con la base deflactores 

append_enaho <- left_join(append_enaho,
                          base_deflactores, by = c("dpto", "año" = "aniorec"))


# Factor de expansión a nivel persona 

append_enaho$factorpob <-  round(append_enaho$factor07*append_enaho$mieperho, 1) 


# Hallamos el ingreso percapita mensual segun el area

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    ingmpc = inghog1d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )


# Hallamos el ingreso percapita mensual segun el area por año

income_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob,
                   nest = TRUE) %>%   
  group_by(año, area) %>% 
  summarise(
    ingmpc = survey_mean(ingmpc, na.rm = T)
  )


# Creamos el gráfico evolución del ingreso percápita mensual del hogar 2015-2020 

income_years %>% ggplot( aes(x = año, y = ingmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + # diseño de fondo
  scale_x_continuous(breaks = c(2015,2016,2017,2018, 2019 , 2020) ) +
  ggtitle("Average monthly income percapita by area")+
  xlab("")+
  ylab("")

# Hallamos el gasto percapita mensual segun el area

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    gmensual_pc = gashog2d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )


# Hallamos el gasto percapita mensual segun el area por año

gasto_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob,
                   nest = TRUE) %>%   
  group_by(año, area) %>% 
  summarise(
    gmensual_pc= survey_mean(gmensual_pc, na.rm = T)
  )


# Creamos el gráfico evolución del gasto percápita mensual del hogar 2015-2020 

gasto_years %>% ggplot( aes(x = año, y = gmensual_pc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + # diseño de fondo
  scale_x_continuous(breaks = c(2015,2016,2017,2018, 2019 , 2020) ) +
  ggtitle("Average monthly expenditure percapita by area")+
  xlab("")+
  ylab("")




>>>>>>> Stashed changes

