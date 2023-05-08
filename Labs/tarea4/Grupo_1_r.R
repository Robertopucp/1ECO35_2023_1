################  Tarea 4 
# Curso: Laboratorio de R y Python 
# @author: Grupo 1 - Melanig Geng, Keyth Hurtado y Fátima Trujillo

# Limpiamos el environment

rm(list = ls())

# Limpiamos los gráficos

graphics.off()

# Limpiamos la consola

cat("\014")

# Otras opciones

options(scipen = 999)      # No scientific notation

# Usamos pacman para llamar librerías

library(pacman) 

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled, datos) 

# Cambiamos el working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# Parte 1: CVR ####

#En esta sección, procesamos la base de datos de la comisión de la verdad y reconciliación (CVR)
#Primero, importamos la base de datos

cvr <- read.spss("../../data/actos_est.sav", to.data.frame=TRUE, use.value.labels = F)
View(cvr) 

#mostramos las etiquetas de valor y las etiquetas de variables

value_label <- read.spss("../../data/actos_est.sav")
attributes(value_label)$label.table
attributes(cvr)$variable.labels 

## 1.1 Filtros ####

#Desaparición 
cvr_desaparicion <- filter(cvr, IDTIPOAC == "LDS")

#secuestros
cvr_secuestros <- filter(cvr, IDTIPOAC == "LSE")

#Reclutamineto forzado
cvr_recl <- filter(cvr, IDTIPOAC == "LRC")

#Muertes en atentados
cvr_muertes <- filter(cvr, IDTIPOAC == "MAT")

## 1.2 Borrar duplicados ####
cvr2<-cvr[!duplicated(cvr$IDACTO),] #Esta opción nos permite quedarnos solo con la primera ocurrencia de un acto de violencia por individuo

## 1.3 Generación de BD ####

# en las siguientes líneas, generamos las bases de datos solicitadas

####1.3.1. Por departamento ####

cvr_dep <- cvr2 %>% dplyr::group_by(DEPNA0) %>%
  summarise(LDS=sum(LDS_LDT), LDT=sum(LDT), LRC=sum(LRC), LSE=sum(LSE), MAE=sum(MAE), MAT=sum(MAT),
            MEF=sum(MEF), TLA=sum(TLA), TTR=sum(TTR),TVS=sum(TVS))  %>%
  view

#### 1.3.2. Por distrito ####

cvr_dist<-cvr2 %>% dplyr::group_by(UBIDIST) %>%
  summarise(LDS=sum(LDS_LDT), LDT=sum(LDT), LRC=sum(LRC), LSE=sum(LSE), MAE=sum(MAE), MAT=sum(MAT),
            MEF=sum(MEF), TLA=sum(TLA), TTR=sum(TTR),TVS=sum(TVS))  %>%
  view  

#logaritmo
#Dado que hay observaciones con 0,añadimos a la base de datos original la unidad
log_cvr <- cvr_dist[-1]+1

#calculamos el logaritmo
log_cvr<-log(log_cvr)

#### 1.3.2. Por departamento y periodo ####
cvr_per<-cvr2 %>% dplyr::group_by(DEPNA0, PERIODO) %>%
  summarise(LDS=sum(LDS_LDT), LDT=sum(LDT), LRC=sum(LRC), LSE=sum(LSE), MAE=sum(MAE), MAT=sum(MAT),
            MEF=sum(MEF), TLA=sum(TLA), TTR=sum(TTR),TVS=sum(TVS))  %>%
  view  


# Parte 2: Enaho ####
# En esta sección, desarrollamos las preguntas relacionadas a la Enaho

#Llamamos a las bases de datos que usaremos

enaho100_19 <- read_dta("../../data/enaho/enaho01-2019-100.dta")
enaho200_19 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
enaho300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")
sumaria_15 <- read_dta("../../data/enaho/sumaria-2015.dta")
sumaria_16 <- read_dta("../../data/enaho/sumaria-2016.dta")
sumaria_17 <- read_dta("../../data/enaho/sumaria-2017.dta")
sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")
sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
base_deflactores <- read_dta("../../data/enaho/deflactores_base2020_new.dta")

## 2.1 Módulos 200 y 300 ####

#Unimos los módulos 200 y 300 de la enaho

enaho19_200_300 <- enaho200_19 %>% 
  left_join(enaho300_19, by = c("conglome","vivienda","hogar","codperso"))

#Filtramos según residentes permanentes
enaho19_200_300 %>% filter( (p204==1 & p205==2) | (p204==2 & p206==1) )

#Establecemos la primera etapa de años de educación según el nivel alcanzado

enaho19_200_300 <- enaho19_200_300 %>% mutate(
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  ))

#Estableemos la segunda etapa según el último año o grado alcanzado

enaho19_200_300$educ2 <- apply(enaho19_200_300[,c("p301b","p301c")], 1 , sum , na.rm = T)

#Usamos ambas etapas para obtener los años de educación 

enaho19_200_300$years_educ <- apply(enaho19_200_300[,c("educ1","educ2")], 1 , sum , na.rm = T)

#Establacemos la cantidad máxima de años de educación alcanzados, según hogar

enaho19_200_300 <- enaho19_200_300 %>% 
  group_by(conglome, vivienda, hogar) %>% 
  mutate(max_educ = max(years_educ, na.rm = T))

#Etiquetamos la variable

# var label

var_label(enaho19_200_300) <- list(max_educ = "Cant max de años de educ, por hogar")


## 2.2 Módulo 100 + Sumaria ####

#Unimos las bases Usamos inner join porque los missings que tiene el módulo 100
#generan problemas al declarar el diseño muestral

enaho19_100_sum <- enaho100_19 %>%
  inner_join(sumaria_19, by = c("conglome","vivienda","hogar"))

#Sumamos las dummys de nbis para obtener el número de nbis del hogar
enaho19_100_sum$nbis <- apply(enaho19_100_sum[,c("nbi1","nbi2", "nbi3", "nbi4", "nbi5")], 1 , sum , na.rm = T)

#Creamos una dummy para establecer si el hogar tiene una o más nbi
enaho19_100_sum$nbi <- ifelse(enaho19_100_sum$nbis > 0, 1, 0)

#Tabla para comprobar nuestros resultados
table(enaho19_100_sum$nbis, enaho19_100_sum$nbi)

#Etiquetamos la variable

  # var label

var_label(enaho19_100_sum) <- list(nbi = "Dummy de incidencia de al menos una NBI")


  # value labels


val_labels(enaho19_100_sum$nbi) <- c("Hogar sin NBI" = 0,
                                     "Hogar con al menos una NBI" = 1)

#Identificamos los departamentos
enaho19_100_sum <- enaho19_100_sum %>% 
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
  )

#Estalbecemos el factor poblacional
enaho19_100_sum$factorpob <- round(enaho19_100_sum$factor07, 1)

#Declaramos el diseño muestral
design <- svydesign(
  data = enaho19_100_sum,
  ids = ~ conglome,
  strata = ~estrato,
  weights = ~ factorpob,
  nest = TRUE
)

#Graficamos la incidencia de NBI, según departamento y considerando el diseño muestral
prop.table(svytable(~ dpto + nbi, design = design), 1) %>%
  as.data.frame() %>% 
  filter(nbi == 1) %>% 
  mutate(nbi = Freq*100) %>% 
  ggplot(aes(y = reorder( dpto, -nbi) , x = nbi   )) +
  geom_col(width = 0.7) +
  scale_fill_identity(guide = "none") +
  theme_minimal()+
  theme(axis.text.y = element_text(hjust = 1))+
  ggtitle("Hogares con al menos una NBI, por departamento")+
  xlab("%")+
  ylab("Departmento")

## 2.3 Sumarias ####     
 
#Hacemos el append de las sumarias

append_enaho <- bind_rows(sumaria_15, sumaria_16, sumaria_17, sumaria_18, sumaria_19, sumaria_20) %>% 
  mutate(  dpto = as.numeric( substr(ubigeo,1,2) ),
           año = as.numeric(año)
  ) 

# Asignamos el código de Lima región  al Callao


append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15


#Unimos nuestra base con la de deflactores
append_enaho <- left_join(append_enaho,
                          base_deflactores, by = c("dpto", "año" = "aniorec"))

# Factor de expansión a nivel persona 

append_enaho$factorpob <-  round(append_enaho$factor07*append_enaho$mieperho, 1) 

### 2.3.1 Ingresos ####

#Generamos el ingreso mensual per cápita deflactado

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    ingmpc = inghog1d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )

# Calculamos el ingreso mensual per cápita deflactado según área, y en promedio por año
# Consideramos el diseño muestral

income_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob,
                   nest = T) %>%   
  group_by(año, area) %>% 
  summarise(
    ingmpc = survey_mean(ingmpc, na.rm = T)
  )

#Realizamos el gráfico

income_years %>% ggplot( aes(x = año, y = ingmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + # diseño de fondo
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019 , 2020) ) +
  ggtitle("Ingreso per cápita promedio mensual, por área")+
  xlab("Año")+
  ylab("Ingreso")

### 2.3.2 Gastos ####

#Generamos el gasto mensual per cápita deflactado

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    gasmpc = gashog2d/(mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )

# Calculamos el gasto mensual per cápita deflactado según área, y en promedio por año
# Consideramos el diseño muestral

spend_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob,
                   nest = T) %>%   
  group_by(año, area) %>% 
  summarise(
    gasmpc = survey_mean(gasmpc, na.rm = T)
  )

#Realizamos el gráfico

spend_years %>% ggplot( aes(x = año, y = gasmpc, group = area, colour = area) ) +
  geom_line()+
  geom_point(size = 1.5) +
  theme_bw() + # diseño de fondo
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019 , 2020) ) +
  ggtitle("Gasto per cápita mensual, por área")+
  xlab("Año")+
  ylab("Gasto")

# Parte 3: ENDES ####

#En esta sección, procesamos la base de datos de la Encuesta Demográfica y de Salud Familiar (ENDES)

## 3.1 Unión de módulos ####

#Unir los módulos RECH84DV, RECH0, RECH1 y RECH23.

# Cargamos los datos de los módulos RECH84DV, RECH0, RECH1 y RECH23

rech84dv <- read_dta("../../data/endes/REC84DV.dta")
rech0 <- read_dta("../../data/endes/RECH0.dta")
rech1 <- read_dta("../../data/endes/RECH1.dta")
rech23 <- read_dta("../../data/endes/RECH23.dta")

# Nos fijamos en los nombres de las variables de cada módulo
names(rech84dv)
names(rech0)
names(rech1)
names(rech23)

# identificamos las variables que se repiten
intersect(colnames(rech1), colnames(rech23))
intersect(colnames(rech0), colnames(rech1))
intersect(colnames(rech0), colnames(rech84dv))

# como vemos que se repite la columna HHID, la usamos para unir rech0, rech1 y rech23
datos_unidos <- merge(rech0, rech1, by = "HHID")
datos_unidos <- merge(datos_unidos, rech23, by = "HHID")

# no tenemos la columna HHID en rech84dv, pero vemos que se parece a la columna caseid con menos caracteres
rech84dv$HHID <- substring(rech84dv$caseid, 1, 15)

# lo unimos con las demas bases de datos
datos_unidos <- merge(datos_unidos, rech84dv, by = "HHID")

## 3.2 Dummy de violencia física ####

#Crear una variable dummy por tipo de violencia física: d105a d105b d105c d105d d105e. Cada variable toma los siguientes valores: 0 (respuesta No), 1 (Frecuentemente), 2 (Algunas veces) y 3 (Nunca).

#d105a : Su esposo/compañero alguna vez la empujó, sacudió o le tiró algo
#d105b : Su esposo/compañero alguna vez la abofeteó o le retorció el brazo
#d105c : Su esposo/compañero alguna vez la golpeó con el puño o con algo que pudo hace
#d105d: Su esposo/compañero alguna vez la ha pateado o arrastrado
#d105e: Su esposo/compañero alguna vez trató de estrangularla o quemarla

#creamos variables dummy para cada tipo de violencia física
datos_unidos$d105a_dummy <- ifelse(datos_unidos$d105a %in% c(1, 2), 1, 0)
datos_unidos$d105b_dummy <- ifelse(datos_unidos$d105b %in% c(1, 2), 1, 0)
datos_unidos$d105c_dummy <- ifelse(datos_unidos$d105c %in% c(1, 2), 1, 0)
datos_unidos$d105d_dummy <- ifelse(datos_unidos$d105d %in% c(1, 2), 1, 0)
datos_unidos$d105e_dummy <- ifelse(datos_unidos$d105e %in% c(1, 2), 1, 0)

#creamos una variable dummy physical
datos_unidos$physical <- ifelse(rowSums(datos_unidos[, c("d105a_dummy", "d105b_dummy", "d105c_dummy", "d105d_dummy", "d105e_dummy")]) > 0, 1, 0)
