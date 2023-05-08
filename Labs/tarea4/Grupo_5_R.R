################  TAREA 4 ############################
## Curso: Laboratorio de R y Python ###########################

# establecemos la direccion del archivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Library ####
library(pacman) 
p_load(dplyr, readxl, tidyverse, foreign, datos) 

# preguntas DATOS DE LA COMISIÓN DE LA VERDAD Y RECONCILIACIÓN ####
#A partir de la base de datos "actos_est.sav":

# 1) Mostrar las etiquetas de valor y las etiquetas de las variables
  
#PARA ETIQUETAS DE VALOR
data1 <- read.spss("../../data/actos_est.sav")
attributes(data1)$label.table

#PARA ETIQUETAS DE VARIABLES
data2 <- read.spss("../../data/actos_est.sav",use.value.labels = F, to.data.frame = TRUE)
attributes(data2)$variable.labels 

# 2) Filtrar la base de datos según los siguientes tipos de eventos: desaparición, secuestros, reclutamiento forzado y muertes en atentados

filtro1=data2 %>% filter(IDTIPOAC == "LDS" | IDTIPOAC == "LSE" | IDTIPOAC == "LRC"| IDTIPOAC == "MAT")

# 3) Borrar los duplicados de la columna (IDACTO)
abr <- unique(data2$IDACTO)
abr=as.data.frame(abr)

# DADO LO ANTERIOR, SE GENERA LO SIGUIENTE ####

# 4) Se trabaja con los cuatro tipos de eventos que filtramos anteriormente, ya que se nos da la indicación de que "Dado lo anteior..." se genere los siguientes puntos, por lo que trabajaremos con los cuatro tipos de eventos mencionados.

#Crear una base de datos con el total de eventos de violencia según su tipo a nivel departamento (DEPNAO). La base de datos debe contar con columnas según el tipo de evento. Apoyese de las variables dummy por cada tipo de evento que está en la base de datos.

owo= data2 %>% dplyr::group_by(DEPNA0) %>% 
  summarise(desaparición = sum(LDS_LDT),
            secuestros = sum(LSE), reclutamiento_fozad0=sum(LRC), muertes_en_atentados=sum(MAT))

#Le asignamos los nombres de los departamentos
owo$DEPNA0  <- factor(owo$DEPNA0, levels = c(1, 2,3,4,5,6,7,8,9,10,90),
                      labels = c("Ayacucho","Apurímac","Huancavelica","Cusco", "Huánuco", "Ucayali", "San Martín","Puno", "Junín", "Lima/Callao", "Otros"))
head(owo)

# 5) Crear una base de datos con el total de eventos de violencia según su tipo a nivel distrito (UBIDIST). La base de datos debe tener tantas columnas por tipo de evento. Adicionlamente, crear variables con el logaritmo del total de eventos. Apoyese de las variables dummy por cada tipo de evento que está en la base de datos.

uwu <- data2 %>%
  group_by(UBIDIST) %>%
  summarise(desaparición = sum(LDS_LDT),
            secuestros = sum(LSE),
            reclutamiento_fozado = sum(LRC),
            muertes_en_atentados = sum(MAT),
            Total_Eventos = sum(desaparición, secuestros, reclutamiento_fozado, muertes_en_atentados)) %>%
  mutate(log_Total_Eventos = log(Total_Eventos))

uwu

# 6) Crear una base de datos con el total de eventos de violencia según su tipo a nivel departamento (DEPNAO) y periodo (PERIODO). La base de datos debe contar con columnas según el tipo de evento. Apoyese de las variables dummy por cada tipo de evento que está en la base de datos.

data5 <- data2 %>% dplyr::group_by(DEPNA0, PERIODO) %>% 
  summarise(desaparición = sum(LDS_LDT), secuestros = sum(LSE), reclutamiento_fozado=sum(LRC), muertes_en_atentados=sum(MAT))


#Le agregamos las etiquetas a DEPNA0 y PERIODO

data5$DEPNA0  <- factor(data5$DEPNA0, levels = c(1, 2,3,4,5,6,7,8,9,10,90), labels = c("Ayacucho","Apurímac","Huancavelica","Cusco", "Huánuco", "Ucayali", "San Martín","Puno", "Junín", "Lima/Callao", "Otros"))

data5$PERIODO  <- factor(data5$PERIODO, levels = c(1,2,3,4,5), labels = c("80 - 82","83 - 85","86 - 88", "89 - 92", "93 - 00"))

data5


##preguntaS ENAHO----
###parte 1----

library(pacman) 
p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled, haven) 

#importamos las bases de datos
enahoDEMO <- haven::read_dta("../../data/enaho/enaho01-2019-200.dta")

enahoDEMO %>% filter((p204==1 & p205==2) | (p204==2 & p206==1))


enahoEDU <- haven::read_dta("../../data/enaho/enaho01a-2019-300.dta")

#creamos la variable de años de educacion
enahoEDU <- enahoEDU %>% mutate(
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

#creamos la variable con el MAX años de educacion alcanzados de algun miembre del hogar
enahoEDU <- enahoEDU  %>%  group_by(vivienda) %>% 
  mutate(
    max_educ = max(years_educ, na.rm = T)) %>% as.data.frame()



###parte 2----

#importamos y juntamos las bases de datos
datos1 <- read_dta("../../data/enaho/enaho01-2019-100.dta")
datos2 <- sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")

enaho19 <- datos1 %>% 
  inner_join(datos2, by = c("conglome","vivienda","hogar"))
names(enaho19)

enaho19 <- enaho19  %>%  mutate (nbi = nbi1+nbi2+nbi3+nbi4+nbi5, na.rm = T)
enaho19 <- enaho19 %>% mutate(
  nbi = case_when(
    nbi == 0 ~ 0,
    between(nbi,1,3) ~ 1))
table(enaho19$dpto, enaho19$nbi)
val_labels(enaho19$nbi) <- c("no tiene NBI" = 0,
                             "tiene al menos una NBI" = 1)
attr(enaho19$nbi, 'labels')

#definimos la variable dpto

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
  theme_minimal() +  #diseño 
  xlab("NBI %") +
  ylab("Departamento")

###parte 3-----
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

append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15 #reemplazo del callao

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
  theme_bw() + # diseño de fondo
  scale_x_continuous(breaks = c(2018, 2019 , 2020) ) +
  ggtitle("Gasto medio mensual per cápita por area")+
  xlab("año")+
  ylab("gasto per capita")

## pregunta ENDES----
#importamos

# Base a nivel hogar
rech0 <- read_dta("../../data/endes/RECH0.dta") %>% 
  mutate(
    HHID = str_trim(HHID)
  )
# Más información a nivel hogar
rech23 <- read_dta("../../data/endes/RECH23.dta") %>% 
  mutate( HHID = str_trim(HHID) )

names(rech23) <- tolower(names(rech23))
# Información a nivel individuo
rech1 <- read_dta("../../data/endes/RECH1.dta") %>% 
  mutate(
    HHID = str_trim(HHID)
  )

names(rech1) <- tolower(names(rech1))
# Salud mental
dv <- read_dta("../../data/endes/REC84DV.dta") %>% 
  mutate(
    caseid = str_trim(caseid)
  )

endes_health <- rech1 %>% 
  left_join(rech0, by = c("hhid" = "HHID")) %>% 
  left_join(rech23, by = "hhid") %>% 
  left_join(dv, by = c("hhid"="caseid"))


#agravios físicos
endes_health <- endes_health %>% mutate(
  empujon = case_when(
    between(d105a,1,2) ~ 1,
    d105a == 1 ~ 0,
    d105a == 3 ~ 0,
  ), na.rm = T)
endes_health <- endes_health %>% mutate(
  abofeton = case_when(
    between(d105b,1,2) ~ 1,
    d105b == 1 ~ 0,
    d105b == 3 ~ 0,
  ), na.rm = T)
endes_health <- endes_health %>% mutate(
  golpe = case_when(
    between(d105c,1,2) ~ 1,
    d105c == 1 ~ 0,
    d105c == 3 ~ 0,
  ), na.rm = T)
endes_health <- endes_health %>% mutate(
  patear = case_when(
    between(d105d,1,2) ~ 1,
    d105d == 1 ~ 0,
    d105d == 3 ~ 0,
  ), na.rm = T)
endes_health <- endes_health %>% mutate(
  extrangular = case_when(
    between(d105e,1,2) ~ 1,
    d105e == 1 ~ 0,
    d105e == 3 ~ 0,
  ), na.rm = T)

#crear la variable  physical 
endes_health <- endes_health  %>%  mutate (physical = empujon+abofeton+golpe+patear+extrangular, na.rm = T)
endes_health <- endes_health %>% mutate(
  physical = case_when(
    physical == 0 ~ 0,
    between(physical,1,5) ~ 1))

val_labels(endes_health$physical) <- c("no subrio me violencia fisica" = 0,
                                       "subrio me violencia fisica" = 1)
attr(endes_health$physical, 'labels')

### acalaración-----
#profesor escribio mal la tarea, no eran las varaibles d103a... sino las variables d105a....
endes_health <- endes_health %>% mutate(
  insulto = case_when(
    between(d103a,1,2) ~ 1,
    d103a == 1 ~ 0,
    d103a == 3 ~ 0,
  ), na.rm = T)
attr(endes_health$insulto, 'labels')

endes_health <- endes_health %>% mutate(
  amenaza = case_when(
    between(d103b,1,2) ~ 1,
    d103b == 1 ~ 0,
    d103b == 3 ~ 0,
  ), na.rm = T)

endes_health <- endes_health %>% mutate(
  insulto_siempre = case_when(
    between(d103c,1,2) ~ 1,
    d103c == 1 ~ 0,
    d103c == 3 ~ 0,
  ), na.rm = T)





