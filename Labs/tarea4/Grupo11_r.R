# Integrantes ####
# Kevin Pareja (20196318)
# Elian Tongombol (20196453)
# Paola Aranda (20196052)
# Maria Alejandra Colan (20190515)

# Consideraciones previas: #####

## Borrando el environment ####
rm(list = ls())

## Borrando los graficos ####
graphics.off()

##Borrando la consola ####
cat("\014")

## Llamando a los directorios necesarios
library(pacman) 
p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled, ggthemes) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#ENAHO

###Guardamoslos módulos de la enaho que utilizaremos-------------------------
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


### Merge ENAHO 200 y 300 del 2019 -------------------------------------------

enaho19 <- enaho200_19 %>% 
  left_join(enaho300_19, by = c("conglome","vivienda","hogar","codperso"))

### Filtramos la base de datos según residentes permanentes en el hogar
enaho19 <- enaho19 %>% filter((p204==1 & p205==2) | (p204==2 & p206==1))

###Años de educación de cada miembro del hogar 

attr(enaho19$p301a, 'labels')

###Nivel educativo alcanzado: 
### Cuando p301a se encuentre entre 1-4,  se le asigna a la variable educ1 el valor de 0.
### Se repite para las otras variables. 

enaho19<- enaho19 %>% mutate(
  educ1=case_when(
    between (p301a,1,4)~0,
    between (p301a,5,6)~6,
    between (p301a,7,10)~11,
    p301a == 11 ~16
  ))

# Sumamos el grado o año de estudios alcanzado. 
enaho19$educ2 <- apply(enaho19[,c("p301b","p301c")], 1, sum, na.rm=T) 

#Creamos la variable años de educación al sumar educ1 y educ2, sin usar los NA en el cálculo.
enaho19$years_educ <- apply(enaho19[,c("educ1","educ2")], 1, sum, na.rm=T)

#Máxima cantidad de años de educación alcanzado por algún miembro del hogar

enaho19 <- enaho19 %>% 
  group_by(conglome, vivienda, hogar) %>% 
  mutate(max_educ = max(years_educ, na.rm = TRUE)) %>% 
  ungroup()


# Merge ENAHO 100 y sumaria del 2019 --------------------------------------------

enaho19_sumaria <- enaho100_19 %>%
  left_join(sumaria_19, by = c("conglome","vivienda","hogar"))

#Dummy necesidad basica insatisfecha
### Tener en cuenta que las nbi ya son variables dicotómicas.
### Sumamos las variables
enaho19_sumaria$suma_nbi <- apply(enaho19_sumaria[,c("nbi1","nbi2", "nbi3", "nbi4", "nbi5")], 1, sum, na.rm=T)

### Creamos "dummy_nbi"
enaho19_sumaria$dummy_nbi <- ifelse(enaho19_sumaria$suma_nbi > 0, 1, 0)
var_label(enaho19_sumaria) <- list(dummy_nbi = "Dummy de necesidades basicas insatisfechas (nbi)")
val_labels(enaho19_sumaria$dummy_nbi) <- c("Hogar con nbi" = 0,
                                     "Hogar sin nbi" = 1)

attr(enaho19_sumaria$dummy_nbi, "labels")
#Grafico de barras ------------------------------------------------------------

### Creamos la variable departamento
enaho19_sumaria <- enaho19_sumaria %>% 
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
         ))

### Factor de expansión
enaho19_sumaria$factorpob <- round(enaho19_sumaria$factor07*enaho19_sumaria$mieperho, 1)

#Creamos el porcentaje de hogares con nbi ajustado al factor de expansión.

data_nbi <- enaho19_sumaria  %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob) %>% 
  dplyr::group_by(dpto) %>% 
  summarise(
    nbi_rate = survey_mean(dummy_nbi, na.rm = T)*100
  )


### 


ggplot(data_nbi, aes(x = dpto, y = nbi_rate, fill = nbi_rate ) ) +
  geom_bar(position = position_fill()) +
  theme_bw() +
  labs(title = "Necesidades Básicas Insatisfechas por departamento",
       x = "Departamento", 
       y = "Porcentaje (%)") + 
  scale_fill_manual(values = c("darkolivegreen3", "firebrick2")) 

# Append sumaria y merge con los deflactores anuales. -------------------------

# El año base es 2020

append_enaho <- bind_rows(sumaria_15, sumaria_16, sumaria_17, sumaria_18, sumaria_19, sumaria_20) %>% 
  mutate(  dpto = as.numeric( substr(ubigeo,1,2) ),
           año = as.numeric(año)
  ) 

# Asignamos el código de Lima region  al Callao


append_enaho$dpto[ append_enaho$dpto == 7 ] <- 15


append_enaho <- left_join(append_enaho,
                          base_deflactores, by = c("dpto", "año" = "aniorec"))


# Factor de expansión a nivel persona 

append_enaho$factorpob <-  round(append_enaho$factor07*append_enaho$mieperho, 1) 

# Uniendo con la base deflactores y creando la variable area (rural-urbano)

append_enaho <- append_enaho %>% 
  mutate(
    area =  factor(case_when(
      estrato <= 5 ~ 1,
      estrato > 5 ~ 2  ), levels = c(1,2), labels = c("Urbano", "Rural")),
    ingmpc = inghog1d/(mieperho*12*i00),
    gmpc = (mieperho*12*i00),
    factorpob = round(factor07*mieperho, 1)
  )

#Para el Ingreso mensual per cápita: 
#### Ajustamos al factor de expansión
ingmpc_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob,
                   nest = TRUE) %>%   
  group_by(año, area) %>% 
  summarise(
    ingmpc = survey_mean(ingmpc, na.rm = T)
  )

#### Gráfico evolución del ingreso percápita mensual del hogar
ingmpc_years %>% 
  ggplot(aes(x = año, y = ingmpc, group = area, colour = area)) +
  geom_line() +
  geom_point(size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  labs(title = "Ingreso per cápita mensual según área urbana o rural",
       x = "Año",
       y = "Ingreso per cápita mensual") +
  scale_color_manual(values = c("red", "blue")) 

#Para el gasto mensual per cápita: ----------------------------------------
#### Ajustamos al factor de expansión
gmpc_years <- append_enaho %>% 
  as_survey_design(ids = conglome, 
                   strata = estrato,
                   weight = factorpob,
                   nest = TRUE) %>%   
  group_by(año, area) %>% 
  summarise(
    gmpc = survey_mean(gmpc, na.rm = T)
  )

#### Gráfico evolución del gasto percápita mensual del hogar
gmpc_years %>% 
  ggplot(aes(x = año, y = gmpc, group = area, colour = area)) +
  geom_line() +
  geom_point(size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020)) +
  labs(title = "Gasto per cápita mensualsegún área urbana o rural",
       x = "Año",
       y = "Gasto per cápita mensual") +
  scale_color_manual(values = c("#AA336A", "#448EE4"))


#ENDES

#Unir los módulos RECH84DV, RECH0, RECH1 y RECH23. Crear una variable dummy de violencia física y sexual respectivamente.

# Cargamos los datos de los módulos RECH84DV, RECH0, RECH1 y RECH23
rech84dv <- read_dta("../../data/endes/REC84DV.dta")
rech0 <- read_dta("../../data/endes/RECH0.dta")
rech1 <- read_dta("../../data/endes/RECH1.dta")
rech23 <- read_dta("../../data/endes/RECH23.dta")

names(rech84dv)
names(rech0)

# Renombramos "caseid" a "HHID" en rech84dv
names(rech84dv)[names(rech84dv) == "caseid"] <- "HHID"

# Unimos los data frames
datos_endes <- merge(rech84dv, rech0, by = "HHID")
datos_endes <- merge(datos_endes, rech1, by = "HHID")
datos_endes <- merge(datos_endes, rech23, by = "HHID")

#Crear una variable dummy por tipo de violencia física: d105a d105b d105c d105d d105e. Cada variable toma los siguientes valores: 0 (respuesta No), 1 (Frecuentemente), 2 (Algunas veces) y 3 (Nunca).

#d103a : Su esposo/compañero alguna vez la empujó, sacudió o le tiró algo
#d103b : Su esposo/compañero alguna vez la abofeteó o le retorció el brazo
#d103c : Su esposo/compañero alguna vez la golpeó con el puño o con algo que pudo hace
#d103d: Su esposo/compañero alguna vez la ha pateado o arrastrado
#d103e: Su esposo/compañero alguna vez trató de estrangularla o quemarla

# Creamos variables dummy para cada tipo de violencia física
datos_endes$d105a_dummy <- ifelse(datos_endes$d105a %in% c(1, 2), 1, 0)
datos_endes$d105b_dummy <- ifelse(datos_endes$d105b %in% c(1, 2), 1, 0)
datos_endes$d105c_dummy <- ifelse(datos_endes$d105c %in% c(1, 2), 1, 0)
datos_endes$d105d_dummy <- ifelse(datos_endes$d105d %in% c(1, 2), 1, 0)
datos_endes$d105e_dummy <- ifelse(datos_endes$d105e %in% c(1, 2), 1, 0)

# Creamos una variable dummy para violencia física en general
datos_endes$physical <- ifelse(rowSums(datos_endes[, c("d105a_dummy", "d105b_dummy", "d105c_dummy", "d105d_dummy", "d105e_dummy")]) > 0, 1, 0)

#Crear una variable dummy por cada variable. La Dummy toma el valor de 1 si la variable de violencia toma los valores 1 y 2, mientras el valor 0, para los valores 0 y 2. Finalmente, crear la variable dummy llamada physical = 1 si alguna de las variables dummies toma el valor de 1, mientras toma el valor de cero si cada variable dummy toma el valor de cero.

# Creamos una variables dummy de violencia física
rech84dv <- rech84dv %>%
  mutate(
    d105a_dummy = ifelse(d105a %in% c(1,2), 1, 0),
    d105b_dummy = ifelse(d105b %in% c(1,2), 1, 0),
    d105c_dummy = ifelse(d105c %in% c(1,2), 1, 0),
    d105d_dummy = ifelse(d105d %in% c(1,2), 1, 0),
    d105e_dummy = ifelse(d105e %in% c(1,2), 1, 0)
  )

# Creammos una variable dummy physical
rech84dv <- rech84dv %>%
  mutate(
    physical = ifelse(d105a_dummy == 1 | d105b_dummy == 1 | d105c_dummy == 1 | d105d_dummy == 1 | d105e_dummy == 1, 1, 0)
  )

