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







