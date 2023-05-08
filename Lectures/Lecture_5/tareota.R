library(pacman) 

# Change working directory

p_load(readxl, tidyverse, foreign,fastDummies, haven, survey,
       srvyr, labelled) 

getwd()



# cargar las bibliotecas
library(haven)
library(dplyr)

# leer los archivos de datos
enaho200_19 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
enaho300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")

# unir los datos de los módulos 200 y 300
enaho_19 <- inner_join(enaho200_19, enaho300_19, by = "conglome")
print(enaho_19)

# filtrar según residentes permanentes en el hogar
enaho_19 <- enaho_19 %>% filter((p204==1 & p205==2) | (p204==2 & p206==1))


#Crear los años de educación de cada miembro del hogar. 
#Luego crear una varaible con la máxima cantidad de años de educación 
#alcanzada por algún miembro del hogar. 
#(Use transform en python, y group_by - mutate en R)


enaho_19 <- enaho_19 %>% mutate (
  educ1 = case_when(
    between(p301a,1,4) ~ 0,
    between(p301a,5,6) ~ 6,
    between(p301a,7,10) ~ 11,
    p301a == 11 ~ 16
  ),
  
  
  clean_data <- bbdd %>% dplyr::group_by(educ1) %>%
    mutate(max = max(enaho_19)) 
  
  #se usa de forma descendente para poder ver cual es el mayor 
  
  
  #Use el módulo 100 del año 2019 ( información a nivel hogar) y unirlo con el módulo sumaria
  
  
  enaho100_19 <- read_dta("../../data/enaho/enaho01-2019-100.dta")
  sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
  
  enaho_sumaria_2019 <- merge(enaho100_19, sumaria_19, by = "conglome")
  print (enaho_sumaria_2019)
  
  #Crear un dummy variable que indique si el hogar presenta al menos una necesidad básica insatisfecha. 
  
  
  attr(enaho19$nb1, "labels")
  
  
  enaho_sumaria_2019$dummy_nb1 <- ifelse(enaho_sumaria_2019$nb1 %in% c(1,2), 1, 0)
  
  enaho_sumaria_2019$dummy_nb2 <- ifelse(enaho_sumaria_2019$nb2 %in% c(1,2), 1, 0)
  
  enaho_sumaria_2019$dummy_nb3 <- ifelse(enaho_sumaria_2019$nb3 %in% c(1,2), 1, 0)
  
  enaho_sumaria_2019$dummy_nb4 <- ifelse(enaho_sumaria_2019$nb4 %in% c(1,2), 1, 0)
  
  enaho_sumaria_2019$dummy_nb5 <- ifelse(enaho_sumaria_2019$nb5 %in% c(1,2), 1, 0)
  
  #sumar las variables 
  
  enaho_sumaria_2019$PesoTotal = rowSums (enaho_sumaria_2019[ , 5:10])
  
  estimacion = ifelse(PesoTotal > 0, 1, 0)
  
  #Crear un gráfico de barras, el cual muestre el porcentaje de hogares
  #con alguna necesidad básica insatisfecha (Usar la variable creada 
  #del elemento anterior) . 
  #Realizar el gráfico debe ser a nivel departamento. Use el factor de expansión a nivel hogar factor07 .
  plot(x = enaho_sumaria_2019$estimacion, main = "Gráfica",
       xlab = "necesidades basicas", ylab = "Frecuencia", by(dpto)
       col = c("royalblue", "seagreen", "purple", "grey"))
  
  ##Realice un Append de los módulos de resumen desde 2015 hasta 2020
  
  sumaria_15 <- read_dta("../../data/enaho/sumaria-2015.dta")
  sumaria_16 <- read_dta("../../data/enaho/sumaria-2016.dta")
  sumaria_17 <- read_dta("../../data/enaho/sumaria-2017.dta")
  sumaria_18 <- read_dta("../../data/enaho/sumaria-2018.dta")
  sumaria_19 <- read_dta("../../data/enaho/sumaria-2019.dta")
  sumaria_20 <- read_dta("../../data/enaho/sumaria-2020.dta")
  
  sumarias <- merge(sumaria_15, sumaria_16, sumaria_17, sumaria_18, sumaria_19, sumaria_20, by="conglome" ) 
  print (sumarias)
         