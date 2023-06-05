
## **Tarea 5** 

## Script en R 

#Plot-----

## 1. Replicar el siguiente gráfico----

# Primero, establecemos nuestro directorio para llamar a los archivos

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(readxl)

# Leemos el archivo y nos encontramos con una base sucia. Por ello, primero omitimos las filas que no necesitamos

cultivation <- read_excel("../../data/produccion_coca/6.1.1_-_Illicit_coca_bush_cultivation.xlsx", skip = 1)[-c(6:10), ]
cultivation <- cultivation[-c(1),]

# Renombramos la primera columna 
colnames(cultivation)[1] <- "Country"

# Renombramos las columnas de los años, ya que no tenemos los años desde 2009 hasta 2013
colnames(cultivation)[2:13] <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020")

# Cambiamos los nombres de los países para que coincidan con el ejemplo
nombres_paises <- c("Bolivia", "Colombia", "Peru", "Total")
cultivation$Country <- factor(cultivation$Country, levels = unique(cultivation$Country), labels = nombres_paises)

# Ya con la base limpia, recreamos el plot
grafico1 <- cultivation[1:3, 2:13]
colores <- c("red", "green", "gray") #los colores del ejemplo

plot(2009:2020, grafico1[1, ], type = "n", xlim = c(2009, 2020), ylim = c(0, max(grafico1)), xlab = "Años", ylab = "Producción de hoja de coca por hectáreas", main = "Figura 1: Producción de hoja de coca en hectáreas")

for (i in 1:3) {
  lines(2009:2020, grafico1[i, ], col = colores[i], lwd = 2)
  points(2009:2020, grafico1[i, ], col = colores[i], pch = 16)
}

legend("topright", legend = c("Bolivia", "Colombia", "Peru"), col = colores, lwd = 2, pch = 16, bty = "n")
mtext("Nota: Este gráfico muestra la producción de hoja de coca por años en la región andina usando UNODC data.", side = 1, line = 4)

## 2. Replicar el gráfico con los datos de erradicación de hectáreas de hoja de coca----

eradication <- read_excel("../../data/produccion_coca/6.1.2_-_Eradication_of_coca_bush.xlsx")

# Esta base también necesita limpieza y los pasos son similares al procedimiento anterior

eradication[1, "Reported eradication of coca bush, 2009-2020"] <- "Country"

colnames(eradication) <- as.character(eradication[1, ])
eradication <- eradication[-1, ]

eradication <- eradication[-c(4:16), -16]

n_paises <- c("Bolivia", "Colombia", "Peru")

eradication$Country <- factor(eradication$Country, levels = unique(eradication$Country), labels = n_paises)

# Ahora con la base limpia, creamos el gráfico con los codigos del ejemplo anterior

grafico2 <- eradication[1:3, 4:15]

colores2 <- c("red", "green", "gray")
plot(2009:2020, grafico2[1, ], type = "n", xlim = c(2009, 2020), ylim = c(0, max(grafico2)), xlab = "Años", ylab = "Erradicación de la producción de hoja de coca por hectárea", main = "Figura 2: Erradicación de la producción de hoja de coca por hectárea, 2009-2020")

for (i in 1:3) {
  lines(2009:2020, grafico2[i, ], col = colores2[i], lwd = 2)
  points(2009:2020, grafico2[i, ], col = colores2[i], pch = 16)
}

legend("topright", legend = c("Bolivia", "Colombia", "Peru"), col = colores, lwd = 2, pch = 16, bty = "n")
mtext("Nota: Este gráfico muestra la erradicación de la producción de hoja de coca por años en la región andina usando UNODC data.", side = 1, line = 4)

## 3. Realizar un gráfico con la producción y erradicación de hoja de coca en el Perú----

library(ggplot2)
library(tidyr)

# Filtramos los datos para Perú en la base de cultivation
cultivation_peru <- subset(cultivation, Country == "Peru")
cultivation_peru <- cultivation_peru[, 2:13]  # Seleccionar solo las columnas de interés

# ahora para eradication
eradication_peru <- subset(eradication, Country == "Peru")
eradication_peru <- eradication_peru[, 4:15]  # Seleccionar solo las columnas de interés

# Le tenemos que dar formato 
cultivation_peru_long <- pivot_longer(cultivation_peru, cols = everything(), names_to = "Año", values_to = "Producción")
eradication_peru_long <- pivot_longer(eradication_peru, cols = everything(), names_to = "Año", values_to = "Erradicación")

# Ahora sí creamos el plot
ggplot() +
  geom_line(data = cultivation_peru_long, aes(x = as.numeric(Año), y = Producción), color = "red", linetype = "solid", size = 1.5) +
  geom_point(data = cultivation_peru_long, aes(x = as.numeric(Año), y = Producción), color = "red", size = 3) +
  geom_line(data = eradication_peru_long, aes(x = as.numeric(Año), y = Erradicación), color = "green", linetype = "solid", size = 1.5) +
  geom_point(data = eradication_peru_long, aes(x = as.numeric(Año), y = Erradicación), color = "green", size = 3) +
  labs(x = "Años", y = "Producción / Erradicación de hoja de coca por hectáreas", title = "Producción y Erradicación de la hoja de coca en Perú") +
  scale_x_continuous(breaks = as.numeric(unique(cultivation_peru_long$Año)), labels = unique(cultivation_peru_long$Año)) +
  theme_minimal()

#Regex------

## 1. Convertir coordenadas en la base de datos metropolitano-----

#abrimos nuestra base de datos
library(readxl)
data2 <- read_excel("../../data/metropolitano.xlsx")

#Creamos una función para convertir las coordenadas (grados, minutos y segundos) a latitud y longitud
library(stringr)
convert_gps <- function(x) {
  signo <- ifelse(str_detect(x, '[swSWoO-]'), -1, 1)
  regex <- "\\s?(\\d+)°\\s*(\\d+)'\\s*(\\d+(?:\\.\\d+)?)"
  match <- str_match(x, regex)
  
  if (!is.na(match[1, 1])) {
    grados <- as.numeric(match[1, 2])
    minutos <- as.numeric(match[1, 3])
    segundos <- as.numeric(match[1, 4])
    
    return(signo * (grados + minutos / 60 + segundos / 3600))
  } else { return(NULL) }}

#Aplicamos la función a nuestra data2
data2$latitud <- sapply(data2$sur_latitud, convert_gps)
data2$longitud <- sapply(data2$oeste_longitud, convert_gps)

#vemos los resultados
data2






