#---Solucionario WG3---#

#  -> Comentarios
## -> Pregunta

# Clean environment variables
rm(list = ls())

# Clean plots
graphics.off()

# Clean console
cat("\014") #Ctrl + shift + c


# Libraries
#install.packages("pacman")
library(pacman) 

............................................................................#

## Map, sapply, apply, función Lambda ----

### 1. Vector aleatorio ----

vector <- c()

for (i in 1:100) {
  numero <- sample(1:1000, 1)
  vector <- c(vector, numero)
}

print(vector)

minimo <- min(vector)
maximo <- max(vector)

escalamiento <- lapply(vector, function(x) (x - minimo) / (maximo - minimo))

print(escalamiento)


### 2. BDD_compras_consumidores ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data_bdd <- read.csv("../../data/BDD_compras_consumidores.csv", sep = ";")

escalamiento2 <- apply(data_bdd[, 3:9], 2, function(x) (x - min(x))/(max(x) - min(x)))
print(escalamiento2)

............................................................................#

## Apply ----

siagie <- read.csv("../../data/siagie.csv")

### 1. Nota promedio final de cada alumno ---- 

promedio_final_alumno <- function(row) {
  return(mean(row[7:length(row)])) # average = promedio
}
promedio_final <- apply(siagie, 1, promedio_final_alumno) # axis=1, para filas
print(promedio_final)


### 2. Nota máxima y mínima  ---- 

nota_maxima <- function(row) {
  return(max(row[7:length(row)]))
}
maxi <- apply(siagie, 1, nota_maxima)
print(maxi)

nota_minima <- function(row) {
  return(min(row[7:length(row)]))
}
mini <- apply(siagie, 1, nota_minima)
print(mini)


### 3. Promedio y mediana ----

promedio <- apply(siagie[, 7:ncol(siagie)], 2, mean)
print(promedio)

mediana <- apply(siagie[, 7:ncol(siagie)], 2, median)
print(mediana)


