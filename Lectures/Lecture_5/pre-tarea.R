################  Workgroup 4 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Grupo6

# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

library(pacman) 


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

datos200_19 <- read_dta("../../data/enaho/enaho01-2019-200.dta")
datos300_19 <- read_dta("../../data/enaho/enaho01a-2019-300.dta")

enaho_2019 <- merge(datos200_19, datos300_19, by = "conglome")
print (enaho_2019)

datos_filtrados <- subset(datos, (p204==1 & p205==2) | (p204==2 & p206==1))
