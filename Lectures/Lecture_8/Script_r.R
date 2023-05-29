################  Laboratorio 7 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 

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

#library(tabulizer)

p_load(tidyverse, stringi)




#stringi : funciones regex 


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Read PDF 











