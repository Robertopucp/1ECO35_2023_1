#  laboratorio 2 ####
# Curso: Laboratorio de R y Python 
# @author: Roberto Mendoza 

#install.packages("dplyr")
# install.packages("readxl")
# install.packages("pacman")

# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

library(readxl)
library(dplyr) 
library(rstudioapi) # set working directory to file location 

# alternativa para subir librerias

library(pacman)  # Package Manager
p_load(dplyr, readxl, rstudioapi)

# Change directory where actual script is located

getwd()

## alternative 1

user <- Sys.getenv("USERNAME")  # username
print(user)
setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2023_1/Lectures/Lecture_2") ) # set directorio
getwd()

## alternative 2

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# -------------------------------------------------------#

## Condicional ----

y <- runif(10,-10,10) # runif( n: cantidad de elementos, inicio , final)

if (mean(y) > 0) {
  dummy <- 1
} else {
  dummy <- 0
}
  
print(dummy)  


# short ifelse

dummy <- ifelse(mean(y) > 0, 1, 0)

dummy <- if (mean(y) > 0) 1 else 0

print(dummy)

## Nested If statement -----

# v <- 2
# v <- NA
# v <- "String"

v <- TRUE


if  ( is.numeric(v) ){
  cat(v, " es un numero entero (no missing)")
} else if ( is.na(v) ) {
  
  cat(v, " es un missing")
  
} else if ( is.character(v) ){
  
  cat(v, " es un string") 
  
} else if ( is.logical(v) ){
  
  cat(v, " es un Boolean") 
  
} else {
  
  print("Sin resultado")
  
}


# Loops -----


# -------------------------------------------------------#


# while without ending, the condition is always met

i <- 0
while(i < 10) {
  print(i + 1)
}


# while requires a counter (i = i + 1)

i <- 0
while(i < 10) {
  i = i + 1
  print(i)
}




#  saving
S <- 1000

# Periods
n <- 10

# interes rate
i <- 0.025


year = 1
while (year < n){
  S <-  S*(1+i)
  year <-  year + 1
  cat( "periodo ", year, ": ", S,"\n")
}


## For ----

ages<-  1:10000

for (age in ages) {
  
  print(age+10 )  
  
}

# a vector 

y <- c()

for(i in 1:length(ages)) {
  y[i] <- log(ages[i])
}

# Measure loop time

start_time <- Sys.time()
for(i in 1:length(ages)) {
  y[i] <- log(ages[i])
  print(i)
}
print(Sys.time() - start_time)    # slow

start_time <- Sys.time()
y <- log(ages)
print(Sys.time() - start_time)    # fast



## For and Next ----

for (i in 1:50) {
  if(i %in% 15:20) { # Ignora los primeros 20 elementos
    next  
  }
 cat("Ejecutanto",i,"\n")
}


## For and Next, break ----


for (i in 1:50) {
  if(i %in% 15:20) { # Ignora los primeros 20 elementos
    break
  }
  cat("Ejecutanto",i,"\n")
}



## While + break ----

w <- 10

while (TRUE){
  coin <- round( runif(1) )  # redondear al entero más cercano
  if (coin == 1) {
    break
  } else {
    w = w + 10
    print(coin)
  }
  
}


# Function -----

# -------------------------------------------------------#

## First function ##

calculator <- function(x,y,z)
{
  result = x*y + z
  return(result)
}

calculator( 158, 38, 10 )

calculator( 158, 38)

# Different arguments location


calculator( z = 158, x = 38, y = 10 )



# Overall, function gives the last line code

calculator <- function(x,y,z)
{
  suma_all = x + y+ z
  
  x*y*z

}

calculator( 158, 38, 10 )

# alternative function 

calculator <- function(x,y,z) x*y*z

calculator( 110, 13, 2 )
  
## return multiple ----

calculator_square <- function(x,y){
  x2 <- x * x
  y2 <- y * y
  
  result <- x2 * y2  
  return(list(x2,y2,paste0("La multiplicación del cuadrado es:", result)) )
}


# list para multiples resultados en una función de R

calculator_square(3, 4)[1]
calculator_square(3, 4)[[1]] # para obtener el elemento simple


# Return named outputs in a list

calculator_square <- function(x,y){
  x2 <- x * x
  y2 <- y * y
  
  result <- x2 * y2  
  return(list(x_2 = x2,
              y_2 = y2,
              text = paste0("La multiplicación del cuadrado es:", result)) )
}

calculator_square(3, 4)


calculator_square(3, 4)$x_2

calculator_square(3, 4)$text

## IF statement and return ----

calculator_square_2 <- function(x,y){
  x2 <- x * x
  y2 <- y * y
  
  result <- x2 * y2 

if (200 >= result) {
  return( cat( "Large number. Get only the result variable: ", result) )
  } else {
  return( print( "Too large number. Do not return variables!") )
    
  }
  
}

  
calculator_square_2(300, 4)


## Alpha between two assets ----


portfolio <- read.csv("../../data/Portafolio.csv", encoding = "UTF-8")

alpha <- function(data){
  
  X <- data$X
  y <- data$Y
  
  alpha_cal <- (var(y)-cov(X,y))/(var(y) + var(X) -2*cov(X,y))
  
  return(alpha_cal)
  
}


alpha(portfolio)














