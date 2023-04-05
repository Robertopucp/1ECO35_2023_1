#  laboratorio 2 ####
# Curso: Laboratorio de R y Python 
# @author: Roberto Mendoza 


# -------------------------------------------------------#

## Condicional ----

y <- runif(10,-10,10) # runif( n: cantidad de elementos, inicio , final)

if (mean(y) > 0) {
  dummy <- 1
} else {
  dummy <- 0
}
  
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

## While + If statement ----

w <- 10

while (w > 7  & w <= 15){
  coin <- round( runif(1) )
  print(coin)
  if (coin == 1) {
    w <- w + 2
  } else {
    w <- w - 10
  }
  
}



## For ----

ages<-  c(21, 23, 25, 24, 20)

for (age in ages) {
  
  print(age+10 )  
  
}


## For and Next ----

for (i in 1:50) {
  if(i %in% 15:20) { # Ignora los primeros 20 elementos
    next  
    print(i  + 1000)
  }
 cat("Ejecutanto",i,"\n")
}


## For and Next, break ----


for (j in 1:100){
  print(j)
  
  if(j > 20){
     break
  }
  
}


## While + break ----

w <- 10

while (TRUE){
  coin <- round( runif(1) )  # redondear al entero más cercano
  if (coin == 1) {
    break
  } else {
    w = w + 10
    print(point)
  }
  
}



# Function -----

# -------------------------------------------------------#

## First function ##

calculator <- function(x,y,z)
{
  result = x*y*z
  return(result)
}

calculator( 158, 38, 10 )

calculator( 158, 38)

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






