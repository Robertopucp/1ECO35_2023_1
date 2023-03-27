################  laboratorio 2 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 

" If statement"
# -------------------------------------------------------


y <- runif(10,-10,10) # runif( n: cantidad de elementos, inicio , final)

if (mean(y) > 0) {
  dummy <- 1
} else {
  dummy <- 0
}
  
print(dummy)  

"Nested If statement" 

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


" Loops "
# -------------------------------------------------------


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

#### While + If statement 

w <- 10

while (w > 7  & w <= 15){
  coin <- round( runif(1) )
  coin
  if (coin == 1) {
    w <- w + 2
  } else {
    w <- w - 10
  }
  
}
print(w)

#### For 

ages<-  c(21, 23, 25, 24, 20)

for (age in ages) {
  
  print(age+10 )  
  
}


###For and Next, break 

for (i in 1:50) {
  if(i %in% 15:20) { # Ignora los primeros 20 elementos
    next  
    print(i  + 1000)
  }
 cat("Ejecutanto",i,"\n")
}


### For and Next, break 


for (j in 1:100){
  print(j)
  
  if(j > 20){
     break
  }
  
}


### While + break

w <- 10

while (TRUE){
  coin <- round( runif(1) )  # redondear al entero más cercano
  if (coin == 1) {
    break
  } else {
    w <- w - 10
    print(w)
  }
  
}



" Function "
# -------------------------------------------------------

## First function ##

calculator <- function(x,y,z)
{
  result = x*y*z
  return(result)
}

calculator( 158, 38, 10 )

calculator( 158, 38)

## return multiple

calculator_square <- function(x,y){
  x2 <- x * x
  y2 <- y * y
  
  result <- x2 * y2  
  return(list(x2,x2,paste0("La multiplicación del cuadrado es:", result)) )
}

calculator_square(3, 4)[1]
calculator_square(3, 4)[[1]] # para ontener el elemento simple


## IF statement and return 
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

## Función y tipo de variables ##

calculator_base_5 <- function( x, y){
  if(! is.numeric(x)) stop("x must be a numero")
  if(! is.double(y)) stop("y must be a double")
  
  result = x*y
  
  return(as.double(result))
  
}

calculator_base_5( NA, 3.0)

calculator_base_5( 1, 3.0)

## Function y valore predeterminados de parámetros

transpose <- function(M, est = TRUE, z = NULL ){
  if(! is.matrix(M)) stop("x must be a matrix")
  
  M <- t(M) 
  
  if (est & is.null(z)){
    for (j in seq(dim(M)[2])) {
      v <- M[,j]
      M[,j] <- ( v - mean(v) )/sd(v)
    }
    
  } else if (! is.null(z) ) {
    M <- M*z
    
  }
  
  return(M)
  
}

A <- matrix(c(seq(0, 9), seq( 10, 19), seq( 30, 39), seq( -20, -11), seq( 2, 20,2)), nrow = 5, byrow =TRUE)

print(transpose(A))

print(transpose(A, est = FALSE, z = 2))

## Equivalent *args de Python en R 

caso1 <- function(...) return(sum(...))
 
caso1(2,4,5)



caso2 <- function(...) {
  
  return(prod(...))
  
  
}  

caso2(sample(1:50, size = 5))

### Try and error 


a = "2"


tryCatch(a/7,
         
         error = function(e)  {
           
          cat("Correción del tipo de variable:" , as.integer(a) / 7)
           
         }
)



"Loop Replacement"
# -------------------------------------------------------

#Check function
str(apply)


  
set.seed(756)   # semilla aleatoria

"Loop Replacement using list"
X <- runif(10)
print(X)

lapply(X, function(square) {square^2})


x <- seq(10)
lapply(X, rep, times = 5)  # recordar rep(numero , times = numero de repeticiones)

x1 <- runif(500)
x2 <- runif(500)
x3 <- runif(500)
x4 <- runif(500)
X <- cbind(matrix(1,500),x1,x2,x3,x4)

# Matrix a lista por cada columna

lapply(seq_len(ncol(X)),function(x) X[ , x])

"sapply es similar  a lappy pero no genera una lista sino valores numéricos"

X <- runif(10)
print(X)

lapply(X, function(square) {square^2})
sapply(X, function(square) {square^2}) # vector canonico simple 

x <- seq(10)
lapply(X, rep, times = 5) 
sapply(X, rep, times = 5)  # matrix output

"Apply aplicado al caso de array multidimensionales (DataFrame , Matrix) "
str(apply)
str(rnorm)

x <- matrix(rnorm(500), 100, 5)  # 100 filas , 5 columnas 

apply(x, 2, mean, simplify = F)  # MARGIN == 2 para columnas
apply(x, 2, mean, simplify = TRUE)

apply(x, 1, sd)  # MARGIN == 1 para filas

apply(x, 2, min)

apply(x, 1, max)

"estandarización usando apply"

apply(x, 2, function(i){
 ( i -  mean(i) ) / sd(i)
} )

"mapply - apply multivariado"

est <- function(mean, sd, x){
  (x - mean)/sd
}

str(mapply)

mapply(est, mean = 1:5, sd = seq(0.1,0.5,0.1), MoreArgs = list(x = x))


" Time "

# Start the clock!
ptm <- proc.time()

for (i in 1:10000000) {
  if(i %in% 15:20) { # Ignora los primeros 20 elementos
    next  
    print(i  + 1000)
  }
  #cat("Ejecutanto",i,"\n")
}

proc.time() - ptm

# Parallel procesing

n <- 1000000
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
x4 <- runif(n)
X <- cbind(matrix(1,n),x1,x2,x3,x4)

ptm <- proc.time()


apply(X, 2, function(i){
  ( i -  mean(i) ) / sd(i)
} )

proc.time() - ptm

"parallel es un paquete instalado por default"

install.packages("parallel") 
# library("parallel")


no_of_cores = detectCores()
print(no_of_cores)

"
parLapply(cl, x, FUN, ...)
parApply(cl = NULL, X, MARGIN, FUN, ...)
parSapply(cl = NULL, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) "

library("parallel")


#####################################################3


ptm <- proc.time()
lapply( X, function(i){
  ( i -  mean(i) ) / sd(i)
} )

proc.time() - ptm


### Parallel process


cl = makeCluster(no_of_cores )

ptm <- proc.time()
parLapply(cl, X, function(i){
  ( i -  mean(i) ) / sd(i)
} )

proc.time() - ptm

######################### Apply #########################

ptm <- proc.time()
apply(X, 2, function(i){
  ( i -  mean(i) ) / sd(i)
} )

proc.time() - ptm

#########################################################


cl = makeCluster(no_of_cores )

ptm <- proc.time()
apply(X, 2, function(i){
  ( i -  mean(i) ) / sd(i)
} )

proc.time() - ptm

stopCluster(cl)


" DataFrame"
# -------------------------------------------------------

df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3))
str(df) # similar describe en Stata

df <- data.frame(face = c("ace", "two", "six"),  
                 suit = c("clubs", "clubs", "clubs"), value = c(1, 2, 3), stringsAsFactors = T) # read FACTOR (string)
str(df)


"Function by OLS estimation, standar error, IV, P-value "

# -------------------------------------------------------

set.seed(756)  # permite que los numeros aleatorios no cambien al correr los códigos
x1 <- runif(500)
x2 <- runif(500)
x3 <- runif(500)
x4 <- runif(500)
e <- rnorm(500)

#Isntrumento 

z <- rnorm(500)

# Poblacional regression (Data Generating Process GDP)

Y <- 1 + 0.8*x1 + 1.2*x2 + 0.5*x3 + 1.5*x4 + e
X <- cbind(matrix(1,500), x1,x2,x3,x4)

ols <- function(M, Y , standar = T, Pvalue = T , instrumento = NULL, index = NULL){
  

  if (standar & Pvalue & is.null(instrumento) & is.null(index)){

    beta <- solve(t(M) %*% M) %*% (t(M) %*% Y)
    
    y_est <- M %*% beta  ## Y estimado 
    n <- dim(M)[1]  # filas
    k <- dim(M)[2] - 1  # varaibles sin contar el intercepto}
    df <- n- k ## grados de libertad
    sigma <- sum(sapply(Y - y_est , function(x) x ^ 2))/ df 
    
    Var <- sigma*solve(t(M) %*% M)
    sd <- sapply(diag(Var) , sqrt) ## raíz cuadrado a los datos de la diagonal principal de Var
    
    t.est <- abs(beta/sd)
    pvalue <-2*pt(t.est, df = df, lower.tail = FALSE) ## pt : t - student densidad
      
    table <- data.frame(OLS = beta,  
                     standar.error = sd, P.value = pvalue)
    
    
  }
  
    
  if ( !is.null(instrumento) & !is.null(index) ){
    
    beta <- solve(t(M) %*% M) %*% (t(M) %*% Y)
    
    index <- index + 1
    
    Z <- X
    Z[,index] <- z  ## reemplazamos la variable endógena por el instrumento en la matrix de covariables
    
    beta_x <- solve(t(Z) %*% Z) %*% (t(Z) %*% X[,index])
    
    x_est <- Z %*% beta_x 
    X[,index] <- x_est ## se reemplaza la variable x endógena por su estimado 
    
    beta_iv <- solve(t(X) %*% X) %*% (t(X) %*% Y)
    
    table <- data.frame(OLS= beta,  
                        OLS.IV = beta_iv)
    
  }

  return(table)
}


ols(X,Y)

ols(X,Y,instrumento = z, index = 1)


a = c(1,2)
typeof(a)
class(a)
is.vector(a)
