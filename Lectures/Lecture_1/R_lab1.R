#  laboratorio 1 ####
# Curso: Laboratorio de R y Python 
# @author: Roberto Mendoza 

# clean environment variables
rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014") #Ctrl + shift + c


# Libraries #### 

#install.packages("dplyr")
#install.packages("stringr")

library(dplyr) 
library(stringr)

#-----------------------------------------------------------------#


#Laboratorio 1 Python

print("Hola Mundo")


## Tipo de variable ####


#Correr la lineas de codugo Ctrl + enter
# Codigo a texto Ctrl + Shift + c"

a1 <- 3.1416
print(a1)
typeof(a1)
class(a1)
is.numeric(a1)

# float to int 
# as.interger es n?mero entero

a2 <- as.integer(a1)
typeof(a2)
class(a2)

is.numeric(a2)

b1 <- 10000
typeof(b1)
b1 <- as.integer(10000)


# Round a number 

print(round(4.51))

# power

print(4^2)

#operator pip %>% (Control + shift + M)
?pip 
x <- sqrt(150) ; x   # ; print()
y <- log(x) ; y
z <- round(y, 2) ; z
w <- abs(z) ; w

150 %>% sqrt() %>% log() %>% round(2) %>% abs()

sqrt(150) %>% log() %>% round(2) %>% abs()

sqrt(150) %>% log %>% round(2) %>% abs %>% as.integer()



#----------------------------------------------------------------------------#

## String ----

c1 <- "My first python code"
print(c1)
typeof(c1)
class(c1)

c1 <- "First python code"
c2 <- 'at R y python Class'

c3 <- c1 + c2 # error 

# paste let us to join strings 


# paste0: este comando permite unir textos sin espacios

a <- paste0(c1,' : semester 2022-1') #paste0 une sin espacios 

print(a)

# paste: permite unir textos usando un símbolo en particular

a <- paste(c1,'semester 2022-1', sep = " : ") 
print(a)


d <- 2022
paste0(c1,' : semester ',d, '-1')
#acá se está uniendo los textos


# cat just contatenate characters to print

#first character

cat('Fisrt letter is :', substr(c1, 1,1)  )   # substr se utiliza para sustraer un texto
# Por ejm, aquí, substr(c1,1,1) indica que se substraiga 1 elemento de la palabra indicada 
# a partir del 1er elemento

#first word

cat('Fisrt word is :', substr(c1, 1,5)  )
# Acá se indica que se substraiga los 5 primeros elementos a partir de la posición 1

# \t: tab(), \n: enter

# Use cat instead print 

cat('Los datos de mi mascota son:\nNombre \t: Sugar\nRaza \t: Sharpei\nColor\t: Marron')


print('Los datos de mi mascota son:\nNombre \t: Sugar\nRaza \t: Sharpei\nColor\t: Marron')

# quotes 

text_1 = "It's a string with a single quote"
text_2 = 'It\'s a string with a single quote'


# Capitalize, lower

# String operations

string_1 = 'hello world'
string_2 = '10'

# Upper  (usando la librería stringr, se debe activar la librería para poder usarlo)

str_to_upper(string_1)  # para convertir a mayúscula

# Lower 

str_to_lower(string_1)

# Tittle              # para convertir a minúscula

str_to_title(string_1)

# count               

str_length(string_1)  # para contar los caracteres, el espacio se cuenta como caracter

# Splitting

str_split(string_1, " ")  # (word, pattern )

str_split(string_1, "w") 

#-------------------#

str0 <- 'I love-Python and not R'

#Separar Strings
str1 <- strsplit(str0, ' ') #str1 (es una lista) contiene separadamente de este texto
print(str1)
str2 <- strsplit(str0, split="[- ]")
print(str2)

str4 <- strsplit (str0, "and")
print(str4)
str4[[2]]="Allison"
print(str4)


# Join elements of a list into a linear regression formula  

var_list <- c('edad', 'gender', 'college', 'exp')

control_vars <- paste(var_list, collapse = "+")
print(control_vars)

# Clean a string 
string_4 <- ' (Hello World), '
str_trim(string_4) # by default clean empty at the ends

# replace 

gsub("[(|)|,]", "",string_4)

str_trim(gsub("[(|)|,]", "",string_4)) # replace and trim

# trim, upper and split

strsplit(str_trim(
  str_to_upper(
    gsub("[(|)|,]", "",string_4)
    )
    ), " ")

#----------------------------------------------------------------------------#

## Booleans (true , false) ----

# 2.0 Logical variables

'a' =='a'

1 == 1

z1 <- (1==1)
typeof(z1)

z1 <- as.integer(z1)
print(z1)
class(z1)

z2 <- (10 > 20)
as.integer(z2)

z3 <- (100 != 100)
z3 <- as.numeric(z3)
typeof(z3)


"TRUE <> T , FALSE <> F"

cat("Resultado para 2>1:", 2>1)
cat("Resultado para 2<1:", 2<1)
cat("Resultado para 2<=1:", 2<=1)
cat("Resultado para 2>=1:", 2>=1)
cat("Resultado para 2==2:", 2==2)
cat("Resultado para 2!=2:", 2!=2)
cat("Resultado para a==A:", 'a' == 'A')
cat("Resultado para a>b:", 'a' > 'b')


#----------------------------------------------------------#

x <- 5
y <- 10

class(x)

is.numeric(x)
x

z <- x**2
z
x == z  # Â¿Comparando o asignando? 1)
x =  z  # Â¿Comparando o asignando? 2)
x == z  # Â¿Comparando o asignando? 3)
x != z  # Â¿Comparando o asignando? 4)
x > y 

# Logic operators 

TRUE & FALSE  # and

T & F   # and

T & T   # and

T | F   # or

F | F   # or

#----------------------------------------------------------------------------#

## Array and Matrix ----

# 1D numeric array

" 3.1 c() Atomic vector: simple vector data"

a <- c(1,2,3,4,"Perú")
print(a)
class(a)
class(a[1])

c2 <- c("Red", "Green", "Purple")
print(c2)
# append

a <- c(1,2,3,4)
a <- append(a, 5)
print(a)

# Rep elements of a vector 

" 3.2 rep( number, times) "

b <-  rep(2,3) # repeat 2, 3 times 
print(b)

# append a and b

append(a,b)

# mean ans standard error

print(mean(a))
print(sd(a))

# sample random numbers

print(sample(1:100, size = 10))
vector<-sample(1:100,size=10)

vector <- rep(sample(1:100, size = 10), 3)

length( rep(sample(1:100, size = 10), 3) )



# selecting elements 

cat("Primer elemento:\n",vector[1])
cat("Primeros 5 elementos:\n",vector[1:5])
cat("index 2 to 20:\n",vector[2:20])
cat("index 2 to on:\n",vector[2:length(vector)])
cat("some indexing:\n",vector[c(2,5,17)])
data[,c("var1","var2")]

#--------------------------------------------------------#

# Sequences 

" 3.3 seq(from , to ,by ) "

y <- seq(from = 0, to = 19, by = 2)

y <- seq(0, 19, 2)

print(y)
typeof(y)

y[1]

# Second example 

y <- seq( 1, 10)
print(y)

# Alternatives

seq(100)

1:100 # easy way

# 50 elementos from interval [100,1000]

seqex <- seq(100,1000, length.out = 50)

length(seqex)

"3.4 Split vector"

indices <- split(seq(100), sort( seq(100) %% 3 ) )
indices

# add names to each vector

names(indices) <- c('training', 'est', 'test') ## add labels 
print(indices)

indices$est
indices$test
indices$training

attributes(indices) # atributos 

# attribute : informaciÃ³n de cualquier objeto en R

" 3.5 Array and Matrix: genera vectores multidimensionales R^n "

# array 1-dim vector 

a <- array(1:20)
print(a)

Matrix <- matrix(runif(100), nrow = 10)
print(Matrix)
typeof(Matrix)  # tipo de elementos
class(Matrix)   # tipo de estrucutura del objeto


# 2D array numeric

M <- matrix(seq(20), nrow = 2)
print(M)

print(M)
typeof(M)
dim(M)


# Create a 1D NumPy array with values from 0 to 20 (exclusively) incremented by 2.5:


" 3.6 Matrix "

A <- matrix(rnorm(100),10)
print(A)

A[2:4,] # rows selecrtion

A[,1:6]  # columns selecrtion

A[,-c(2,3)] # drop columns 

# Reshape
matrix(A, nrow = 50, ncol = 2)

# Join matrix and special Matrix

M1 <- matrix(0,8,2)

print(M1)

M2 <- matrix(1,8, 4) 
print(M2)

# horizontal stack 

M3 <- cbind(M1,M2)
M3

M4 = matrix(seq(12), nrow =2)
print(M4)

# vertical stack 

M5 <- rbind(M3,M4)

print(M5)

## transpose

t(M5)

# Matrix Identity

I <- diag(8)
print(I)


" 3. 8 List"

dis2 <- list('ATE', 'BARRANCO','BREÃñA', 'CALLAO', 'CARABAYLLO','CHACLACAYO','CHORRILLOS','CIENEGUILLA'
             ,'COMAS','EL_AGUSTINO','INDEPENDENCIA')
dis2[[1]] # get element


dis2[2:5]

dis2[-1] # drop first element

# unlist

unlist(dis2)

# add new element using append

num <- list(13,5,5,8,9,10,5,8,13,1,20)
append(num, 102)

# add a list 

num2 <- list(10,20,30)
append(num, num2)


cat("Suma:", sum(unlist(num)),'\n', "Minimo:", min(unlist(num)), '\n', "Maximo:", max(unlist(num)))

# list made of many class objects

list1 <- list(100:130, "R", list(TRUE, FALSE))


### Plot histogram ----

X <- rnorm(1000, mean=1, sd=0.5)

Xbar <- mean(X)

Sigma2 <- sum((Xbar-X)^2)/length(X)

Sigma <- sqrt(Sigma2)

hist(X, breaks = 100)

#-------------------------------------------------------------#

## OLS ----

set.seed(756)  # semilla aleatoria 

x1 <- runif(500)  # distribución unifrome 
x2 <- runif(500)
x3 <- runif(500)
x4 <- runif(500)
e <- rnorm(500)

# Poblacional regression (Data Generating Process GDP)

Y <- 1 + 0.8*x1 + 1.2*x2 + 0.5*x3 + 1.5*x4 + e


#M1 <- matrix(0,8,2)

X <- cbind(matrix(1,500), x1,x2,x3,x4)
head(X)

#inv(X) or solve (X)

beta <- solve( t(X) %*% X ) %*% (t(X) %*% Y)
print(beta)

# %*% multiplicar ,matrices 
# t(X) : transpuesta
# solve(X)  : inversa


#References

browseURL("https://r4ds.had.co.nz/")




