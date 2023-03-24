#  laboratorio 1 ####
## Curso: Laboratorio de R y Python 
## @author: Roberto Mendoza 

" Laboratorio 1 Python"

## Tipo de variables ####


" Correr la lineas de codugo Ctrl + enter"
"Codigo a texto Ctrl + Shift + c"

a2 <- 3.1614
print(a2)
typeof(a2)
class(a2)


a2 <- as.integer(a2)
typeof(a2)
class(a2)

b1 <- 10000
typeof(b1)

## String ----

c1 <- "My first python code"
print(c1)
typeof(c1)
class(c1)

c1 <- "First python code"
c2 <- 'at R y python Class'
cat(c1," : ",c2)


a <- paste0(c1,' : semester 2022-1')


print(a)

d <- 2022
paste0(c1,' : semester ',d, '-1')


#first character

cat('Fisrt letter is :', substr(c1, 1,1)  )

#first word
cat('Fisrt word is :', substr(c1, 1,5)  )


"cat just contatenate characters to print"

## Booleans (true , false) ----

"2.0 Logical variables"

a == a

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


## Array ----

# 1D numeric array

" 3.1 c() Atomic vector: simple vector data"

a <- c(1,2,3,4,"Perú")
print(a)
class(a)
class(a[1])

c2 <- c("Red", "Green", "Purple")
print(c2)


a <- c(1,2,3,4)
a <- append(a, 5)

" 3.2 rep( number, times) "

b <-  rep(2,3) # repeat 2, 3 times 

