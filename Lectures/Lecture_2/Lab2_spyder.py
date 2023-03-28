# -*- coding: utf-8 -*-
"""
Created on Wed Aug 24 22:17:21 2022

@author: Roberto
"""

#%% Intro 

#### Itntro 2.0 

#%% If statement

"""
If statement

"""

import random
import numpy as np
import math

y = np.random.randint(-10, 10, 10)

if np.mean(y) >0 :
    
    dummy = 1   #tab arriba del botom mayuscula 
    
else :
    dummy = 0
        
print(dummy)

"""
Nested If statement

"""

v = 2  # Ctrl 1

# v = np.nan  # missing 
# v = "String"
# v = False


if isinstance( v, int ):
    print(v, " es numero entero (no missing)")
elif math.isnan(v):
    print(v, " es un missing")
elif isinstance( v, str ):
    print(v, " es un string") 
elif isinstance( v, bool ):
    print(v, " es un logical")     
else:
    print("Sin resultado")  
      
    
#%% While Loop

### If I have my savings today of S/.1,000.00. How much will my savings be worth
### in 10 years at an interest rate of 2.5%?

"     S_{y+1}	=S_{y}(1+i) "

#  sasve
S = 1000

# Periods
n = 10

# interes rate
i = 0.025


year = 1

while year < n:
    S =  S * (1+i)
    year += 1 # sumo un unidad
    print( year, S)
    
 



#%% Class

#### While + If statement 

w = 10

while (w > 7  & w <= 15) :
  coin = round( np.random.uniform(0,1) ) # numero aleatorio entre 0 y 1
  
  if coin == 1:
    w =  w + 2
  else :
    w = w - 10
   
print(w)

#### For Loop

ages = np.array([21, 23, 25, 24, 20])

for age in ages:

    print(age+10 )  
  
#### For and Next, break 

# Example 1
for i in range(50):
    if i in range(15,21) :
        None
    else :
        print("Ejecutanto",i,"\n")


# Example 2

for j in range(101):
    print(j)
    
    if j > 20:
        break
    
#### While + break

w = 10

while True :
    coin = round( np.random.uniform(0,1) ) # redondear al entero más cercano
    if coin == 1 :
        break 
    else:
        w = w + 10
        print(w)
    

#%%  Function 

def calculator(x,y,z):
    result = x*y*z
    
    return result

print( calculator( 158, 38, 10 ) )

calculator( 158, 38,15)


## return multiple

def calculator_square( x, y ):
    
    x2 = x * x
    y2 = y * y
    
    result = x2 * y2   
    
    return result, x2, f"La multiplicación del cuadrado es: {result}"

calculator_square(3, 4) 
                  
print( calculator_square(3, 4)[1] )
calculator_square(3, 4)[2]
                

#### IF statement and return 
def calculator_square_2( x, y ):
    
    x2 = x * x
    y2 = y * y
    
    result = x2 * y2   
    
    if result <= 200:
       return f"Large number. Get only the result variable {result}"
    else:
       return  print( "Too large number. Do not return variables!")



calculator_square_2(300, 4)

## Función y tipo de variables
def calculator_base_5( x , y = 5 ):
    
    result = x * y
    
    return result


calculator_base_5( 7 )



def calculator_base_5( x : int, y : float ) -> float:
    
    if not isinstance( x , int ):
        raise TypeError( "X variable is not int type.")
        
    if not isinstance( y, float ):
        raise TypeError( "Y variable is not float type.")

    result = x * y
    
    
    return result

calculator_base_5( 8.358, 3 )

calculator_base_5( np.nan, 3.0)

#### Function y valore predeterminados de parámetros

def transpose(M, est = True, z = None):
    
    if not isinstance( M , np.ndarray ) :      
        raise TypeError( "x must be a n-array")
  
    
    elif (est and (z is None) ) :
        
        M = M.T
        Z = np.zeros((M.shape[0], M.shape[1]))
        for i in range(M.shape[1]):
    
            a = np.mean(M[:,i])
            b = np.std(M[:,i])
            Z[:,i] = (M[:,i]-a)/b
            
        return Z
    
    elif not z is None:
        
        M = M*z 
        return M
  

      
A = np.array([np.arange(0,10), np.arange(10,20), np.arange(30,40), np.arange(-20,-10), np.arange(2,21,2)])

print(transpose(A))

print(transpose(A, est = False, z = 2))

#### Try and Exception

a = "2"

try:     
    
    print(a/7)   # No corre el código si detecta un error 
    
except TypeError:
  print("El argumento  deberia ser un número")
    
# caso 2

try:
    
    print(a/7)
    
except Exception:
    a = np.nan
    print(a)

# caso 3
try:
    
    print(a/7)

except Exception:
    a = np.nan
    print(a)

finally:
    
    print("Siempre se ejecuta")

#%% *args 

"""
The special syntax *args in function definitions in python is used to pass a variable number 
of arguments to a function. The object *args is a tuple that contains all the arguments.
 When you build your code, you should consider *args as a tuple.
"""

def calculator( *args ):
    
    print( f"args is a {type( args )}" )
    # Get the first value
    result = args[ 0 ]
    
    # Keep the rest of values
    args1 = args[ 1: ]
    
    # multiply all elements
    for element in args1:
        result = result * element
    
    return result

calculator( 8, 9, 50, 40, 10, 1)

def calculator( *list_vars ):
    
    print( f"args is a { type( list_vars ) }" )
    # Get the first value
    result = list_vars[ 0 ]
    
    # Keep the rest of values
    list_vars1 = list_vars[ 1: ]
    
    # multiply all elements
    for element in list_vars1:
        result = result * element
    
    return result


#### *Kwargs

def calculator( *list_vars, **kwargs):
    
    print( type( list_vars ) )
    print( type( kwargs ) )
    
    if ( kwargs[ 'function' ] == "media" ) :
        
        # Get the first value
        result = np.mean( list_vars )
    
    elif ( kwargs[ 'function' ] == "adicion" ) :

        result = sum(list_vars)
    else:
        raise ValueError( f"The function argument {kwargs[ 'function' ]} is not supported." )

    return result


calculator( 4, 5, 6, 7, 8, function = "adicion" )

calculator( 4, 5, 6, 7, 8, function = "media" )

calculator( 4, 5, 6, 7, 8, function = "inversa" )

#### Class

class class_name:
    
    def __init__(self, parameter1, parameter2):
        None
        
## Atributos

import numpy as np 

A = np.arange( 8, 25 )

print(A.size)
A.shape
A.mean()
        
dir(A)    # list de atributos 

"""
Method 
A function which is defined inside a class body. 
If called as an attribute of an instance of that class,
 the method will get the instance object as its first argument (which is usually called self). 
 See function and nested scope.
"""

from sklearn import linear_model
print(dir(linear_model))


#### __init__

class MyFirstClass:
    
    def __init__( self, name, age ):
        self.name = name
        self.age = age
    
    # best way to define a method
    def print_name_1( self ):
        print( f'I am { self.name }.' )
    
    # wrong way to define a method 
    def print_name_2():
        print( f'This is my { name }.' )
    
    
    # the worst way to call a parameter
    # we need to define them as attributes
    def print_name_3( self ):
        print( f'This is my { name }.' )
        
        
class MyFirstClass:
    
    def __init__( self, name, age, school ):
        self.name = name
        self.age = age
        self.school = school
    
    # how to define a method
    def print_name_1( self ):
        print( f'I am { self.name }.' )
    
    # other method
    def person_age( self ):
        print( f' I am { self.name } , I am { self.age } old. ' )
    
    # method
    def person_school( self ):
        print( f' I am {self.name} , I study at {self.school}. ' )
      
    # wrong way to define a method 
    def print_name_2():
        print( f'This is my { name }.' )
    
    # the worst way to call a parameter
    # we need to define them as attributes
    def print_name_3( self ):
        print( f'This is my { name }.' )        


student = MyFirstClass( name = "Jose" , age = 22, school = "Saco Oliveros" )

print(student.age)
print(student.school)
student.age
student.person_age()
student.print_name_1()

#%% Loop Replacement using list

vector = list(np.arange(100))

vector = np.arange(100)

list( map( lambda x: x**2 , vector)   )         
     
# time 

from tqdm import tqdm

for i in tqdm( range(100000) ):
        print(i)
       


  # apply 
  
import pandas as pd 
  
# list of name, degree, score
var1 = np.random.rand(50000)
var2 = np.arange(0,50000)
var3 =  np.random.rand(50000)
  
# dictionary of lists 
dict = {'v1': var1, 'v2': var2, 'v3': var3} 
    
df = pd.DataFrame(dict)

df.apply(np.sum, axis = 0)  # columna por columna 
df.apply(np.sum, axis = 1)  # fila por fila

df['nueva_var'] = df['v2'].apply(lambda x : x**99)


# !pip install swifter

import swifter

df['nueva_var'] = df['v2'].swifter.apply(lambda x : x**99) # parallel procesing 


#%% OLS

from scipy.stats import t # t - student 
import pandas as pd 

np.random.seed(175)

x1 = np.random.rand(500) # uniform distribution  [0,1]
x2 = np.random.rand(500) # uniform distribution [0,1]
x3 = np.random.rand(500) # uniform distribution [0,1]
x4 = np.random.rand(500) # uniform distribution [0,1]
e = np.random.normal(0,1,500) # normal distribution mean = 0 and sd = 1
z = np.random.rand(500)
# Poblacional regression (Data Generating Process GDP)


Y = 1 + 0.8*x1 + 1.2*x2 + 0.5*x3 + 1.5*x4 + e

X = np.column_stack((np.ones(500),x1,x2,x3,x4))

def ols(M,Y, standar = True, Pvalue = True , instrumento = None, index = None):

    if standar and Pvalue and (instrumento is None)  and (index is None) :
        
         beta = np.linalg.inv(X.T @ X) @ ((X.T) @ Y ) 
        
         y_est =  X @ beta 
         n = X.shape[0]
         k = X.shape[1] - 1 
         nk = n - k    
         sigma =  sum(list( map( lambda x: x**2 , Y - y_est)   )) / nk 
         Var = sigma*np.linalg.inv(X.T @ X)
         sd = np.sqrt( np.diag(Var) )
         t_est = np.absolute(beta/sd)
         pvalue = (1 - t.cdf(t_est, df=nk) ) * 2
         df =   pd.DataFrame( {"OLS": beta , "sd" : sd, "Pvalue":pvalue})   
         
    
    elif (not instrumento is None) and (not index is None) :
        
        beta = np.linalg.inv(X.T @ X) @ ((X.T) @ Y )
        
        index = index  - 1 
        Z = X
        Z[:,index] = z
        beta_x = np.linalg.inv(Z.T @ Z) @ ((Z.T) @ X[:,index] ) 
        x_est  = Z @ beta_x
        X[:,index] = x_est
        beta_iv = np.linalg.inv(X.T @ X) @ ((X.T) @ Y )
        df = pd.DataFrame( {"OLS": beta , "OLS_IV" : beta_iv})  

    return df



ols(X,Y)

ols(X,Y,instrumento = z, index = 2)






