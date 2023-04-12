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
import pandas as pd  # cargamos pandas (modulo que nos permitirá manipular bases de datos)
from tqdm import tqdm
import os

y = np.random.randint(-10, 10, 50)   # valores entre -10 y 10. Extraer 50 numeros. 

if np.mean(y) > 0 :
    dummy = 1
else :
    dummy = 0
        
print(dummy)

# short ifelse

dummy = 1 if np.mean(y) > 0 else  0

print(dummy)

#%% Nested if statement

# v = 2
# v = np.nan
# v = "String"
v = False

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
    
#%% While

# while without ending, the condition is always met

i = 0

while i < 10:
    print(i+1)

# while requires a counter (i = i + 1)

i = 0

while i < 10:
    i = i + 1
    print(i)
    
### Si hoy tengo mis ahorros de S/.1,000.00. ¿Cuánto valdrán mis ahorros en 10 años a una tasa de interés de 5%?

#  save
S = 1000

# Periods
n = 5

# interes rate
i = 0.05


year = 1
while year < n:
    S =  S * (1+i)
    year += 1
    print( year, S)
    
    
#%% For loop


ages = np.arange(1,10000)

for age in ages:

    print(age+10 )  
    
#######################################

y = np.zeros(len(ages)) # vector fills with 0

for i in range(len(ages)):
    y[i] = np.log(ages[i])
    
#######################################


# Using tqdm 

for i in tqdm(range(len(ages))):
    y[i] = np.log(ages[i])
    print(i)
    
#######################################

y = np.log(ages)


# For + continue

for i in range(50):
    if i in range(15,21) :
        continue
        print(i)
    else :
        print("Ejecutanto",i,"\n")
        
        
# For + break

for i in range(50):
    if i in range(15,21) :
        break
    else :
        print("Ejecutanto",i,"\n")
        
        
        
# For + none

for i in range(50):
    if i in range(15,21) :
        None
        print(i)
        
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
        break   # sale del while
    else:
        w = w + 10  # se ejecuta si el coin es igual a cero 
        print(coin)
        
#%% Function

def calculator(x,y,z):
    result = x*y + z
    
    return result

print( calculator( 158, 38, 10 ) )

calculator( 158, 38)


# Different arguments location

print( calculator( z = 158, x = 38, y = 10 ) )



## return multiple

def calculator_square( x, y ):
    
    x2 = x * x
    y2 = y * y
    
    result = x2 * y2   
    
    return (result, x2, f"La multiplicación del cuadrado es: {result}")  # multiple resultado 

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


# Ejemplo en finanzas -------------------

os.getcwd()

user = os.getlogin()   # Username
print(user)

# Set directorio

os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2023_1/Lectures/Lecture_3") # Set directorio

portfolio = pd.read_csv(r'../../data/Portafolio.csv')

print(portfolio.shape)
portfolio.head()

def alpha(data):
    X = data['X']
    y = data['Y']
    
    cal_alpha = (np.var(y, ddof=1) - np.cov(X,y)[0][1]) / (np.var(X, ddof=1) + np.var(y, ddof=1) - 2*(np.cov(X,y)[0][1]))
    
    return cal_alpha

# "\" consecutive line codes 

def alpha(data):
    X = data['X']
    y = data['Y']
    
    cal_alpha = (np.var(y, ddof=1) - np.cov(X,y)[0][1])\
    / (np.var(X, ddof=1) + np.var(y, ddof=1) - 2*(np.cov(X,y)[0][1]))
    
    return cal_alpha


alpha(portfolio)

