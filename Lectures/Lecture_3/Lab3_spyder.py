# -*- coding: utf-8 -*-
"""
Loop replacemnet

@author: Roberto
"""

import numpy as np
import pandas as pd
from pandas import DataFrame, Series
import re # Regex



#%% Funciones lambda

# Función tradicional #

def interes(C, T, R):
  i = C*T*R
  return i

interes(5000, 24, 0.02)

# forma parsimoniosa de generar funciones

interes2 = lambda C, T, R : C*T*R

interes2(5000, 24, 0.02)

# Más ejemplos 

# map, es una función que a cada elemento de un conjunto de datos aplica la función que le indiquemos

genero = ['F', 'M', 'M', 'F', 'M']
list( map( lambda x: 0 if x == 'M' else 1, genero )  )


NSE = ['A', 'A', 'C', 'B', 'C']

list( map( lambda x: 3 if x == 'A' else (2 if x == 'B' else 1), NSE )  )

# Más ejemplos #

vector = np.arange(100)

map( lambda x: np.sqrt(x) - np.mean(vector), vector ) 

list( map( lambda x: np.sqrt(x) - np.mean(vector), vector)   )    

np.sqrt(vector) - np.mean(vector)


'''
Ejemplos
'''


'''
Función 1
'''

def cube(x):
    
    out = x*(1/3) - 0.5*x
    
    return out 

list( map( lambda x: cube(x) , vector)   )  


'''
Función 2, de estandarización
'''

def sdv(x,mean,sd):
    
    out = (x-mean)/sd
    
    return out 


list( map( lambda x, v1 = np.mean(vector), v2 = np.std(vector): sdv(x,v1, v2) , vector)  )  

# función explicita en la función lambda

list( map( lambda x: (x - np.mean(vector))/np.std(vector), vector)  )  

'''
Función 3, If statement

Valores menos a 50 asigne el numero 1 y asignar missing values para valores mayor i igual a 50
'''

vector = np.arange(100)

def function2(x):
    
    if x < 50:
         out = 1
    else:
         out = np.nan # Missing values

    return out 

list( map( lambda x: function2(x) , vector)   )


'''
Función 4, extrae lo numeros de un texto

La función extrae los numeros de texto

'''

texto_vector = np.array(["Municipio San Luis: 12450","Municipio La victoria: 1450",
                         "Municipio La Molina: 3550","Municipio Ate: 506"])


list( map( lambda x: re.sub('\D',"", x) , texto_vector)   )

# \D: significa todo menos a los dígitos


# re.sub( patron de texto, sustitución, texto)

''' Loop replacement in Matrix '''

np.random.seed(15632)
x1 = np.random.normal(0,1,500) # normal distribution
x2 = np.random.normal(0,1,500) # normal distribution 
x3 = np.random.normal(0,1,500) # normal distribution
x4 = np.random.normal(0,1,500) # normal distribution


X = np.column_stack((np.ones(500),x1,x2,x3,x4))

print(X.shape)

'''

En el caso de aplicar funciones como mean, std, y entre otros se puede aplicar pro filas o columnas

axis = 0 se aplica la función a cada columa
axis = 1 se aplica la función por filas

Numpy apply for matrix

numpy.apply_along_axis(func1d, axis, arr, *args, **kwargs)
 
'''

# mead y desviación estandar por columnas


np.mean(X, axis=0)   # axis = 0 (se aplica por columnas)
np.std(X, axis=0)

# mead y desviación estandar por filas

np.mean(X, axis=1) 
len( np.mean(X, axis=1) )
np.std(X, axis=1)


'''

Dos formas de estandarizar una matriz 

'''

 
XNormed = (X - np.mean(X, axis=0))/np.std(X, axis=0)

            
def standarize(x):
       out = (x - np.mean(x))/np.std(x)
          
       return out
   
X_std_2 = np.apply_along_axis(standarize, 0, X)
    
# axis = 0, se aplicará la función a los elementos de cada columna


#%% Handle dataset 

'''
 We use US census data from the year 2012 to analyse the effect of gender 
 and interaction effects of other variables with gender on wage jointly.
 The dependent variable is the logarithm of the wage, the target variable is *female*
 (in combination with other variables). All other variables denote some other 
 socio-economic characteristics, e.g. marital status, education, and experience. 
 For a detailed description of the variables we refer to the help page.
 '''
 
import pyreadr  # Load R dataset
import os # for usernanme y set direcotrio

user = os.getlogin()   # Username

# Set directorio

os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2022_2/Lab4") # Set directorio

cps2012_env = pyreadr.read_r("../data/cps2012.Rdata") # output formato diccionario


cps2012_env  # es un diccionario. En la llave "data" está la base de datos 
cps2012 = cps2012_env[ 'data' ] # extrae información almacenada en la llave data del diccionario cps2012_env
dt = cps2012.describe()
 
# Borrar variables constantes 

variance_cols = cps2012.var().to_numpy() # to numpy
X = cps2012.iloc[ : ,  np.where( variance_cols != 0   )[0] ]

# np.where( variance_cols != 0   ) resulta la posición de lasa columnas con varianza != 0

np.where( variance_cols != 0   )[0] # array

# np.where() permite obtener la posición de columnas que cumplen la condición

# Retirar la media de las variables 

def demean(x):
    dif = x - np.mean( x ) # tima la media de la columna 
    return dif 

X = X.apply( demean, axis = 0 )  # axis :0 se aplica la función por columna


#%% *args 

"""
The special syntax *args in function definitions in python is used to pass a variable number 
of arguments to a function. The object *args is a tuple that contains all the arguments.
 When you build your code, you should consider *args as a tuple.
"""

'''
*args : tipo tuple o array
'''

"Keyword: *args, incluir una cantidad variable de argumentos"


def calculator( *args ):
    
    print( f"args is a {type( args )}" )
    
    
    vector = np.array( list(args) )  # *args : tuple
    
    minimo = np.min(vector)
    
    maximo = np.max(vector)
    
    result = np.prod(vector)
    
    
    return result, minimo, maximo


calculator( 8, 9, 50, 10, 12 ,15,20,100,120)

'''
*args se puede usar otro nombre siempre que se use * al inicio
'''


def calculator( *list_vars ):
    
    print( f"args is a {type( list_vars )}" )
    
    
    vector = np.array( list_vars )  # *args : tuple
    
    minimo = np.min(vector)
    
    maximo = np.max(vector)
    
    result = np.prod(vector)
    
    
    return result, minimo, maximo


calculator( 8, 9, 50, 40, 10, 1)


#%%  *Kwargs


'''
**Kwargs is an acronym of keyword arguments. 
It works exactly like *Args but instead of accepting a variable number of positional arguments, 
it accepts a variable number of keyword or named arguments.
'''

'''
**kwargs: tipo diccionario 
'''

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
        
        # Mensaje de error por tipo de argumento

    return result


calculator( 4, 5, 6, 7, 8, function = "adicion" )

calculator( 4, 5, 6, function = "media" )

calculator( 4, 5, 6, 7, 8, function = "inversa" )

calculator( np.arange(10), function = "media" )

'''
Example using dataset cps2012
'''



def transform(Data, *select, **function) -> pd.DataFrame: #output DataFrame 
    
    select = list(select)  # se transforma a una lista
    Data_select = Data[select] # se filtra por columnas 
    
    if function['method'] == "demean":
        
        X = Data_select.apply(lambda row: row - np.mean(row), axis =0)
        
    elif function['method'] == "estandarize":
        
        X = Data_select.apply(lambda row: (row - np.mean(row))/np.std(row), axis =0)
        
    return X


transform(cps2012, "lnw", "exp1","exp2", method = "estandarize")




