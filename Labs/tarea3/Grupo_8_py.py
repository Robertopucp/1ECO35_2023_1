#%% Trabajo Grupal 3

#   Integrantes Grupo 8:  
#    - Renzo Mosquera (20181960)
#    - Yenner Huancahuire (20173340)
#    - Pamela Obregón (20173040)

# Importamos librerias necesarias

import random
import numpy as np 
import pandas as pd
import math
from pandas import DataFrame, Series 
import re
import os

user = os.getlogin()   # Username

# Directorio

os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2023_1/Labs/tarea3")

#%% Aplicación de Map, Sapply, Apply, Lambda

# Creamos la función "escalamiento1" y "escalamiento2"

def escalamiento1(x) :
    
    R1 = (x-np.min(x))/(np.max(x)-np.min(x)) 
    
    return R1

def escalamiento2(x,min,max):
    
    R2 = (x-min)/(max-min)
    
    return R2

# Creamos el vector de 100 números aleatorios

vectoral = [np.random.randint(200,301) for i in range(0,100)]
len(vectoral)
    
# Importamos la base de datos

datosx = pd.read_csv(r"../../data/BDD_compras_consumidores.csv", sep = ";") # Mejoramos la separación de la base
datosx.info() # Vemos sus características
   
    
# Aplicamos la función al vector


Resultvector = list( map( lambda x, min = np.min(vectoral), max = np.max(vectoral): escalamiento2(x,min,max) , vectoral)  )  


# Aplicamos la función a la base de datos importada


Resultdatos = datosx.iloc[:,0:8].apply(lambda x: escalamiento1(x), axis = 0)


#%% Ejercicio de Apply

# Importamos la base de datos solicitada

notas = pd.read_csv(r"../../data/siagie.csv", sep = ",") # Mejoramos la separación de la base
notas.info() # Vemos sus características

# Identificamos que los cursos se encuentran desde la columna 6 a la 16

# Hallamos la NOTA PROMEDIO FINAL de cada alumno

NFP = notas.iloc[:,6:17].apply(lambda x: np.mean(x), axis = 1)
NFP

# Hallamos la nota MÁXIMA y MÍNIMA de cada alumno

NMAX = notas.iloc[:,6:17].apply(lambda x: np.max(x), axis = 1)
NMIN = notas.iloc[:,6:17].apply(lambda x: np.min(x), axis = 1)


# Hallamos el PROMEDIO de notas por curso

NPC = notas.iloc[:,6:17].apply(lambda x: np.mean(x), axis = 0)
print(NPC)

# Hallamos la MEDIANA de notas por curso

NMC = notas.iloc[:,6:17].apply(lambda x: np.median(x), axis = 0)
print(NMC)



#%% Funciones

#3.1.- Crear una función que permita calcular el factorial de un número (n).


def factorial(n):
    
    x=1
   
    if n<0:
        x=print("No valido")
        
    elif n==0:
        x=1
        
    elif n>0:
        for i in range(1,n+1):
            x=x*i
    return x

# Por ejemplo: 

# si n<0
factorial(-3)

# si n=0
factorial(0)

# si n>0
factorial(7)



#3.2-FUNCION DE MASA CORPORAL
#Creamos una funcion que indica el Indice de masa corporal (peso/talla^2)

def BMI(peso,talla):
    IMC= peso/(talla*talla)
    
    if 18.5<=IMC<25:
        clasificacion="Normal"
    elif 25<=IMC<30:
        clasificacion="Sobrepeso"
    elif 30<=IMC<35:
        clasificacion="Obesidad grado 1"
    elif 35<=IMC<=40:
        clasificacion="Obesidad grado 2"
    elif 40<IMC:
        clasificacion="Obesidad grado 3"
    IMC = "{:.2f}".format(IMC)
    resultado=(peso,talla,IMC,clasificacion)
    return resultado

#Pasamos a colocar los datos de los estudiantes propuestos

#Estudiante 1

print("Estudiante 1")
print(BMI(70, 1.5))

#Estudiante 2

print("Estudiante 2")
print(BMI(85, 1.8))

#Estudiante 3

print("Estudiante 3")
print(BMI(50, 1.6))

#3.3-FUNCION APLICADO A DOS ACTIVOS FINANCIEROS

portfolio = pd.read_csv("../../data/Portafolio.csv")
portfolio

# Extraemos cada columna de datos

x = portfolio['X']
y = portfolio['Y']

w1= 0.2 #Fijamos el peso del activo "X"
w2= 0.8 #Fijamos el peso del activo "Y"

# Formula de Coeficiente de correlacion de Pearson

def coeficiente_pearson(x,y):
    coeficiente=np.cov(x,y)[0][1]/(math.sqrt(np.var(x, ddof=1)*np.var(y, ddof=1)))
    return coeficiente

coeficiente_pearson(x,y)


#Determinamos la varianza del portafolio

def varianza(x,y,w1,w2):
    varianza_p=w1*w1*np.var(x, ddof=1)+w2*w2*np.var(y, ddof=1)+2*w1*w2*np.cov(x,y)[0][1]
    return varianza_p

varianza(x,y,w1,w2)



#3.4.- KEYWORDS ESPECIALES
## Definimos dos métodos que admita la función de escalamiento y transformación

def f_resc_inv(*args, **kwargs):
    if (kwargs['function']=="reescalamiento"):
        #Insertamos la función utilizada en la primera sección
        vector = np.array(list(args))
        result=reescalamiento_0 = list(map( lambda x: (x - np.min(vector))/(np.max(vector)-np.min(vector)), vector) )
    elif (kwargs['function']=="transformacion"):
        x = np.array(list(args))
        result=np.log(x+np.sqrt(np.square(x)+1))
    else:
        raise ValueError( f"The function argument {kwargs[ 'function' ]} is not supported." )
        
    return result

#1. Aplicamos la función para el Reescalamiento
A=f_resc_inv(2,5,7,9, function="reescalamiento")
print(A)

#2.Aplicamos la función para la Transformación
B=f_resc_inv(99,2,0,67,57,34,7,function="transformacion")
print(B)

# Otros casos: Error
C=f_resc_inv(1,9, function="suma")
print(C)



