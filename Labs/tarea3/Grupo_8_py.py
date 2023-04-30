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

