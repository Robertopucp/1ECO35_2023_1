# Solucionario WG2

#import os
import pandas as pd
import numpy as np
import math
import random

#%% Map, sapply, apply, lambda

#%%% 1. Vector aleatorio
vector = []

for _ in range(100):
    numero = random.randint(1, 1000)
    vector.append(numero)
print(vector)
# Creamos un vector de números aleatorios, con un loop que se genera 1 número aleatorio entre 1 y 1000.
# Con el comando append, agregamos cada resultado a la lista de vectores.

minimo = min(vector)
maximo = max(vector)

escalamiento = list(map(lambda x: (x-minimo) / (maximo-minimo), vector))
# Map se utiliza para aplicar la función lambda a cada elemento del vector de números aleatorios.

print(escalamiento)

#%%% 2. BDD_compras_consumidores
data_bdd = pd.read_csv("../../data/BDD_compras_consumidores.csv", sep = ";")

escalamiento2 = data_bdd.iloc[:,2:9].apply(lambda x: (x - np.min(x))/(np.max(x)-np.min(x)) , axis = 0)
print(escalamiento2)

#%% Apply

# Hallar la nota promedio final de cada alumno, la max y min. Promedio y mediana de cada curso.

siagie = pd.read_csv("../../data/siagie.csv")

#%%% 1. Nota promedio final de cada alumno

def promedio_final_alumno(row):
    return row[6:].mean() # average = promedio
promedio_final = siagie.apply(promedio_final_alumno, axis=1) # axis=1, para filas
print(promedio_final)
#%%% 2. Nota máxima y mínima

def nota_maxima(row):
    return row[6:].max() 
maxi = siagie.apply(nota_maxima, axis=1) 
print(maxi)

def nota_minima(row):
    return row[6:].min()
mini = siagie.apply(nota_minima, axis=1) 
print(mini)

#%%% 3. Promedio y mediana

def promedio(col):
    return col.mean() 
promedio = siagie.iloc[:, 6:].apply(promedio, axis=0) 
print(promedio)

def mediana(col):
    return col.median()
mediana = siagie.iloc[:, 6:].apply(mediana, axis=0) 
print(mediana)

#%% Funciones

#%%% 1. Factorial

def factorial(n):
    if n == 0:
        return 1  # El factorial de 0 es 1.
    elif n < 0:
        return None  # Restringimos a los números positivos.
    else:
        respuesta = 1
        for i in range(1, n + 1):
            respuesta *= i
        return respuesta

respuesta = factorial(13) # Elegimos un número al azar para comprobar nuestra función.

print(respuesta) # Comprobamos que es correcto.


#%%% 2. IMC

def f_imc(peso, talla):
    imc = peso / (talla**2)
    clasificacion = ""

    if 18.5 <= imc < 24.9:
        clasificacion = "Normal"
    elif 25 <= imc < 29.9:
        clasificacion = "Sobrepeso"
    elif 30 <= imc < 34.9:
        clasificacion = "Obesidad grado I"
    elif 35 <= imc < 39.9:
        clasificacion = "Obesidad grado II"
    elif imc >= 40:
        clasificacion = "Obesidad grado III"

    outcomes = {
        "peso": peso,
        "talla": talla,
        "imc": imc,
        "clasificacion": clasificacion }

    return(outcomes)

# Outcomes de los estudiantes:
    
estudiante1 = f_imc(peso=70, talla=1.5)
estudiante2 = f_imc(peso=85, talla=1.8)
estudiante3 = f_imc(peso=50, talla=1.6)

print(estudiante1)
print(estudiante2)
print(estudiante3)

#%%% 3. y 4. Activos financieros y Coef. de Pearson

portafolio = pd.read_csv("../../../data/Portafolio.csv", econding="UTF-8")
print(portafolio)

def rpta(data):
    X = data["X"]
    Y = data["Y"]

    wx = 0.2
    wy = 0.8

    coeficiente_pearson = np.cov(X,Y)[0][1]/(math.sqrt(np.var(X, ddof=1)*np.var(Y, ddof=1)))
    vari = (wx**2) * np.var(X, ddof=1) + (wy**2) * np.var(Y, ddof=1) + (2 * wx * wy * np.cov(X,Y)[0][1])
# La función numpy.cov(X,Y): [0][1] Es la covarianza entre X y Y.
return(coeficiente_pearson, vari)
                                                           
resultado = rpta(portafolio)
print(resultado)


#%%% 5. Keywords especiales
# args y kargs

def args_kwargs(*args, **kwargs):
    if (kwargs['function']=="escalamiento"):
        #Insertamos la función utilizada en la primera sección
        vector = np.array(list(args))
        result=escalamiento = list(map( lambda x: (x - np.min(vector))/(np.max(vector)-np.min(vector)), vector) )
    elif (kwargs['function']=="i_h_function"):
        x = np.array(list(args))
        result=np.log(x+np.sqrt(np.square(x)+1))

        
    return result
