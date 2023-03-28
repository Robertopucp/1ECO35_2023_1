# -*- coding: utf-8 -*-
"""
# Laboratorio 1 Python

##  Types of variables

@author: Roberto Mendoza
"""


# Select code lines and press F9 to run 

# Contorl + 1 for making line code to comment and viceversa

# Control + enter to run a cell 

#%% Types of variables

########################################################

"""
 1.0 Types of variables 
"""

print("Hola Mundo")

a = "Hola Mundo"
print(a)

#####################################################


a1 = 3.141

type(a1)

# from float to int 

c = int(a1)
type(c)

b1 = 10000
type(b1)

# from int to float 

float(b1)

b2 = 8
type(b2)

# Round a number

print(round(4.51) )

# Power

print(pow(4,2)) # using function pow
print(4**2)  # using **


#%% String 

c1 = "My first python code"  # "My first python code" usando doble comilla
print(c1)  # show that varaible's content
type(c1) # providing varaibles's type 

# including a space using \n

c1 = "First python code"
c2 = "at R y python Class"
print(c1,'\n',c2)

# join string 

print(c1 + " : " + c2)

# f-string

print(f'{c1} : semester 2022-1')

d = 2022

print(f'{c1} : semester {d}-1')

# alternative f - string 

print('{} : semester {}-1'.format(c1,d))

# Substr elements from string

c1[0:5]

#first character
print('Fisrt letter is :',c1[0])

#first word
print('Fisrt word is :',c1[0:5])

# \t: tab(), \n: enter

print('Los datos de mi mascota sont:\nNombre \t: Sugar\nRaza \t: Sharpei\nColor\t: Marron')

# raw string: it treats backslashes as a literal (raw) character
# We use r'' to set a path, to call a datasets
print(r'Los datos de mi mascota son:\nNombre \t: Sugar\nRaza \t: Sharpei\nColor\t: Marron')

# quotes ?

text_1 = "It's a string with a single quote"
text_2 = 'It\'s a string with a single quote'
text_2

# String operations

string_1 = 'hello world'
string_2 = '10'

# Capital letter the first word

string_1.capitalize()

# All capital case

string_1.upper()

# Lower case

string_1.lower()

# Title

string_1.title()

# How many l ?

string_1.count('l')

# Longitud de un string
len(string_1)

# split an string
string_1.split()

string_1.split('w')

str0 = 'I love Python and not R'

#Separar Strings
str1 = str0.split(' ')
str2 = str0.split(' ', maxsplit=1)
str3 = str0.split(' ', maxsplit=2)
print(str1, str2, str3)

# Join elements of a list 

var_list = ['edad', 'gender', 'college', 'exp']
var_list = ' + '.join(var_list)
print(var_list)

# Clean a string 
string_4 = ' (Hello World), '
string_4.strip() # by default clean empty at the ends

string_4.strip('(), ') # clean (),

# Replace string
string_5 = 'Hello World'
string_5.replace('Hello', 'Hi')

string_4

# Encontrando posicion de una parte del string
string_4.index('o')

string_4[1:-3]

# invert an string
string_4[::-1]

# Example

string_4.strip('(), ').upper().split()

#%% Booleans 

page = True
print(type(page))

"a" == "a"

1 > 1

z1 = (1==1)
z1 

int(z1)  # true is 1 in numerical system

z2 = (10 > 20) 
int(z2) # false is 0 in numerical system

print(f"Resultado para 2>1: {2>1}")
print(f"Resultado para 2<1: {2<1}")
print(f"Resultado para 2<=1: {2<=1}")
print(f"Resultado para 2>=1: {2>=1}")
print(f"Resultado para 2==2: {2==2}")
print(f"Resultado para 2!=2: {2!=2}")
print(f"Resultado para 'a'=='A': {'a' == 'A'}")
print(f"Resultado para 'a'>'b': {'a' > 'b'}")

# Logic operators ###################

not True

not False

True and True

True and False

True or False

True + True

False + True

int(True)

int(False)

float(False)

float(True)

#%% Tuple

T1 = (1,4,8,10,20,15,4,5,3,8)

type(T1)

# aritmethic operations

print('Suma:', sum(T1),"\n", "Minimo:", min(T1), '\n', "Maximo:", max(T1))

len(T1) # lenght of tuple

## Indexing Tuple. Overall,  all list object in python 

T1 = (1,4,8,10,20,15,4,5,3,8)


T1[0]

T1[0:5] # desde la posición 0 hasta el 4 

T1[1] # get tuple's element 

T1[0:3]


# It is not possible to change
T1[0] = 4

T1 = (1,4,8,10,20,15,4,5,3,8)
#                   -3,-2,-1
T1[-3:-1]

#position
T1.index(8)

# Creando una tupla a partir de otra
t = (1, 2, 3)
t1 = (t, 'a', 'b') # tupla into a tupla 
t2  = (*t, 'a', 'b') # join tuplas

print(f'Tupla with t: {t1}')
print(f'Tupla with *t: {t2}')

#%% List (lista)

# alternative way to create an empty list 

L1 = []
L2 = list()
print(L1)
type(L2)

# Mix 2 lits
l1 = [1, 2]
l2 = [3, 4]
l3 = l1 + l2
l3

dis1 = [ "ATE", 'BARRANCO','BREÑA', 'CALLAO', 'CARABAYLLO' ] 
dis1

# Reorder

[dis1[4]] + dis1[:3]

dis2 = ['ATE', 'BARRANCO','BREÑA', 'CALLAO', 'CARABAYLLO','CHACLACAYO','CHORRILLOS','CIENEGUILLA'
        ,'COMAS','EL_AGUSTINO','INDEPENDENCIA']

len(dis2) # numero de elementos 

dis2[0] = "CALLAO"
print(dis2)

# Idenxing

print(dis2[1])

dis2[2:5]   # (5-1=4)

dis2[3:7]

dis2[-5:-1]

# sort

num = [13,5,5,8,9,10,5,8,13,1,20]
num.sort()
print(num)

## Pop to delete elements by indexing (by defaul the last element)

num.pop()

num.pop(0) # drop the element placed in indexing 0. 

num

# remove especific values

num.remove(5)
num

# replace 

num[1] = 1000
num

# check if an element is in a list
print(f'1000 está en la lista num: {1000 in num}')

# Evaluar si un objeto se encuentra en una lista

mylist = [1, 2] + [3, 4]
print(mylist)
print(1 in mylist)
print(13 in mylist)
print(2 not in mylist)
print(14 not in mylist)

# seek element in a specific index
print(num)
num.index(5, 1)  # 5 (index), 1 from index 1

### List: append

# append new elements 

num.append(102)


num.insert(2,100000) # insert element to a list at a specific location 
num

### List: extend

# append new list

num2 = [10,20,30]
num.extend(num2)
print(num)

print(num.index(102))

print("Suma:", sum(num),'\n', "Minimo:", min(num), '\n', "Maximo:", max(num))


#%% Sets (conjuntos)

my_set1 = set()
print(my_set1)


nombres = {'Jose', 'Miguel', 'Juan'}
print(type(nombres))
nombres

my_set1.add(10)
my_set1.add('Economia')
my_set1

my_list = [1, 1, 1, 2, 3, 4, 4, 4, 4, 2, 2]
set(my_list)  # drop duplicates. Unique elements

list(set(my_list))

# # Differences

A = {1, 2, 3, 4, 5}
B = {3, 4, 5, 6, 7}

print(A - B)
print(B - A)

# # Symetric differences
print(A ^ B)

# # Intesection 2 sets
print(A & B)

# Subsets
A = {1, 2, 3}
B = {1, 2, 3, 4, 5}
C = {1, 2, 3, 10}

print(f'A < B: {A < B}')
print(f'C < B: {C < B}')
print(f'B > A: {B > A}')
print(f'B > C: {B > C}')
print(f'A < A: {A < A}')
print(f'A <= A: {A <= A}')
print(f'A >= A: {A >= A}')

#%% Dictionary

diccionario = {}
otro_dicc = dict()
print(type(diccionario))
print(type(otro_dicc))

Postal_code = { 'Majes': 40520 , 'Mollendo': 40701, 'Islay': 40704, 'Cotahuasi': 40801, 'Alca': 40802  }

type(Postal_code)

# dict de diccionario 
# 'Majes', 'Mollendo' son llaves 

Postal_code

Postal_code.keys()

# Get information from key
Postal_code['Alca']

# Get information from key
Postal_code.get('Alca')

# Drop key
Postal_code.pop('Islay')
Postal_code

# add new elements 

Postal_code.update( { "CHARCANA" :  [1,2] } )

Postal_code['ATE'] =15032

#diccioanrio dentro de otro diccionario
Postal_code.update( {"LOMAS": {"UBIGEO": 40311, "Poverty Rate" : "18.2%", "Population" : "20 mil"}})


Postal_code

Postal_code.get('LOMAS').get('Poverty Rate')

Postal_code['LOMAS']['Poverty Rate'] # forna mas usada 

Postal_code['LOMAS']


# keys
cities = ['Fray Martin','Santa Rosa de Puquio','Cuchicorral','Santiago de Punchauca','La Cruz (11 Amigos)','Cerro Cañon','Cabaña Suche','San Lorenzo','Jose Carlos Mariategui','Pascal','La Esperanza','Fundo Pancha Paula','Olfa','Rio Seco','Paraiso','El Rosario','Cerro Puquio','La Campana','Las Animas','Vetancio','Roma Alta','San Jose','San Pedro de Carabayllo','Huacoy','Fundo Pampa Libre','Ex Fundo Santa Ines','Reposo','Carmelito','Santa Elena','Don Luis','Santa Ines Parcela','Asociacion Santa Ines','Roma Baja','Residencial Santa Lucia','San Francisco','Santa Margarita - Molinos','Sipan Peru','Fundo Cuadros','Bello Horizonte','El Hueco','Ex Fundo Mariategui','Naranjito','Vista Hermosa','El Sabroso de Jose Carlos Mariategui','Granja Carabayllo','Agropecuario Valle el Chillon','Camino Real','Copacabana','El Trebol','Tablada la Virgen','San Fernando de Carabayllo','San Fernando de Copacabana','La Manzana','Chacra Grande','Torres de Copacabana','San Pedro de Carabayllo','San Lorenzo','Chaclacayo','Chorrillos','Cieneguilla','Lindero','Pichicato','San Isidro','San Vicente','Piedra Liza','Santa Rosa de Chontay (Chontay)','La Libertad','El Agustino','Independencia','Jesus Maria','La Molina','La Victoria','Lince','Las Palmeras','Chosica','Lurin','Los Almacigos','Rinconada del Puruhuay','Fundo Santa Genoveva','Los Maderos','Casco Viejo','Vista Alegre','Buena Vista Alta','Lomas Pucara','Fundo la Querencia','Magdalena del Mar','Pueblo Libre','Miraflores','Pachacamac','Puente Manchay','Tambo Inga','Pampa Flores','Manchay Alto Lote B','Invasion Cementerio','Manchay Bajo','Santa Rosa de Mal Paso','Cardal','Jatosisa','Tomina','Pucusana','Honda','Quipa','Los Pelicanos','Playa Puerto Bello','Ñaves','Granja Santa Elena','Alvatroz II','Poseidon - Lobo Varado','Playa Minka Mar','Playa Acantilado','Puente Piedra','Punta Hermosa','Capilla Lucumo','Cucuya','Pampapacta','Avicola San Cirilo de Loma Negra - 03','Avicola San Cirilo de Loma Negra - 02','Avicola San Cirilo de Loma Negra - 01','Pampa Mamay','Cerro Botija','Agricultores y Ganaderos','Pampa Malanche Avicola Puma','Punta Negra','Chancheria','Rimac','San Bartolo','Plantel 41','Granja 4','Granja 5','Granja 07','Granja 44','Granja 47','Santa Maria I','Las Torres Santa Fe','San Francisco de Borja','San Isidro','San Juan de Lurigancho','Ciudad de Dios','San Luis','Barrio Obrero Industrial','San Miguel','Santa Anita - los Ficus','Santa Maria del Mar','Don Bruno','Santa Rosa','Santiago de Surco','Surquillo','Villa el Salvador','Villa Maria del Triunfo', 'Pueblo libre']
# values
postal_code1 = [15001,15003,15004,15006,15018,15019,15046,15072,15079,15081,15082,15083,15088,15123,15004,15011,15012,15019,15022,15023,15026,15476,15479,15483,15487,15491,15494,15498,15047,15049,15063,15082,15083,15121,15122,15313,15316,15318,15319,15320,15321,15324,15320,15320,15320,15320,15320,15320,15121,15320,15320,15121,15320,15320,15121,15121,15122,15122,15121,15121,15121,15320,15320,15320,15320,15320,15320,15121,15121,15121,15320,15121,15319,15121,15121,15121,15320,15320,15121,15121,15121,15121,15320,15320,15320,15122,15122,15122,15122,15122,15122,15122,15122,15121,15121,15122,15122,15121,15121,15122,15122,15121,15122,15122,15122,15472,15476,15054,15056,15057,15058,15063,15064,15066,15067,15593,15594,15593,15593,15593,15593,15593,15593,15593,15311,15312,15313,15314,15316,15324,15326,15327,15328,15332,15003,15004,15006,15007,15008,15009,15011,15018,15022,15311,15328,15331,15332,15333,15046, 15001]

# Return a dictionarie
ct_pc = dict( zip( cities , postal_code1) )

ct_pc

# Loop and dict 

for key,value in ct_pc.items():
    print(key, '-',value)

# # Keys

for ciudad in ct_pc.keys():
    print(ciudad)

# # Values
for codigo in ct_pc.values():
    print(codigo)
    
    
#%% Arrays and Matrix

import numpy as np

np.array( [1, 2, 3, 4, 5] )

# A library is a collection of modules or a single module. Libraries are the tools we will use to make our program.

# 1D array (vector)
a = np.array( [1, 2, 3, 4, 5] )

print(a)

type(a)

np.mean(a)
np.sum(a)

np.std(a)

# standar error fixed fo finte sample

np.std(a,ddof=1) # bias standar error


# 2D array (matrix 2x2)
M = np.array( [ [1, 2, 3], [4, 5, 6] ] )

print(M)

# dimensiones

M.shape # numero de filas 

# shape()

print("Rows:",M.shape[0],"\n", "Columns: ", M.shape[1])


### Range(): consecutive numbers

# deafult one by one 

y = np.arange( 1, 11)
print(y)

list(range(11))


# range to list 

print( list(range(5,9)) )
list(range(1,9,2))


# arange to list 

np.arange( 0 , 24 ).tolist()

### List comprehension


[i for i in range(9)]

[str(i) for i in range(9)]  # string numbers

[str(i) for i in range(9) if i != 5] # add a condition

# rep() from R equivalent in python

print( np.repeat(2, 4) )

print( np.repeat(range(11), 4) )

# split array

np.array_split(np.arange( 0, 10), 4)

## Matrix

Matrix = np.random.rand(10,10)  # random numbers from uniform distribution 
Matrix

Matrix[2:5,:]  # rows selecrtion (2,3 and 4)


Matrix[:,5:9]  # columns selecrtion (5,6,7,8)

Matrix[:,[1,4]]  # columns selection (1 y 4)

# Transpose
Matrix.transpose()

print(Matrix.T)

#Inverse
np.linalg.inv(Matrix)

# Plot ###########################################3

import matplotlib.pyplot as plt

#Numeros aleatorios

X = np.random.normal(1,.5, 10000)  #1000 observations with mean 1 y standar error .5

Xbar = X.mean()

Sigma2 = sum((Xbar-X)**2)/len(X)

Sigma = np.sqrt(Sigma2)

plt.hist(X, bins = 100)

# Matrix made of zeros 

M1 = np.zeros( (8, 2) )
print(M1)

#  Matrix made of zeros 

M2 = np.ones( (10, 4) )
print(M2)

## Join matrix

M3 = np.hstack((Matrix,M2))  # stack horizontal 
M3

# Create a 1D NumPy array of ones of length 10:
w = np.ones(10)
print(w)

# Create the identity matrix of size 8:
I = np.eye(8)
print(I)



#%% OLS regression

np.random.seed(175) # semilla aleatoria para producir numeros aleatorios 
#  permite que los numeros aleatorios no cambien al correr los códigos
# numpy random permite generar números aleatorios unicos 

x1 = np.random.rand(500) 
x2 = np.random.rand(500) # uniform distribution [0,1]
x3 = np.random.rand(500) # uniform distribution [0,1]
x4 = np.random.rand(500) # uniform distribution [0,1]
e = np.random.normal(0,1,500) # normal distribution mean = 0 and sd = 1

# Poblacional regression (Data Generating Process GDP)

Y = 1 + 0.8*x1 + 1.2*x2 + 0.5*x3 + 1.5*x4 + e

X = np.column_stack((np.ones(500),x1,x2,x3,x4))
X

beta = np.linalg.inv(X.T @ X) @ ((X.T) @ Y )
print(beta)









