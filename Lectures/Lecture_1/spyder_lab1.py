# -*- coding: utf-8 -*-
"""
# Laboratorio 1 Python

##  Types of variables

@author: Roberto Mendoza
"""
#%% Types of variables

########################################################
"""
 1.0 Types of variables 
"""

# Control + 1 permite pasar de código a comentario y viceversa 

a1 = 3.141

type(a1)


a2 = 3.1416165516
type(a2)

# from float to int 

c = int(a2)
type(c)

b1 = 10000
type(b1)

# from int to float 

float(b1)

b2 = 8
type(b2)

c1 = "My first python code"
print(c1)  # show that varaible's content
type(c1) # providing varaibles's type 

# including a space using \n

c1 = "First python code"
c2 = "at R y python Class"
print(c1,'\n',c2)

# join string 

print(c1 + " : " + c2)


# f-using 


print(f'{c1} : semester 2022-1')

d = 2022

print(f'{c1} : semester {d}-1')

print('{} : semester {}-1'.format(c1,d))

c1[0:5]

#first character
print('Fisrt letter is :',c1[0])

#first word
print('Fisrt word is :',c1[0:5])


#%% Bool variables 

########################################################
""" 
2. 0 Bool variables 
"""

"a" == "a"

1 > 1

z1 = (1==1)
int(z1)

z2 = (10 > 20)
int(z2)

z3 = (100 != 100)
int(z3)


#%% Tuple

########################################################
"""
 3.0 Tuple
"""

# It is an ordered and immutable Python object

T1 = (1,4,8,10,20,15,4,5,3,8)
print(T1)
type(T1)

# aritmethic operations

print('Suma:', sum(T1),"\n", "Minimo:", min(T1), '\n', "Maximo:", max(T1))

len(T1) # lenght of tuple

"""
Indexing tuple
"""

T1[0:5] # give us elements from 0 position until 4 position 

T1[1] # get tuple's element 

T1[0:3]

# It is not possible to change
T1[0] = 4

T1[-1] # last element

#position
T1.index(8)

