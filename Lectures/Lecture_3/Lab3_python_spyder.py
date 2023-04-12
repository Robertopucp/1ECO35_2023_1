# -*- coding: utf-8 -*-
"""

@author: Roberto
"""

'''
Importar librerias
- numpy para operaciones de vectores y matrices
- pandas para manipular DataFrame (base de datos)
- Series para manipular columnas de DataFrame 
'''

import numpy as np
import pandas as pd
from pandas import DataFrame,Series
import os # for usernanme y set direcotrio

# Debemos construir el directorio donde se trabajará
# 

user = os.getlogin()   # Username
print(user)

# Set directorio

os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2022_2/Lab3") # Set directorio

netflix = pd.read_csv("../data/netflix_titles.csv") # Subir base de datos
netflix

netflix.info() # varaible's type 

print( netflix.shape ) # filas y columnas

'''
Equivalent sum de Stata, solo la variable release_year es numerica
'''

netflix.describe() 


## Verificando ID


print( netflix.show_id.unique() )

print( len( netflix.show_id.unique() ) )

print( netflix.show_id.is_unique )


##################################

print( len(netflix['show_id'].unique() ) )

netflix['show_id'].unique()

netflix['show_id'].is_unique # verificando repeticiones

#%% Exploring DataFrame 

# Series en Python

print(type(netflix['director']))
netflix['director']

## revisado missing values (NaN in Python)

print( netflix.director.unique() )

## isnull() and isna() son similares 

print( netflix['director'].isnull() )

print( netflix['director'].isna() )


# cantidad de missing values en la variables director
netflix['director'].isna().sum()

# Borrar missing values 

netflix.dropna() # borra todas las filas o columnas con al menos un missing value
netflix.dropna(axis = 0) # borras filas con missing value
netflix.dropna(axis = 1) # columnas con al menos un missign values

'''
- Notese que la base de datos Netflix no ha sido alterada. Para que la base de datos se altere debe ocurrir dos cosas:
1. Asignarse a una nueva base de datos 
2. Si la función lo permite, incluir el input ***Inplace = True***
'''

netflix.dropna(subset = ['director']) # drop observaciones que presentan missing values en la columna director

netflix.dropna(subset = ['director']) 

 # Reemplazar missing values de la columna director con la palabra "Sin director"
netflix.director = netflix.director.fillna("Sin director")

# Alrerar la misma base de datos sin la necesidad de asignar a uno nuevo 

netflix.director.fillna("Sin director", inplace = True)


# Qué funciones admiten Inplace = True: drop, fillna, rename y otros. 


# Create new variable
netflix['new_col'] = np.arange(0, netflix.shape[0]) # creat new varaible 

#%% Filter DataFrame 


# Filter rows

netflix.loc[0:100]
netflix.loc[210:500]
netflix.loc[210:] # desde la fila posición 210 hasta al final 

# filter columns 

netflix.loc[:,['show_id','type','description']]

netflix.loc[1000:2000,['show_id','type','description']]

netflix2 = netflix.set_index( [ 'show_id' ] )

# filter filas usando index 

netflix2.loc[['s1','s100','s7000'],:]


# Reset index 

netflix2.reset_index() 

'''
iloc usa las posiciones de filas y columnas
'''

# Filter rows

netflix.iloc[0:100]
netflix.iloc[210:500]

# filter columns 

netflix.iloc[:,[0,1,12]]


netflix.iloc[1000:2000,[0,1,12]]


# Column names in a list
list(netflix.columns)

#%% Sorting and Subsetting

# sort ascendet 
net_old = netflix.sort_values("release_year")

# sort de manera descendente
net_new = netflix.sort_values("release_year", ascending = False)

# crear nueva variable

netflix['number'] = np.random.randint(1, 10, netflix.shape[0])


netflix.sort_values(["release_year","number"])
net_two_sort = netflix.sort_values(["release_year","number"], ascending = [True,False]) 

#%% Subsetting columns

netflix.director

netflix[["director","cast"]]


#%% Subsetting rows

netflix[netflix["release_year"] < 2011].head(10)

netflix[(netflix["release_year"] < 2011) & (netflix["number"] > 5)]
netflix[( netflix.release_year < 2011 ) & (netflix.number > 5)]
netflix[['director']].iloc[0:10]

#%% Subsetting based on text data

net_peru = netflix[netflix.country == "Peru"]
net_mex = netflix[netflix["country"] == "Mexico"]

netflix[(netflix["type"] == "TV Show") & (netflix["country"] == "Peru")]
# No hay series peruanas en Netflix

len(netflix[(netflix["country"] == "Brazil") | (netflix["country"] == "Peru")])
netflix[(netflix["type"] == "TV Show") | (netflix["country"] == "Peru")]

netflix.loc[(netflix["type"] == "Movie") & (netflix["country"] == "United States")
            & (netflix["release_year"] > 2019)]

# .isin()
#If you want to filter by multiple values of a categorical variable,
# the easiest way is to use the isin() method.


# Peliculas de Perú y Chile

net_per_ch = netflix[netflix["country"].isin(["Peru","Chile"])] # filter by variables's values


# Peliculas diferentes de Perú y Chile

netflix[~netflix["country"].isin(["Peru","Chile"])]   # ~ negación en Python ALT + 126 

"""
Alternative methods to filter
"""

movie = netflix["type"] == "Movie"
m_usa = netflix["country"] == "United States"
m_actual = netflix["release_year"] > 2019

movie_usa = netflix[movie & m_usa & m_actual]



movie_usa = netflix.loc[ ( netflix[ "type" ] == "Movie" ) 
                        & ( netflix[ "country" ] == "United States")
                        & ( netflix[ "release_year" ] > 2019 ) ]



movie_usa.drop(['show_id', 'director'], axis=1 ) # axis = 1 , drop por columna 

movie_usa[movie_usa.release_year != 2020]

#Rename variables

movie_usa.rename(columns = {'title':'Tituto_movie', 'duration':'Duration_movie'}, inplace = True)


### Export data

movie_usa.to_csv("../movie_usa.csv")

movie_usa.to_excel("../movie_usa.xlsx")
















