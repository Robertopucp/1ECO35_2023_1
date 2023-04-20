# -*- coding: utf-8 -*-
"""
Lab 4

@author: Roberto


"""

import pandas as pd
import numpy as np
import os


# Change directory 

user = os.getlogin()   # Username

# Set directorio

os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2023_1/Lectures/Lecture_4") # Set directorio


#%% Lectura base de datos

datoscsv = pd.read_csv(r"../../data/Riesgo_morosidad.csv")
datoscsv

# Los datos están separados por ;

datoscsv = pd.read_csv(r"../../data/Riesgo_morosidad.csv", sep = ";")
datoscsv.info()
datoscsv


# archivos .txt

datost = pd.read_table(r"../../data/Riesgo_morosidad.dat", sep="\t")
datost


# read spss (.sav)

pd.read_spss( r"../../data/Riesgo_morosidad.sav")

# read spss (.sav)

datospss = pd.read_spss( r"../../data/Riesgo_morosidad.sav", convert_categoricals= False)
datospss

# convert_categoricals= False : las etiquetas no son parte de los datos.

import savReaderWriter as sav 


with sav.SavHeaderReader( r"../../data/Riesgo_morosidad.sav", ioUtf8=True) as header:
    metadata = header.all() 
    labels_enapres2020_1 = metadata.valueLabels  # values' label 
    var_labels_enapres2020_1 = metadata.varLabels # variables' label 

print(labels_enapres2020_1, "\n")
var_labels_enapres2020_1

# adding labels to variables enapres2020_1 from pandas 

datospss.attrs[ 'value_labels' ] = labels_enapres2020_1 # value's labels 
datospss.attrs[ 'var_labels' ] = var_labels_enapres2020_1 # var labels

# Etiqueta de valores

datospss.attrs[ 'value_labels' ]

# Etiquetas de variables
datospss.attrs[ 'var_labels' ]

# import excel files

datos_xlsx = pd.read_excel("../../data/Riesgo_morosidad.xlsx")
datos_xlsx

datos_xls = pd.read_excel("../../data/Riesgo_morosidad.xls")
datos_xls

# read special characters 

netflix = pd.read_csv("../../data/netflix_titles.csv") # por default UTF-8 para caracteres especiales y lectura de missing 
netflix

# missing NaN


#%% Limpieza datasets


bbdd = pd.read_csv(r"../../data/paises.csv")
bbdd

# tipo de variables 
bbdd.info()

# dimensiones de la base de datos
print(bbdd.shape, "\n")
print("Esdadisticas de variables numericas: \n \n",bbdd.describe())

# check missing variables 

print(bbdd.isna().sum())

# Tabular: contabilizar los diferentes valores que toma una variable

bbdd.continente.value_counts()

# tabulación cruzada 

pd.crosstab(bbdd.anio, bbdd.continente)

#%% Filtro dataset

# Nos quedamos con el año 1957

bbdd[bbdd.anio == 1957]

# Nos quedamos con un año diferentes a 1957

bbdd[  bbdd.anio != 1957]

# multiples filtros

bbdd[(bbdd.pais == "China") & (bbdd.anio == 2002)]

# colocar cada condición en parentesis

# Chile y Peru

bbdd[(bbdd.pais == "Chile") | (bbdd.pais == "Perú")]

# isin([])  es el equivalente a %in% de R 

bbdd[ (bbdd.pais == "Perú") & (bbdd.anio.isin([1997,2002,2007]))]

#  Ordenando observaciones según la esperanza de vida 

bbdd.sort_values("esperanza_de_vida")

bbdd

# asignando la nueva base de datos

bbdd = bbdd.sort_values("esperanza_de_vida")
bbdd


#%% Inplace True

# Modify the DataFrame rather than creating a new one

# ordenamiento de forma descendente 

bbdd.sort_values("esperanza_de_vida", ascending = False, inplace = True)
bbdd

# algunas fuciones de limpieza de datos como sort_values, rename, drop, etc permiten actualizar la base de datos 
# Solo se debe incluir el argumento inplace = True 


# Filtrando y ordenando

bbdd[bbdd.anio == 1957].sort_values("esperanza_de_vida")

#      Ejercicio  1  #
# Muestre la información de Europa del año 1987 ordenada 
# según el PIB de manera descendente

# creación de variables 

bbdd['esperanza_de_vida_meses'] = 12*bbdd['esperanza_de_vida']
bbdd['pbipc_miles'] = bbdd['pib_per_capita']/1000

#%% Filtro de filas y columnas .loc
 
# .loc permite filtrar filas o columnas usando el nombre de las filas o columnas

# selección de columnas 

bbdd.loc[:,["pais","anio","poblacion"]]

# : selecciona todas las filas

# selección de filas según el indexing

bbdd.loc[[803,552,36,671],:]

#%% Filtro de filas y columnas .iloc

# .iloc permite filtrar filas o columnas usando la posición respectiva

print(bbdd.iloc[:,[0,2,4]]) # selección columnas posición 0, 2 y 4

bbdd.iloc[:,:3]  # selección de las columnas 0,1y2

bbdd.iloc[:,0:3]  # selección de las columnas consecutivas 0:2 y la columan posición 4


# Seleccionar la ultima columna ?#

#      Ejercicio 2   #

# población en millones, año 2007, países de áfrica


### Select columns

bbdd[['pais', 'anio', 'poblacion']]   # selección de 3 variables

bbdd[['pais', 'anio', 'poblacion']].iloc[100:]

bbdd[['pais', 'anio', 'poblacion']].iloc[100:151]

#%% Rename columns

bbdd.rename(columns = {'pais':'Pais', 'anio':'Año', 'pib_per_capita':'PBI'})

bbdd

bbdd.rename(columns = {'pais':'Pais', 'anio':'Año', 'pib_per_capita':'PBI'}, inplace = True)
paises2 = bbdd[['Pais','Año','PBI']]

#%% Indexing filas (rows)

# Reseteo del indexing 

bbdd.reset_index()

bbdd

bbdd.reset_index(drop = True, inplace = True)
bbdd

bbdd['new_var'] = range(1,1705)
bbdd

# Columna de la base de datos como nuevo indexing

bbdd.set_index('new_var')

# save datasets

paises2.to_csv(r"../../data/Paises_renombrados_py.csv", index=False)

#%% Drop columns

bbdd['var1'] = 1000
bbdd['var2'] = "Indicadores"
bbdd

bbdd.drop(['new_var','var1','var2'], axis = 1)

# drop and update dataset

bbdd.drop(['new_var','var1','var2'], axis = 1, inplace = True)  # axis = 1 (drop de columnas)

bbdd

# drop de columnas usando "delete"

bbdd['var1'] = 1000
print(bbdd)
del bbdd['var1']  # delete la columna var1
bbdd

# Uso de delete para borrar varias columnas. Se separa en comas las columnas a borrar

#del bbdd['new_var'], bbdd['var1'], bbdd['var2']

#%% Order columns

cols = bbdd.columns.tolist() # nombre de varaible a lista 
cols

new_cols_orders = [ cols[2] , cols[1] , cols[0] ] + cols[ 3: ]  # lista de nuevo ordenamiento de columnas
bbdd = bbdd.loc[ : , new_cols_orders ]   # filtramos
bbdd

# Como llevar la ultima columan al principio ?




## Summarise

np.mean(bbdd['esperanza_de_vida'])

np.mean(bbdd[bbdd.Año == 2007]['esperanza_de_vida'])

np.mean(bbdd[bbdd.Año.isin([1957,2007]) ]['esperanza_de_vida'])

np.mean(bbdd[bbdd.Año.isin(range(1950,1991)) ]['esperanza_de_vida'])

np.max(bbdd[bbdd.Año.isin(range(1950,1991)) ]['PBI'])

#%% Groupby

# - numpy permite realizar las operaciones a nivel agrupamiento
# - np.mean, np.max, np.min, np.size, np.median

# Resumiendo por año y continente
# Por default la variable de agrupamiento pasa a ser el nuevo indexing de filas
df1 = bbdd.groupby('Año').agg(mean_esperanza_de_vida = ('esperanza_de_vida', np.mean))
df1

# as_index = False permite que la variable de agrupamiento no sea el nuevo indexing de filas

bbdd.groupby('Año', as_index = False).agg(mean_esperanza_de_vida = ('esperanza_de_vida', np.mean))

# Agrupamiento por año y continente

bbdd.groupby(['Año','continente'], as_index = False).agg(mean_esperanza_de_vida = ('esperanza_de_vida', np.mean))

# media de la esperanza de vida y desviación estándar

bbdd.groupby(['Año','continente'], as_index = False).agg(mean_esperanza_de_vida = ('esperanza_de_vida', np.mean),
                                                        sd_esperanza_de_vida = ('esperanza_de_vida', np.std))

# Filtro año 2007, contabilizar el numero de países por contienente

bbdd[bbdd.Año == 2007].groupby('continente', as_index = False).agg( total_paises = ('Pais', np.size))


# Añadiendo el summarise a la base de datos 

bbdd['mean_pbipc_pais'] = bbdd.groupby('Pais')['PBI'].transform('mean')
bbdd.sort_values('Pais')

bbdd['median_pob'] = bbdd.groupby('continente')['poblacion'].transform('mean')
bbdd.sort_values('continente')

# summarise del pbi percapita de los paises de las Américas, mediana, minimo y máximo



