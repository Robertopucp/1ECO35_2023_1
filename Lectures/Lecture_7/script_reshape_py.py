# -*- coding: utf-8 -*-
"""

@author: Roberto
"""

import pandas as pd
import numpy as np
import chardet # to get string character format 
import re  # for regular expression 
import os # for usernanme y set direcotrio

# help para revisar la documentación de una libreria 

help(re) 


#%% Load dataset


user = os.getlogin()   # Username

os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2022_2/Lab8") # Set directorio

panel = pd.read_stata("../../../datos/panel/743-Modulo1478/sumaria-2016-2020-panelf.dta",
                      convert_categoricals=False)


# Filter dummy hpanel1620 == 1: hogares entrevistados en el periodo 2016-2020

panel = panel[panel.hpanel1620 == 1]


# nombre de las variables en minuscula

panel.columns = map(str.lower, panel.columns)  


print( panel.columns)

# Se filtra variables selecionadas 

index_columns = np.where( panel.columns.str.contains(
    '(año)|(^conglome)|(vivienda)|(hogar)|(estrato_)|(mieperho)|(gashog2d)|(pobreza_)|(factor07)',
    regex=True))[0]



# index_columns: se guarda las posiciones de columnas de aquellas variables que satisface 
# con algunos de los casos del regex

panel = panel.iloc[:,index_columns] 


# Rename las  variables de año 
    
panel.rename(columns = {'año_16':'year_16', 'año_17':'year_17', 'año_18':'year_18', 'año_19':'year_19',
                       'año_20':'year_20', 'conglome':'cong', 'vivienda':'viv'}, inplace = True)


# Se crea un identificados del hogar 

panel['hog'] = np.arange(1, panel.shape[0] + 1)

# reordenando las columnas 

cols = panel.columns.tolist() # nombre de varaible a lista 
new_cols_orders = cols[ 0:2 ] + [cols[ -1 ]] + cols[ 2:-1 ] 

# cols[ 0:2 ] dos primeras columnas (conglome, vivienda)
# [cols[ -1 ]] ultima columna (hog)
# cols[ 2:-1 ] desde la tercera hasta la penultima

panel = panel.loc[:, new_cols_orders]


## Reshape Wide to long 

# reshape_panel = pd.wide_to_long(panel, stubnames = new_list,
#                                 i = ['cong', 'viv', 'hog'] , j = 'period' , sep = '_').reset_index()


# new_list: debe contener las variables en comun en todos los años (sin _año)

panel.columns[3::]


filter_list = list( map( lambda x: re.sub('.{3}$',"", x) , list(panel.columns)[3::] )   )
    
#se reemplaza los tres ultimos caracteres 
#(como se usa el carcater especial .), se toma cualquier tipo de caracter ya sea espacio, numero, letras, #!", etc

# Se puede ibservar que filter list tiene duplicados

# Se procede a elimitar los duplicados en la lista

new_list = list(dict.fromkeys(filter_list))


## Reshape Wide to long 


reshape_panel = pd.wide_to_long(panel, stubnames = new_list, i = ['cong', 'viv', 'hog'] , 
                                j = 'period' , sep = '_')


reshape_panel = pd.wide_to_long(panel, stubnames = new_list, i = ['cong', 'viv', 'hog'] , 
                                j = 'period' , sep = '_').reset_index()


# drop a las siguientes variables

del reshape_panel['cong']
del reshape_panel['viv']
del reshape_panel['hog']
del reshape_panel['period']










