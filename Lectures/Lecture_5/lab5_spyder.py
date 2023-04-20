# -*- coding: utf-8 -*-
"""
Created on Sat Sep 24 

@author: Roberto

@script: Clean ENAHO
"""

#!pip install weightedcalcs

import os   # for usernanme y set direcotrio
import pandas as pd
import numpy as np
import weightedcalcs as wc # ponderador
from tqdm import tqdm  # controlar el tiempo en un loop


user = os.getlogin()   # Username


os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2022_2/Lab7") # Set directorio

# Set directorio
#%% Merge 


" Read Stata dataset usando pandas"

"Se puede observar que pandas lee las etiquetas de las valores de cada variables"

enaho_2020 = pd.read_stata(r"../../../datos/2020/737-Modulo01/737-Modulo01/enaho01-2020-100.dta")

"Debemos colocar convert_categoricals=False. Esto por deafult es True"
" De esta manera respetará los value's label "


enaho01 = pd.read_stata(r"../../../datos/2020/737-Modulo01/737-Modulo01/enaho01-2020-100.dta",
                           convert_categoricals=False)


labels01 = pd.read_stata(r"../../../datos/2020/737-Modulo01/737-Modulo01/enaho01-2020-100.dta",
                           convert_categoricals=False, iterator=True)



labels01.variable_labels()

labels01.value_labels().keys()

labels01.value_labels()['p110']


#identificador por miembro del hogar: conglome, vivienda, hogar, codperso

"Elegimos la base de datos como Master Data: módulo 02: características de los miembros del hogar"

enaho02 = pd.read_stata(r"../../../datos/2020/737-Modulo02/737-Modulo02/enaho01-2020-200.dta",
                           convert_categoricals=False)

"Hacemos merge con el resto de base módulos (using data)"

"Presenta información de coordenadas"

enaho03 = pd.read_stata(r"../../../datos/2020/737-Modulo03/737-Modulo03/enaho01a-2020-300.dta",
                           convert_categoricals=False)


enaho04 = pd.read_stata(r"../../../datos/2020/737-Modulo04/737-Modulo04/enaho01a-2020-400.dta",
                           convert_categoricals=False)


enaho05 = pd.read_stata(r"../../../datos/2020/737-Modulo05/737-Modulo05/enaho01a-2020-500.dta",
                           convert_categoricals=False)

enaho34 = pd.read_stata(r"../../../datos/2020/737-Modulo34/737-Modulo34/sumaria-2020.dta",
                           convert_categoricals=False)


enaho37 = pd.read_stata(r"../../../datos/2020/737-Modulo37/737-Modulo37/enaho01-2020-700.dta",
                           convert_categoricals=False)

# labels

labels37 = pd.read_stata(r"../../../datos/2020/737-Modulo37/737-Modulo37/enaho01-2020-700.dta",
                           convert_categoricals=False, iterator=True)

labels37.variable_labels()


# identificador por hogar: conglome, vivienda, hogar

enaho_merge = pd.merge(enaho02, enaho01,
                       on = ["conglome", "vivienda", "hogar"],
                       how = "left", 
                       validate = "m:1")


enaho_merge['latitud'].isna().sum()

# enaho02: base de datos con información de miembros del hogar
# enaho01: base de datos a nivel de hoagres
# on: variable que permite identificar las observaciones en común en las bases de datos
# how: cómo se realizará el merge
# validate: modo de unificar las bases de datos. 

#cols_to_use = enaho02.columns.difference(enaho01.columns)

enaho_merge = pd.merge(enaho02, enaho01,
                       on = ["conglome", "vivienda", "hogar"],
                       how = "left", 
                       validate = "m:1", suffixes=('', '_y'))

# suffixes: renombrar las variables comunes en las bases de datos 

# merge que selecciona las variables en el using data

enaho_merge_2 = pd.merge(enaho02, enaho01[["conglome", "vivienda", "hogar",'longitud','latitud']],
                       on = ["conglome", "vivienda", "hogar"],
                       how = "left", 
                       validate = "m:1")

#%% Tipos de Merge


# output basico de STATA (_merge =1,3) Keepus Master

enaho_merge_left = pd.merge(enaho01, enaho37, 
                         on = ["conglome", "vivienda", "hogar"],
                       how = "left", 
                       validate = "1:1")



# output basico de STATA (_merge =2,3) Keepus using

enaho_merge_right = pd.merge(enaho01, enaho37, 
                         on = ["conglome", "vivienda", "hogar"],
                       how = "right", 
                       validate = "1:1")


# output basico de STATA (_merge =3)

enaho_merge_inner = pd.merge(enaho01, enaho37, 
                         on = ["conglome", "vivienda", "hogar"],
                       how = "inner", 
                       validate = "1:1")



enaho_merge_outer = pd.merge(enaho01, enaho37, 
                         on = ["conglome", "vivienda", "hogar"],
                       how = "outer", 
                       validate = "1:1")

datos= np.array([enaho_merge_left.shape[0], 
          enaho_merge_right.shape[0],
          enaho_merge_inner.shape[0],
          enaho_merge_outer.shape[0],          
          ])

pd.DataFrame(index = ['Left','Right','Inner','Outer'],
             data = datos, columns = ["observaciones"]
             )

# 

# output basico de STATA (_merge = 1,2,3)

enaho_merge_3 = pd.merge(enaho01, enaho37, 
                         on = ["conglome", "vivienda", "hogar"],
                       how = "outer", 
                       validate = "1:1")

enaho_merge_3.shape


# merge using individual dataset

#%% Merge using different Key variables 



enaho03.rename(columns={"conglome":"cong", "vivienda":"viv", "hogar":"hog","codperso":"perso"}, 
               inplace = True)

merge_1 = pd.merge(enaho02, enaho03, 
                         left_on = ["conglome", "vivienda", "hogar","codperso"],
                         right_on = ["cong","viv","hog","perso"],
                       how = "left", 
                       validate = "1:1")


enaho03.rename(columns={"cong":"conglome", "viv":"vivienda", "hog":"hogar","perso":"codperso"}, 
               inplace = True)



## merge con dataset a nivel hogar enaho01, enaho34, enaho37


num = ["34","37"]

merge_hog = enaho01

merge_hog['ubigeo_dep2'] = merge_hog['ubigeo'].str[:2]
merge_hog['ubigeo_dep6'] = merge_hog['ubigeo'].str[:2]+"0000"

merge_hog = merge_hog[merge_hog.ubigeo_dep2.isin(["15","03","04"])]



for i in tqdm(num):
    merge_hog = pd.merge(merge_hog, globals()[f'enaho{i}'], 
                         on = ["conglome", "vivienda", "hogar"],
                       how = "left", 
                       suffixes=('', '_y'),
                       validate = "1:1")
    
    
    
# Merge a nivel miembros del hogar

num = ["03"]

merge_ind = enaho02  # modulo de personas 

merge_ind['ubigeo_pr'] = merge_ind['ubigeo'].str[:2]

merge_ind= merge_ind[merge_ind.ubigeo_pr.isin(["15","03","04"])]


# glablas para que python entienda que trabajamos una un dataset en el loop

for i in tqdm(num):
    merge_id = pd.merge(merge_ind, globals()[f'enaho{i}'], 
                         on = ["conglome", "vivienda", "hogar","codperso"],
                       how = "left", 
                       suffixes=('', '_y'),
                       validate = "1:1")


# Merge hogares e individuos 

merge_base_2020 = pd.merge(merge_id, merge_hog, 
                            on = ["conglome", "vivienda", "hogar"],
                            how = "left",
                            validate = "m:1",
                            suffixes=('', '_y'),
                            )

# drop varibales que terminan en _y

index_columns = np.where( merge_base_2020.columns.str.contains('_y$', regex=True))[0]

merge_base_2020.drop(merge_base_2020.columns[index_columns], axis = 1, inplace = True)


merge_base_2020['linea']


###########################################
############# Merge 2019 ##################
###########################################


enaho01 = pd.read_stata(r"../../../datos/2019/687-Modulo01/687-Modulo01/enaho01-2019-100.dta",
                           convert_categoricals=False)

enaho02 = pd.read_stata(r"../../../datos/2019/687-Modulo02/687-Modulo02/enaho01-2019-200.dta",
                           convert_categoricals=False)
 
enaho03 = pd.read_stata(r"../../../datos/2019/687-Modulo03/687-Modulo03/enaho01a-2019-300.dta",
                           convert_categoricals=False)

enaho04 = pd.read_stata(r"../../../datos/2019/687-Modulo04/687-Modulo04/enaho01a-2019-400.dta",
                           convert_categoricals=False)


enaho05 = pd.read_stata(r"../../../datos/2019/687-Modulo05/687-Modulo05/enaho01a-2019-500.dta",
                           convert_categoricals=False)

enaho34 = pd.read_stata(r"../../../datos/2019/687-Modulo34/687-Modulo34/sumaria-2019.dta",
                           convert_categoricals=False)


enaho37 = pd.read_stata(r"../../../datos/2019/687-Modulo37/687-Modulo37/enaho01-2019-700.dta",
                           convert_categoricals=False)


enaho02 = enaho02[["conglome", "vivienda", "hogar" , "codperso",
                  "ubigeo", "dominio" ,"estrato" ,"p208a", "p209",
                  "p207", "p203", "p201p" , "p204",  "facpob07"]]

enaho03 = enaho03[["conglome", "vivienda", "hogar" , "codperso",
                  "p301a", "p301b", "p301c" , "p300a"]]


enaho05 = enaho05[["conglome", "vivienda", "hogar" , "codperso",
                  "i524e1", "i538e1", "p558a5" , "i513t", "i518",
                  "p507", "p511a", "p512b", "p513a1", "p505" , "p506", "d544t", "d556t1",
                  "d556t2" , "d557t" , "d558t" , "ocu500" , "i530a" , "i541a"]]

num = ["34","37"]

merge_hog = enaho01

merge_hog['ubigeo_pr'] = merge_hog['ubigeo'].str[:2]
merge_hog['ubigeo_pr'] = merge_hog['ubigeo'].str[:2]+"0000"


merge_hog = merge_hog[merge_hog.ubigeo_pr.isin(["15","03","04","12"])]


for i in tqdm(num):
    merge_hog = pd.merge(merge_hog, globals()[f'enaho{i}'], 
                         on = ["conglome", "vivienda", "hogar"],
                       how = "left", 
                       suffixes=('', '_y'),
                       validate = "1:1")
    

num = ["03"]
merge_ind = enaho02

merge_ind['ubigeo_pr'] = merge_ind['ubigeo'].str[:2]

merge_ind= merge_ind[merge_ind.ubigeo_pr.isin(["15","03","04","12"])]

for i in tqdm(num):
    merge_ind = pd.merge(merge_ind, globals()[f'enaho{i}'], 
                         on = ["conglome", "vivienda", "hogar","codperso"],
                       how = "left", 
                       suffixes=('', '_y'),
                       validate = "1:1")

# Merge hogares e individuos 

merge_base_2019 = merge_ind.merge(merge_hog, 
                            on = ["conglome", "vivienda", "hogar"],
                            how = "left",
                            validate = "m:1",
                            suffixes=('', '_y'),
                            )

## Drop variables que termina en _y ##

index_columns = np.where( merge_base_2019.columns.str.contains('_y$', regex=True))[0]

merge_base_2019.drop(merge_base_2019.columns[index_columns], axis = 1, inplace = True)





#%% Append


merge_append = merge_base_2020.append(merge_base_2019, ignore_index = True)


#ignore_index= True : no haya conflictos de indexing 

merge_append.to_stata("../../../append_enaho.dta", write_index = False)

# write_index=False: no guardar con una columan de index


#%% Poverty mesure

# 1) Ingreso per capita mensual
# 2) Gasto per capita mensual

# inghog1d: ingreso anual del hogar 
# gashog2d: gasto anual del hogar
# mieperho: integrantes del hogar
# ingreso_month: ingreso per capita mensual 
# gasto_month: gasto per capita mensual

merge_base_2020["ingreso_month"] = merge_base_2020["inghog1d"]/(12*merge_base_2020["mieperho"])

merge_base_2020["gasto_month"]  = merge_base_2020["gashog2d"]/(12*merge_base_2020["mieperho"])


#Generamos variable "pobre" mediante comparación gasto y linea de pobreza

merge_base_2020["pobre"] = np.where(
    merge_base_2020["gasto_month"] < merge_base_2020["linea"], 
                                    "pobre", "no pobre")

# En este caso gasto_month ni linea presentan missing, si alguno tuviera missing, 
# entonces la dummy debe generar missing 

merge_base_2020["pobre"] = np.where( 
    merge_base_2020[['gasto_month','linea']].isnull().any(axis=1), np.nan,  # si al menos uno es missing, se coloca nan
    np.where(  # caso se contrario se aplica la condición que me interesa
    merge_base_2020["gasto_month"] < merge_base_2020["linea"], 
                                    "pobre",
                                    "no pobre")
    )

# value_counts hallar la frecuancia absoluta segun los valores de la variables 

merge_base_2020["pobre"].value_counts()


merge_base_2020["pobre"].unique()

# np.where(Condicioón, colocar v si es verdadero, colcoar w si es falso)

merge_base_2020["dummy_pobre"] = np.where(
    merge_base_2020["gasto_month"] < merge_base_2020["linea"],
                                    1, 0)


# tomando en cuenta si existe algún missing 

merge_base_2020["dummy_pobre"] = np.where( 
    merge_base_2020[['gasto_month','linea']].isnull().any(axis=1), np.nan,  # si al menos uno es missing, se coloca nan
    np.where(  # caso se contrario se aplica la condición que me interesa
    merge_base_2020["gasto_month"] < merge_base_2020["linea"], 
                                    1,
                                    0)
    )
    
    
#Generamos variable "pc_pobre" recodificando variable "pobreza"


merge_base_2020["pc_pobre"] = merge_base_2020["pobreza"].replace({1: "Pobre extremo", 
                                                  2: "Pobre",
                                                  3: "No pobre"})

merge_base_2020["pobreza"]
merge_base_2020["pc_pobre"]

merge_base_2020["pc_pobre"].unique()

# pobreza (1 pobre extremo, 2 pobre no extremo, 3 no pobre)

###############################################
################ Dummies ######################
###############################################

merge_base_2020["p301a"].unique()
merge_base_2020["p301a"].value_counts()

merge_base_2020["p301a"].replace({np.nan: 99}, inplace =True)


# reempalzar 99 de valores perdidos por missing 

merge_base_2020["p301a"].replace({99:np.nan}, inplace =True)


merge_base_2020["p301a"].unique()
merge_base_2020["p301a"].value_counts()


#Replace missing values 

merge_base_2020["p301a"].replace({99: np.nan}, inplace =True)

#Generate dummies

pd.get_dummies(merge_base_2020["p301a"])


# juntar las varibales dummies en el dataframe

levels = len(merge_base_2020["p301a"].unique()) - 1  # no tomar en cuenta la cageoria NA



merge_base_2020[ [f"var_{i+1}" for i in range(levels)] ] = pd.get_dummies(merge_base_2020["p301a"])


# alternativa 

merge_base_2020 = pd.concat([ merge_base_2020 , pd.get_dummies(merge_base_2020["p301a"]) ], axis = 1 )

# axis = 1, se junta las bases de forma horizontal 

merge_base_2020.columns

merge_base_2020[10]


#%% Collapse - groupby

# maximo nivel educativo alcanzado, dummy si educación superior, menor nivel alcanzado 

df1 = merge_base_2020.groupby( [ "conglome", "vivienda", "hogar" ]).agg( edu_max = ( 'p301a', np.max ) ,
                                                       edu_min = ( 'p301a', np.min ) ,
                   total_miembros1 = ('conglome', np.count), # Contabiliza incluso los missings
                   total_miembros2 = ('conglome', np.size),  # ignore  NA missing
                        sup_educ = ( 'var_10', np.sum ))

# ignorar missing np.namax, np.nasum

df1 = merge_base_2020.groupby( [ "conglome", "vivienda", "hogar" ]).agg( edu_max = ( 'p301a', np.nanmax ) ,
                                                       edu_min = ( 'p301a', np.nanmin ) ,
                   total_miembros2 = ('conglome', np.size),  # ignore  NA missing
                        sup_educ = ( 'var_10', np.nasum ))
#   as_index = true (default), las varibales de agrupamiento son variables indenxing
   
   
df1['vivienda'] # no existe
                         
df1['hogar'] # no existe
    
# var_10: universitaria completa    

# ""           var_10
# 1023 12 01   1    
# 1023 12 01   0     
# 1023 12 01   1                                                   
# ""           


# as_index = False  genera que "conglome", "vivienda", "hogar"  sea parte de la base de datos 
                                       
df1 = merge_base_2020.groupby( [ "conglome", "vivienda", "hogar" ],
                              as_index = False ).agg( edu_max = ( 'p301a', np.nanmax ) ,
                                                       edu_min = ( 'p301a', np.nanmin ) ,
                        total_miembros = ('conglome', np.size),  # count no missings
                        sup_educ = ( 'var_10', np.nansum ))
      
                                                     
df1['vivienda'] # se puede verificar que vivienda pertenece a la base de datos
                     
        
df2 = merge_base_2020.groupby( [ "ubigeo_dep2" ],
                                  as_index = False ).agg( index_poverty = ( 'dummy_pobre', np.nanmean ))                                               
                                   

#    Dummy
# 1  0
# 2  1
# 3  0
# 4  1

# (1+0+1+0)/4 = 2/4 = 0.5, promedio de una dummy es un porcentaje !!

                  
merge_base_2020["dpto"] = merge_base_2020["ubigeo_dep2"].replace({
    "15": "Lima","03": "Apurimac","04": "Arequipa"
                                            })


df3 = merge_base_2020.groupby( [ "dpto" ],
                                  as_index = False ).agg( index_poverty = ( 'dummy_pobre', np.nanmean ))                                                
                           

                          
#Tabla de comparación  value_counts similar tab in stata



print("*-----------------------------------*")
print("Comparación de variables de pobreza")
print("*-----------------------------------*")
print("Pobreza")
print(merge_base_2020["pobre"].value_counts())
print("*-----------------------------------*")
print("pc pobre")
print(merge_base_2020["pc_pobre"].value_counts())
print("*-----------------------------------*")
print("Dummy pobre")
print(merge_base_2020["dummy_pobre"].value_counts())
print("*-----------------------------------*")


# Cross tab (tabla cruzada)  tab var1 var 2 in stata

pd.crosstab(merge_base_2020["dpto"], merge_base_2020["dummy_pobre"])

#Generamos tablas sin ponderador

pd.crosstab([merge_base_2020["estrsocial"]], 
            merge_base_2020["pc_pobre"] , margins=True)

#Tasa de pobreza usando factor expansión / ponderador facpob07

calc = wc.Calculator("facpob07")

apurimac = merge_base_2020[ merge_base_2020["dpto"] == "Apurimac" ]

# Ahora la tasa de pobreza segun (pobre extremo, pobre o no pobre) pues toma en cuenta
# el factor de expansión 

calc.distribution(apurimac,"pc_pobre").round(3).sort_values(ascending=False)


"References: "

# https://pandas.pydata.org/docs/reference/api/pandas.read_stata.html



