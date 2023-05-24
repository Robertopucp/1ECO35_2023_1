# -*- coding: utf-8 -*-
"""
@author: Roberto
"""



# import libraries

import pandas as pd
import numpy as np
import re  # for regular expressions (REGEX)
import os  # for directorio
import swifter  # for parallel procesing
import unidecode # to drop tildes
from datetime import datetime  # library for time

# !pip install swifter
# !pip install xlrd

user = os.getlogin()   # Username

print(user)

# Set directorio

os.chdir(f"C:/Users/{user}/Documents/GitHub/1ECO35_2022_2/Lab8") # Set directorio

data = pd.read_excel("../data/Centro_salud/Centro_salud_mental.xls") # Subir base de datos
data


 # from capital letters to lower 

data.columns = map(str.lower, data.columns)

# map es el loop replacement que permite aplicar la función str.lower a cada elemento de la lista data.columns


#%% regex re.sub


"1.0 extraccción de texto"

# Extraer solo texto de una celda que contiene numero y texto 

data['inst1'] = data['institución_ruc'].apply(lambda x: re.sub('[0-9]','',x))

# re.sub es de sustución 

"[0-9]*: ninguno, uno o más digitos"


# Usando una librería de parallel procesing para que la computadora sea rápida

data['inst1'] = data['institución_ruc'].swifter.apply(lambda x: re.sub('[0-9]','',x))
data['inst1'] = data['institución_ruc'].swifter.apply(lambda x: re.sub('[0-9]','',x))

# Alternativas 1

data['inst2'] = data['institución_ruc'].apply(lambda x: re.sub('\d','',x))


# Alternativas 2

data['inst3'] = data['institución_ruc'].apply(lambda x: re.sub('[^a-zA-Z\s]','',x))


"2.0 extraccción de numeros"


# Alternativas 1

data['ruc1'] = data['institución_ruc'].apply(lambda x: re.sub('[a-zA-Z]','',x))


# Alternativas 2

data['ruc2'] = data['institución_ruc'].apply(lambda x: re.sub('[a-zA-Z\s]','',x))

# Alternativas 3

data['ruc3'] = data['institución_ruc'].apply(lambda x: re.sub('\D','',x))

#\D: caracteres diferetnes de digitos

# Alternativas 4

data['ruc4'] = data['institución_ruc'].apply(lambda x: re.sub('[^0-9]','',x))

# Sustituir caracteres diferentes a la fecha

data['fecha_apertura'] = data['fecha_apertura'].apply(lambda x: re.sub('(:00:00)|(!%&)|(00/00/00)','',x))

# | permite incluir varias opciones (disyuntivo 0 )

#%% re.findall


data['coordinates1'] = data['gps'].apply(lambda x: re.findall('-\d+.\d+,-\d+.\d+',x))


#Se obtiene error pues la variable gps celdas nan que son float y no string

# columa de string con nan, pero nan es float (en STATA sucede lo mismo)


data['coordinates2'] = data['gps'].apply(lambda x: re.findall('-\d+.\d+,-\d+.\d+', str(x)  ) )

# se captura la presencia de más de un digito \d+, str(x): transforma a texto cada fila de la columna 

data['coordinates3'] = data['gps'].apply(lambda x: re.findall('-\d{1,2}.\d{1,3},-\d{2}.\d{1,4}', str(x)  ))

# en lugar de indicar uno o más digitos con \d+, indicad la cantidad de digitos {1,2} de 1 a dos dígitos
# \d{1,4} de uno a cuatro digitos 

data['coordinates4'] = data['gps'].apply(lambda x: re.findall('-\d*.\d*,-\d*.\d*', str(x))  )

#\d* ningun caso, uno + más casos 

# Se retira los elementos dentro del output re.findall que es una lista

data['coordinates4']= data['coordinates4'].apply(lambda x: ''.join(x) )

data.info()

#%% re.search


# extracción del nombre de distrito y región 


x = "AVENIDA LA PAZ CUADRA 3 LA PERLA CALLAO CALLAO distrito LA PERLA Region El CALLAO"

re.search('\.*[D/d]istrito\s([\w+\-\s]+)\s[R/r]egion\s([\w+\s]+)', x).group(1)



re.search('\.*[D/d]istrito\s([\w+\-\s]+)\s[R/r]egion\s([\w+\s]+)', x).group(2)


#\\.* : captura ninguna, una, o más de un caracter (cualquiera: espacios, letras, numeros, #!%&/())
# () permite capturar lo que me interesa. Puede acceder a cada grupo de interés mediante .group()



def dist_region(x):
    
    output =  re.search('\.*[D/d]istrito\s([\w+\-\s]*)\s[R/r]egion\s([\w+\s]*)',x)
    
    return output.group(1), output.group(2)
 
     
data['distrito'] = data['dirección'].apply(lambda x: dist_region(x)[0])


data['region'] = data['dirección'].apply(lambda x: dist_region(x)[1])


# try and except cuando la celda tiene missing, simplemente me devolverá None

def phone(x):
    
    try:
        match = re.search('\.*\s(\d+\-\d+)', str(x))
    
        return match.group(1)
    
    # Si no encuentra un grupo por precencia de missing, se ejecuta la siguiente linea
    
    except:
        
        pass

data['phone'] = data['telefono'].apply(lambda x: phone(x))

data['phone'] = data['telefono'].apply(phone)


def reso_info(x):
    
    match = re.search('DS-([0-9]+)-([0-9]+)\s([A-Z]+)', x)
    
    return match.group(1), match.group(2), match.group(3)


data['code_res'] = data['resolucion'].apply(lambda x: reso_info(x)[0])

data['year_res'] = data['resolucion'].apply(lambda x: reso_info(x)[1])

data['entidad_res'] = data['resolucion'].apply(lambda x: reso_info(x)[2])


# Extracción del número telefónico 

# Se coloca str(x) pues tenemos casos de float (nan)
# Colocamos * para el caso de las celdas vacias "". Cuando no se cumpla la condición me devuelva vacío ""

data['telefono1'] = data['telefono'].apply(lambda x: re.search("\.*([\d+\-\d+]*)$", str(x)).group(1))


data['telefono2'] = data['telefono'].apply(lambda x: re.search("\.*([...\-\d+]*)$", str(x)).group(1))


# extraer usuario de correo 

correo = "rmendozu4am@pup.edu.pe"

re.search("(\w+)\@\.*", correo).group(1)


#%% str.contains

data['Gob_regional_jur'] =  np.where(data['institución_ruc'].str.contains('(^G)|(^R)', regex=True),1,0)


data['Minsa_jur'] =  np.where(data['institución_ruc'].str.contains('^M', na = False),1,0)

# na = False no toma cuenta los missing

#%% Look around

# positive lookahead (?=)


data['apertura1'] = data['horario'].apply(lambda x: re.search("\d+\:\d+(?= am)",x).group())

# Solo debe colocarse group()

data['apertura2'] = data['horario'].apply(lambda x: re.search("[\d+\:]+(?= am)",x).group())

# positive lookbehind (?<=)

data['apertura3'] = data['horario'].apply(lambda x: re.search("(?<=apertura )[\d+\:]+",x).group())


# negative lookbehind (?<!)


data['cierre1'] = data['horario'].apply(lambda x: re.search("(?<!apertura )\d+\:\d+",x).group())

# negative lookbahead (?!)

data['pres_soles'] = data['presupuesto'].apply(lambda x: re.search("[\d*\,]+(?!\$)",x).group())


 # \b: el string no está rodeado de letras o numeros
# \B: el string está rodeado de letras o numeros

data['pres_soles2'] = data['presupuesto'].apply(lambda x: re.findall("\w+\B[\d+\,]+\B",x)[0])

# retirar tildes del texto

data['presupuesto'] = data['presupuesto'].apply(lambda x: unidecode.unidecode(x))


data['presupuesto'] = data['presupuesto'].apply(unidecode.unidecode)


#%% Fechas


data['fecha_apertura_date'] = pd.to_datetime(data['fecha_apertura']
                                             , dayfirst = True).dt.strftime('%d/%m/%Y')

# dayfirst = True se indica la columna fecha_apertura 
# empieza con el día, luego con strftime se indica el formato dia/mes/año de la fecha 


#%% Segunda aplicación, filtro de filas

junin = pd.read_excel("../data/Region_Junin.xlsx")



# select districs that contanis "AC"

junin.loc[ junin['District'].str.contains('AC') ]


junin.loc[junin['Place'].str.contains('pacha')]


junin.loc[junin['Place'].str.contains('pacha', flags = re.I)]

 # re.I ignoring capital o lower letters 
 
 
 # Ignoring format re.I 

junin.loc[junin['District'].str.contains('CIUDAD', flags = re.I)]



# If regex is False, then contains seeks "^hu" literaly 
# # Beginning word with hu

junin.loc[junin['District'].str.contains('^hu', flags = re.I, na = False, regex = True)]

 # regex True para identificar la expression regular, na = False para ignorar missing

# Ending word

junin.loc[junin['Place'].str.contains('ro$', flags = re.I, na = False, regex = True)]


junin.loc[junin['Place'].str.contains('ca$', flags = re.I, na = False, regex = True)]

# match : a , c , ac

newbase  = junin.loc[junin['Place'].str.contains('^ac*', flags = re.I, na = False, regex = True)]


# match : ac (strict)

newbase  =  junin.loc[junin['Place'].str.contains('^ac+', flags = re.I, na = False, regex = True)]

# match a or c

newbase  =  junin.loc[junin['Place'].str.contains('^ac?', flags = re.I, na = False, regex = True)]


