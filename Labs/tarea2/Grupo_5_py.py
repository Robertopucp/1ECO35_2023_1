#!/usr/bin/env python
# coding: utf-8

# # **Script en Python**

# ### Condicionales 

# In[ ]:


# Pregunta 1

compra = int(input("¿Cuánto dinero gastas? ")) #se agrega un cuadro para que escojan el valor del gasto
if compra < 100:
    print("compra de",compra,"soles y pago en efectivo")
elif 100 < compra < 300:
    print("compra de",compra,"soles y pago en débito")
elif compra >= 300:
    print("compra de",compra,"soles y pago en crédito")


# In[ ]:


# Pregunta 2

x = int(input("¿valor de x? "))
if 0 < x < 100: #si se cumple la condición, entonces se resuelve la función Fx
    Fx=x**(1/2)
    print("F(x) es",Fx) #se devuelve el valor de F(x) 
elif 100 < x < 300:
    Fx=x-5
    print("F(x) es",Fx) 
elif x >= 300:
    Fx=50
    print("F(x) es",Fx) 
else:
    print ("0, X es un número negativo")


# La función aplicará las condiciones de acuerdo al valor que se le otorgue a X.

# ### Loops en Python 

# In[ ]:


utilidadesanuales = [100, 152, -1 , 8, 12, 156,35, -10, 100, -0.5, 30, 1050 , 7, -10]

for utilidad in utilidadesanuales:
    if utilidad < 0:     
        continue    #para que no considere los valores negativos y salte al siguiente
    if utilidad > 1000 :
        break       #si supera 1000, se rompe el loop
    else:
        print("La utilidad neta anual es", utilidad, "millones")


# Según lo solicitado, el loop debe detenerse si la utilidad supera los 1000 millones, por lo que se realiza el break despues de "if utilidad > 1000 " y el loop ya no considera los siguientes valores. Por ello, el 7, a pesar de que no sea una utilidad negativa no es considerada por el loop.
