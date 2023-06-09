# Solucionario WG2

import random

#%% Condicionales

#%%% 1.

##Use un condicional que imprima el valor de la compra y el tipo de pago.

## Asuma tres tipos de pagos: pago en efectivo, pago con tarjeta de crédito y pago con tarjeta de débito. 
## El tipo de pago depende del monto de la compra. 
## Si gasta hasta S/.100, se paga con dinero en efectivo.
## Por otro lado, si gasta más de S/.100 pero menos de S/.300, se paga con tarjeta de débito. 
## Si no, pago con tarjeta de crédito.
## Además, si la compra es mayor a 300 soles, se hace un descuento de 10%.

## Un ejemplo de lo que debe imprimir: "Compra de 150 soles y pago en efectivo"

x = random.randint(1, 400) # Generamos un número entero aleatorio entre 1 y 400 (puede ser cualquier rango > 300).
print(x) # Notamos cuál es el número.

# Aplicamos los condicionales:
    
if x <= 100:
    print(f"Compra de {x} soles y pago en efectivo.")
elif x < 300:
    print(f"Compra de {x} soles y pago con tarjeta de débito.")
else:
    x = 0.9 * x
    print(f"Compra de {x} soles y pago con tarjeta de crédito.")

 
#%%% 2.
## Elaborar una estructura condicional.

aleatorio = random.sample( range(501) , 1)

if x<=100:
    y = x**0.5 # Si x se encuentra entre 0 y 100, se le eleva a la 1/2.
    
elif (x>100 & x<=300):
    y = x - 5 # Si x se enuentra entre 100 y 300, se le resta 5.
    
elif x>300:
    y = 50 # Si x es mayor a 300, f(x) = 50
    
print(f'x={x}') # x resulta cualquier número hasta 500 de manera aleatoria.
print(f'F(X)={y}') # Comprobamos que se cumple la función.


#%% Loops

## Sea los siguientas utilidades netas anuales de varias empresas medidas en millones
## (100, 152, -1 , 8, 12, 156, 35, -10, 100, -0.5, 30, 1050 , 7, -10)

## Use el loop for que imprima lo siguiente "La utilidad neta anual es (monto) ".
## Además, el loop no debe imprimir las utilidades negativas (use next y continue según corresponda), 
##y, finalmente, el loop debe deternerse si la utilidad supera los 1000 millones.

# Creamos la lista que contiene a las utilidades propuestas:
utilidades = [100, 152, -1, 8, 12, 156, 35, -10, 100, -0.5, 30, 1050, 7, -10]

# Loop:
for i in range(len(utilidades)):
    if utilidades[i]<0:
        next ## Si las utilidades son menores a 0 (negativas), use next.
    else:
        if utilidades[i]>1000: # Si utilidad > 1000, se detiene el loop.
            print("Loop detenido. Utilidad mayor a 1000 millones.")
            break # Significa que el loop se detenga y siga a la siguiente secuencia.
        else:
            print(f'La utilidad neta anual es {utilidades[i]} millones.')
