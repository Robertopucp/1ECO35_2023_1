{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": 1,
   "id": "73592f93",
   "metadata": {
    "scrolled": true
   },
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Ejercicio_1\n"
     ]
    }
   ],
   "source": [
    "print(\"Ejercicio_1\")"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 2,
   "id": "0e2821a6",
   "metadata": {},
   "outputs": [
    {
     "name": "stdout",
     "output_type": "stream",
     "text": [
      "Facultad de Ciencias Sociales\\2023\n"
     ]
    }
   ],
   "source": [
    "a=2023\n",
    "print(f'Facultad de Ciencias Sociales\\{a}')  #Se usa el f-string para poder crear esa frase que contenga a \"a\" que sera leido como un objeto de texto"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 3,
   "id": "db213e9e",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "'China'"
      ]
     },
     "execution_count": 3,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "listas = [\"australia\", \"Brunei Darussalam\", \"Canadá\", \"Corea\", \"Chile\", \"China\", \"Estados Unidos\", \"Filipinas\", \"Hong Kong\", \"Indonesia\", \"Japón\", \"Malasia\"]\n",
    "listas [5]      #se crea un alista de los paises y para obtener china, se busca su numero de orden que es 5"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 4,
   "id": "ce704ff3",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "'Malasia'"
      ]
     },
     "execution_count": 4,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "listas [11]  #El orden de malasia en la lista es de 11"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 5,
   "id": "8f2a26e6",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "['China', 'Estados Unidos', 'Filipinas', 'Hong Kong', 'Indonesia']"
      ]
     },
     "execution_count": 5,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "listas [5:10]   #para extraer esa sucesion de palabras se toma de 5 a 10, que es el orden de China y un puesto más adelante que indonesia "
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 13,
   "id": "0f7cf7be",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "{'Numero atómico': [1, 6, 47, 88],\n",
       " 'Masa atómica': [1.008, 12.011, 107.87, 226],\n",
       " 'Familia': ['No metal', 'No metal', 'Metal', 'Metal']}"
      ]
     },
     "execution_count": 13,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "letras = ['Numero atómico','Masa atómica','Familia']\n",
    "contenido = [[1, 6, 47, 88], [1.008, 12.011, 107.87, 226], ['No metal', 'No metal', 'Metal', 'Metal']]\n",
    "\n",
    "ct_pc = dict( zip( letras , contenido) )  \n",
    "ct_pc"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 7,
   "id": "5cd38cde",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "'Metal'"
      ]
     },
     "execution_count": 7,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "ct_pc['Familia'][3]     #extraigo metal, del grupo Familia, y como es la palabra en el orden 3, pongo el numero 3"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 8,
   "id": "a9a0814d",
   "metadata": {},
   "outputs": [
    {
     "data": {
      "text/plain": [
       "47"
      ]
     },
     "execution_count": 8,
     "metadata": {},
     "output_type": "execute_result"
    }
   ],
   "source": [
    "ct_pc['Numero atómico'][2]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": 31,
   "id": "7e5bba37",
   "metadata": {},
   "outputs": [],
   "source": [
    "letra = ['books']\n",
    "libros = ['cien años de soledad, los miserables, historia de dos  ciudades']\n",
    "books = dict( zip(letras , libros ) )"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3 (ipykernel)",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.10.9"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
