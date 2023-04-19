# PASTE ----

## En primer lugar, escribimos la primera mitad de la frase que queremos crear, y verificamos que se ha creado la variable
c1 <- 'Facultad de Ciencias '
print (c1)

## En segundo lugar, unimos con la funcion paste0
a <- paste0(c1,'Sociales \" 2023') 
print(a) #tuvimos problemas para incluir el backslash solo ya que R lo lee como una función

## Otro ejemplo
paste("Facultad", "de", "Ciencias", "Sociales", "\\", "\ 2023")

# OPERADOR PIP ----
## Usamos el operador para tomar el valor absoluto de la cifra -3.1416. Tambien elevamos al acuadrado, tomamos logaritmo y finalmente, convertimos al número entero.

-3.1416 %>% abs() %>% round(2) %>% log() %>% as.integer()