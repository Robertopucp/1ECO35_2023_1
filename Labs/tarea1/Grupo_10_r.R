library(dplyr)
library(stringr)

# Operador paste
c1 <- "Facultad de Ciencias Sociales"
a <- paste(c1,'2023', sep = " / ") 
print(a)

## Se utiliza el separador "/" 

# Operador pip %>%
abs(-3.1416) %>% sqrt() %>% log() %>% round(0)

## Se está aplicando valor absoluto, luego raíz cuadrada,
## luego logaritmo y finalmente se vuelve número entero.