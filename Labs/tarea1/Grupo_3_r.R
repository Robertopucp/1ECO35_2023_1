install.packages("dplyr")

library("dplyr")

paste("Facultad de Ciencias Sociales\ 2023")

-3.1416 %>%       # toma el valor absoluto de -3.1416
  abs() %>%       
  # eleva al cuadrado
  `^`(2) %>%      
  # toma el logaritmo
  log() %>%       
  # convierte a entero
  as.integer()    
