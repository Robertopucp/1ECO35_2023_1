#TAREA 5
#PARTE EN R Y PYTHON
#Primero instalamos todos los paquetes necesarios para desarrollar el ejercicio:
rm(list = ls())
graphics.off()
cat("\014")
library(pacman)
p_load(dplyr, readxl, tidyverse, foreign, ggthemes, datos)
#Segundo, cargamos la base de datos:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#----------------------------------------------------------------------------------------------------
#PLOT
#Abrimos la base de datos:
prod <- read_excel("../../data/produccion_coca/6.1.1_-_Illicit_coca_bush_cultivation.xlsx", range = "6_1_1!A3:M7", col_names = TRUE)
prod <- prod[-1, ]
nuevos_nombres <- c('País', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
colnames(prod) <- nuevos_nombres

prod <- prod %>%
  mutate(País = ifelse(País == "Colombia a", "Colombia", País)) %>%
  mutate(País = ifelse(País == "Peru b", "Peru", País))

prod_transformado <- prod %>%
  pivot_longer(cols = -País, names_to = "años", values_to = "valor")

#Ya arreglamos la base de datos 
#Ahora graficamos
summary(prod)
attributes(prod)$label.table

prod_transformado <- prod_transformado[order(prod_transformado$País, prod_transformado$años), ]

# Convertir la columna "años" a tipo numérico
prod_transformado$años <- as.numeric(prod_transformado$años)

# Crear el gráfico de líneas
grafico_lineas <- ggplot(prod_transformado, aes(x = años, y = valor, color = País, shape = País)) +
  geom_line() +
  geom_point(size = 1.5, show.legend = FALSE) +
  labs(x = "year", y = "Coca Production", title = "Figure 1: Global illicit cultivation of coca bush, 2009-2020 (hectares)", caption = "Source: Plurinational State of Bolivia, Colombia, Peru - UNODC") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0, margin = margin(t = 10)),
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 20))) +
  scale_color_manual(values = c("#104E8B", "#66CD00", "#B22222")) +
  guides(shape = guide_legend(override.aes = list(color = NULL)))

# Eliminar la etiqueta "País" de la leyenda
grafico_lineas <- grafico_lineas +
  labs(color = NULL)

# Mostrar el gráfico
print(grafico_lineas)

#Guardar
ggsave("../../output/plots/G9_Productionr.png",
        bg = "white"
       , height = 7  # alto
       , width = 9  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)


#-------------------------------------------------------------------------------------
#ERRADICACIÓN DE COCA
err <- read_excel("../../data/produccion_coca/6.1.2_-_Eradication_of_coca_bush.xlsx", range = "6.1.2!C2:Q5", col_names = TRUE)
err <- err[, -c(2:3)]
nuevos_nombres <- c('País', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
colnames(err) <- nuevos_nombres
err_transformado <- err %>%
  pivot_longer(cols = -País, names_to = "años", values_to = "valor")

err_transformado <- err_transformado[order(err_transformado$País, err_transformado$años), ]

# Convertir la columna "años" a tipo numérico
err_transformado$años <- as.numeric(err_transformado$años)

# Crear el gráfico de líneas
grafico_lineas <- ggplot(err_transformado, aes(x = años, y = valor, color = País, shape = País)) +
  geom_line() +
  geom_point(size = 1.5, show.legend = FALSE) +
  labs(x = "year", y = "Coca Erradication", title = "Figure 2: Reported eradication of coca bush, 2009-2020", caption = "Source: United Nations Office on Drugs and Crime.") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0, margin = margin(t = 10)),
        plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 20))) +
  scale_color_manual(values = c("#104E8B", "#66CD00", "#B22222")) +
  guides(shape = guide_legend(override.aes = list(color = NULL)))

# Eliminar la etiqueta "País" de la leyenda
grafico_lineas <- grafico_lineas +
  labs(color = NULL)

# Mostrar el gráfico
print(grafico_lineas)
#Guardar
ggsave("../../output/plots/G9_Erradicationr.png",
       bg = "white"
       , height = 7  # alto
       , width = 9  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)
#------------------------------------------------------------------------------------
#Realizar un gráfico con la producción y erradicación de hoja de coca en el Perú.

# Combinar las bases de datos por la columna "años"
combined_data <- merge(prod_transformado, err_transformado, by = c("años", "País"), all = TRUE)
#Nos quedamos solo con Perú
peru_data <- combined_data %>%
  filter(País == "Peru")

peru_data <- combined_data %>%
  filter(País == "Peru") %>%
  rename(production = valor.x, erradication = valor.y)

# Mostrar el resultado
print(peru_data)

#Graficamos
source_text <- "Source: United Nations Office on Drugs and Crime and Plurinational State of Bolivia, Colombia, Peru - UNODC"

grafico_lineas_peru <- ggplot(peru_data, aes(x = años)) +
  geom_line(aes(y = production, color = "Production")) +
  geom_line(aes(y = erradication, color = "Erradication")) +
  geom_point(aes(y = production, color = "Production"), shape = 16, size = 3) +
  geom_point(aes(y = erradication, color = "Erradication"), shape = 17, size = 3) +
  labs(x = "Year", y = "Quantity", title = "Coca Production and Eradication in Peru",
       caption = str_wrap(source_text, width = 90)) +
  scale_color_manual(values = c("#104E8B", "#B22222"), labels = c("Production", "Erradication")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centrar el título
    axis.title.x = element_blank(),  # Eliminar etiqueta del eje x
    legend.position = "bottom",  #la leyenda en la parte de abajo
    legend.box = "horizontal",  
    legend.title = element_blank(),  # Eliminar el título de la leyenda
    legend.spacing.x = unit(0.2, "cm"),  # Espacio entre los elementos de la leyenda
    plot.caption = element_text(hjust = 0)  # Alinear el texto de la fuente a la izquierda
  )

# Mostrar el gráfico
print(grafico_lineas_peru)
#guardar
ggsave("../../output/plots/G9_Perur.png"
       , height = 7  # alto
       , width = 9  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)
########################################################################################
#Regex

