################  Laboratorio 6 ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza 

# clean environment variables

rm(list = ls())

# clean plots
graphics.off()

# clean console

cat("\014")

# additional options
options(scipen = 999)      # No scientific notation

# Library ####

library(pacman) 


p_load(readxl, tidyverse, foreign, ggthemes)

# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#--------------------------------------------------------------#

value_label <- read.spss("../../data/Riesgo_morosidad.sav"
)

# etiqueta de valores

attributes(value_label)$label.table

#----------------------------------------------------------#

datospss <- read.spss("../../data/Riesgo_morosidad.sav",
                      use.value.labels = F, 
                      to.data.frame = TRUE)

attributes(datospss)$variable.labels  # etiqueta de variable 

# Generamos la figura

ggplot(datospss) + aes(morosidad)

# Generando variables categóricas

datospss$sexo      <- factor(datospss$sexo, levels = c(1, 2),
                             labels = c("Masculino", "Femenino"))

datospss$morosidad <- factor(datospss$morosidad, levels = c(1,2),
                             labels = c("No moroso", "Moroso"))

datospss$fonopart  <- factor(datospss$fonopart, levels = c(1, 2),
                             labels = c("No", "Si"))

datospss$fonolab   <- factor(datospss$fonolab, levels = c(1, 2),
                             labels = c("No", "Si"))

datospss$autovaluo <- factor(datospss$autovaluo, levels = c(1, 2),
                             labels = c("No", "Si"))

datospss$esaval    <- factor(datospss$esaval, levels = c(1, 2),
                             labels = c("No", "Si"))

datospss$tieneaval <- factor(datospss$tieneaval, levels = c(1, 2),
                             labels = c("No", "Si"))

datospss$tiporenta <- factor(datospss$tiporenta, levels = c(2, 3),
                             labels = c("Fijo", "Variable"))

datospss$dpto      <- factor(datospss$dpto, 
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c("Lima", "Trujillo", "Arequipa",
                                        "Cusco", "Ica", "Piura"))


## Gráfico de barras ----------------------------------------

ggplot(datospss) + aes(morosidad) +
  geom_bar(stat = "count")  # por default es count de contabilizar

# equivalencias

ggplot(datospss) +  aes(x = morosidad) + geom_bar() # es equivalente
ggplot(datospss, aes(morosidad)) + geom_bar()     # es equivalente

# Cambio de orientación ( vertical )

ggplot(datospss) +  aes(y = morosidad) + geom_bar()

# usando coord_flip

ggplot(datospss) + aes(morosidad) + geom_bar() + 
  coord_flip()

# Temas de fondo con theme()

ggplot(datospss) + aes(morosidad) +
  geom_bar() + theme_test()

ggplot(datospss) + aes(morosidad)  +
  geom_bar() + 
  theme_bw(12) 

# Añadiendo titulo principal y titulo en ejes

ggplot(datospss, aes(morosidad)) + geom_bar()+
  labs(title = "Gráfico de Barras de morosidad", 
       x = "Condición de la morosidad", 
       y = "Frecuencia absoluta") 



#### Colores  ---------------------------------------

browseURL("http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf")

# asignamos colores

ggplot(datospss, aes(morosidad)) +
  geom_bar(color = "blue", fill = "white")+
  theme_test() +
  labs(title = "Gráfico de Barras de morosidad",
       x = "Condición de la morosidad", 
       y = "Frecuencia absoluta") 

# Diferentes colores

ggplot(datospss, aes(morosidad)) +
  geom_bar(color = "blue", fill = c("white","orange"))+
  theme_test() +
  labs(title = "Gráfico de Barras de morosidad",
       x = "Condición de la morosidad", 
       y = "Frecuencia absoluta") 

# Colores en formato hexadecimal (RGB: red, bkue and green)

browseURL("https://www.rapidtables.com/web/color/RGB_Color.html")


ggplot(datospss) + aes(morosidad) + 
  geom_bar(color = "#0000FF", fill = c("#FFFFFF", "#FFA500")) +
  theme_test() + 
  labs(title = "Gráfico de Barras de morosidad",
       x = "Condición de la morosidad", 
       y = "Frecuencia absoluta") 


# Paletas de colores a partir de las funciones 
#    rainbow(n), heat.colors(n), terrain.colors(n), 
#    topo.colors(n), and cm.colors(n)


ggplot(datospss) + aes(morosidad) + 
  geom_bar(color = "black", fill = rainbow(2)) +
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x = "Condición de la morosidad", 
       y = "Frecuencia") 

# Nivel departamento

ggplot(datospss) + aes(x = dpto) + 
  geom_bar(fill = rainbow(6))

# colores cálidos

ggplot(datospss) + aes(x = dpto) + 
  geom_bar(fill = heat.colors(6)) 

ggplot(datospss) + aes(x = dpto) + 
  geom_bar(fill = terrain.colors(6)) 

# Stack barras ----------------

ggplot(datospss) + aes(x = tiporenta, fill = morosidad) +
  geom_bar(position = position_stack()) +
  theme_bw() +
  labs(title = "Situación de la Morosidad según Tipo de Renta", 
       x = "Tipo de Renta",
       y = "Frecuencia") 

##### La opción position = "stack" es por default


#     (Stacked Bar Plot en proporción) 


ggplot(datospss, aes(x = tiporenta, fill = morosidad ) ) +
  geom_bar(position = position_fill()) +
  theme_bw() +
  labs(title = "Situación de la Morosidad según el Tipo de Renta (%)", 
       x = "Tipo de Renta",
       y = "Proporción") + 
  scale_fill_manual(values = c("darkolivegreen3", "firebrick2")) 


# Histogramas  ---------------------------------------------

# (variable continua)

ggplot(datospss) + aes(edad) + geom_histogram()


# borde blanco de las barras
ggplot(datospss) + aes(edad) + geom_histogram(color = "white")

# borde balnco de la barra y ancho de barra igual a 20

ggplot(datospss) + aes(edad) + geom_histogram(color = "white",
                                           binwidth = 5)

# bins: cantidad de intervalos

ggplot(datospss) + aes(edad) + geom_histogram(color = "white",
                                              bins = 50)

# theme_classic()

ggplot(datospss) + aes(edad) + geom_histogram(color = "white",
                                           fill = "deepskyblue3") + 
  labs(title = "Histograma de la Edad", 
       x = "Edad", 
       y = "Frecuencia absoluta") +
  theme_classic() 


# theme_test()

ggplot(datospss, aes(edad) )+ geom_histogram(color = "white",
                                              fill = "deepskyblue3") + 
  labs(title = "Histograma de la Edad", 
       x = "Edad", 
       y = "Frecuencia absoluta") +
  theme_test() +
  scale_y_continuous( expand = c(0, 0) )  

# borrar el espacio debajo del histograma

# Histograma de la edad según situación de morosidad 

datospss |>
ggplot() + aes(x = edad, fill = morosidad ) + 
  geom_histogram( alpha = 0.4, color = "black") + # alpha: nivel de transparencia
  scale_fill_manual(values=c("#0000CD", "red")) +
  theme(legend.position = "left") # posición de la leyenda

# legend.position="right"
# legend.position="left"
# legend.position="top"
# legend.position="bottom"
# legend.position="none"
# legend.position=c(0.8, 0.8)


datospss |>
  ggplot() + aes(x = edad, fill = morosidad ) + 
  geom_histogram( alpha = 0.4, color = "black") + # alpha: nivel de transparencia
  scale_fill_manual(values=c("#0000CD", "red")) +
  theme(legend.position = "bottom",   # leyenda ubicada en la parte inferior
        legend.title = element_blank())  # sin titulo en la leyenda

# unicación manual

datospss |>
  ggplot() + aes(x = edad, fill = morosidad ) + 
  geom_histogram( alpha = 0.4, color = "black") + # alpha: nivel de transparencia
  scale_fill_manual(values=c("#0000CD", "red")) +
  theme(legend.position = c(0, 0) , 
        legend.title = element_blank()) 



# Subgráficos de histogramas ---------------------------------


ggplot(datospss) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") 

ggplot(datospss) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") +
  facet_grid(dpto ~ .)  +   #  filas ~ columnas (por filas)
  theme(legend.position = "none")

ggplot(datospss) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") +
  facet_grid(. ~ dpto)  +   # por columnas
  theme(legend.position = "none")

ggplot(datospss) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") +
  facet_wrap(~ dpto)  +    # facet_wrap("dpto"), 
  theme(legend.position = "none")  

# Series de tiempo -------------------------


# usamos la base de datos economics de la libreria ggplot

str(economics)

View(economics)

# psavert : personal savings rate 
# pce : consumption expenditures, in billions of dollars,
# uempmed : median duration of unemployment, in weeks
# unemploy : number of unemployed in thousands

ggplot(economics) + aes(x = date, y = unemploy) + 
  geom_line(size = 0.5, color = "blue") +
  theme_minimal() 

ggplot(economics) + aes(x = date, y = psavert) + 
  geom_line(size = 0.5, color = "blue") +
  theme_classic() +
  labs(title= "Saving rate (%)",
       x = "Years", y = "",
       ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    title =element_text(size=12),
    axis.title.x = element_text(size=10,color='black')
  ) +
  scale_x_continuous( expand = c(0, 0) ) 


# save plot

ggsave("../../output/plots/time_series_save.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)


# Diagrama de dispersión

ggplot(datospss) + aes(x = antiguedad, y = edad) +
  geom_point() +
  labs(title = "Diagrama de Dispersión", 
       x = "Antiguedad", 
       y = "Edad") +
  theme_replace() 






# References ---------------------------------

# Colección de gráficos realizados con R
browseURL("https://www.r-graph-gallery.com/")

# Data Visualization with R. Rob Kabacoff
browseURL("https://rkabacoff.github.io/datavis/")

browseURL("https://damzar.medium.com/eso-que-quieres-decir-hazlo-con-gr%C3%A1ficas-fc7b4963d9d3")





