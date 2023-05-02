#----------------------------------#
#  CURSO: R FOR BUSINESS ANALYTICS #
#         GRÁFICOS CON R           #    
#     Mg. Jesús Salinas Flores     # 
#     jsalinas@lamolina.edu.pe     #
#----------------------------------#

# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())

# Para limpiar el área de gráficos
graphics.off()

# Limpiar la consola
cat("\014")

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)      # Eliminar la notación científica
options(digits = 3)        # Número de decimales

# Paquetes
library(pacman)
p_load(foreign, plotrix, PerformanceAnalytics, ggplot2, 
       gganimate, png, magick, plotly, 
       esquisse, gridExtra, RColorBrewer)

library(gifski)

#------------------------------------#
#  CAPÍTULO 4. GRÁFICOS CON GGPLOT2  #
#------------------------------------#

library(foreign)
datos <- read.spss("Riesgo_morosidad.sav", 
                  use.value.labels = T,  
                  to.data.frame = TRUE)

attr(datos, "variable.labels") <- NULL

str(datos)

mean(edad)      # será un error
mean(datos$edad)

# attach() permite acceder a los nombres de las variables
#          directamente

attach(datos)   
mean(edad)
mean(datos$edad)

# detach() deshace el attach()
detach(datos)
attach(datos)   


#-------------------------------------------#
# I. Gráficos para una variable cualitativa #
#-------------------------------------------#

# 1.1 Gráficos de Barras con ggplot2 --------------------------
library(ggplot2) 
# qplot() Básicos   ggplot() Básicos Avanzados

ggplot(datos) + aes(morosidad)


# Gráfico de Barras para una variable categórica
ggplot(datos) +  aes(morosidad) + geom_bar(stat = "count")   

ggplot(datos) +  aes(x = morosidad) + geom_bar() # es equivalente
ggplot(datos, aes(morosidad)) + geom_bar()     # es equivalente
ggplot(datos) + geom_bar(aes(morosidad))       # es equivalente

# Gráfico horizontal con coord_flip()
ggplot(datos) + aes(morosidad) + geom_bar() + 
  coord_flip()

ggplot(datos) +  aes(y = morosidad) + geom_bar()   


# Añadiendo temas de fondo con theme_light()
ggplot(datos) + aes(morosidad) + geom_bar() + theme_test()

# Añadiendo títulos al gráfico con labs()
ggplot(datos) + aes(morosidad)  +
       geom_bar() + 
       theme_light() + 
       labs(title = "Gráfico de Barras Vertical", 
            x = "Condición de la morosidad", 
            y = "Frecuencia") 

#### Manejo de Colores  ---------------------------------------
browseURL("http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf")
colors()

# 1. Especificando colores de la barra
ggplot(datos) + aes(morosidad) + 
  geom_bar(color = "blue", fill = "White") + 
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x = "Condición de la morosidad", 
       y = "Frecuencia") 

# 2. Usando diferentes colores en las barras
ggplot(datos) + aes(morosidad) + 
  geom_bar(color = "black", fill = c("darkgreen", "orange")) +
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x = "Condición de la morosidad", 
       y = "Frecuencia")

# 3. Usando diferentes colores en formato hexadecimal
browseURL("https://www.rapidtables.com/web/color/RGB_Color.html")

ggplot(datos) + aes(morosidad) + 
  geom_bar(color = "#010800", fill = c("#DF2B57", "#77926A")) +
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x = "Condición de la morosidad", 
       y = "Frecuencia")


# 4. Cambiando color de la barra por defecto usando 
#    la opción fill
ggplot(datos) + aes(x = morosidad, fill = morosidad) + 
  geom_bar() + 
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x = "Condición de la morosidad", 
       y = "Frecuencia") 


ggplot(datos) + aes(x = dpto) + geom_bar()                 

#                   x= predictora, fill = target                         
ggplot(datos) + aes(x = dpto, fill = morosidad) + 
  geom_bar()                 # Gráfico de Barras Apilado


# 5. Cambiando los colores por defecto usando scale_fill_manual
ggplot(datos) + aes(morosidad, fill = morosidad) + 
  geom_bar(color = "black") +
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x = "Condición de la morosidad", 
       y = "Frecuencia") +
  scale_fill_manual(values = c("darkgreen", "orange"))


# 6. Aplicando paletas de colores a partir de las funciones 
#    rainbow(n), heat.colors(n), terrain.colors(n), 
#    topo.colors(n), and cm.colors(n)

ggplot(datos) + aes(morosidad) + 
  geom_bar(color = "black", fill = rainbow(2)) +
  theme_light() + 
  labs(title = "Gráfico de Barras Vertical", 
       x = "Condición de la morosidad", 
       y = "Frecuencia")

length(levels(morosidad))

ggplot(datos) + aes(x = dpto) + 
  geom_bar(fill = rainbow(6)) 

ggplot(datos) + aes(x = dpto) + 
  geom_bar(fill = rainbow(length(levels(dpto)))) 


# Gráfico interactivo con el paquete plotly
library(plotly)
ggplotly()


#      Ejercicio   1     #
# Construya un gráfico de barras interactivo para
# la variable dpto


# Diagrama de barras con animación
ggplot(datos) + aes(morosidad) + 
  geom_bar(color = "black", fill = c("darkgreen", "orange"))  

library(gganimate)
ggplot(datos) + aes(morosidad) + 
  geom_bar(color = "black", fill = c("darkgreen", "orange"))  +
  transition_states(morosidad) 

ggplot(datos) + aes(morosidad) + 
       geom_bar(color = "black", fill = c("darkgreen", "orange"))  +
       transition_states(morosidad)  +
       enter_fade() +
       exit_fade()  

p1 <- ggplot(datos) + aes(morosidad) + 
  geom_bar(color = "black", fill = c("darkgreen", "orange"))  +
  transition_states(morosidad)  +
  enter_fade() +
  exit_fade()  

animate(p1, nframes = 200)

# enter_grow() + exit_shrink()


#      Ejercicio   2     #
# Construya un gráfico de barras animado para la variable dpto




#---------------------------------------------#
# II. Gráficos para dos variables categóricas #
#---------------------------------------------#

# 2.1 Gráfico de barras apiladas en valor absoluto ------------
#     (Stacked Bar Plot) 
ggplot(datos) + aes(tiporenta) + geom_bar()

ggplot(datos) + aes(x = tiporenta, fill = morosidad) +
       geom_bar(position = position_stack()) +
       theme_bw() +
       labs(title = "Situación de la Morosidad según Tipo de Renta", 
            x = "Tipo de Renta",
            y = "Frecuencia") 

##### La opción position = "stack" es por default

# 2.2 Gráfico de barras apiladas en proporción ----------------
#     (Stacked Bar Plot) 
ggplot(datos, aes(x = tiporenta, fill = morosidad ) ) +
       geom_bar(position = position_fill()) +
       theme_bw() +
       labs(title = "Situación de la Morosidad según el Tipo de Renta", 
            x = "Tipo de Renta",
            y = "Proporción") + 
       scale_fill_manual(values = c("darkolivegreen3", "firebrick2")) 

ggplotly()


#      Ejercicio   3     #
# Construya un gráfico de barras apilado en proporción para 
# dpto vs morosidad



# 2.3 Gráfico de barras agrupadas en valor absoluto -----------
#     (Grouped Bar Plot) 
ggplot(datos) + aes(x = tiporenta, fill = morosidad) +
  geom_bar(position = position_dodge()) +
  theme_bw() +
  labs(title = "Situación de la Morosidad según el Tipo de Renta", 
       x = "Tipo de Renta",
       y = "Frecuencia")  +
  scale_fill_manual(values = c("darkolivegreen3", "firebrick2")) 



#      Ejercicio  4     #
# Construya un gráfico de barras agrupadas (sin apilar)
# con valores absolutos para fonolab vs morosidad



library(esquisse)
esquisse::esquisser(datos, viewer = "browser")
esquisser()
esquisse::esquisser(viewer = "browser")



#------------------------------------------#
# III. Gráficos para una variable continua #
#------------------------------------------#

# 3.1 Histogramas ---------------------------------------------
ggplot(datos) + aes(edad) + geom_histogram()
ggplot(datos) + aes(edad) + geom_histogram(color = "white")
ggplot(datos) + aes(edad) + geom_histogram(color = "white",
                                           binwidth = 10)
ggplot(datos) + aes(edad) + geom_histogram(color = "white",
                                           bins = 10)

ggplot(datos) + aes(edad) + geom_histogram(color = "white",
                                           fill = "deepskyblue3") + 
       labs(title = "Histograma de la Edad", 
            x = "Edad", 
            y = "Frecuencia") +
       theme_bw() 

ggplotly() 

# Histograma de una variable cuantitativa según los niveles
# de una variable categórica
ggplot(datos) + aes(x = edad, fill = morosidad) +
  geom_histogram(alpha=0.5, color = "black") + 
  theme(legend.position = "left") # Cambia la posición de la leyenda

# legend.position="right"
# legend.position="left"
# legend.position="top"
# legend.position="bottom"
# legend.position="none"
# legend.position=c(0.8, 0.8)

# Histograma por grupo usando facets() Subgráficos
# Subgráficos
ggplot(datos) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") 
  
ggplot(datos) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") +
  facet_grid(dpto ~ .)  +   #  filas ~ columnas
  theme(legend.position = "none")

ggplot(datos) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") +
  facet_grid(. ~ dpto)  + 
  theme(legend.position = "none")

ggplot(datos) + aes(edad, fill = dpto) + 
  geom_histogram(alpha = 0.5, color = "azure4") +
  facet_wrap(~ dpto)  +    # facet_wrap("dpto")
  theme(legend.position = "none")  


#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   5     #
# Construya un histograma para la variable antiguedad 
# según la morosidad en gráficos separados (facet)



#-----------------------------------------------#
# IV. Gráficos para dos variables cuantitativas #
#-----------------------------------------------#

# 4. Diagrama de Dispersión -----------------------------------
ggplot(datos) + aes(x = antiguedad, y = edad) +
       geom_point() +
       labs(title = "Diagrama de Dispersión", 
            x = "Antiguedad", 
            y = "Edad") +
       theme_replace() 

#----------------------------------------------#
# V. Gráficos para dos variables cuantitativas #    
#     y otra cualitativa                       #
#----------------------------------------------#

# 5. Diagrama de Dispersión -----------------------------------

# Diagrama de dispersión con cambio de color por grupos 
ggplot(datos) + aes(x = antiguedad, y = edad, color = morosidad) +
       geom_point()   


# Diagrama de dispersión por grupos con animación
library(gganimate)
ggplot(datos) + aes(x = antiguedad, y = edad, color = morosidad) + 
       geom_point() +
       transition_states(morosidad) +
       enter_fade() +
       exit_fade()


# Mostrando varias gráficas en una sola pantalla --------------
a <- 2
a = 2
2 -> a
2 = a   # Es un error

library(gridExtra)   # library(cowplot) library(patchwork)
grid.arrange(a, b, c, ncol = 3)
grid.arrange(a, b, c, nrow = 3)
grid.arrange(a, b, c, nrow = 2)


#---------------------------------#
# VI. Gráfico de Series de Tiempo #
#---------------------------------#

# 6. Gráficos de Series de Tiempo -----------------------------
library(ggplot2)

data(package = "ggplot2")

?economics 

str(economics)

View(economics)


ggplot(economics) + aes(x = date, y = unemploy) + 
      geom_line(size = 0.5, color = "blue") +
      theme_minimal() 

# geom_smooth(method = "lm") 
  
# Serie de Tiempo con animación
library(gganimate)
p2 <- ggplot(economics) + aes(x = date, y = unemploy) + 
      geom_line(size = 0.8, color = "#E46726") +
      theme_minimal() + 
      labs(title = "Serie de Tiempo", 
           x = "Años", 
           y = "Desempleo") +
      transition_reveal(date) +
      enter_fade() +
      exit_fade() 

p2 

animate(p2, nframes = 150, renderer = magick_renderer())
anim_save("Serie de Tiempo.gif", p2)
# Se pueden usar otros formatos como *.gif , *.mp4

#________________________
# ________ \\|// ________
# ________( o o ) _______ 
# ___oo0____(_)____Ooo___
#      Ejercicio   6     #
# Construya un gráfico de serie de tiempo animado para 
# la variable uempmed


# Material complementario -------------------------------------
# 1. Colección de gráficos realizados con R
     browseURL("https://www.r-graph-gallery.com/")

# 2. Libro: Data Visualization with R. Rob Kabacoff
     browseURL("https://rkabacoff.github.io/datavis/")

# 3. Eso que quieres decir, hazlo con gráficas
     browseURL("https://damzar.medium.com/eso-que-quieres-decir-hazlo-con-gr%C3%A1ficas-fc7b4963d9d3")

# 4. Revisar el vídeo de Hans Rosling, "200 países, 200 años"
     browseURL("https://www.youtube.com/watch?v=0Oe_j7SZadA")
     