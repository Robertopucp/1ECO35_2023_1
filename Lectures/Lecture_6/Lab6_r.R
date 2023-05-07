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


p_load(readxl, tidyverse, foreign, ggthemes, datos)

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


# Stack barras ----------------

ggplot(datospss) + aes(x = tiporenta, fill = morosidad) +
  geom_bar(position = position_stack(), color = "black" ,
           fill = c("#E69F00", "#56B4E9") , width = 0.6) +
  theme_classic() +
  labs(title = "Situación de la Morosidad según Tipo de Renta", 
       x = "Tipo de Renta",
       y = "Frecuencia") 

# width: ancho de la barra

##### La opción position = "stack" es por default

#     (Stacked Bar Plot en proporción) 


ggplot(datospss, aes(y = tiporenta, fill = morosidad ) ) +
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
  geom_histogram( alpha = 0.5, color = "black") + # alpha: nivel de transparencia
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(legend.position = "left") +# posición de la leyenda
  labs(x = "Edad", 
       y = "Frecuencia absoluta")

# legend.position="right"
# legend.position="left"
# legend.position="top"
# legend.position="bottom"
# legend.position="none"
# legend.position=c(0.8, 0.8)


datospss |>
  ggplot() + aes(x = edad, fill = morosidad ) + 
  geom_histogram( alpha = 0.5, color = "black") + # alpha: nivel de transparencia
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(legend.position = "bottom",   # leyenda ubicada en la parte inferior
        legend.title = element_blank()) + # sin titulo en la leyenda
  labs(x = "Edad", 
       y = "Frecuencia absoluta")

# unicación manual

datospss |>
  ggplot() + aes(x = edad, fill = morosidad ) + 
  geom_histogram( alpha = 0.5, color = "black") + # alpha: nivel de transparencia
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  theme(legend.position = c(0, 0) , 
        legend.title = element_blank()) + 
  labs(x = "Edad", 
       y = "Frecuencia absoluta")


#### Subgráficos de histogramas ---------------------------------


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


paises %>% filter(continente == "Américas") %>% 
  ggplot() + aes(x= anio, y=pib_per_capita, color = pais) +
  geom_line(show.legend = F) + facet_wrap(vars(pais))

# Presente un gráfico que muestre la evolución del 
# pbi por país de América


# Visualizando la esperanza de vida media

paises %>%
  group_by(anio) %>%
  summarise(pbi_pc_media = mean(pib_per_capita)) %>%
  ggplot()+ aes(x = anio, y = pbi_pc_media) + 
  geom_line(color = "blue") + 
  geom_point() +
  geom_text(aes(label = round(pbi_pc_media, 1)),
            vjust = -2, size = 3) + ylim(3000,13000) +
  labs(x= "Año",
       y="PBI per-cápita media") +
  scale_x_continuous(breaks = seq(1952,2007,5)) + theme_few()
  

ggsave("../../output/plots/time_series_pbi.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)



# Diagrama de dispersión -----------------------

load("../../data/wage2015_subsample_inference.Rdata")

dim(data)

data <- data %>% filter(clg == 1)  

# nos quedamos con la muestra de individuos con nivel educativo universitario


# gráfico de dispersión salario (logaritmo) vs experiencia laboral 

ggplot(data, aes(y = lwage, x = exp1)) + geom_point()

ggplot(data, aes(y = lwage, x = exp1)) + geom_line()

# Bins plot #
#---------------------------------------------------------#

options(repr.plot.width = 10, repr.plot.height =10)  # plot size

ggplot(NULL,aes(exp1,lwage)) +
  stat_summary_bin(data=data, fun='mean', bins=20,
                   color='red', size=3.5, geom='point') +
  theme_classic()+
  labs(title= "",
       x = "Experiencia laboral", y = "Salario (log)",
  ) +
  theme(
    axis.title = element_text(size=12,color='black'),
    axis.text = element_text(size=10,color='black')
  )
  

ggsave("../../output/plots/wage_exp.jpg"
       , height = 10  # alto
       , width = 15  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)

#---------------------------------------------------------#

# Presente un gráfico que compare el pbi y la esperanza de vida para el año 2007
# use la base de datos paises 




# References ---------------------------------

# Colección de gráficos realizados con R
browseURL("https://www.r-graph-gallery.com/")

# Data Visualization with R. Rob Kabacoff
browseURL("https://rkabacoff.github.io/datavis/")

# World banck Git Hub repository

browseURL("https://worldbank.github.io/r-econ-visual-library/")





