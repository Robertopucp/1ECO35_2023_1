#-------------------------------------------------------
#               Graficos de Densidad
#         Edward Cruz (Economía con Manzanitas)
#-------------------------------------------------------
install.packages(c("tidyr","hrbrthemes","viridis"))
library(tidyverse)
library(hrbrthemes)
library(tidyr)
library(viridis)

data = diamonds

#primer grafico de densidad e histograma
summary(data$price)

data %>%
  ggplot( aes(x=price)) +
  geom_density()

data %>%
  ggplot( aes(x=price)) +
  geom_histogram()

# colores Hexadecimales
# lista de colores
colors()
data %>%
  filter( price<5000 ) %>%
  ggplot( aes(x=price)) +
  geom_density(fill="sienna1", color="#6C3483", alpha=0.4)+
  ggtitle("densidad de precio del diamante")+
  theme_ipsum()


# combiando graficos de densidad
table(data$cut)

data %>% 
  filter( cut== "Premium" | cut == "Very Good") %>% 
 ggplot(aes(x=price, fill=cut)) +
  geom_density(adjust=1.5, alpha=0.4) +
  theme_ipsum()

#multiples graficos de densidad (facet_wrap)
ggplot(data=diamonds, aes(x=price, group=cut, fill=cut)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~cut)+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank())


# proporcion de densidades
ggplot(data=diamonds, aes(x=price, fill=cut)) +
  geom_density(adjust=1.5, position="fill") +
  theme_ipsum()


#---------------ridgeline plot ---------------
install.packages("ggridges")
library(ggridges)

# desidad por categorias
ggplot(diamonds, aes(x = price, y = cut, fill = cut)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


weather = lincoln_weather
View(weather)
#grafico de densidad de la temperatura por mes
ggplot(weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_gradient(low = "#FCF3CF", high = "#E74C3C")+
  labs(title = 'Temperatures por meses') +
  theme_classic() +
  theme(legend.position="none")

