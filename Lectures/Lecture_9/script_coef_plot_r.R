################  Clase 11 Coef Plot ############################
## Curso: Laboratorio de R y Python ###########################
## @author: Roberto Mendoza

# clear environment

rm(list=ls(all=TRUE))


# Cargamos librerias ----

#install.packages("librarian")

# librarian::shelf

librarian::shelf(tidyverse, sandwich, lmtest, xtable,haven)

# sandwich: libreria de modelos lineales.
# lmtest: errores estandar robustos,
# xtable: exportar matriz o datafrma, table a latex

user <- Sys.getenv("USERNAME")  # username

setwd( paste0("C:/Users/",user,"/Documents/GitHub/1ECO35_2022_2/Lab10") ) # set directorio


# ubimos la base de datos

data <- read_dta("../data/Pesos/peso.dta")

# Typo de variables

as.list(sapply(data, class))


# Dummy variable

data <- data %>%  mutate(Dummy = ifelse(cigs > 0 ,  1 , 0 ) ) %>%
    mutate(Dummy1 = case_when(Dummy == 0 ~ "No smoking",
                              Dummy == 1 ~ "Smoking"))

# bwghtlbs: peso del bebe en libras
# fill: desagregar los grpaficos
# Hitograma del peso del bebé ----

data %>%  ggplot() + aes(x=bwghtlbs, fill = Dummy1) +
    geom_histogram( color="black", alpha=0.6, size=0.5) +
    scale_fill_manual(values=c("#69b3a2", "#404080"), name=NULL) +  # Set colores usando código
    labs(x = " ", y = "Absolute frequency", title = "Smoking status and newborn weight (lbs)", size = 10) +
    theme_classic()  +   # Fondo blanco
    theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = c(0.7,0.8)) +
    scale_x_continuous(limits = c(0,18), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,300), expand = c(0, 0))

# fill permite desagregar según los valores de Dummy1
# plot.title = element_text(hjust = 0.5) Permite centrar el título

ggsave("../plots/hist_pesos.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)



# Histograma en frecuencia relativa ----

data %>%  ggplot(aes(x=bwghtlbs, fill = Dummy1)) +
    geom_histogram( aes(y = ..density..), color="black", alpha=0.3, position = 'identity', size=0.6) +
    scale_fill_manual(values=c("blue", "red"), name=NULL) +  # for legend section
    labs(x = " ", y = "Relative frequency", title = "Smoking status and newborn weight (lbs)", size = 15) +
    theme_classic() +
    theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = c(0.85,0.9)) +
    scale_x_continuous(limits = c(0,18), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,0.4), expand = c(0, 0))



#y = ..density.. (frecuencia relativa)

ggsave("../plots/hist_relative_pesos.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)

# Densidad ----

data %>% ggplot(aes(x=bwghtlbs, fill = Dummy1 , colour=Dummy1)) +
    geom_density(alpha=0.5, color = "black", size=0.6) +
    scale_fill_manual(values=c("blue", "red"), name=NULL) + # Color
    ggtitle("Smoking status and newborn weight (lbs)") +
    theme_classic() +
    theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
    labs(x = "",
         y = "Kernel Density") +
    theme(legend.position = c(0.85,0.9)) +
    scale_x_continuous(limits = c(0,18), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,0.4), expand = c(0, 0))


# scale limita el intervalo de los ejes

# alpha : grado de transparencia

# Linear regression -----

# lbwght : logaritmo del peso del bebé
# Dummy: toma 1 si la mujer fumó durante el embarazo

# First Model ~
# ~ : alt + 126
# el intercept se añade por default


model1 <- lm(lbwght ~ Dummy, data = data)

# lmtest: coeftest

model1.tab <- coeftest(model1, vcov=vcovHC(model1, type='HC1'))

# HC: heteerocedasticidad, HC1: matriz varianza y cov de Huber - White

model1_coef <- model1.tab[2,1]

model1_coef_se = model1.tab[2,2]

# HC1: standar error robust aginst heterocedasticity

# Intervalo de confianza
# intervalo de cofianza ajsutado por heterocedasticidad
model1_lower = coefci(model1, df = Inf, vcov. = vcovHC, type = "HC1")[2,1]

model1_upper = coefci(model1, df = Inf, vcov. = vcovHC, type = "HC1")[2,2]

# Second Model, se añado los años de educación de la madre

model2 <- lm(lbwght ~ Dummy + motheduc, data = data)

model2.tab <- coeftest(model2, vcov=vcovHC(model2, type='HC1'))

model2_coef <- model2.tab[2,1]

model2_coef_se = model2.tab[2,2]

model2_lower = coefci(model2, df = Inf, vcov. = vcovHC, type = "HC1")[2,1]
model2_upper = coefci(model2, df = Inf, vcov. = vcovHC, type = "HC1")[2,2]

# type: HC1: robust standar error Huber-White

# Third Model

model3 <- lm(lbwght ~ Dummy + motheduc + lfaminc + white + Dummy:(motheduc + lfaminc + white), data = data)

model3.tab <- coeftest(model3, vcov=vcovHC(model3, type='HC'))

model3_coef <- model3.tab[2,1]

model3_coef_se = model3.tab[2,2]

model3_lower = coefci(model3, df = Inf, vcov. = vcovHC, type = "HC")[2,1]
model3_upper = coefci(model3, df = Inf, vcov. = vcovHC, type = "HC")[2,2]

# Model Matrix

m <- lm(lbwght ~ -1 + Dummy:(motheduc + lfaminc + white) , data)
X <- as.data.frame( model.matrix(m) )


# Tabla de resultaods

table<- matrix(0, 3, 4) # 3 filas y 4 columnas


table[1,1]<- model1_coef
table[1,2]<- model1_coef_se

table[2,1]<- model2_coef
table[2,2]<- model2_coef_se

table[3,1]<- model3_coef
table[3,2]<- model3_coef_se

table[1,3]<- model1_lower
table[1,4]<- model1_upper

table[2,3]<- model2_lower
table[2,4]<- model2_upper

table[3,3]<- model3_lower
table[3,4]<- model3_upper

colnames(table)<- c("Estimate","se","lower_bound","upper_bound")
rownames(table)<- c("OLS baseline", "OLS with controls", "OLS interactive model")

# Exportación a Latex
xtable(table)


tab <- as.data.frame(table)

# table de matriz a dataframe (tab)

# Coef-plot

theme_set(theme_bw(20)) # tema con fondo blanco y cuadro con bordes en negro

options(repr.plot.width = 8, repr.plot.height =5)  # tamaño del gráfico

# aes: ejes

tab  %>% ggplot(aes(x=rownames(tab), y=Estimate)) +
    geom_point(size=2, color = 'black') +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound) , width = 0.05,color="black", size = 0.8) +
    labs(x="", y="") + ggtitle("Smoking Coefficient (95% CI)")  +
    theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
   geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
    scale_x_discrete(limits = c("OLS baseline","OLS with controls", "OLS interactive model")) + # order x - axis
    theme(panel.grid.major = element_blank(), # borras las cuadrículas en el fondo
          panel.grid.minor = element_blank())

# geom_errorbar solicita el limite inferior y superior
# width  : ancho de la abrra superior
# scale_x_discrete: Nombre de modelos en eje inferior
# geom_hline: añadir lines horizontal
# panel.grid.major = element_blank(), panel.grid.minor = element_blank() borra las cuadrículas en el fondo

ggsave("../plots/Coef_plot.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)











