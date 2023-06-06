################  Laboratorio 9 ############################
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


p_load(tidyverse, sandwich, lmtest, xtable,haven)

# sandwich: libreria de modelos lineales.
# lmtest: errores estandar robustos,
# xtable: exportar matriz o dataframe, table a latex

# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# ubimos la base de datos

data <- read_dta("../../data/Pesos/peso.dta")

# Typo de variables

as.list(sapply(data, class))


# Dummy variable


data$Dummy <- ifelse(data$cigs > 0 ,  1 , 0 )


# bwghtlbs: peso del bebe en libras
# fill: desagregar los grpaficos
# Hitograma del peso del bebé ----

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

model3 <- lm(lbwght ~ Dummy + motheduc + lfaminc + white , data = data)

model3.tab <- coeftest(model3, vcov=vcovHC(model3, type='HC'))

model3_coef <- model3.tab[2,1]

model3_coef_se = model3.tab[2,2]

model3_lower = coefci(model3, df = Inf, vcov. = vcovHC, type = "HC")[2,1]
model3_upper = coefci(model3, df = Inf, vcov. = vcovHC, type = "HC")[2,2]


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
rownames(table)<- c("OLS baseline", "OLS with controls", "OLS with controls II")

# Exportación a Latex
xtable(table)


tab <- as.data.frame(table)

# table de matriz a dataframe (tab)

# Coef-plot

options(repr.plot.width = 8, repr.plot.height =5)  # dimensiones del gráfico

# aes: ejes

tab  %>% ggplot(aes(x=rownames(tab), y=Estimate)) +
    geom_point(size=2, color = 'black') +
    geom_errorbar(aes(ymin=lower_bound, ymax=upper_bound) , width = 0.05,color="darkblue", linewidth = 0.8) +
    labs(x="", y="") + ggtitle("Smoking Coefficient (95% CI)")  +
    theme(text=element_text(size =15), plot.title = element_text(hjust = 0.5)) +
   geom_hline(yintercept=0, linetype="dashed", color = "black", size=0.8) +
    scale_x_discrete(limits = c("OLS baseline",
                                "OLS with controls", "OLS with controls II")) +
  scale_y_continuous(breaks = seq(-0.2,0.2,0.1) , limits = c(-0.2, 0.2)) +
  theme_classic(14)

  
# geom_errorbar solicita el limite inferior y superior
# width  : ancho de la abrra superior
# scale_x_discrete: Nombre de modelos en eje inferior
# geom_hline: añadir lines horizontal
# panel.grid.major = element_blank(), panel.grid.minor = element_blank() borra las cuadrículas en el fondo

ggsave("../../output/plots/Coef_plot.png"
       , height = 8  # alto
       , width = 12  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)


# Model Matrix -----------------

m <- lm(lbwght ~ -1 + Dummy:(motheduc + lfaminc + white) , data)
X <- as.data.frame( model.matrix(m) )









