################  Laboratorio 11 parte 1 ------------------------
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

options(warn = -1) 

# Library ####


# Load libraries ----

library(pacman) 


p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
  , sf
 , haven 
 ,viridis
 ,gridExtra  # combine many plots 
)

# Get data of labor

sex_work <- read_dta('../../data/geopandas_data/8_trab_sex_20.dta')

# code por departamento 
sex_work$dpt_code <- substr(sex_work$cod_ubigeo, 1, 2)

# code por provincia
sex_work$prov_code <- substr(sex_work$cod_ubigeo, 1, 4)


# Sex work

women_work = sex_work[ which(sex_work$sex == 3), ]


# Get the total number of women workers by dpt

dpt_women_work <- women_work |> 
  group_by(dpt_code, month) |>
  summarise(women_empl = sum(empl)) |> ungroup() |>
  arrange(dpt_code, month)

# data by department 

df2 <- dpt_women_work |>
  group_by(dpt_code) |>
  summarise(women_empl = mean(women_empl))


# Load shapefile -------------------------

dpt_shp <- st_read(
  '../../data/geopandas_data/LIMITE_DEPARTAMENTO/LIMITE_DEP.shp'
)


# Merge shapefile data and df2

df3 <- dpt_shp |>
  left_join(df2, by = c("CCDD"="dpt_code"))


# Plot the heatmap
ggplot() +
  geom_sf(data = df3, aes(fill = women_empl), linetype = "dotted",  color = "black") +
  scale_fill_gradient(name = "Employment", low = "white", high = "red") + # mapa calor
  theme_void() +
  theme(legend.position = "right")




# Define the color scale and breaks
my_colors <- viridis(5)

min <- min(df3$women_empl)
max <- max(df3$women_empl)


my_breaks <- c(min, 20000, 40000, 60000, 100000, max)

### Plot the heatmap  --------


ggplot() +
  geom_sf(data = df3, aes(fill = cut(women_empl, breaks = my_breaks,
                                     include.lowest = TRUE)), 
          linetype = "dotted",  color = "black") +
  scale_fill_manual(values = my_colors, 
                    name = "Number of Employers",
                    breaks = levels(cut(df3$women_empl, breaks = my_breaks,
                                        include.lowest = TRUE
                                        )),
                    labels = c("[ 675.33, 20000 [",
                               "[ 20000, 40000 [",
                               "[ 40000, 60000 [",
                               "[ 60000, 100000 [",
                               " [100000, 656477.23 ]"
                               )) +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7), 
        strip.background = element_blank()
        )


#----------------------------------------------------#


# Filter the data
df3_filtered <- df3[df3$CCDD != "15", ]

### Plot the choropleth map ----------

ggplot() +
geom_sf(data = df3_filtered, aes(fill = women_empl), linetype = "dotted", 
        color = "black") +
  scale_fill_viridis(
    breaks = c(20000, 40000,60000)
  ) +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7), ) +
  labs(fill = "Women Employment")

### Escala de grises ----

ggplot() +
  geom_sf(data = df3_filtered, aes(fill = women_empl), linetype = "dotted", 
          color = "black") +
  scale_fill_gradient(
    low = "grey", high = "black",
    breaks = c(20000, 40000,60000)
  ) +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7), ) +
  labs(fill = "Women Employment")


# PEA por departamento , genero y mes 

df4 <- sex_work |>
  group_by(dpt_code, month, sex) |> # group_by
  summarise(empl = sum(empl)) |> ungroup() |>
  pivot_wider(id_cols = c(dpt_code, month),  # reshape_pivot 
              names_from = sex,
              values_from = empl) |>
  rename( "SI" = "1",        # rename 
          "Hombre" = "2" ,
          "Mujer" =  "3" ) |>
  mutate(prop_wom = Mujer/(SI+Hombre+Mujer)*100) # percent variable 


# merge con shapefile 


df5 <- dpt_shp |>
  left_join(df4, by = c("CCDD"="dpt_code"))


# month names from numbers 

df5$month_name <- month.name[df5$month]

### Mapas por mes --------------


# Define the grid of subplots
grid <- expand.grid(row = 1:4, col = 1:3)

# Define the figure size
options(repr.plot.width=15, repr.plot.height=15)

# Create a list of plots
plots <- list()


# Loop through each subplot
for (i in 1:nrow(grid)) {
  # Subset the data for the current month
  month <- unique(df5$month_name)[i]
  df5_filter <- df5[df5$month_name == month,]
  
  # Create the choropleth map
  p <-ggplot() +
    geom_sf(data = df5_filter, aes(fill = prop_wom), linetype = "dotted", 
            color = "black") +
    scale_fill_gradient(
      low = "white", high = "red"
    ) +
    theme_void() +
    theme(legend.position = "right",
          legend.title=element_blank()) +
    labs( title = month) 
  
  # Add the plot to the list
  plots[[month]] <- p
}


# Combine the plots into a single plot
grid.arrange(grobs = plots, ncol = 3)


# Replace missing Lima 

df5$prop_wom[df5$NOMBDEP == "LIMA"] <- NA

### Mapa con missing value  --------------

min <- min(df5$prop_wom, na.rm = T)
max <- max(df5$prop_wom)


my_breaks <- c(min, 20, 30,40, 50, 100)


# Paleta de naanja degradada

my_palette <- c("#FFF5EB", "#FFDAB9", "#FFA07A", "#FF8C00", "#FF4500")


ggplot() +
  geom_sf(data = df5, aes(fill = cut(prop_wom, breaks = my_breaks,
                                     include.lowest = TRUE)),  color = "black") +
  scale_fill_manual(
    values = my_palette, 
                    name = "Women proportion",
                    breaks = levels(cut(df5$prop_wom, breaks = my_breaks,
                                        include.lowest = TRUE
                    )),
                    labels = c("14.04, 20", "20, 30",
                               "30, 40",
                               "40, 50",
                               "50, 100"
                    ),
    na.value = "grey") +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8), 
        strip.background = element_blank()
  ) 

ggsave("../../output/plots/map_pea_women.jpg"
       , height = 7  # alto
       , width = 9  # ancho
       , dpi = 320   # resolución (calidad de la imagen)
)
















