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


# Load libraries ----

library(pacman) 


p_load(
  tidyverse  # dplyr, tidyr, stringr, ggplot2, etc in unique library
, tqdm
, XML
, RSelenium
)


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# Inicializar el controlador de RSelenium
driver <- rsDriver(browser = "chrome")
remDr <- driver[["client"]]

# Crear un diccionario vacío
all_tables <- list()

for (i in tqdm(0:9)) {
  
  # Navegar a la página web
  url <- paste0("https://example.com/page", i)
  remDr$navigate(url)
  
  # Obtener el contenido HTML de la tabla
  table_html <- remDr$findElement(using = "id", value = "maintable_wrapper")$getElementAttribute("innerHTML")[[1]]
  
  # Convertir el contenido HTML de la tabla en un objeto de tabla data.frame
  table <- readHTMLTable(table_html)[[1]]
  
  # Llenar el diccionario con el objeto de tabla data.frame
  all_tables[[paste0("table_page_", i)]] <- table
  
  # Hacer clic en el botón "Siguiente"
  remDr$findElement(using = "id", value = "maintable_next")$clickElement()
  
  # Esperar 2 segundos antes de continuar
  Sys.sleep(2)
}

# Cerrar el controlador de RSelenium
remDr$close()

