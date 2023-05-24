################  Laboratorio 7 ############################
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


p_load(readxl, lubridate, tidyverse, stringi)


# Change working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


data <- data.frame(read_excel("../../data/Centro_salud/Centro_salud_mental.xls"))


sapply(data, typeof)

apply(data , 2, function(x) sum(is.na(x)))

# Nombre de las variables a minuscula

colnames(data) <- tolower(colnames(data)) # capital letters to lower letters


#------- Regex -----------


# Patrones basicos

# 1. [] : permitir indicar tipo de caracter, definir el rango de las caracteres
# 2. (): permite agrupar caracteres
# 3. \\-,  \\#,  \\?, se especifica carcateres espaciales
# 4. " " : dentro de comillar se debe espeficiar el patron del texto

# \\\ , un \;  \\\\, para 2\

# 2: patrones de texto 

# \\d : identifica digitos
# \\w : identifica caracteres alfanumericos (letras y numeros)
# \\s : identificas espacios
# [a-z], [A-Z], [a-zA-Z] : rango de letras mayusculas o minusculas
# [0-9]: rango de numeros

# \\D : NO identifica digitos
# \\W : NO identifica caracteres alfanumericos (letras y numeros)
# \\S : NO identificas espacios
# [^0-9] : No caputara numero del rango 0 al 9
#  [^a-zA-Z]: No captura o identifica letras (mayuscula o minuscula)

"Patrones de inicio y fin"

# ^\\d, ^M, ^2, ^\\-  : captura inicio de un texto
# \\d$, _M$, -2$, \\-$: captura los textos que terminan en digitos, M, 2 o -
# \\. : identificar cualquier tipo de caracter (espacios, numeros, letras, #!%$)

"jdhdh 77575"

# [0-9]*: astericos permite capterar ninguno, uno o mas de uno
# [0-9]+: el signo mas permite capturar uno o mas uno
# [0-9][a-z]? : ?, permite capturar a lo mas una ocurrencia.



# 1.0 Extraer texto ----


data$inst1 <- apply(data['institución_ruc'],
                    1 ,    # margin 1: aplicar la funcion por filas , por observaciones
                    function(x) gsub("[0-9]", '', x))

data$inst1 <- apply(data['institución_ruc'],
                    1 ,    # margin 1: aplicar la funcion por filas , por observaciones
                    function(x) gsub("[0-9]", '', x))

# gsub permitir reemplazar, gusb( se espeficica el patron de texto, '', string)

"[0-9]*: ninguno, uno o mas digitos"

data$inst2 <- apply(data['institución_ruc'],
                    1 ,
                    function(x) gsub("\\d", '', x))


"\\d: digitos"

# usando la funcion extraer letras y espacio

data$inst3 <- sapply(data$institución_ruc,
                    function(x) str_extract(x,"[a-zA-Z\\s]+"))


data$inst4 <- sapply(data$institución_ruc,
                    function(x) str_replace(x,"[^a-zA-Z\\s]+",''))


#  substituye lo que sea diferente a letras y espacio por nada ('').



# 2.0 Extraer numero ----


data$ruc1 <- sapply(data$institución_ruc,
                    function(x) gsub("[a-zA-Z]", '', x))


# se extrae digitos de uno o mas ocurrencia

data$ruc2 <- sapply(data$institución_ruc,
                   function(x) str_extract(x,"[0-9]+"))

# extraer solo 3 digitos del rango 0-9

data$ruc3 <- sapply(data$institución_ruc,
                   function(x) str_extract(x,"[0-9]{3}"))

# {3} me permtie extraer 3 digitos

data$ruc4 <- sapply(data$institución_ruc,
                    function(x) str_extract(x,"[0-9]{1,}"))


# usando [^0-9], lo que sea diferente de numero en el rango 0 a 9,
# me reemplazas por nada.

data$ruc5 <- sapply(data$institución_ruc,
                   function(x) gsub("[^0-9]", '', x))


# usando \\D, lo que sea diferentes de digitos, me reemplazas por nada ''

data$ruc6 <- sapply(data$institución_ruc,
                    function(x) gsub("\\D", '', x))




# Retirar :00:00 , !%& y 00/00/00

# usando str_replace para reemplazar caracteres
# () permite agrupar, | (permite espeficar diferentes textos)

# 3.0 str_ extract ----

data$fecha_apertura <- sapply(data$fecha_apertura,
                   function(x) str_replace(x,"(:00:00)|(!%&)|(00/00/00)", ''))


# Extraer las coordenadas de la variables GPS


data$coordinates <- sapply(data$gps,
              function(x) str_extract(x,"-([0-9]{1,2}).([0-9]{1,3}),-([0-9]{2}).([0-9]{1,4})"))

# [0-9]{1,2} uno o digitos

# @-1.15,-74.155$%&//5


#------ 4.0 str_match ---------

# Extraer una seccion del texto sin especificar la forma completa del texto


x <- "dada--dss kks. 12434 distrito  San damian   region Huarochiri"

str_match(x,"[D/d]istrito ([\\w+\\s]+) [R/r]egion ([\\w+\\s]+)")


# \\d*: ninguna, uno o mas ocurrencias
# \\d+: uno o mas ocurrencias

#\\.* : captura ninguna, una, o mas de un caracter (cualquiera: espacios, letras, numeros, #!%&/())
# () permite capturar lo que me interesa

str_match(x,"[D/d]istrito ([\\w+\\s]+) [R/r]egion ([\\w+\\s]+)")[2] # distrito

str_match(x,"[D/d]istrito ([\\w+\\s]+) [R/r]egion ([\\w+\\s]+)")[3]  # region


data$distrito <- apply(data['dirección'],
                       1 ,
                       function(x) str_match(x,"\\.*[D/d]istrito\\s([\\w*\\-\\s]*)\\s[R/r]egion\\s([\\w*\\s]*)")[2])

data$region <- apply(data['dirección'],
                     1 ,
                     function(x) str_match(x,"\\.*+[D/d]istrito\\s([\\w*\\-\\s]*)\\s[R/r]egion\\s([\\w*\\s]*)")[3])


data <- data |> rowwise() |>
  mutate(distrito_var = str_match( dirección ,
                  "[D/d]istrito ([\\w+\\s]+) [R/r]egion ([\\w+\\s]+)")[2],
         region_var = str_match( dirección ,
            "[D/d]istrito ([\\w+\\s]+) [R/r]egion ([\\w+\\s]+)")[3]
         ) |> ungroup()


View(data[,c('dirección','region_var')])

#extraccion del numero telefonico

#telf: 123-4559

data$telefono_fijo <- sapply(data$telefono,
                     function(x) str_match(x,"\\.*(\\d+\\-\\d+)$")[2])


data$telefono_fijo_2 <- sapply(data$telefono,
                            function(x) str_match(x,"\\.*(...\\-\\d+)$")[2])


data$telefono_fijo_3 <- sapply(data$telefono,
                              function(x) str_match(x,"\\.*(\\d+.\\d+)$")[2])



# Extraer seccion de un texto cuando se tiene que especificar toda la estrucutra del texto


match_output <- stringr::str_match(data$resolucion,
                   'DS-(\\d+)-([0-9]+)\\s([A-Z]+)')

# DS-54-2015 PCM

# pip str_match , stringr, str_match

"[0-9]+: existe uno o mas digitos"

data <- data %>% mutate(code_res = match_output[,2], year_res = match_output[,3],
                         entidad_res = match_output[,4])


# Regex que detecte un patron y crear dummies

# str_detect es un bool variable (True , False). Coloca True si el texto contiene el patron.

data <- data %>% mutate(code_res = match_output[,2], year_res = match_output[,3],
                        entidad_res = match_output[,4],
                        dGobreg = ifelse(str_detect(institución_ruc,"(^G)|(^R)"), 1 , 0 ),
                        dMinsa = ifelse(str_detect(institución_ruc,"^M"), 1 , 0 )
)



#----- Look around ------------


correo <- "rmendozam31@gmail.com"

str_match(correo, "(\\w+)\\@.*")


"Horarios de apertura"

# positive lookahead (?=)

# "aperuta 8:30:20:01:05 am, cierre 16:00pm"

data$apertura1 <- sapply(data$horario,
                              function(x) str_extract(x,"\\d+\\:\\d+(?= am)"))


data$apertura2 <- sapply(data$horario,
                         function(x) str_extract(x,"[\\d+\\:]+(?= am)"))

# positive lookbehind (?<=)

data$apertura3 <- sapply(data$horario,
                        function(x) str_extract(x,"(?<=apertura )[\\d+\\:]+"))


# usando Pips ( |>  y  %>% )


data$apertura3 <- data$horario |> str_extract("[\\d+\\:]+(?= am)")

data$apertura4 <- data$horario |> str_extract("(?<=apertura )[\\d+\\:]+")


data$apertura5 <- data$horario %>%  str_extract("[\\d+\\:]+(?= am)")

data$apertura6 <- data$horario %>%  str_extract("(?<=apertura )[\\d+\\:]+")


"Horarios de cierre"

# negative lookbehind (?<!)

data$cierre1 <- data$horario |> str_extract("(?<!apertura )\\d+\\:\\d+")

# negative lookbahead (?!)

data$pre_soles1 <- data$presupuesto |> str_extract("[\\d+\\,]+(?!\\$)")

View(data[,c("presupuesto","pre_soles1")])


# retirar tildes

data$presupuesto <- stri_trans_general(str = data$presupuesto, id = "Latin-ASCII")

# ---------------- Fechas en R -----------------

data$fecha_apertura_format <-  as.Date(data$fecha_apertura,format='%d/%m/%Y')


#date un different columns

data$year = as.numeric(format(data$fecha_apertura_format ,"%Y"))
data$month = as.numeric(format(data$fecha_apertura_format ,"%m"))
data$day = as.numeric(format(data$fecha_apertura_format ,"%d"))


# crear nueva variables de fecha

data <- data |> dplyr::mutate(
  date = dmy( fecha_apertura  )
)

# diferencia entre paste y paste0, paste0 une sin espacio
# mientras paste permite indicar como separar los strings

data$year <- NULL


#  Segunda aplicacion ----


# Select varias lineas de codigo (Ctrl + alt + cambios)

 junin_data = read_excel("../../data/Region_Junin.xlsx")
 newbase <- dplyr::filter(junin_data, grepl('AC', District))
 newbase <- dplyr::filter(junin_data, grepl('pacha', Place))

# ignore.case=TRUE: ignora mayuscula o minuscula (upper or lower case)

newbase <- junin_data %>% filter(grepl('pacha', Place, ignore.case=TRUE))


newbase <- junin_data %>% filter(grepl('CIUDAD', District, ignore.case=TRUE))

# Beginning word with hu

newbase <- junin_data %>% filter(grepl('^hu', District, ignore.case=TRUE))


# Ending word

newbase <- junin_data %>% filter(grepl('ro$', Place, ignore.case=TRUE))


newbase <-  junin_data %>% filter(grepl('^ac*', Place, ignore.case=TRUE))

# match : a , c , ac

newbase <-  junin_data %>% filter(grepl('^ac+', Place, ignore.case=TRUE))

# match : ac (strict)









