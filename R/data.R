## base de datos cruda del censo 2020
## cargue la base de datos desde mi propio directorio, utilizando el especifico para esta.
## Lo importante es que la base esa guardada como un archivo .RData 

library(tidyverse)
library(readxl)
library(readr)
library(stringr)

## Fuentes/Censos/input/censo2020.xls
data <- read_xlsx("/Users/fernandorojasgonzalez/Documents/Fuentes/Censos/Input/Censo2020.xlsx",
                  skip = 11, col_names = F)
save(data,file = "R/data.RData")
