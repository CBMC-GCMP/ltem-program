library(tidyverse)

### CARGAR DATOS ---------------

fish_data <- readxl::read_xlsx("data/ltem_2021_update_fish_data.xlsx")
fish_meta <- readxl::read_xlsx("data/ltem_2021_update_fish_data.xlsx", sheet = 3)

# Juntar las bases fish_data y fish_meta (con la formula merge(), join_left())



# calculo de la biomasa (mutate())



### Calculo de la biomasa (para peces)
# W = aTL^b


### Graficas de biomasa promedio por Reef en 2021



