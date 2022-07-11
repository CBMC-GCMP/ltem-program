library(tidyverse)

### CARGAR DATOS ---------------

fish_data <- readxl::read_xlsx("data/ltem_2021_update_fish_data.xlsx")

fish_meta <- readxl::read_xlsx("data/ltem_2021_update_fish_data.xlsx", sheet = 3)

# Juntar las bases fish_data y fish_meta (con la formula merge(), join_left())

library(dplyr)

fish_comun <- fish_data %>% 
  inner_join(fish_meta, by=c("Species"= "Species"))

fish_complement <- fish_data %>% 
  left_join(fish_meta, by=c("Species"= "Species"))

data_species <- merge(fish_data, fish_meta, by = "Species")

fish_ID <- merge(fish_data, fish_meta[,c("")], by = "ID"))

# calculo de la biomasa (mutate(

data_species %>% 
  select(Species, `A ord`, `talla num`,`B pen`, Total)

  mutate(data_species,
         Biomasa= ((`A ord` * `talla num`)^`B pen`))
  


### Calculo de la biomasa (para peces)
# W = aTL^b
(A ord * talla num)^B pen



### Graficas de biomasa promedio por Reef en 2021

data_species %>% 
  group_by(Region, Reefs, IDDepths, IDTransects, Species) %>%
  summarise(Biomasa= sum(Biomasa)) %>% 
  group_by(Reefs)
  summarise(Biomasa = mean(Biomasa)) %>% 
  ggplot(aes(x= reorder(Reefs,Biomasa), y=Biomasa)) +
  geom_col()+
  labs(x="", y="Biomasa ton/he")
  coord_flip()
  theme_bw()
  

data_species %>% 
    group_by(Region, Reefs, IDDepths, IDTransects, Species) %>%
    summarise(Biomasa= sum(Biomasa)) %>% 
    group_by(Species)
    summarise(Biomasa = mean(Biomasa)) %>% 
    top_n(10) %>% 
    ggplot(aes(x= reorder(Species,Biomasa), y=Biomasa)) +
    geom_col()+
    labs(x="", y="Biomasa ton/he")
    coord_flip()
    theme_bw()
    theme(axis.text.y = element_text(face= "italic"))
  

