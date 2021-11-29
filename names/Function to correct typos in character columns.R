library(readxl)
library(rfishbase)
library(data.table)
library(tidyverse)

### LOAD MONITORING DATABASE ----

(Data <- read_xlsx ("ltem_2021_update_fish_data.xlsx", sheet= 1))

### Filter sheet 1, DAta Base Monitoring , for spelling correction ---- 

(DB_SPECIES <- read_xlsx ("data/ltem_2021_update_fish_data.xlsx", sheet= 1))
DBs_Species <-  DB_SPECIES %>% 
  select(Species, IDLabel) %>% 
  filter (IDLabel== "PEC") %>%
  filter(!str_detect(Species, " sp| spp" ))
#' LINE 13; The column "species" and "IDLabel" was selected
#' LINE 14; Filter the variable "pec" to skip others variables like "INV" 
#' For loop to species list ----

table <-(data.table(DBs_Species))


rfbc_species <- rfishbase::country(species_list = ) %>% 
    select(Species,country) %>% 
    filter(country== "Mexico")

rfbc_correct <- rfbc_species$Species


for (i in 1:nrow(table)){#range that goes for all the species rows in our table 
  string <- table[i, Species]# String of our species' names
  max <- 0 #Do not change these values
  similarity <- 0
  
  
  for(j in rfbc_correct){
    similarity <-   length(Reduce(intersect, strsplit(c(string, j), split = "")))
    if(similarity > max){
      max <- similarity
      to_replace <- j
    }
  }
  table[i,"Species"] <- to_replace
}
#### End ####

# Archivo final de los nombres de especies para "PEC"
Ltem_DataBase <- as.data.frame(table)


#######################################################################

# VALIDATE SPECIES NAMES --------------------------------------------------

### Filter sheet 3 to validate name ----
(D_B_SPECIES_2 <- read_xlsx ("data/ltem_2021_update_fish_data.xlsx", sheet= 3))

(D_B_S_Species_2 <-  D_B_SPECIES_2 %>% 
 rename(IDSpecies="IDSpecies (Species)") %>% 
   select(Species, Label, IDSpecies ) %>% 
  filter (Label== "PEC") %>%
  filter(str_detect(Species, " " )) %>% 
  filter(!str_detect(Species, " sp| spp" )))


#DB_ok <- data.frame(Original =DB_correct$Species, 
                   # New= validate_names(DB_correct$Species)) 

#DB_correct <- filter(DB_correct,(Original))


# fish_ok <- data.frame(Original =D_B_S_Species_2$Species, New= validate_names(D_B_S_Species_2$Species),
                     # IDSpecies = D_B_S_Species_2$IDSpecies) 
fish_ok <- D_B_S_Species_2 %>% 
  mutate(New= validate_names(Species),
         Validated= ifelse(Species== New, Species, New))

# otra florma
# A <- filter(fish_ok, (Original != New)) %>% 

