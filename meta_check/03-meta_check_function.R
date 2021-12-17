library(tidyverse)

 historical <- readxl:: read_xlsx("data/ltem_monitoring.xlsx")

# refs_meta <- readxl:: read_xlsx("data/refs_meta_fish.xlsx")


# refs_meta <- historical %>% 
#   group_by(IDSpecies, Species) %>% 
#   summarise(Size_min = min(Size, na.rm=T),
#          Size_max =max(Size, na.rm=T),
#          Quantity_max= max(Quantity, na.rm=T))
# 
# 
# 
# test <- merge(refs_meta, clean_fish[,c("IDSpecies", "Validated")], by="IDSpecies", all=T) %>% 
#   mutate(Species= ifelse(is.na(Validated), Species, Validated)) %>% 
#   select(-Validated)
# 
# refs_meta<- merge(test, clean_inv[,c("IDSpecies", "valid_name")], by="IDSpecies", all=T) %>% 
#   mutate(Species= ifelse(is.na(valid_name), Species, valid_name)) %>% 
#   select(-valid_name) %>% 
#   filter(!is.na(Quantity_max))
# 
# writexl::write_xlsx(refs_meta, "data/auxiliar/refs_meta.xlsx")


refs_meta <- readxl:: read_xlsx("data/auxiliar/updated_catalogo_spp.xlsx")
  


ltem <- readxl::read_xlsx("data/ltem_2021_valid_names.xlsx") 



data_check <- function(x, type) {
            if (type == "Quantity") {
                        merge(x, refs_meta[,c("IDSpecies", "Size_min", "Size_max", "Quantity_max")], 
                              by = "IDSpecies") %>% 
                                    mutate(q_flag = ifelse(Quantity > Quantity_max, "FLAG_Q", "NO")) %>% 
                                    filter(q_flag == "FLAG_Q")
                        
            }else if (type == "Size") {
                        merge(x, refs_meta[,c("IDSpecies", "Size_min", "Size_max", "Quantity_max")], by = "IDSpecies") %>% 
                                    mutate(q_flag = ifelse(Size > Size_max, "FLAG_S", "NO"),
                                           q_flag= ifelse(Size < Size_min, "FLAG_S_min", q_flag)) %>% 
                                    filter(q_flag =="FLAG_S"|q_flag =="FLAG_S_min" )
                
            }else {
                        stop("Tienes que poner Quantity o Size!")
            }
            
} 




size <- data_check(ltem, "Size")

quantity <- data_check(ltem, "Quantity")


writexl::write_xlsx(size, "data/auxiliar/size_check.xlsx")


writexl::write_xlsx(quantity, "data/auxiliar/quantity_check.xlsx")




c_size <- read_xlsx("data/auxiliar/size_check.xlsx") %>% 
  filter(status=="eliminar")
c_quantity <- read_xlsx("data/auxiliar/quantity_check.xlsx") %>% 
  filter(status=="eliminar")


merge_ltem <- merge(ltem, c_size[,c("ID", "status")], by="ID", all=T) %>%
  mutate(status= ifelse(is.na(status), "correct", status)) %>% 
  filter(status != "eliminar") %>% 
  select(-status)

ltem_database <- merge(merge_ltem, c_quantity[,c("ID", "status")], by="ID", all=T) %>%
  mutate(status= ifelse(is.na(status), "correct", status)) %>% 
  filter(status != "eliminar") %>% 
  select(-status, -ID) %>% 
  rename(Reef=Reefs)



writexl::write_xlsx(ltem_database,"data/ltem_database_07122021.xlsx")

