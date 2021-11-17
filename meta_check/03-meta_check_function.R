library(tidyverse)


refs_meta <- readRDS("data/refs_meta_fish.RDS")

fish <- readxl::read_xlsx("data/ltem_2021_update_fish_data.xlsx")


data_check <- function(x, type) {
            if (type == "Quantity") {
                        merge(x, refs_meta, by = "Species") %>% 
                                    mutate(q_flag = ifelse(Total > Quantity_max, "FLAG", "NO")) %>% 
                                    filter(q_flag == "FLAG")
                        
            }else if (type == "Size") {
                        merge(x, refs_meta, by = "Species") %>% 
                                    mutate(q_flag = ifelse(`talla num` > Size_max, "FLAG", "NO")) %>% 
                                    filter(q_flag == "FLAG")
            }else {
                        stop("Tienes que poner Quantity o Size!")
            }
            
} 


data_check(fish, "Size")





