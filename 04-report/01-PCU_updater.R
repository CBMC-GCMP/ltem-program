library(tidyverse)
library(readxl)

pcu_h <- read_xlsx("data/PCU/em_gc_pcu_san_basilio.xlsx") %>% 
  select(-"Main id")

sb <- read_xlsx("data/PCU/PCU_SB.xlsx")


sb <- sb %>% 
  mutate(points=ifelse(is.na(points), 150, points),
         cover=count/points) %>% 
  select(-c(monitor, habitat))

updated <- rbind(pcu_h, sb) %>% 
  rowid_to_column("Maind_id")

writexl::write_xlsx(updated, "data/PCU/em_gc_pcu_san_basilio_30062022.xlsx")

names(pcu_h)
names(sb)