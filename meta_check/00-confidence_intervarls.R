library(tidyverse)

ltem <- readRDS("report/outputs/cls_lor_cp_extract.RDS")

alpha = 0.01

scores <- ltem %>% 
  group_by(IDSpecies, Species) %>% 
  summarise(m_size= mean(Size,na.rm = T),
            sd_size=sd(Size, na.rm = T),
            n=length(Species),
            se_size= sd_size/ sqrt(n),
            m_quantity= mean(Quantity),
            sd_quantity=sd(Quantity),
            se_quantity= sd_quantity/ sqrt(n),
            df= n-1,
            t_score= qt(p=alpha/2, df=df, lower.tail = F),
            me_size= t_score*se_size,
            me_quantity= t_score*se_quantity
  )
            
refs_meta <- scores%>% 
  filter(n > 1) %>% 
  mutate(flag= ifelse(n<=30, "Flag", "Correct")) %>%
  group_by(IDSpecies, Species) %>% 
  summarise(flag=flag,
            Size_min=round(m_size-me_size,0),
            Size_max= round(m_size +me_size,0),
           Quantity_max=round( m_quantity+me_size, 0))

mm <- ltem %>% 
  group_by(IDSpecies, Species) %>% 
  summarise(size_min= min(Size, na.rm = T),
         size_max= max(Size, na.rm= T),
         q_max= max(Quantity, na.rm=T))

refs_meta <- merge(refs_meta, mm, by= c("IDSpecies", "Species"), all=T) %>% 
  mutate(flag= ifelse(is.na(flag), "Flag", flag),
         c_interval= ifelse(flag=="Correct", "True", "False" ),
        Size_min= ifelse(flag=="Flag", size_min, Size_min),
         Size_max= ifelse(flag=="Flag", size_max, Size_max),
         Quantity_max= ifelse(flag=="Flag", q_max, Quantity_max)
         ) %>% 
  # filter(Quantity_max > 1) %>% 
  select(-c(flag, size_max, size_min, q_max))