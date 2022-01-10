library(tidyverse)


# Load historical database

## Pudes cargar una base cualquiera como ejemplo, ya que no tienes la histórica
ltem <- readRDS("report/outputs/cls_lor_cp_extract.RDS")


#Define our confidence level (99%)
alpha = 0.01

#Calculate statistics

## Me basé en tu script para intervalo de confianza de una media, solo modifiqué en algunas cosas
scores <- ltem %>% 
  group_by(IDSpecies, Species) %>% 
  summarise(m_size= mean(Size,na.rm = T), #Mean Size
            sd_size=sd(Size, na.rm = T), #Size standard deviation
            n=length(Species), #Size n
            se_size= sd_size/ sqrt(n), #Size standard error
            m_quantity= mean(Quantity), #Mean quantity
            sd_quantity=sd(Quantity), # Quantity standar deviation
            se_quantity= sd_quantity/ sqrt(n), # Quantity Standard error
            df= n-1, #Both Size $ Quantity degrees of freedom
            t_score= qt(p=alpha/2, df=df, lower.tail = F), # T-Student scores
            me_size= t_score*se_size, #Size margin errors
            me_quantity= t_score*se_quantity # Quantity margin errors
  )
            
refs_meta <- scores%>% 
  filter(n > 1) %>% #Elimate all species with only 1 observation
  mutate(flag= ifelse(n<=30, "Flag", "Correct")) %>% #Flag all species with a sample number below 30
  group_by(IDSpecies, Species) %>% 
  summarise(flag=flag,
            #Ranges of confidence intervals
            Size_min=round(m_size-me_size,0),
            Size_max= round(m_size +me_size,0),
           Quantity_max=round( m_quantity+me_size, 0))

#Max & Min values for Size and Quantity
mm <- ltem %>% 
  group_by(IDSpecies, Species) %>% 
  summarise(size_min= min(Size, na.rm = T),
         size_max= max(Size, na.rm= T),
         q_max= max(Quantity, na.rm=T))

#Replace all flagged confidence intervals with their respective Max & Min size and quantity values
refs_meta <- merge(refs_meta, mm, by= c("IDSpecies", "Species"), all=T) %>% 
  mutate(flag= ifelse(is.na(flag), "Flag", flag),
         c_interval= ifelse(flag=="Correct", "True", "False" ),
        Size_min= ifelse(flag=="Flag", size_min, Size_min),
         Size_max= ifelse(flag=="Flag", size_max, Size_max),
         Quantity_max= ifelse(flag=="Flag", q_max, Quantity_max)
         ) %>% 
  select(-c(flag, size_max, size_min, q_max))