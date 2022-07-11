library(tidyverse)
library(sf)

ltem <- readRDS("data/auxiliar/LTEM_historic_v2.RDS")

unique_sites <- ltem %>% 
  select(IDReef,Reef, Latitude,Longitude) %>% 
group_by(IDReef, Reef) %>% 
  unique()
unique_sites<- st_as_sf(unique_sites, coords = c("Longitude", "Latitude"), crs = 4326)
st_write(unique_sites, "data/auxiliar/shp/unique_sites.shp")

# sites <- ltem %>%
#   group_by(IDReef, Reef, Depth) %>%
#   summarise(
#     Total_years = length(unique(Year)),
#     Transects_number = max(Transect),
#     Transects_total = length(unique(Transect)),
#     Species_mean = round(mean(Quantity),0),
#     Latitude = unique(Latitude),
#     Longitude = unique(Longitude)
#   ) %>%
#   arrange(Total_years, Species_mean)
# 
# 
#  
# write.csv(sites, "data/auxiliar/sites_summary.csv")

# sites <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

sites_year <- ltem %>%
  group_by(Year,IDReef, Reef, Depth) %>%
  summarise(
            Transects_number= max(Transect),
            Transects_total= length(unique(Transect)),
            Species_n=sum(Quantity),
            Latitude=unique(Latitude),
            Longitude=unique(Longitude)
            
  ) %>%  

  group_by(IDReef, Reef,Depth) %>%
summarise(
          Year_min= min(Year),
          Year_max=max(Year),  
          Years= list(unique(Year)),
          Total_years= length(unique(Year)),
          Transects_min= min(Transects_total),
          Transects_max=max(Transects_total),
          Species_mean=round(mean(Species_n),0),
          Latitude=Latitude,
          Longitude=Longitude
) %>% 
  unique() 
  

sites_sp <- sites_year %>% 
  select(-Years) %>% 
  mutate(Category= ifelse(Total_years==1, "1 año", "ELSE"),
         Category= ifelse(2 <= Total_years & Total_years <=6 , "2-6 años", Category),
         Category= ifelse(7<= Total_years & Total_years <=11 , "7-11 años", Category),
         Category= ifelse(12<= Total_years & Total_years <=22, "12-22 años", Category))

write.csv(sites_sp, "data/auxiliar/sites_check.csv")


sites_sp <- st_as_sf(sites_sp, coords = c("Longitude", "Latitude"), crs = 4326)

st_write(sites_sp, "data/auxiliar/shp/sites_sp.shp")








##

all_sites_2009 <- ltem %>%
  filter(Year>=2009) %>% 
  select(IDReef, Reef, Latitude, Longitude) %>% 
  unique()
write.csv(all_sites_2009, "data/auxiliar/all_sites_2009.csv")

sites_2009 <- sites_sp %>% 
  as.data.frame() %>% 
  filter(Year_min >= 2009)

write.csv(sites_2009, "data/auxiliar/sites_since_2009.csv")
