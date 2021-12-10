
# Loading libraries -------------------------------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(ggspatial)
library(patchwork)
library(see)
library(ggpval)
library(scales)



# Loading logo CBMC and GCMP

img <- png::readPNG("../San Basilio 2021/logos/LOGOMAIN.png")
rast <- grid::rasterGrob(img, interpolate = T)

# Loading MPA shapefile

mx_mpa <- read_sf("../Atlas-de-Buceo-Mexico/shp/MX_MPAs.shp")

# Loading MX shapefile 

spdf_mx <- st_transform(st_as_sf(ne_countries(country = 'mexico', 
                                              scale = "large")), crs = 4326)


# Loading database output from 00-data_updater.R script

db <- readRDS("report/outputs/ltem_output_base.RDS") %>% 
  filter(Reef != "DEDO_NEPTUNO") %>% 
  mutate(Season = ifelse(Month < 10, "August", "November"))



# Merging with historical data --------------------------------------------



histor <- readRDS("report/outputs/cls_lor_cp_extract.RDS")

histor %>% 
  filter(Region == "Los Cabos") %>% 
  select(Year, Label, Species) %>% 
  unique() %>% 
  group_by(Year, Label) %>% 
  count() %>% 
  filter(Year > 2000) %>% 
  group_by(Label) %>% 
  summarise(rich = mean(n), rich_sd = sd(n, na.rm = T))

histor <- histor %>% 
  select(Label, Year, Month, Region, Reef, MPA, TrophicLevelF, TrophicGroup, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)

db_2 <- db %>% 
  select(Label, Year, Month, Region, Reef, MPA, TrophicLevelF, TrophicGroup, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)

db_merged <- rbind(histor, db_2) %>% 
  mutate(Biomass = as.numeric(Biomass)) %>% 
  filter(Month != 11) # Here I eliminate the double Los Cabos expedition

rm(histor, db_2)




# Map of rocky reef study area --------------------------------------------


# Converting db to spatial object

db_sf <- st_as_sf(db, coords = c("Longitude", "Latitude"), crs = "WGS84", remove = F)


# Getting a bounding box for los cabos coordinates
coord_lim <- st_bbox(
  db_sf %>% 
    filter(Region == "Los Cabos")
)


# Creating a reef ID to map according to longitude 

id_reefs <- db %>% 
  filter(Region == "Los Cabos") %>% 
  select(Reef, Longitude) %>% 
  unique() %>% 
  arrange(Longitude) %>% 
  mutate(ID_map = 1:length(Reef)) %>% 
  select(-Longitude)

# adding the new id to the dataset 

db_sf <- merge(db_sf, id_reefs, by = "Reef")

# actual Mapping

reef_map <- ggplot(spdf_mx) +
  geom_sf(fill = "gray90", 
          col = "gray20") +
  geom_sf(data = mx_mpa, 
          fill = NA, 
          col = "red") +
  geom_sf(data = db_sf, 
          aes(col = MPA, label = ID_map)) +
  coord_sf(xlim = c(-110, -109.7), 
           ylim = c(22.8, 23.0)) +
  geom_label_repel(data = db_sf %>% 
                    filter(Region == "Los Cabos") %>% 
                    as.data.frame() %>% 
                    select(Region, ID_map, Longitude, Latitude, MPA) %>% 
                    unique(), 
                  aes(x = Longitude, y = Latitude, col = MPA, label = ID_map)) +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_minimal) +
  annotation_custom(rast, ymin = 22.80, ymax = 22.83, xmin = -110.0, xmax = -109.85) +
  scale_color_manual(values = c("firebrick", "darkgreen")) +
  theme_bw() +
  theme(legend.position = "")


# inset map
gen_map <- ggplot(db_sf) +
  geom_sf(data = spdf_mx, 
          fill = "gray90",
          col = NA) +
  annotate("rect", 
           xmin = -112, 
           xmax = -109, 
           ymin = 22.0, 
           ymax = 23.0, 
           alpha = 0.2, 
           fill = "red",
           col = "black") +
  ggthemes::theme_map() +
  theme(panel.background = element_rect(fill = "white"))


# Assembling the map
reef_map + inset_element(gen_map, 0.8, 0.8, 1, 1, align_to = 'full')

#saving the map
ggsave("report/figs/figure_1_map.png", dpi = 600)











# Fish biomass ------------------------------------------------------------

## Biomass graph violinplots

p1 <- db_sf %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "PEC", Region == "Los Cabos") %>% 
  group_by(Season, ID_map, MPA, Reef, Transect, Species) %>% 
  summarise(Biomass = sum(Biomass)) %>% 
  group_by(Season, ID_map, MPA, Reef, Transect) %>% 
  summarise(Biomass = mean(Biomass)) %>% 
  ggplot(aes(x = factor(ID_map), y = (Biomass))) +
  geom_violin(trim = F, aes(fill = MPA), alpha = .3) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = MPA), alpha = .3) +
  scale_color_manual(values = c("firebrick", "darkgreen")) +
  scale_fill_manual(values = c("firebrick", "darkgreen")) +
  ylim(0, 1.5) +
  labs(x = "", y = "Biomass (Ton/ha)", fill = "Protection level") +
  theme_modern() +
  theme(legend.position = "top") +
  guides(colour = "none")


p2 <- db_sf %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "PEC", Region == "Los Cabos") %>% 
  group_by(Season, ID_map, MPA, Reef, Transect, Species) %>% 
  summarise(Biomass = sum(Biomass)) %>% 
  group_by(Season, ID_map, MPA, Reef, Transect) %>% 
  summarise(Biomass = mean(Biomass)) %>% 
  ggplot(aes(x = factor(MPA), y = (Biomass))) +
  geom_violin(trim = F, aes(fill = MPA), alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = MPA), alpha = .3) +
  scale_color_manual(values = c("firebrick", "darkgreen")) +
  scale_fill_manual(values = c("firebrick", "darkgreen")) +
  ylim(0, 1.5) +
  labs(x = "", y = "Biomass (Ton/ha)", fill = "Protection level") +
  theme_modern() +
  theme(legend.position = "top") +
  guides(colour = "none")







# Region comparison -------------------------------------------------------


p3 <- db %>% 
  mutate(Region = factor(Region)) %>% 
  #mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  #filter(ID_map != "2") %>% 
  filter(Label == "PEC", Region == "Los Cabos") %>% 
  group_by(Season, Region, Reef, Transect, Species) %>% 
  summarise(Biomass = sum(Biomass)) %>% 
  group_by(Season, Region, Reef, Transect) %>% 
  summarise(Biomass = mean(Biomass, na.rm = T)) %>% 
  ggplot(aes(x = factor(Season), y = (Biomass))) +
  geom_violin(trim = F, aes(fill = Season), alpha = .3) +
  geom_boxplot(width  = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = Season)) +
  scale_color_material_d() +
  scale_fill_material_d() +
  ylim(0, 1.5) +
  labs(x = "", y = "Biomass (Ton/ha)", fill = "Season") +
  theme_modern() +
  theme(legend.position = "top") +
  guides(colour = "none")


p3




add_pval(p3, pairs = list(c(1, 2))) + add_pval(p2, pairs = list(c(1, 2))) + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("report/figs/figure_fish_biomass_reefs.png", dpi = 600, height = 6, width = 8)


# Historical trends -------------------------------------------------------



histor <- readRDS("report/outputs/cls_lor_cp_extract.RDS")

histor %>% 
  filter(Region == "Los Cabos") %>% 
  select(Year, Label, Species) %>% 
  unique() %>% 
  group_by(Year, Label) %>% 
  count() %>% 
  filter(Year > 2000) %>% 
  group_by(Label) %>% 
  summarise(rich= mean(n), rich_sd = sd(n, na.rm = T))
  


histor <- histor %>% 
  select(Label, Year, Month, Region, Reef, MPA, TrophicLevelF, TrophicGroup, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)


db_2 <- db %>% 
  select(Label, Year, Month, Region, Reef, MPA, TrophicLevelF, TrophicGroup, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)

db_merged <- rbind(histor, db_2) %>% 
  mutate(Biomass = as.numeric(Biomass)) %>% 
  filter(Month != 11) 
  




# historical region comparison -------------------------------------------------------


p4 <- db_merged %>% 
  mutate(Region = factor(Region, 
                  levels = c("Loreto", "Cabo Pulmo", "Los Cabos"))) %>% 
  #mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  #filter(ID_map != "2") %>% 
  filter(Label == "PEC") %>% 
  group_by( Region, Reef, Transect, Species) %>% 
  summarise(Biomass = sum(Biomass)) %>% 
  group_by( Region, Reef, Transect) %>% 
  summarise(Biomass = mean(Biomass, na.rm = T)) %>% 
  ggplot(aes(x = factor(Region), y = (Biomass))) +
  geom_violin(trim = F, aes(fill = Region), alpha = .3) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = Region), alpha = .3) +
  geom_boxplot(width  = 0.1, outlier.shape = NA, alpha = .3) +
  scale_color_material_d() +
  scale_fill_material_d() +
  ylim(0, 3.5) +
  labs(x = "", y = "Biomass (Ton/ha)", fill = "Region") +
  theme_bw() +
  theme(legend.position = "") +
  guides(colour = "none") 


p4

add_pval(p4, pairs = list(c(1, 2), c(1,3), c(2,3)))


p2 <- db %>% 
  as.data.frame() %>% 
  filter(Label == "PEC") %>%  
  group_by(Year, Region, Reef, MPA, Transect, TrophicGroup) %>% 
  summarise(tot_biomass = sum(Biomass)) %>% 
  group_by(Region, TrophicGroup) %>% 
  summarise(tot_biomass = mean(tot_biomass)) %>% 
  group_by(Region) %>% 
  mutate(rel_biomass = (tot_biomass/sum(tot_biomass))*100) %>%  
  ggplot(aes(x = Region, y = rel_biomass, fill = TrophicGroup)) +
  geom_col(col = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(y="Relative biomass (%)", x= "", fill = "") +
  theme_bw()



(p4 + p2 ) +
  plot_annotation(tag_levels = "A")

ggsave("report/figs/trophic_bars.png", dpi = 600, width = 7, height = 5)




###### Junk


pec <- db_sf %>% 
  as.data.frame() %>% 
  filter(ID_map != "2") %>% 
  filter(Label == "PEC", Region == "Los Cabos") %>%  
  group_by(Region, Reef, MPA, Latitude, Longitude, Depth, Transect, Species) %>% 
  summarise(tot_biomass = sum(Biomass)) %>% 
  group_by(Region, Reef, MPA, Latitude, Longitude, Depth, Transect) %>% 
  summarise(tot_biomass = mean(tot_biomass))
  


rel_pec <- db_sf %>% 
  as.data.frame() %>% 
  filter(ID_map != "2") %>% 
  filter(Label == "PEC", Region == "Los Cabos") %>% 
  group_by(Region, Reef, MPA, Latitude, Longitude, Depth, Transect, TrophicLevelF) %>% 
  summarise(biomass = sum(Biomass)) %>% 
  group_by(Region, Reef, MPA, Latitude, Longitude, Depth, Transect) %>% 
  mutate(rel_biomass = (biomass / sum(biomass))*100) 

fishes <- merge(rel_pec, pec) %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA")))

levels(factor(fishes$MPA))

fishes$logTotBiom <- log1p(fishes$tot_biomass)


fishes$Fbiomass <- cut(fishes$logTotBiom, 
                       breaks = c(0, 0.25, 0.5, 1.0, 2), 
                       labels = c("0-0.25", "0.25-0.50", "0.50-1", ">1"), 
                       right = FALSE)


p2 <- fishes %>% 
  group_by(MPA, TrophicLevelF) %>%
  summarise(biomass = sum(biomass)) %>% 
  group_by(MPA) %>% 
  mutate(rel_biomass = biomass/sum(biomass)) %>% 
  ggplot(aes(x=factor(TrophicLevelF), y=rel_biomass, fill=TrophicLevelF))+
  geom_col(alpha =.4, col="black")+
  facet_grid(~MPA)+
  scale_fill_brewer(palette = "Set1", name = "Trophic level")+
  labs(x="Trophic level", y = "Relative biomass (%)") +
  scale_y_continuous(limits = c(0, .85), breaks = seq(0.25, 0.75, by = 0.25))+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(colour = "gray90"), 
        legend.position = "", text= element_text(family="serif"))

p3 <- fishes %>% 
  group_by(Region, TrophicLevelF) %>%
  summarise(biomass = sum(biomass)) %>% 
  group_by(Region) %>% 
  mutate(rel_biomass = biomass/sum(biomass)) %>% 
  ggplot(aes(x=factor(TrophicLevelF), y=rel_biomass, fill=TrophicLevelF))+
  geom_col(alpha =.4, col="black")+
  facet_grid(~Region)+
  scale_fill_brewer(palette = "Set1", name = "Trophic level")+
  labs(x="Trophic level", y = "Relative biomass (%)")+
  scale_y_continuous(limits = c(0, .85), breaks = seq(0.25, 0.75, by = 0.25))+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(colour = "gray90"), 
        legend.position = "", text= element_text(family="serif"))

(p2/p3)+plot_annotation(tag_levels = "A")

ggsave('report/figs/trophic_pyramid.tiff', width = 6, height = 13)




