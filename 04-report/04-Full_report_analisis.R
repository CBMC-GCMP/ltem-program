
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
library(tidytext)




# Loading additional data -------------------------------------------------

# Loading logo CBMC and GCMP

img <- png::readPNG("report/logos/LOGOMAIN.png")
rast <- grid::rasterGrob(img, interpolate = T)

# Loading MPA shapefile

mx_mpa <- read_sf("report/shp/MX_MPAs.shp")

# Loading MX shapefile 

spdf_mx <- st_transform(st_as_sf(ne_countries(country = 'mexico', 
                                              scale = "large")), crs = 4326)






# Loading 2021 database ---------------------------------------------------




# Loading database output from 00-data_updater.R script

db <- readRDS("report/outputs/ltem_output_base.RDS") %>% 
  filter(Reef != "DEDO_NEPTUNO") %>% 
  mutate(Season = ifelse(Month < 10, "August", "November")) # creating expedition factor



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
  select(Label, Year, Month, Region, Reef, MPA, TrophicLevelF, TrophicGroup, Taxa2, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)

db_2 <- db %>% 
  select(Label, Year, Month, Region, Reef, MPA, TrophicLevelF, TrophicGroup, Taxa2, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)

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



# Creating the map



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








# Invertebrates -----------------------------------------------------------

# Top 10 abundance------
(p1 <- db_sf %>% 
   mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>%
   filter(Label == "INV", Region == "Los Cabos") %>% 
   group_by(ID_map, MPA, Reef, Transect, Species) %>%
   summarise(Abundance = sum(Quantity)) %>%
   group_by(MPA,Species) %>% 
   summarise(Abundance = mean(Abundance)) %>%
   # filter(MPA=="MPA") %>%
   group_by(MPA) %>%
   top_n(10, Abundance) %>%
   ungroup() %>% 
   mutate(Species= reorder_within(Species, Abundance, MPA)) %>% 
   ggplot(aes(x=Species, y = Abundance, fill=MPA)) +
   geom_col()+
   coord_flip()+
   facet_wrap(~MPA, scales="free_y")+
   scale_x_reordered() +
   scale_color_material_d() +
   scale_fill_manual(values = c("firebrick", "darkgreen"))+
   labs(y = "Abundance", x="", title= "2021") +
   theme_modern() +
   theme(legend.position = "") +
   guides(colour = "none")+
   theme(axis.text.y = element_text(face= "italic"),
         plot.title = element_text(hjust=0.5)
   )
)


p2 <- db_merged %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "INV", Region == "Los Cabos") %>% 
  group_by(Year, MPA, Reef, Transect, Species) %>%
  summarise(Abundance = sum(Quantity)) %>%
  group_by(MPA, Species) %>% 
  summarise(Abundance = mean(Abundance)) %>%
  # filter(MPA=="MPA") %>%
  group_by(MPA) %>%
  top_n(10, Abundance) %>%
  ungroup() %>% 
  mutate(Species= reorder_within(Species, Abundance, MPA)) %>% 
  ggplot(aes(x=Species, y = Abundance, fill=MPA)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~MPA, scales="free_y")+
  scale_x_reordered() +
  scale_color_material_d() +
  scale_fill_manual(values = c("firebrick", "darkgreen"))+
  labs(y = "Abundance", x="", title="Historical") +
  theme_modern() +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"),
        plot.title = element_text(hjust=0.5))


(p1/p2)+ plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave("report/figs/top10_inv.png", width = 10, height = 7, dpi = 600)


# Violinplots Invertebrate abundances----
p1 <- db_sf %>% 
  as.data.frame() %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "INV", Region == "Los Cabos", Taxa2 %in% c("Echinoidea", "Asteroidea", "Hexacorallia", "Octocorallia")) %>% 
  group_by(Season, ID_map, MPA, Reef, Transect, Taxa2) %>% 
  summarise(Quantity = sum(Quantity)/30) %>% 
  ggplot(aes(x = factor(MPA), y = (Quantity))) +
  geom_violin(trim = F, aes(fill = MPA), alpha = .3) +
  geom_boxplot(width  = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = MPA), alpha = .3) +
  scale_color_manual(values = c("firebrick", "darkgreen")) +
  scale_fill_manual(values = c("firebrick", "darkgreen")) +
  ylim(0, 20) +
  labs(x = "", y = "Abundance", fill = "Protection level") +
  theme_modern() +
  facet_grid(~Taxa2) +
  theme(legend.position = "top") +
  guides(colour = "none")

p2 <- db_sf %>% 
  as.data.frame() %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "INV", Region == "Los Cabos", Taxa2 %in% c("Echinoidea", "Asteroidea", "Hexacorallia", "Octocorallia")) %>% 
  group_by(Season, ID_map, MPA, Reef, Transect, Taxa2) %>% 
  summarise(Quantity = sum(Quantity)/30) %>% 
  ggplot(aes(x = factor(Season), y = (Quantity))) +
  geom_violin(trim = F, aes(fill = Season), alpha = .3) +
  geom_boxplot(width  = 0.1, outlier.shape = NA) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = Season), alpha = .3) +
  scale_color_manual(values=c("orange", "purple")) +
  scale_fill_manual(values=c("orange", "purple")) +
  ylim(0, 20) +
  labs(x = "", y = "Abundance", fill = "Season") +
  theme_modern() +
  facet_grid(~Taxa2) +
  theme(legend.position = "top") +
  guides(colour = "none")


add_pval(p2, pairs = list(c(1, 2))) / add_pval(p1, pairs = list(c(1, 2))) + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")


ggsave("report/figs/invertebrates_comparison.png", dpi = 600, height = 10, width = 10)


### Density plot invertebrate abundances -----

(p1 <- db_sf %>% 
   as.data.frame() %>% 
   mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
   filter(Label == "INV", Region == "Los Cabos", Taxa2 %in% c("Echinoidea", "Asteroidea", "Hexacorallia", "Octocorallia")) %>% 
   group_by(Season, ID_map, MPA, Reef, Transect, Taxa2) %>% 
   summarise(Quantity = sum(Quantity)/30) %>%
   ggplot(aes(x=Quantity, group=Season, fill=Season))+
   geom_density(adjust=1.5, alpha=.4)+
   ylim(0,0.75)+
   # xlim(0,10)+
   facet_grid(~Taxa2, scales="free_x")+
   labs(x="Abundance", y="Density")+
   scale_fill_manual(values = c("August" = "orange",
                                "November" = "purple"))+
   
   # scale_x_continuous(breaks = seq(0,10, by=5)) + 
   theme_modern() +
   theme(legend.position = "top") +
   guides(colour = "none")
)


(p2 <- db_sf %>% 
    as.data.frame() %>% 
    mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
    filter(Label == "INV", Region == "Los Cabos", Taxa2 %in% c("Echinoidea", "Asteroidea", "Hexacorallia", "Octocorallia")) %>% 
    group_by(Season, ID_map, MPA, Reef, Transect, Taxa2) %>% 
    summarise(Quantity = sum(Quantity)/30) %>%
    ggplot(aes(x=Quantity, group=MPA, fill=MPA))+
    geom_density(adjust=1.5, alpha=.4)+
    ylim(0,0.75)+
    # xlim(0,10)+
    facet_grid(~Taxa2,scales="free_x")+
    labs(x="Abundance", y="Density")+
    scale_fill_manual(values = c("Open Area" = "firebrick",
                                 "MPA" = "darkgreen"))+
    
    # scale_x_continuous(breaks = seq(0,10, by=5)) + 
    theme_modern() +
    theme(legend.position = "top") +
    guides(colour = "none")
)
(p1/p2)+ plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("report/figs/groups_comparison_dens.png", width = 10, height = 7, dpi = 600)



# Invertebrate trends----
db_merged %>% 
  filter(Label == "INV", 
         Year > 2008,
         Region == "Los Cabos", 
         Taxa2 %in% c("Echinoidea", "Asteroidea", "Hexacorallia", "Octocorallia")) %>%  
  mutate(Taxa2 = factor(Taxa2, levels = c("Asteroidea", "Echinoidea", "Hexacorallia", "Octocorallia"), 
         labels = c("Asteroidea", "Echinoidea", "Hexacorallia", "Octocorallia"))) %>% 
  group_by(Year, Reef, Transect, Taxa2) %>% 
  summarise(Quantity = sum(Quantity)) %>% 
  group_by(Year, Reef, Taxa2) %>% 
  summarise(Quantity = mean(Quantity)) %>% 
  ggplot(aes(x = Year, y = (Quantity), col = Taxa2, fill = Taxa2)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Taxa2, scales = "free_y") +
  scale_color_material_d() +
  scale_fill_material_d() +
  labs(x = "", y = "Abundance") +
  theme_bw() +
  theme(legend.position = "") +
  guides(colour = "none") 


ggsave("report/figs/invertebrate_timetrend.png", dpi = 600, height = 6, width = 6)



# Abundancia Relativa por grupos -----
(p1 <- db %>% 
  as.data.frame() %>% 
  filter(Label == "INV",
         Taxa2 %in%
           c("Echinoidea", "Asteroidea", "Hexacorallia", "Octocorallia")) %>%  
  group_by(Year, Region, Reef, MPA, Transect, Taxa2) %>% 
  summarise(tot_abundance = sum(Quantity)) %>% 
  group_by(Region, Taxa2) %>% 
  summarise(tot_abundance = mean(tot_abundance)) %>% 
  group_by(Region) %>% 
  mutate(rel_abundance = (tot_abundance/sum(tot_abundance))*100) %>%  
  ggplot(aes(x = Region, y = rel_abundance, fill = Taxa2)) +
  geom_col(col = "black", width = 0.5) +
  scale_fill_brewer(palette = "Set1") +
  labs(y="Relative abundance (%)", x= "", fill = "") +
  theme_bw()
)
ggsave("report/figs/relative_abundance.png", width = 4, height = 3, dpi = 600)

# Abundancia promedio historica INVERTEBRADOS------
tomod <- db_merged %>%
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "INV", Region == "Los Cabos",
         Taxa2 %in%
      c("Echinoidea", "Asteroidea", "Hexacorallia", "Octocorallia")) %>% 
  filter(Year > 2000) %>% 
  group_by(Year, Reef, MPA,Taxa2, Species, Transect) %>% 
  summarise(Quantity = sum(Quantity)) %>% 
  group_by(Year, Reef, MPA) %>% 
  summarise(Abundance = mean(Quantity))


library(mgcv)
library(gratia)

m1 <- gam(Abundance ~ s(Year, by = MPA), data = tomod, family = Gamma(link = "identity"))

summary(m1)

draw(m1)


m1 <- gam(Abundance ~ s(Year), data = tomod, family = Gamma(link = "identity"))

summary(m1)
draw(m1)

gam.check(m1)


new_data <- data.frame(
  Year = seq(from = 2009, to = 2021, by = 1)
)

pred_paci <- predict(m1,new_data, se.fit = T)




new_data$predicted <- pred_paci$fit
new_data$se.fit <- pred_paci$se.fit

new_data_full <- merge(new_data, tomod, by=c("Year"))



(p1 <- new_data_full %>% 
    ggplot(aes(x=Year, y=Abundance)) +
    geom_point(aes(col = MPA)) +
    geom_ribbon(data = new_data,
                aes(x = Year,
                    y = predicted, 
                    ymin = predicted - se.fit, 
                    ymax = predicted + se.fit), 
                alpha = .2) +
    geom_line(data = new_data,
              aes(x=Year, y=predicted)) +
    labs(x="Year", y=bquote("Average abundance")) +
    scale_x_continuous(breaks = seq(2009,2021, by=2)) +
    scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
    scale_color_manual(values = c("red", "blue", "darkgreen", "orange")) +
    theme_bw() +
    theme(legend.position = "top", 
          legend.title = element_blank()))

ggsave("report/figs/inv_trend.png", dpi = 600)


# Fish biomass ------------------------------------------------------------

## Biomass graph violinplots

p1 <- db_sf %>% 
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


p2 <- db %>% 
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
  scale_color_manual(values=c("orange", "purple")) +
  scale_fill_manual(values=c("orange", "purple")) +
  ylim(0, 1.5) +
  labs(x = "", y = "Biomass (Ton/ha)", fill = "Season") +
  theme_modern() +
  theme(legend.position = "top") +
  guides(colour = "none")




add_pval(p2, pairs = list(c(1, 2))) + add_pval(p1, pairs = list(c(1, 2))) + plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("report/figs/figure_fish_biomass_reefs.png", dpi = 600, height = 6, width = 8)



# Region comparison -------------------------------------------------------


p3 <- db_merged %>% 
  mutate(Region = factor(Region, 
                         levels = c("Loreto", "Cabo Pulmo", "Los Cabos"))) %>% 
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



p4 <- db %>% 
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



(p3 + p4 ) +
  plot_annotation(tag_levels = "A")

ggsave("report/figs/Regional_comparisons.png", dpi = 600, width = 7, height = 5)


# Historical Richness -----------------------------------------------------


db_merged %>% 
  ungroup() %>% 
  select(Label, Year, Region, Reef, Transect, Species) %>% 
  unique() %>% 
  group_by(Label, Year, Region, Reef, Transect) %>% 
  count() %>% 
  filter(Year > 2008) %>% 
  ggplot(aes(x=Year, y=n, col = Label)) +
  geom_point() +
  geom_smooth() +
  facet_grid(Label~Region)




# Historical biomass ------------------------------------------------------

tomod <- db_merged %>%
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "PEC", Region == "Los Cabos") %>% 
  filter(Year > 2000) %>% 
  group_by(Year, Reef, MPA, TrophicLevelF, Species, Transect) %>% 
  summarise(Biomass = sum(Biomass)) %>% 
  group_by(Year, Reef, MPA) %>% 
  summarise(Biomass = mean(Biomass))


library(mgcv)
library(gratia)

m1 <- gam(Biomass ~ s(Year, by = MPA), data = tomod, family = Gamma(link = "identity"))

summary(m1)

draw(m1)


m1 <- gam(Biomass ~ s(Year), data = tomod, family = Gamma(link = "identity"))

summary(m1)
draw(m1)

gam.check(m1)


new_data <- data.frame(
  Year = seq(from = 2009, to = 2021, by = 1)
)

pred_paci <- predict(m1,new_data, se.fit = T)




new_data$predicted <- pred_paci$fit
new_data$se.fit <- pred_paci$se.fit

new_data_full <- merge(new_data, tomod, by=c("Year"))



(p1 <- new_data_full %>% 
    ggplot(aes(x=Year, y=Biomass)) +
    geom_point(aes(col = MPA)) +
    geom_ribbon(data = new_data,
                aes(x = Year,
                    y = predicted, 
                    ymin = predicted - se.fit, 
                    ymax = predicted + se.fit), 
                alpha = .2) +
    geom_line(data = new_data,
              aes(x=Year, y=predicted)) +
    labs(x="Year", y=bquote("Average biomass (ton/ha)")) +
    scale_x_continuous(breaks = seq(2009,2021, by=2)) +
    scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
    scale_color_manual(values = c("red", "blue", "darkgreen", "orange")) +
    theme_bw() +
    theme(legend.position = "top", 
          legend.title = element_blank()))

ggsave("report/figs/fish_trend.png", dpi = 600)





