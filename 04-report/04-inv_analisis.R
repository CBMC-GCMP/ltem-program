library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggrepel)
library(ggspatial)
library(patchwork)
library(see)
library(ggpval)
library(tidytext)


1# Loading database output from 00-data_updater.R script

db <- readRDS("report/outputs/ltem_output_base.RDS") %>% 
 mutate( Cycle= ifelse (Month==9 | Month==8, 1, 2),
Day= ifelse(Month==9 & Day==26, 22, Day))


historic <- readRDS("report/outputs/cls_lor_cp_extract.RDS")

# Converting db to spatial object

db_sf <- st_as_sf(db, coords = c("Longitude", "Latitude"), crs = "WGS84", remove = F)
historic_sf <- st_as_sf(historic, coords = c("Longitude", "Latitude"), crs = "WGS84", remove = F)

# Getting a bounding box for los cabos coordinates
coord_lim <- st_bbox(
  historic_sf %>% 
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

historic_sf <- merge(historic_sf, id_reefs, by = "Reef")
db_sf <- merge(db_sf, id_reefs, by = "Reef")

db %>% 
  filter(Region == "Los Cabos") %>% 
  select(Year, Month, Day, Reef, Depth, Transect) %>% 
  arrange(Year, Month, Day, Reef, Depth, Transect) %>% 
  unique()



#Abundance 2021: Top 10--------


# First Cycle: August-September


(p1 <- db_sf %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>%
  filter(Label == "INV", Region == "Los Cabos", Cycle==1) %>% 
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
  labs(y = "Abundance", x="") +
  theme_modern() +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"))
)
ggsave("report/figs/2021_abundance_aug-sep.png", width = 12, height =9, dpi = 600)


# Second Cycle: November
(p1 <- db_sf %>% 
    mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>%
    filter(Label == "INV", Region == "Los Cabos", Cycle==2) %>% 
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
    labs(y = "Abundance", x="") +
    theme_modern() +
    theme(legend.position = "") +
    guides(colour = "none")+
    theme(axis.text.y = element_text(face= "italic"))
)

ggsave("report/figs/2021_abundance_nov.png", width = 12, height =9, dpi = 600)









# Abundance trend-----


histor <- readRDS("report/outputs/cls_lor_cp_extract.RDS")


histor <- histor %>% 
  select(Label, Year, Region, Reef, MPA, TrophicLevelF, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)


db_2 <- db %>% 
  select(Label, Year, Region, Reef, MPA, TrophicLevelF, Species, Latitude, Longitude, Transect, Size, Quantity, Biomass)

db_merged <- rbind(histor, db_2) %>% 
  mutate(Biomass = as.numeric(Biomass))



tomod <- db_merged %>%
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "INV", Region == "Los Cabos") %>% 
  filter(Year > 2000) %>% 
  group_by(Year, Reef, MPA, Transect, Species) %>% 
  summarise(Abundance = sum(Quantity)) %>% 
  group_by(Year,Reef,MPA) %>% 
  summarise(Abundance = mean(Abundance))


library(mgcv)
library(gratia)

m1 <- gam(Abundance ~ s(Year, by = MPA), data = tomod, family = Gamma(link = "identity"))

summary(m1)
plot.gam(m1)

appraise(m1)
draw(m1)


m1 <- gam(Abundance ~ s(Year), data = tomod, family = Gamma(link = "identity"))

summary(m1)
plot.gam(m1)

appraise(m1)
draw(m1)

gam.check(m1)


new_data <- data.frame(
  Year = rep(seq(from = 2009, to = 2021, by = 1), 4)
)

pred_paci <- predict(m1,new_data, se.fit = T)




new_data$predicted <- pred_paci$fit
new_data$se.fit <- pred_paci$se.fit

new_data_full <- merge(new_data, tomod, by=c("Year"))



(p3 <- new_data_full %>% 
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
    labs(x="Year", y=bquote("Abundance")) +
    scale_x_continuous(breaks = seq(2009,2021, by=2)) +
    scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
    scale_color_manual(values = c("red", "blue", "darkgreen", "orange")) +
    theme_bw() +
    theme(legend.position = "top", 
          legend.title = element_blank()))

ggsave("report/figs/all-inv_abundance_trend.png", width = 5, height = 4,dpi = 600)


#Important species
newmod <- db_merged %>%
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "INV", Region == "Los Cabos") %>%
  filter(Year > 2000) %>% 
  filter(Species %in% c("Leptogorgia rigida", "Pavona duerdeni", "Pacifigorgia agassizii")) %>% 
  group_by(Year, Reef, MPA, Transect, Species) %>% 
  summarise(Abundance = sum(Quantity)) %>% 
  group_by(Year, Species, MPA) %>% 
  summarise(Abundance = mean(Abundance)) 

m2 <- gam(Abundance ~ s(Year, by = MPA), data = newmod, family = Gamma(link = "identity"))

summary(m2)
plot.gam(m2)

appraise(m2)
draw(m2)


m2 <- gam(Abundance ~ s(Year), data = tomod, family = Gamma(link = "identity"))

summary(m2)
plot.gam(m2)

appraise(m2)
draw(m2)

gam.check(m2)


new_data <- data.frame(
  Year = rep(seq(from = 2009, to = 2021, by = 1), 4)
)

pred_paci <- predict(m2,new_data, se.fit = T)




new_data$predicted <- pred_paci$fit
new_data$se.fit <- pred_paci$se.fit

merge_newmod <- merge(new_data, newmod, by=c("Year"))


# (p4 <- merge_newmod %>% 
#     ggplot(aes(x=Year, y=Abundance)) +
#     geom_point(aes(col = Species)) +
#     geom_ribbon(data = new_data,
#                 aes(x = Year,
#                     y = predicted, 
#                     ymin = predicted - se.fit, 
#                     ymax = predicted + se.fit), 
#                 alpha = .2) +
#     geom_line(data = new_data,
#               aes(x=Year, y=predicted)) +
#     labs(x="Year", y=bquote("Abundance")) +
#     scale_x_continuous(breaks = seq(2009,2021, by=2)) +
#     scale_fill_manual(values = c("red", "blue", "darkgreen", "orange")) +
#     scale_color_manual(values = c("red", "blue", "darkgreen", "orange")) +
#     theme_bw() +
#     theme(legend.position = "top", 
#           legend.title = element_blank()))
# 
# ggsave("report/figs/species_abundance_trend.png", dpi = 600)


(p5 <- db_merged %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  filter(Label == "INV", Region == "Los Cabos") %>%
  filter(Year > 2000) %>% 
  filter(Species %in% c("Leptogorgia rigida",
                        "Pavona duerdeni",
                        "Pacifigorgia agassizii")
         ) %>% 
  group_by(Year, Reef, MPA, Transect, Species) %>% 
  summarise(Abundance = sum(Quantity)) %>% 
  group_by(Year,MPA, Species) %>% 
  summarise(Abundance = mean(Abundance)) %>% 
  ggplot(aes(x=Year, y= Abundance, fill=MPA))+
  geom_smooth(aes(group=MPA))+
  facet_wrap(~Species)


)


#Historical Abundance: Top 10-------


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
  labs(y = "Abundance", x="") +
  theme_modern() +
  theme(legend.position = "") +
  guides(colour = "none")+
  theme(axis.text.y = element_text(face= "italic"))
p2

ggsave("report/figs/historical_abundance.png", width = 12, height = 9, dpi = 600)



## Abundance graph violinplots-----

p1 <- db_sf %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  # filter(ID_map != "2") %>% 
  filter(Label == "INV", Region == "Los Cabos") %>% 
  group_by(ID_map, MPA, Reef, Transect, Species) %>% 
  summarise(Abundance = sum(Quantity)) %>% 
  group_by(ID_map, MPA, Reef, Transect) %>% 
  summarise(Abundance = mean(Abundance)) %>% 
  ggplot(aes(x = factor(ID_map), y = (Abundance))) +
  geom_violin(trim = F, aes(fill = MPA), alpha = .3) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = MPA)) +
  scale_color_manual(values=c("firebrick", "darkgreen"))+
  scale_fill_manual(values=c("firebrick", "darkgreen"))+
  labs(x = "", y = "Abundance", fill = "Protection level") +
  theme_modern() +
  theme(legend.position = "top") +
  guides(colour = "none")

p2 <- db_sf %>% 
  mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  # filter(ID_map != "2") %>% 
  filter(Label == "INV", Region == "Los Cabos") %>% 
  group_by(ID_map, MPA, Reef, Transect, Species) %>% 
  summarise(Abundance = sum(Quantity)) %>% 
  group_by(ID_map, MPA, Reef, Transect) %>% 
  summarise(Abundance = mean(Abundance)) %>% 
  ggplot(aes(x = factor(MPA), y = (Abundance))) +
  geom_violin(trim = F, aes(fill = MPA), alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = MPA)) +
  scale_color_manual(values=c("firebrick", "darkgreen"))+
  scale_fill_manual(values=c("firebrick", "darkgreen"))+
  labs(x = "", y = "Abundance", fill = "Protection level") +
  theme_modern() +
  theme(legend.position = "top") +
  guides(colour = "none")



p1 + add_pval(p2, pairs = list(c(1, 2))) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("report/figs/figure_inv_abundance_reefs.png", width = 5, height = 4, dpi = 600)


# Region comparison -------------------------------------------------------
db %>% 
  filter(Taxa=="")

p1 <- db_sf %>% 
  mutate(Region = factor(Region)) %>% 
  #mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  #filter(ID_map != "2") %>% 
  filter(Label == "INV") %>% 
  group_by( Region, Reef, Transect, Species) %>% 
  summarise(Abundance = sum(Quantity)) %>% 
  group_by( Region, Reef, Transect) %>% 
  summarise(Abundance = mean(Abundance, na.rm = T)) %>% 
  mutate(Abundancelog = log1p(Abundance)) %>% 
  ggplot(aes(x = factor(Region), y = (Abundancelog))) +
  geom_violin(trim = F, aes(fill = Region), alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = Region)) +
  scale_color_material_d() +
  scale_fill_material_d() +
  ylim(1, 4.5)+
  labs(x = "", y = "Abundance", fill = "Region") +
  theme_modern() +
  theme(legend.position = "") +
  guides(colour = "none")



(p1 <- db %>% 
  filter(Label == "INV") %>% 
  filter(Taxa2 %in% c("Echinoidea", "Octocorallia", "Asteroidea", "Hexacorallia")) %>% 
  mutate(Region = factor(Region),
         Taxa2= factor(Taxa2)) %>% 
  #mutate(MPA = factor(MPA, levels = c("", "Cabo San Lucas"), labels = c("Open Area", "MPA"))) %>% 
  #filter(ID_map != "2") %>% 
  group_by( Region, Reef, Transect, Taxa2, Species) %>% 
  summarise(Abundance = sum(Quantity)) %>% 
  group_by( Region, Taxa2, Reef, Transect) %>% 
  summarise(Abundance = mean(Abundance, na.rm = T)) %>% 
  mutate(Abundancelog = log1p(Abundance)) %>% 
  ggplot(aes(x = factor(Taxa2), y = (Abundancelog))) +
  geom_violin(trim = F, aes(fill = Taxa2), alpha = .3) +
  geom_boxplot(width  = 0.1) +
  geom_jitter(width = 0.09, pch = 21, aes(fill = Region)) +
  scale_color_material_d() +
  scale_fill_material_d() +
  # ylim(1, 4.5)+
  labs(x = "", y = "Abundance", fill = "Region") +
  theme_modern() +
  theme(legend.position = "bottom") +
  guides(colour = "none")
)

















p2 <- db %>% 
  as.data.frame() %>% 
  filter(Label == "INV") %>%  
  group_by(Region, Reef, MPA, Latitude, Longitude, Depth, Transect, Phylum) %>% 
  summarise(tot_abundance = sum(Quantity)) %>% 
  group_by(Region, Phylum) %>% 
  summarise(tot_abundance = mean(tot_abundance)) %>% 
  group_by(Region) %>% 
  mutate(rel_abundance = (tot_abundance/sum(tot_abundance))*100) %>% 
  ggplot(aes(x = reorder(Region, rel_abundance), y = rel_abundance, fill = Phylum)) +
  geom_col(col = "black") +
  # scale_color_material_d() +
  scale_fill_material_d() +
  labs(y="Relative abundance (%)", x= "", fill = "") +
  theme_bw()


p1+p2 +
  plot_layout(guides = "collect")

ggsave("report/figs/region_comparison_inv.png", width = 9, height = 6, dpi = 600)



