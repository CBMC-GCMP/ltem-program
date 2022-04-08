
#Load monitoring database
ltem <- read_xlsx()

#Load species list
spp <- read_xlsx()

#Load reefs list
reefs <- tbl(ltem_db,
             "ltem_monitoring_reefs")


#Merge monitoring database with species catalog

mg_spp <-function(db, spp){
db <- merge(db, spp, by = c("IDSpecies", "Species", "Label"))
}

#merge 

mg_reef <-function(db, reefs){
db <- merge(db, reefs, by = c("Region", "IDReef", "Reef"))

}

#Fish Biomass

biomass <-function(ltem){
ltem_biomass <- ltem %>% 
  mutate( Biomass = (Quantity * A_ord * (Size^B_pen))/(Area * 100),
          TrophicLevelF = cut(as.numeric(TrophicLevel), 
                              breaks = c(2, 2.5, 3, 3.5, 4, 4.6), 
                              labels = c("2-2.5", "2.5-3", "3-3.5", "3.5-4", "4-4.5"), 
                              right = FALSE)
  )
}

#Invertebrate Abundance

abundance <-function(db_sf){
ltem_abundance <- db_sf %>% 
  filter(Label == "INV") %>% 
  group_by(IMPA, Reef, Transect, Species) %>%
  summarise(Abundance = sum(Quantity)) %>%
  group_by(MPA,Species) %>% 
  summarise(Abundance = mean(Abundance)) %>%
  group_by(MPA) %>%
  top_n(10, Abundance) %>%
  ungroup() %>% 
  mutate(Species= reorder_within(Species, Abundance, MPA))

}
