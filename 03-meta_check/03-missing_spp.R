library(tidyverse)


# Missing Species ---------------------------------------------------------



ltem <- readRDS("data/auxiliar/LTEM_historic_v2.RDS")


ltem_species <- ltem %>% 
  group_by(IDSpecies, Species) %>% 
  summarise(Occurrences= sum(Quantity)) %>% 
  rename(ltem_species= Species)

species_list <- read.csv("data/lists/ltem_monitoring_species.csv")


merge <- merge(ltem_species, species_list[, c("IDSpecies", "Species")],
               by="IDSpecies", all=T) %>% 
  select(IDSpecies, ltem_species, Species, Occurrences) %>% 
  mutate(Missing= ifelse(is.na(ltem_species), "TRUE", "FALSE")) 
write.csv(merge, "data/auxiliar/missing_spp.csv")

(ratio <- merge %>% 
  group_by(Missing) %>% 
  summarise(Total= n(),
            Percent= round((Total*100)/(332+149), 0)
  ) %>% 
    mutate(Species= "Species",
      Missing= factor(Missing, levels=c("FALSE", "TRUE"), labels=c("Found","Missing")) ) %>% 
  ggplot(aes(x=reorder(Species,Percent), y=Percent, fill=Missing))+
  geom_bar(position="stack", stat="identity", width = 0.25)+
  labs(x="LTEM Species", y="Total (%)")+
  coord_flip()+
  scale_fill_manual(values = c("darkgreen", "firebrick")) +
  scale_color_manual(values = c("darkgreen", "firebrick"))+
    theme_minimal() +
    theme(legend.position = "top", 
          legend.title = element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
)
ggsave("missing_spp.jpeg" ,dpi = 500, width = 12, height = 10)
missing <- merge %>% 
  filter(Missing=="TRUE")
write.csv(missing, "data/auxiliar/only_missing_spp.csv")
