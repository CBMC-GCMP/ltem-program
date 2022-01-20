library(tidyverse)

# Load data ---------------------------------------------------------------


## Load latest (and complete) LTEM database
ltem <- readRDS("report/outputs/cls_lor_cp_extract.RDS")



# Confidence Intervals ----------------------------------------------------

## Define our confidence level (95%)
alpha = 0.05
z = 1.96

## Calculate statistics per species
scores <- ltem %>%
  group_by(IDSpecies, Species) %>%
  summarise(
    # Mean Size
    m_size = mean(Size, na.rm = T),
    # Size standard deviation
    sd_size = sd(Size, na.rm = T),
    # Sample number
    n = length(Species),
    # Size standard error
    se_size = sd_size / sqrt(n),
    # Size lower limit
    Size_min = round(m_size - (z * se_size), 0),
    # Size upper limit
    Size_max = round(m_size + (z * se_size), 0),
    # Mean quantity
    m_quantity = mean(Quantity),
    # Quantity standard deviation
    sd_quantity = sd(Quantity),
    # Quantity Standard error
    se_quantity = sd_quantity / sqrt(n),
    # Quantity lower limit
    ll_quantity = m_quantity - (z * se_quantity),
    # Quantity upper limit
    Quantity_max = round(m_quantity + (z * se_quantity), 0) 
  )


unique <- scores %>% 
  filter(n==1)
  
## Clean our results
refs_meta <- scores %>%
  filter(n > 1) %>% #Elimate all species with only 1 observation
  mutate(flag = ifelse(n <= 30, "Flag", "Correct")) %>% #Flag all species with a sample number below 30
  select(IDSpecies, Species, Size_min, Size_max, Quantity_max, flag)

## Historic Max & Min values for Size and Quantity
mm <- ltem %>%
  group_by(IDSpecies, Species) %>%
  summarise(
    size_min = min(Size, na.rm = T),
    size_max = max(Size, na.rm = T),
    q_max = max(Quantity, na.rm = T)
  )

## Replace all flagged confidence intervals with their
## respective Max & Min size and quantity values
refs_meta <-
  merge(refs_meta,
        mm,
        by = c("IDSpecies", "Species"),
        all = T) %>%
  mutate(
    flag = ifelse(is.na(flag), "Flag", flag),
    c_interval = ifelse(flag == "Correct", "True", "False"),
    Size_min = ifelse(flag == "Flag", size_min, Size_min),
    Size_max = ifelse(flag == "Flag", size_max, Size_max),
    Quantity_max = ifelse(flag == "Flag", q_max, Quantity_max)
  ) %>%
  select(-c(flag, size_max, size_min, q_max))



# Export data -------------------------------------------------------------

## Export reference metadata for Size and Quantity

write_rds(refs_meta, "data/auxiliar/refs_meta.RDS")


# END ---------------------------------------------------------------------
