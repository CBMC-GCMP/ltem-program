#Confidence Intervals 95%
ci_95 <- function(BASE_HISTORICA){
  alpha = 0.05
  z = 1.96
  scores <- BASE_HISTORICA %>%
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
   refs_meta <- scores %>%
    filter(n > 1) %>% #Elimate all species with only 1 observation
    mutate(flag = ifelse(n <= 30, "Flag", "Correct")) %>% #Flag all species with a sample number below 30
    select(IDSpecies, Species, Size_min, Size_max, Quantity_max, flag)
}

#Historical minimun and max values of Size & Quantity
ranges <- function(HISTORICAL_DB){
       mm <- HISTORICAL_DB %>%
    group_by(IDSpecies, Species) %>%
    summarise(
      size_min = min(Size, na.rm = T),
      size_max = max(Size, na.rm = T),
      q_max = max(Quantity, na.rm = T)
    )
  write_rds(mm,"data/ref_meta_max_min.RDS")     
}


## This is our function for automatic flagging values outside given their respective
## confidence interval, per species
data_check <- function(x, type) {
  refs_meta <- readRDS("data/auxiliar/ref_meta_max_min.RDS") %>% 
    rename(Size_min= size_min,
           Size_max= size_max,
           Quantity_max= q_max)
  if (type == "Quantity") {
    quantity <- merge(x, refs_meta[, c("IDSpecies", "Size_min", "Size_max", "Quantity_max")],
          by = "IDSpecies") %>%
      mutate(q_flag = ifelse(Quantity > Quantity_max, "FLAG_Q", "NO")) %>%
      filter(q_flag == "FLAG_Q")
    ## Export flagged values
    writexl::write_xlsx(quantity, "data/auxiliar/quantity_check.xlsx")
    
  } else if (type == "Size") {
    size <- merge(x, refs_meta[, c("IDSpecies", "Size_min", "Size_max", "Quantity_max")], by = "IDSpecies") %>%
      mutate(
        q_flag = ifelse(Size > Size_max, "FLAG_S", "NO"),
        q_flag = ifelse(Size < Size_min, "FLAG_S_min", q_flag)
      ) %>%
      filter(q_flag == "FLAG_S" |
               q_flag == "FLAG_S_min")
    ## Export flagged values
    writexl::write_xlsx(size, "data/auxiliar/size_check.xlsx")
    
  } else {
    stop("Tienes que poner Quantity o Size!")
  }
  
}


