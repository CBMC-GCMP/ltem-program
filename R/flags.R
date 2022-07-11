



flags <- function(LTEM_corrected) {
  IDs <- c("IDReef", "IDSize", "IDSpecies")
  correct <-
    c("correct_size", "correct_reef", "correct_id", "correct_sp")
  Before <- c("IDBefore", "ReefBefore", "SpeciesBefore")
  flags <- LTEM_corrected %>%
    filter(str_detect(Status, "Modified_")) %>%
    select(
      Label,
      Year,
      Month,
      Day,
      Region,
      Depth,
      Transect,
      Observer,
      any_of(IDs),
      Species,
      any_of(Before),
      any_of(correct),
      Status
    )
  print(unique(flags$correct_sp))
  
  output <- flags
  
}
