
antall <- lapply(c(fyl = "Skoleeier fylkeskommune", kom = "Skoleeier kommune", 
                   gs = "Skoleleder grunnskole", vgs = "Skoleleder videregående"),
                 FUN = tell_og_konverter_mer)

########

svarprosent <-
  tbl_2.4 %>% 
  dplyr::filter(` ` == "Svarprosent bruttoutvalg") %>% 
  dplyr::select(-1) %>% 
  as.list() %>% 
  setNames(nm = c("gs", "vgs", "kom", "fyl"))



######

respondent_grupper <- 
  levels(hel_data$resp) %>% 
  stringr::str_to_lower() %>% 
  cli::ansi_collapse(., sep = ", ", last = " og ")

