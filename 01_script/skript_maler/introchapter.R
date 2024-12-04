library(magrittr)
tbl_1.1_temaoversikt <- 
  readxl::read_excel(path = here::here(paths$chapter_overview[[params$cycle]])) %>%
  dplyr::filter(dplyr::if_any(tidyselect::any_of("Tema"), 
                              .fns = ~!.x %in% c("Sammendrag", 
                                                 "Innledning", "Introduksjon",
                                                 "Gjennomføring", "Metode", "Data og metode", "Metode og data")), 
                !as.character(Kapittelnr) %in% c("1", "2")) %>% 
  dplyr::select(Tema, Grunnskole, Videregående, Kommune, Fylkeskommune)

