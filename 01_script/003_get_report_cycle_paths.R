
# Kapitteloversikten i Excel. Kan oftest klargjøres så snart skjemaet er i Qualtrics. 
# Må minimum inneholde Kapittelforfatter, Tema, dep, indep og 
# avkrysning for hvilke resp som temaet angår (se kap 2): 
# Grunnskole	Videregående	Kommune	Fylkeskommune
# Kan også inneholde andre kolonner. Husk at rader for innledning og Beskrivelse av utvalget må med for riktig nummerering.
paths$chapter_overview[[params$cycle]] <-
  fs::path("..", params$cycle, "Oversikt over kapitlene.xlsx")

# Populasjonen man lager utvalg fra. Kun nødvendig i kapittel 2.
paths$data$population[[params$cycle]] <- 
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", "Survey", 
           params$cycle, "endelig utvalg", "pop_alle.rds")

# Utvalget det trekkes fra i den gitte gjennomføringen. Kun nødvendig i kapittel 2.
paths$data$sampling_frame[[params$cycle]] <- 
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", 
           "Survey", params$cycle, "ferdige filer", "utvalg_final.rds")

# Ikke nødvendig lenger. Ble brukt tidligere når dataene var vasket i Stata. Stata kutter lablene om måtte settes på igjen i R basert på de opprinnelige etikettene.
paths$df_labels[[params$cycle]] <-
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", "Survey", 
           params$cycle, "arbeidsfiler", "labes_r.xls")
#   here::here("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", "Survey", params$cycle, "råfiler", "data fra sx", "csv", "variables.csv")

# Her er surveydataene påkoblet registerdata og de som ikke har svart er fjernet. Brukes til de fleste kapitlene (bortsett fra kap 2).
paths$data$survey[[params$cycle]] <-
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", "Survey", 
           params$cycle, "ferdige filer", "arbeidsfil_final.rds")

# kartkoordinater for grunnskoler og videregående. Brukes i kapittel 2.
paths$data$gs[[params$cycle]] <- 
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", 
                                          "Survey", params$cycle, "kartfiler", "gs_u.dta") # GS koordinater
paths$data$vgs[[params$cycle]] <- 
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", 
                                           "Survey", params$cycle, "kartfiler", "vgs.dta") # VGS koordinater

# Filbanen hvor 200_prep_data legger ferdigbehandlet datasett klar for saros.
paths$data$saros_ready[[params$cycle]] <-
  here::here("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", 
             "Sarosdata", params$cycle, paste0(params$cycle, "_saros_main_chapters"))
fs::dir_create(fs::path_dir(paths$data$saros_ready[[params$cycle]]))



# Mappen hvor kapittel-utkast skal genereres. Det blir lagd undermapper for ikke-fylke og fylke.
paths$output[[params$cycle]] <- 
  fs::path("Rapporter", params$cycle, "Output")
