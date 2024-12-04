paths <- list()
paths$project$abs <- fs::path(Sys.getenv("USERPROFILE"), "NIFU", "21206 Utdanningsdirektoratets spørringer - General")
paths$data <- list()
paths$data$abs <- fs::path(Sys.getenv("USERPROFILE"), "NIFU", "21206 Utdanningsdirektoratets spørringer - Data - Data")

stopifnot(rlang::is_string(params$cycle))
paths$data$population[[params$cycle]]$abs <- 
  fs::path(paths$data$abs, "Survey", params$cycle, "ferdige filer", "kap2_populasjon.rds")
paths$data$sampling_frame[[params$cycle]]$abs <- 
  fs::path(paths$data$abs, "Survey", params$cycle, "ferdige filer", "kap2_utvalg.rds")
paths$data$gs[[params$cycle]]$abs <- 
  fs::path(paths$data$abs, "Survey", params$cycle, "kartfiler", "rådata", "gs_u.dta") # GS koordinater
paths$data$vgs[[params$cycle]]$abs <- 
  fs::path(paths$data$abs, "Survey", params$cycle, "kartfiler", "rådata","vgs.dta") # VGS koordinater
paths$chapter_overview[[params$cycle]]$abs <-
  fs::path(paths$project$abs, params$cycle, "Oversikt over kapitlene.xlsx")


paths$map$fylker$abs <- fs::path(paths$project$abs, "..", "Metode - General", "SAROS-core", "shared resources", "maps", "fylker2021.json")
paths$map$kommuner$abs <- fs::path(paths$project$abs, "..", "Metode - General", "SAROS-core", "shared resources", "maps", "kommuner2021.json")
