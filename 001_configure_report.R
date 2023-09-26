here::i_am("001_configure_report.R")


params <- 
  list(
    cycle = "2023V",
    response_group = "",
    datefrom = "14. mars",
    dateto = "26. april",
    aar = "2023",
    gjennomforing = "våren 2023",
    nummer_i_rekken = "tretti",
    prefacetext =  "",
    prefaceAuthorA = "Vibeke Opheim",
    prefaceAuthorAposition = "Direktør",
    prefaceAuthorB = "Cay Gjerustad",
    prefaceAuthorBposition = "Forskningsleder",
    projectnumber = "21206",
    reportnumber = "2023:2",
    isbn = "978-82-327-0000-0",
    projectowner = "Utdanningsdirektoratet",
    projectowneraddress = "Postboks 9359, Grønland, 0135 Oslo")

paths <- list()
paths$data <- list()
paths$data$population <- list()
paths$data$sample <- list()
paths$data$survey <- list()
paths$r[[params$cycle]] <-
  here::here("51_skript", params$cycle)

paths$resources <-
  here::here("52_ressurser/")
paths$drafts_produced <-
  here::here("53_utkastgenereringer")
paths$drafts_completed <-
  here::here("55_fullførte_utkast")
paths$drafts_editing <-
  here::here("54_utkast_under_redigering")
paths$site <-
  fs::path(Sys.getenv("TEMP"), "21206", "5_Saros_SSN")
paths$site_drafts_completed <-
  fs::path(paths$site, "55_fullførte_utkast")
paths$site_resources <-
  fs::path(paths$site, "52_ressurser")

paths$topImagePath <- here::here("52_ressurser", "PNG", "cover_ovre.png")

paths$saros <-
  here::here()

paths$map$fylker <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "fylker2021.json")
paths$map$kommuner <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "kommuner2021.json")

paths$df_labels[[params$cycle]] <- 
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", "Survey", "V2023", "arbeidsfiler", "labes_r.xls")
paths$data$survey[[params$cycle]] <-
  fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", "Survey", 
           "V2023", "ferdige filer", "arbeidsfil_final.dta")

paths$data$population[[params$cycle]] <- fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", "Survey", 
                                                  "V2023", "endelig utvalg", "pop_alle.dta")

paths$chapter_overview[[params$cycle]] <-
  fs::path("..", "V2023", "Oversikt over kapitlene.xlsx")
paths$data$gs[[params$cycle]] <- fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", 
                                   "Survey", "V2023", "kartfiler", "gs_u.dta") # GS koordinater
paths$data$vgs[[params$cycle]] <- fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", 
                                    "Survey", "V2023", "kartfiler", "vgs.dta") # VGS koordinater
paths$data$sampling_frame[[params$cycle]] <- fs::path("..", "..", "21206 Utdanningsdirektoratets spørringer - Data - Data", 
                                      "Survey", "V2023", "ferdige filer", "utvalg_final.dta")

