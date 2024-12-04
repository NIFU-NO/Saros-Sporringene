here::i_am("01_script/000_initialize_project.R")

remotes::install_github(c("NIFU-NO/saros", "NIFU-NO/saros.base", "NIFU-NO/saros.utils"), quiet = TRUE, 
                         upgrade = "always", force = FALSE)

params <- list()
params$reports <- "Rapporter"

###########

paths <- list()
paths$project_abs <- fs::path(Sys.getenv("USERPROFILE"), "NIFU", "21206 Utdanningsdirektoratets spørringer - General")
paths$data <- list()
paths$data$abs <- fs::path(Sys.getenv("USERPROFILE"), "NIFU", "21206 Utdanningsdirektoratets spørringer - Data - Data")
paths$data$population <- list()
paths$data$sampling_frame <- list()
paths$data$survey <- list()
paths$data$gs <- list()
paths$data$vgs <- list()
paths$df_labels <- list()

paths$r <-
  fs::path("01_script")
paths$resources <-
  here::here("02_resources")
paths$drafts_produced <-
  here::here(params$reports)
# paths$drafts_editing <-
#   here::here("54_utkast_under_redigering")
paths$drafts_completed <-
  here::here(params$reports)


paths$site <-
  fs::path(Sys.getenv("USERPROFILE"), "Saros", "SSN")
 # fs::path(paths$project_abs, "Saros")
# fs::path(".") # Tested for SSN, does not work, plenty of weird errors
paths$site_drafts_completed <-
  fs::path(paths$site, params$reports)
paths$site_resources <-
  fs::path("02_resources")
paths$local_basepath <- fs::path(paths$site, "_site")

paths$remote_basepath <- "/home/nifuno/domains/stephan/ssn.nifu.no/public_html/"

paths$organization_global_draft_report_settings <-
  fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "_draft_report_settings.yaml")
paths$organization_global_refine_chapter_overview_settings <-
  fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "_refine_chapter_overview_settings.yaml")

paths$local_main_password_path <-
  fs::path("..", "..", "Metode - Sensitivt - Sensitivt", ".main_htpasswd_private")
paths$topImagePath <- here::here("02_resources", "PNG", "cover_ovre.png")

paths$saros <-
  here::here()
paths$output <- list()


paths$map$fylker <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "fylker2021.json")
paths$map$kommuner <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "kommuner2021.json")

##################

df_labels <- list()
survey_data <- list()

chapter_overview <- list()
chapter_structure <- list()
df_chunk_templates <- list()
config_macro <- list()
output_files <- list()
check_labels <- list()
