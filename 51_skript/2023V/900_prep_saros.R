library(tidyverse)
# library(sarosverse) # Ikke enda aktiv
library(saros)
library(saros.structure)

### prep_saros.R ###
chapter_overview <-
  paths$chapter_overview[[cycle]] %>%
  readxl::read_excel() %>%
  rename(chapter=Tema, author=Kapittelforfatter) %>%  # Om det gjøres slik i Excel-fila slipper vi denne linjen.
  mutate(chapter = ifelse(chapter == "Behov for informasjon og støtte til kvalitetsutvikling i skolen", "Behov omkring kvalitetsutvikling", chapter)) # Noen kapittelnavn er for lange for Sharepoint

# Kan egentlig gjøres senere, legge direkte til config_macro
always_bottom_vars <- labelled::look_for(data, "Annet|Vet ikke", values = FALSE, details = FALSE) %>% pull(variable)

cat(always_bottom_vars, sep="\n- ")

############################################################
## config_report.R
config_macro <- read_default_draft_report_args(path = fs::path(paths$resources, "YAML", "_report_generation_setup.yaml"))
config_macro$title <- cycle 
# config_macro$variables_always_at_bottom <- always_bottom_vars
tryCatch(fs::dir_delete(fs::path(paths$drafts_produced,
                                 "Rapporter", response_group, config_macro$title)), error=function(e) cli::cli_warn(e))
tryCatch(fs::dir_delete(fs::path(paths$drafts_completed,
                                 "Rapporter", response_group, config_macro$title)), error=function(e) cli::cli_warn(e))

draft_report(data = data,
             chapter_overview = chapter_overview,
             !!!config_macro,
             path = fs::path(paths$drafts_produced, "Rapporter", response_group, config_macro$title))


## Mesos-rapporter
config_mesos <- config_macro
config_mesos$mesos_report <- TRUE
config_mesos$mesos_var <- "fylke"
config_mesos$title <- ""
config_mesos$translations$empty_text_chunk <- ""
config_mesos$element_names <- c("uni_cat_prop_plot", "bi_catcat_prop_plot")

output_files <-
  draft_report(
    data = data,
    chapter_overview = chapter_overview,
    !!!config_mesos,
    path = fs::path(paths$drafts_produced, "Rapporter", response_group, config_macro$title, "mesos"))
