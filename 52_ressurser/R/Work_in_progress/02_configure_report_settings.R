################################################################################
######################## Report-specific settings ##############################
################################################################################
here::i_am(path = "")

## Innhent årgang fra mappenavnet til der hvor qmd_filene og data ligger?
cycle <- "2023V" # Alltid på formatet åååå-semesterbokstav. Dersom spesialundersøkelse, legg til f.eks. -L for lærer, etc

cycle_pretty <- # Trekke ut til en egen excel-fil eller eget generate_report skript?
  stringr::str_replace(cycle, 
                     "([[:digit:]]{4})([[:alpha:]]{1})",
                     "\\2\\1") |> 
  stringr::str_replace_all(pattern = "^[[:alpha:]]", 
                           replacement = function(.x) {
                             dplyr::case_when(.x=="H" ~ "høsten ", 
                                                           .x=="V" ~ "våren ",
                                                           .x=="S" ~ "sommeren ",
                                                           .x=="L" ~ "til lærere i ",
                                                           .x=="X" ~ "ekstrasurvey ",
                                                           TRUE ~ .x)
                             })

################################################################################
########## VARIABLE (COLUMN) NAMES USED IN THIS REPORT #########################
################################################################################

# Some variables may vary across report cycles, they can be listed below.
vars$predictor_binary = c(vars$predictor_binary)
vars$predictor_ordinal = c(vars$predictor_ordinal)
vars$predictor_nominal = c(vars$predictor_nominal)
vars$predictor_interval = c(vars$predictor_interval)

# if the outcome variables below are empty, will assume all non-predictor variables as outcome variables.
vars$outcome_binary <- c(vars$outcome_binary)
vars$outcome_ordinal <- c(vars$outcome_ordinal)
vars$outcome_nominal <- c(vars$outcome_nominal)
vars$outcome_integer <- c(vars$outcome_integer)

################################################################################
################ Rapportspesifikke mappe- og filbaner ##########################
################################################################################
