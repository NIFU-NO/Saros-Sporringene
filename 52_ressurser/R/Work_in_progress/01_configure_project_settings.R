################################################################################
### Global project settings across all SAROS-based reports and website
### Usually no need to touch the below if folder structure follows standard.
### Not to be run by itself. Will be run when running configure_report_settings.R
### NB: Possibly convert this into an easier-to-read _vars.yaml?
################################################################################

project_long <- "Spørringer til Skole-Norge"
project_short <- "SSN"

################################################################################
########## VARIABLE (COLUMN) NAMES USED ACROSS ALL REPORTS #####################
################################################################################

vars <- list() ### Initialize variable list here.
# Some variables may be constant across report cycles, then can be listed below.
# They will be merged with any  variables in a specific configure_report_settings.R
# if the outcome variables below are empty, will assume all non-predictor variables as outcome variables.
vars$outcome_binary <- c()
vars$outcome_ordinal <- c()
vars$outcome_nominal <- c()
vars$outcome_integer <- c()
########
vars$predictor_binary <- c()
vars$predictor_ordinal <- c()
vars$predictor_nominal <- c()
vars$predictor_interval <- c()



################################################################################
####################### Paths to folders and files #############################
################################################################################
