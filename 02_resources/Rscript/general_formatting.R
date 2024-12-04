
prosjektpalett <- c(
  '#C84957',
  '#363636',
  '#EDE2D2',
  '#2D8E9F',
  '#DBD2E0',
  '#E8AE59',
  '#E8B0B7',
  '#90D4E0',
  '#7E2630',
  '#1B555F')
nifu_global_general_formatting <-
  fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "nifu_global_general_formatting.R")
if(!file.exists(nifu_global_general_formatting)) {
  cli::cli_abort("{.file {nifu_global_general_formatting}} er utilgjengelig. Har du synkronisert Metode - General-teamet til din maskin?")
} else source(nifu_global_general_formatting, echo = FALSE)

# Skitten unødvendig kode fordi global_settings_set() ikke tar NULL riktig
tmp<-saros::global_settings_get("makeme")
tmp["hide_for_all_crowds_if_hidden_for_crowd"] <- list(NULL)
tmp[["simplify_output"]] <- TRUE
tmp["variables_always_at_bottom"] <- 
	ls(pattern = "^data_") |>
	mget() |>
	lapply(FUN = function(x) if(is.data.frame(x)) labelled::look_for(x,
                              "Annet|Vet ikke|Andre|Annen", ignore.case = FALSE, values = FALSE, details = FALSE) %>%
							  {if(nrow(.)>0) pull(., variable) else NULL}) |>
	unlist() |>
	unique() |>
	grep(x = _, pattern = "_TEXT", invert = TRUE, value = TRUE) %>%
	{if(length(.)==0) list(NULL) else .}

tmp2 <- options("saros")
tmp2$saros$makeme_defaults <- tmp
options(saros = tmp2$saros)
rm(tmp, tmp2)
