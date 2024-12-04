#Våren 2023 spørringene maps, the last Odyssey
library(magrittr)

###############################################################################
################## FYLKER OG KOMMUNER GRUNNKART ###############################
###############################################################################
fylker_nord_norge <- c(4, 11)
fylker_sør_norge <- c(1, 2, 3, 10, 5, 6, 7, 8, 9)

omkode_fylker <- function(data) {
  dplyr::mutate(data,
                fylknr = as.numeric(fylknr),
                fylknr = dplyr::case_when( #Matcher fylknr så de stemmer overens med fylknr i SvarprosentGS3
                  fylknr == 3 ~ 1,
                  fylknr == 11 ~ 2,
                  fylknr == 15 ~ 3,
                  fylknr == 18 ~ 4,
                  fylknr == 30 ~ 5,
                  fylknr == 34 ~ 6,
                  fylknr == 38 ~ 7,
                  fylknr == 42 ~ 8,
                  fylknr == 46 ~ 9,
                  fylknr == 50 ~ 10,
                  fylknr == 54 ~ 11,
                  .default = NA_real_
                ))
}

#Fylkeskart fil
bakgrunnF <-  
  sf::read_sf(paths$map$fylker$abs,
              stringsAsFactors = TRUE, as_tibble = TRUE, layer = "fylker2021") %>%
  dplyr::mutate(Fylkesnummer = as.numeric(Fylkesnummer))
#Kommunekart fil
bakgrunn <-
  sf::read_sf(paths$map$kommuner$abs, 
              stringsAsFactors = TRUE, as_tibble = TRUE, layer = "Kommuner2021")

###############################################################################
################################ HELE UTVALGET ################################
###############################################################################

utvalg_final <- 
	haven::read_dta(paths$data$population[[params$cycle]]$abs) %>%
  labelled::unlabelled() %>% 
  dplyr::filter(kilder == "Utvalgt")

###############################################################################
################################ GRUNNSKOLER ##################################
###############################################################################

#Grunnskoler i hele Norge
GS_koordinater <- 
  haven::read_dta(paths$data$gs[[params$cycle]]$abs) %>% 
  omkode_fylker()


GS_utvalg <-
  utvalg_final %>% #Svarprosent grunnskole (resp ==1, er grunnskoler)
  dplyr::filter(resp == "Skoleleder grunnskole", kommune != "Svalbard") %>% 
  omkode_fylker()


GS_utvalg_svarprosent_per_fylke <-
  GS_utvalg %>% 
  lag_svarprosenter_per_enhet()


GS_utvalg_med_koord <-
  dplyr::left_join(GS_utvalg, mutate(GS_koordinater, orgno=as.character(orgno)), 
                   by = c("orgno", "fylknr"), relationship = "one-to-one") #%>%
  # dplyr::filter(!is.na(svarprosent))

if(GS_utvalg_med_koord %>% filter(is.na(`_CY`) | is.na(`_CX`)) %>% nrow() > 5) {
  
  cli::cli_abort(c("STOPP EN HALV, FOR MANGE SKOLER UTEN KOORDINATER?",
                   "Juster terskelen etter du har sjekket skolene: {GS_utvalg_med_koord %>% filter(is.na(`_CY`) | is.na(`_CX`)) %>% pull(navn.x)}"))
}

GS_utvalg_med_koord <-
  GS_utvalg_med_koord %>%
  filter(!is.na(`_CY`), !is.na(`_CX`)) %>% 
  sf::st_as_sf(coords = c("_CX", "_CY")) #Gjør om et objekt til sf og spesifiserer hvilke columns som er koordinater

sf::st_crs(GS_utvalg_med_koord) = 25833 #sf objektet har ingen CRS kode enda, så vi må spesifisere det selv. 25833 er hentet fra https://epsg.io/25833 (Europe offshore and onshore). Nå vet R referansepunktet til koordinatene vi spesifiserte tidligere

if(nrow(GS_utvalg_med_koord) > 1000 || nrow(GS_utvalg_med_koord) < 600) {
  cli::cli_abort(c("STOPP EN HALV, FOR MANGE ELLER FOR FÅ GS?",
                   "Juster tersklene etter du har sjekket GS_utvalg_med_koord som har {nrow(GS_utvalg_med_koord)} skoler"))
  
}


GS_svarprosent <-
  dplyr::left_join(bakgrunnF, GS_utvalg_svarprosent_per_fylke, 
                   by = c("Fylkesnummer" = "fylknr")) %>% 
  dplyr::filter(!is.na(svarprosent))

GS_svarprosentNord <-
  GS_svarprosent %>% 
  dplyr::filter(Fylkesnummer %in% fylker_nord_norge)

GS_svarprosentSør <-
  GS_svarprosent %>%
  dplyr::filter(Fylkesnummer %in% fylker_sør_norge)


###############################################################################
################################ VIDEREGÅENDE #################################
###############################################################################

#laster inn vgs koordinater kart

VGS_koordinater <-
  haven::read_dta(here::here(paths$data$vgs[[params$cycle]]$abs)) %>%
  dplyr::rename(orgno = organisasj) %>%
  dplyr::rename(navn = skolenavn) %>%
  dplyr::mutate(orgno = as.character(orgno),
                fylknr = fylkesnumm) %>% 
  omkode_fylker()


VGS_utvalg <-
  utvalg_final %>%
  dplyr::filter(resp == "Skoleleder videregående") %>%
  dplyr::mutate(utvalgt = ifelse(!grepl("Longyearbyen", navn), 1, 0),  #utvalgt not to be confused with utvalg. Gir alle respondentene etter filtrering en verdi 1 på variabelen "utvalgt", tar også bort Longyearbyen på grunn av kart koordinater
                orgno = as.character(orgno)) %>% 
  omkode_fylker()


######## mutate OVENFOR FLYTTES TIL prep_data.R #######

VGS_utvalg_med_koord <-
  dplyr::left_join(VGS_utvalg, VGS_koordinater, by = c("fylknr", "orgno")) %>%
  dplyr::filter(!is.na(`_CX`)) %>%  #kan ikke ha missing variables når du spesifiserer coords med st_as_sf så tar dem bort
  sf::st_as_sf(coords = c("_CX", "_CY"))

sf::st_crs(VGS_utvalg_med_koord) <- 25833 #se forklaring lenger oppe på st_crs(Grunnskoler_pop_utvalg)


VGS_utvalg_svarprosent_per_fylke <-
  VGS_utvalg %>%
  lag_svarprosenter_per_enhet()
  
VGS_svarprosent <-
  dplyr::left_join(bakgrunnF, VGS_utvalg_svarprosent_per_fylke, 
					by = c("Fylkesnummer" = "fylknr")) %>% 
  dplyr::filter(!is.na(svarprosent))

VGS_svarprosentNord <-
  VGS_svarprosent %>% 
  dplyr::filter(Fylkesnummer %in% fylker_nord_norge)

VGS_svarprosentSør <-
  VGS_svarprosent %>%
  dplyr::filter(Fylkesnummer %in% fylker_sør_norge)


###############################################################################
################################ KOMMUNER #####################################
###############################################################################

######## mutate NEDENFOR FLYTTES TIL prep_data.R #######

kommuner_utvalg <-
  utvalg_final %>% #Stor K
  dplyr::filter(resp == "Skoleeier kommune") %>%
  dplyr::mutate(utvalgt = 1) %>% 
  omkode_fylker()


######## mutate OVENFOR FLYTTES TIL prep_data.R #######

kommuner_utvalg_med_koord <-
  dplyr::left_join(x=bakgrunn,
                   y=kommuner_utvalg %>%
                     dplyr::select(kommune, utvalgt),
                   by = c("Kommunenavn" = "kommune")) %>%
  dplyr::mutate(utvalgt = factor(ifelse(is.na(utvalgt), "white", "#2D8E9F")))


# Collapse the dataset by "fylknr", calculating the sum of "godkjent" and "ikke_godkjent" #tallene stemmer litt, Oslo (3) stemmer ikke. Noe jeg har glemt?
kommuner_utvalg_svarprosent_per_fylke <-
  kommuner_utvalg %>%
  lag_svarprosenter_per_enhet()

kommuner_svarprosent <-
  dplyr::left_join(bakgrunnF, kommuner_utvalg_svarprosent_per_fylke, 
                   by = c("Fylkesnummer" = "fylknr"))

#Velger ut de to fylkene som er i nord; Nordland, og Troms og Finnmark
kommuner_svarprosentNord <-
  kommuner_svarprosent %>%
  dplyr::filter(Fylkesnummer %in% fylker_nord_norge)

#Velger ut bare de fylkene som er med i Sør kartet
kommuner_svarprosentSør <-
  kommuner_svarprosent %>%
  dplyr::filter(Fylkesnummer %in% fylker_sør_norge)


###############################################################################
################################ FIGURER ######################################
###############################################################################


#Figur 2.1 grunnskoler
fig_2.1_gs <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggplot2::labs(caption = "Grunnskoler") +
  ggiraph::geom_sf_interactive(data = bakgrunn, mapping=ggplot2::aes(data_id = Kommunenavn, tooltip = Kommunenavn), fill = "white") +
  ggiraph::geom_sf_interactive(data = GS_utvalg_med_koord, color = "#2D8E9F", alpha = .5, size = .6)

#Figur 2.1 Videregående
fig_2.1_vgs <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggplot2::labs(caption = "Videregående") +
  ggiraph::geom_sf_interactive(data = bakgrunnF, mapping=ggplot2::aes(data_id = Fylkesnavn, tooltip = Fylkesnavn), fill = "white") +
  ggiraph::geom_sf_interactive(data = VGS_utvalg_med_koord, color = "#2D8E9F", alpha = .5, size = 1.2)

#Figur 2.1, noen kommuner i Nordland som ikke er fyllt inn men men...
fig_2.1_kom <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggplot2::labs(caption = "Kommuner") +
  ggiraph::geom_sf_interactive(data = kommuner_utvalg_med_koord,
          mapping = ggplot2::aes(fill = utvalgt, data_id = Kommunenavn, tooltip = Kommunenavn)) +
  ggplot2::scale_fill_identity()

fig_2.1 <-
patchwork::wrap_plots(list(
  A = fig_2.1_gs,
  B = fig_2.1_vgs,
  C = fig_2.1_kom), 
  design = 
    "AB
     CC")

#Figur 2.2 Nord
#Lager kart av Nord med svarprosent fylling
fig_2.2_nord <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = GS_svarprosentNord,
                   mapping = ggplot2::aes(fill = cat_svarpr, 
                                          data_id = Fylkesnavn, 
                                          tooltip = Fylkesnavn)) +
  ggiraph::geom_sf_text_interactive(data = GS_svarprosentNord, colour="white",
                   mapping = ggplot2::aes(label = svarpr_label, 
                                          data_id = Fylkesnavn,
                                          tooltip = Fylkesnavn)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = "Fylkesvis svarprosent", reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

#Figur 2.2 sør
#Lager kart av Sør og fyller svarprosent på grunnskoler per fylke.
fig_2.2_sor <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggiraph::geom_sf_interactive(data = GS_svarprosentSør,
          mapping = ggplot2::aes(fill = cat_svarpr, data_id = Fylkesnavn, tooltip = Fylkesnavn)) +
  ggiraph::geom_sf_text_interactive(data = GS_svarprosentSør,colour="white",
                                 mapping = ggplot2::aes(label = svarpr_label,
                                                        data_id = Fylkesnavn,
                                                        tooltip = Fylkesnavn)) +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = "Fylkesvis svarprosent", reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

fig_2.2 <-
patchwork::wrap_plots(list(
  fig_2.2_sor,
  fig_2.2_nord
  ), 
  ncol = 2, guides = "collect")



#Figur 2.3 Nord
#Lager kart av Nord med VGS svarprosent fylling.
fig_2.3_nord <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = VGS_svarprosentNord,
                   mapping = ggplot2::aes(fill = cat_svarpr, 
                                          data_id = Fylkesnavn, 
                                          tooltip = Fylkesnavn)) +
  ggiraph::geom_sf_text_interactive(data = VGS_svarprosentNord,colour="white",
                                 mapping = ggplot2::aes(label = svarpr_label,
                                                        data_id = Fylkesnavn,
                                                        tooltip = Fylkesnavn)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = "Fylkesvis svarprosent", reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

#Figur 2.3 Sør, riktig tall hvis du ser på tabell 2.11 i rapporten, men ikke riktig tall i forhold til figur 2.3 i rapporten. Figur 2.3 feil fargelagt?
#Lager kart av Sør og fyller svarprosent på VGS per fylke.
fig_2.3_sor <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = VGS_svarprosentSør,
          mapping = ggplot2::aes(fill = cat_svarpr, 
                                 data_id = Fylkesnavn, 
                                 tooltip = Fylkesnavn)) + 
  ggiraph::geom_sf_text_interactive(data = VGS_svarprosentSør,colour="white",
                                 mapping = ggplot2::aes(label = svarpr_label,
                                                        data_id = Fylkesnavn,
                                                        tooltip = Fylkesnavn)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = "Fylkesvis svarprosent", reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

fig_2.3 <-
patchwork::wrap_plots(list(
  fig_2.3_sor,
  fig_2.3_nord), 
  ncol = 2, guides = "collect")




#Figur 2.4 Nord
#Lager kart av Nord med Kommuner svarprosent fylling.
fig_2.4_nord <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = kommuner_svarprosentNord,
                   mapping = ggplot2::aes(fill = cat_svarpr, data_id = Fylkesnavn, tooltip = Fylkesnavn)) +
  ggiraph::geom_sf_text_interactive(data = kommuner_svarprosentNord,colour="white",
                                 mapping = ggplot2::aes(label = svarpr_label,
                                                        data_id = Fylkesnavn,
                                                        tooltip = Fylkesnavn)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

#Lager kart av Sør og fyller svarprosent på kommuner per fylke.
fig_2.4_sor <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = kommuner_svarprosentSør,
                   mapping = ggplot2::aes(fill = cat_svarpr, 
                                          data_id = Fylkesnavn, 
                                          tooltip = Fylkesnavn)) +
  ggiraph::geom_sf_text_interactive(data = kommuner_svarprosentSør,colour="white",
                                 mapping = ggplot2::aes(label = svarpr_label,
                                                        data_id = Fylkesnavn,
                                                        tooltip = Fylkesnavn)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

fig_2.4 <-
  patchwork::wrap_plots(list(
    fig_2.4_sor,
    fig_2.4_nord), 
    ncol = 2, guides = "collect")


