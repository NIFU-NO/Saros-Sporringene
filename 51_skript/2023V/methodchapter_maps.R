#Våren 2023 spørringene maps, the last Odyssey
library(magrittr)

#Fylkekart fil
bakgrunnF <-  
  sf::read_sf(here::here(paths$map$fylker),
              stringsAsFactors = TRUE, as_tibble = TRUE, layer = "fylker2021")

#Kommunekart fil
bakgrunn <-
  sf::read_sf(here::here(paths$map$kommuner), 
              stringsAsFactors = TRUE, as_tibble = TRUE, layer = "Kommuner2021")


#Grunnskoler i hele Norge
Grunnskoler_populasjon <- haven::read_dta(here::here(paths$data$gs[[params$cycle]]))

#Utvalg
utvalg_final <- haven::read_dta(here::here(paths$data$sampling_frame[[params$cycle]]))

Grunnskole_utvalg_final <-
  utvalg_final %>% #Svarprosent grunnskole (resp ==1, er grunnskoler) #prøver utvalg_final1 fordi dataset filen er rar, burde være samme utvalg_final.dta
  dplyr::filter(resp == 1, kommune != "Svalbard")  #tar ut svalbard (på grunn av kart)

###### NEDENFOR FLYTTES TIL data_prep.R #######

Grunnskole_utvalg_final <-
  Grunnskole_utvalg_final %>%  #lager en ny variabel "ikke_godkjent" som er 0 for alle observasjoner
  dplyr::mutate(ikke_godkjent = dplyr::case_when(
    svarstatus > 2 ~ 1,
    .default = 0
  )) %>%
  dplyr::group_by(fylknr) %>%
  dplyr::summarize(godkjent = sum(godkjent),
            ikke_godkjent = sum(ikke_godkjent)) %>%
  dplyr::mutate(svarprosent = (godkjent / (godkjent + ikke_godkjent)) * 100,
         cat_svarpr = dplyr::case_when(
    svarprosent >= 0 & svarprosent < 10 ~ "0-9 prosent",
    svarprosent >= 10 & svarprosent < 20 ~ "10-19 prosent",
    svarprosent >= 20 & svarprosent < 30 ~ "20-29 prosent",
    svarprosent >= 30 & svarprosent < 40 ~ "30-39 prosent",
    svarprosent >= 40 & svarprosent < 50 ~ "40-49 prosent",
    svarprosent >= 50 & svarprosent < 60 ~ "50-59 prosent",
    svarprosent >= 60 & svarprosent < 70 ~ "60-69 prosent",
    svarprosent >= 70 & svarprosent < 80 ~ "70-79 prosent",
    svarprosent >= 80 & svarprosent < 90 ~ "80-89 prosent",
    svarprosent >= 90 & svarprosent <= 100 ~ "90-100 prosent",
  ), cat_svarpr = factor(cat_svarpr))

###################### OVENFOR FLYTTES TIL prep_data.R ##################

Grunnskoler_pop_utvalg <-
  dplyr::left_join(Grunnskoler_populasjon, Grunnskole_utvalg_final, by = "fylknr") %>%
  dplyr::filter(!is.na(svarprosent.y)) %>%
  sf::st_as_sf(coords = c("_CX", "_CY")) #Gjør om et objekt til sf og spesifiserer hvilke columns som er koordinater

sf::st_crs(Grunnskoler_pop_utvalg) = 25833 #sf objektet har ingen CRS kode enda, så vi må spesifisere det selv. 25833 er hentet fra https://epsg.io/25833 (Europe offshore and onshore). Nå vet R referansepunktet til koordinatene vi spesifiserte tidligere

#Geografisk beliggenhet til vgs skoler som var med

######## mutate NEDENFOR FLYTTES TIL prep_data.R #######

VGS_utvalg_final <-
  utvalg_final %>%  #resp 2 = skoleleder vgs.
  dplyr::filter(resp == 2) %>%
  dplyr::mutate(utvalgt = ifelse(!grepl("Longyearbyen", navn), 1, 0),  #utvalgt not to be confused with utvalg. Gir alle respondentene etter filtrering en verdi 1 på variabelen "utvalgt", tar også bort Longyearbyen på grunn av kart koordinater
                orgno = as.character(orgno))
#laster inn vgs koordinater kart

koordinaterVGS <-
  haven::read_dta(here::here(paths$data$vgs[[params$cycle]])) %>%
  dplyr::rename(orgno = organisasj) %>%
  dplyr::rename(navn = skolenavn) %>%
  dplyr::mutate(orgno = as.character(orgno))

VGS_populasjon_utvalg <-
  dplyr::left_join(VGS_utvalg_final, koordinaterVGS, by = "orgno") %>%
  dplyr::filter(!is.na(`_CX`)) %>%  #kan ikke ha missing variables når du spesifiserer coords med st_as_sf så tar dem bort
sf::st_as_sf(coords = c("_CX", "_CY"))

sf::st_crs(VGS_populasjon_utvalg) <- 25833 #se forklaring lenger oppe på st_crs(Grunnskoler_pop_utvalg)




kommuner_utvalg_final <-
  utvalg_final %>% #Stor K
  dplyr::filter(resp == 3) %>%
  dplyr::rename(Kommunenummer = kmnr) %>%
  dplyr::rename(Kommunenavn = kommune) %>%
  dplyr::mutate(utvalgt = 1)

K_utvalgLjoin <-
  bakgrunn %>%
  dplyr::left_join(y=kommuner_utvalg_final %>%
                     dplyr::select(Kommunenavn, utvalgt),
                   by = c("Kommunenavn")) %>%
  dplyr::mutate(utvalgt = factor(ifelse(is.na(utvalgt), "white", "#2D8E9F")))



#Figur 2.1 grunnskoler
fig_2.1_gs <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggiraph::geom_sf_interactive(data = bakgrunn, fill = "white") +
  ggiraph::geom_sf_interactive(data = Grunnskoler_pop_utvalg, color = "#2D8E9F", alpha = .5, size = .8)

#Figur 2.1 Videregående
fig_2.1_vgs <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggiraph::geom_sf_interactive(data = bakgrunnF, fill = "white") +
  ggiraph::geom_sf_interactive(data = VGS_populasjon_utvalg, color = "#2D8E9F", alpha = .5, size = 1.2)

#Figur 2.1, noen kommuner i Nordland som ikke er fyllt inn men men...
fig_2.1_kom <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggiraph::geom_sf_interactive(data = K_utvalgLjoin,
          mapping = ggplot2::aes(fill = utvalgt)) +
  ggplot2::scale_fill_identity()

fig_2.1 <-
patchwork::wrap_plots(list(
  fig_2.1_gs,
  fig_2.1_vgs,
  fig_2.1_kom))

#Gjør om Fylkesnummer til numeric
bakgrunnF <-
  bakgrunnF %>%
  dplyr::mutate(Fylkesnummer = as.numeric(Fylkesnummer),
                Fylkesnummer = dplyr::case_when( #Matcher Fylkesnummer så de stemmer overens med Fylkesnummer i SvarprosentGS3
    Fylkesnummer == 1 ~ 3,
    Fylkesnummer == 2 ~ 11,
    Fylkesnummer == 3 ~ 15,
    Fylkesnummer == 4 ~ 18,
    Fylkesnummer == 5 ~ 30,
    Fylkesnummer == 6 ~ 34,
    Fylkesnummer == 7 ~ 38,
    Fylkesnummer == 8 ~ 42,
    Fylkesnummer == 9 ~ 46,
    Fylkesnummer == 10 ~ 50,
    Fylkesnummer == 11 ~ 54,
    .default = NA_real_
  ))


#Renameer fylknr til Fylkesnummer så vi kan left_joine
Grunnskole_utvalg_final <-
  Grunnskole_utvalg_final %>%
  dplyr::rename(Fylkesnummer = fylknr)

Grunnskole_svarprosent <-
  dplyr::left_join(bakgrunnF, Grunnskole_utvalg_final, by = "Fylkesnummer")


#Velger ut de to fylkene som er i nord; Nordland, og Troms og Finnmark, Figur 2.2 Nord
Grunnskole_svarprosentNord <-
  Grunnskole_svarprosent %>% #MB = maybe, men det funka så kan kalle det noe mer meningsfullt
  dplyr::filter(Fylkesnummer == 54 | Fylkesnummer == 18)



#Velger ut bare de fylkene som er med i Sør kartet
Grunnskole_svarprosentSør <-
  Grunnskole_svarprosent %>%
  dplyr::filter(Fylkesnummer == 3 | Fylkesnummer == 11 |
           Fylkesnummer == 15 | Fylkesnummer == 30 |
           Fylkesnummer == 34 | Fylkesnummer == 38 |
           Fylkesnummer == 42 | Fylkesnummer == 46 |
           Fylkesnummer == 50)



#Figur 2.2 Nord
#Lager kart av Nord med svarprosent fylling
fig_2.2_nord <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = Grunnskole_svarprosentNord,
                   mapping = ggplot2::aes(fill = cat_svarpr)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

#Figur 2.2 sør
#Lager kart av Sør og fyller svarprosent på grunnskoler per fylke.
fig_2.2_sor <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggiraph::geom_sf_interactive(data = Grunnskole_svarprosentSør,
          mapping = ggplot2::aes(fill = cat_svarpr)) + #Og hvordan legger jeg til antall grunnskoler i utvalget inni fylkene?
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

fig_2.2 <-
patchwork::wrap_plots(list(
  fig_2.2_nord,
  fig_2.2_sor), ncol = 2, guides = "collect")


VGS_utvalg_final <-
  VGS_utvalg_final %>%  #lager en ny variabel "ikke_godkjent" som er 0 for alle observasjoner
  dplyr::mutate(ikke_godkjent = 0,
                ikke_godkjent = dplyr::case_when(
    svarstatus > 2 ~ 1,
    .default = ikke_godkjent
  ))


# Collapse the dataset by "fylknr", calculating the sum of "godkjent" and "ikke_godkjent"
VGS_utvalg_final_groupedfylknr <-
  VGS_utvalg_final %>%
  dplyr::group_by(fylknr) %>%
  dplyr::summarize(godkjent = sum(godkjent),
            ikke_godkjent = sum(ikke_godkjent)) %>%
  dplyr::mutate(svarprosent = (godkjent / (godkjent + ikke_godkjent)) * 100, # Create a new variable called "svarprosent" that represents the percentage of "godkjent" responses out of the total number of responses
                cat_svarpr = dplyr::case_when( #putter svarprosent inn i kategorier
    svarprosent >= 0 & svarprosent < 10 ~ "0-9 prosent",
    svarprosent >= 10 & svarprosent < 20 ~ "10-19 prosent",
    svarprosent >= 20 & svarprosent < 30 ~ "20-29 prosent",
    svarprosent >= 30 & svarprosent < 40 ~ "30-39 prosent",
    svarprosent >= 40 & svarprosent < 50 ~ "40-49 prosent",
    svarprosent >= 50 & svarprosent < 60 ~ "50-59 prosent",
    svarprosent >= 60 & svarprosent < 70 ~ "60-69 prosent",
    svarprosent >= 70 & svarprosent < 80 ~ "70-79 prosent",
    svarprosent >= 80 & svarprosent < 90 ~ "80-89 prosent",
    svarprosent >= 90 & svarprosent <= 100 ~ "90-100 prosent",
    .default = NA_character_
  ), cat_svarpr = factor(cat_svarpr)) %>%
  dplyr::rename(Fylkesnummer = fylknr) #Renameer fylknr til Fylkesnummer så vi kan left_joine

VGS_svarprosent_populasjon_utvalg <-
  dplyr::left_join(bakgrunnF, VGS_utvalg_final_groupedfylknr, by = "Fylkesnummer")

#Velger ut de to fylkene som er i nord; Nordland, og Troms og Finnmark
VGS_svarprosent_populasjon_utvalgNord <-
  VGS_svarprosent_populasjon_utvalg %>% #Ljoin = left_join
  dplyr::filter(Fylkesnummer == 54 | Fylkesnummer == 18)






#Velger ut bare de fylkene som er med i Sør kartet
VGS_svarprosent_populasjon_utvalgSør <-
  VGS_svarprosent_populasjon_utvalg %>%
  dplyr::filter(Fylkesnummer == 3 | Fylkesnummer == 11 |
           Fylkesnummer == 15 | Fylkesnummer == 30 |
           Fylkesnummer == 34 | Fylkesnummer == 38 |
           Fylkesnummer == 42 | Fylkesnummer == 46 |
           Fylkesnummer == 50)



#Figur 2.3 Nord
#Lager kart av Nord med VGS svarprosent fylling.
fig_2.3_nord <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = VGS_svarprosent_populasjon_utvalgNord,
                   mapping = ggplot2::aes(fill = cat_svarpr)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

#Figur 2.3 Sør, riktig tall hvis du ser på tabell 2.11 i rapporten, men ikke riktig tall i forhold til figur 2.3 i rapporten. Figur 2.3 feil fargelagt?
#Lager kart av Sør og fyller svarprosent på VGS per fylke.
fig_2.3_sor <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = VGS_svarprosent_populasjon_utvalgSør,
          mapping = ggplot2::aes(fill = cat_svarpr)) + #Og hvordan legger jeg til antall grunnskoler i utvalget inni fylkene?
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

fig_2.3 <-
patchwork::wrap_plots(list(
  fig_2.3_nord,
  fig_2.3_sor), ncol = 2, guides = "collect")




kommuner_utvalg_final <-
  kommuner_utvalg_final %>%  #Kopierer datasettet SvarprosentK og gir det nytt navn SvaprosentVGS1 og lager en ny variabel "ikke_godkjent" som er 0 for alle observasjoner
  dplyr::mutate(ikke_godkjent = dplyr::if_else(svarstatus > 2, 1L, 0L, missing=0L))


# Collapse the dataset by "fylknr", calculating the sum of "godkjent" and "ikke_godkjent" #tallene stemmer litt, Oslo (3) stemmer ikke. Noe jeg har glemt?
kommuner_utvalg_final_groupedfylknr <-
  kommuner_utvalg_final %>%
  dplyr::group_by(fylknr) %>%
  dplyr::summarize(godkjent = sum(godkjent),
            ikke_godkjent = sum(ikke_godkjent)) %>%
  dplyr::mutate(svarprosent = (godkjent / (godkjent + ikke_godkjent)) * 100,
                cat_svarpr = dplyr::case_when(
    svarprosent >= 0 & svarprosent < 10 ~ "0-9 prosent",
    svarprosent >= 10 & svarprosent < 20 ~ "10-19 prosent",
    svarprosent >= 20 & svarprosent < 30 ~ "20-29 prosent",
    svarprosent >= 30 & svarprosent < 40 ~ "30-39 prosent",
    svarprosent >= 40 & svarprosent < 50 ~ "40-49 prosent",
    svarprosent >= 50 & svarprosent < 60 ~ "50-59 prosent",
    svarprosent >= 60 & svarprosent < 70 ~ "60-69 prosent",
    svarprosent >= 70 & svarprosent < 80 ~ "70-79 prosent",
    svarprosent >= 80 & svarprosent < 90 ~ "80-89 prosent",
    svarprosent >= 90 & svarprosent <= 100 ~ "90-100 prosent",
    .default = NA_character_
  ), cat_svarpr = factor(cat_svarpr)) %>%
  dplyr::rename(Fylkesnummer = fylknr) #Renameer fylknr til Fylkesnummer så vi kan left_joine


Kom_svarprosent_utvalg_populasjon <-
  dplyr::left_join(bakgrunnF, kommuner_utvalg_final_groupedfylknr, by = "Fylkesnummer")

#Velger ut de to fylkene som er i nord; Nordland, og Troms og Finnmark
Kom_svarprosent_utvalg_populasjonNord <-
  Kom_svarprosent_utvalg_populasjon %>%
  dplyr::filter(Fylkesnummer == 54 | Fylkesnummer == 18)




#Velger ut bare de fylkene som er med i Sør kartet
Kom_svarprosent_utvalg_populasjonSør <-
  Kom_svarprosent_utvalg_populasjon %>%
  dplyr::filter(Fylkesnummer == 3 | Fylkesnummer == 11 |
           Fylkesnummer == 15 | Fylkesnummer == 30 |
           Fylkesnummer == 34 | Fylkesnummer == 38 |
           Fylkesnummer == 42 | Fylkesnummer == 46 |
           Fylkesnummer == 50)

#Figur 2.4 Nord
#Lager kart av Nord med Kommuner svarprosent fylling.
fig_2.4_nord <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = Kom_svarprosent_utvalg_populasjonNord,
                   mapping = ggplot2::aes(fill = cat_svarpr)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

#Lager kart av Sør og fyller svarprosent på kommuner per fylke.
fig_2.4_sor <-
  ggplot2::ggplot() +
  ggiraph::geom_sf_interactive(data = Kom_svarprosent_utvalg_populasjonSør,
                   mapping = ggplot2::aes(fill = cat_svarpr)) +
  ggplot2::theme_void() +
  ggplot2::guides(fill = ggiraph::guide_legend_interactive(title = NULL, reverse = TRUE)) +
  nifutheme::scale_fill_nifu(palette = "blues", discrete = TRUE, drop=FALSE)

fig_2.4 <-
  patchwork::wrap_plots(list(
  fig_2.4_nord,
  fig_2.4_sor), ncol = 2, guides = "collect")


