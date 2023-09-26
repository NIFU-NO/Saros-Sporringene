source(here::here(paths$r, "methodchapter_functions.R"))
library(magrittr)
utv_data <- 
  haven::read_stata(here::here(paths$data$sampling_frame[[params$cycle]])) %>%
  labelled::to_factor() %>%
  dplyr::mutate(dplyr::across(tidyselect::where(~is.numeric(.x)) & !Antall_svar, 
                              ~labelled::to_factor(.x)),
                # typevgs OG landsdel => FIKSE I prep_data.R
                typevgs = dplyr::case_when(typevgs == "Skolen har kun yrkesfaglige studieretninger" ~ "Yrkesfag",
                                           typevgs == "Skolen har både studieforberedende- og yrkesfaglige studieretninger" ~ "Kombinert",
                                           typevgs == "Skolen har kun studieforberedende studieretninger" ~ "Studiespesialiserende"),
                landsdel = dplyr::case_when(!is.na(landsdel_gs) ~ landsdel_gs,
                                            !is.na(landsdel_vgs) ~ landsdel_vgs,
                                            !is.na(landsdel_kom) ~ landsdel_kom,
                                            !is.na(landsdel_fylk) ~ landsdel_fylk)
                ) %>%
  dplyr::rename(`Antall svar` = Antall_svar)

pop_data <- 
  haven::read_stata(here::here(paths$data$population[[params$cycle]])) %>%
  labelled::to_factor() %>% 
  dplyr::mutate(dplyr::across(tidyselect::where(~is.numeric(.x)), ~labelled::to_factor(.x)),
                vgs3d = forcats::fct_recode(vgs3d, NULL = "0"), # FIKSE I prep_data.R
                landsdel_kom = dplyr::case_when(!is.na(kstør_kom) ~ landsdel, .default = NA))  #FIKSE I prep_data.R

hel_data <- 
  dplyr::bind_rows(Populasjon = pop_data, 
                   Utvalg = utv_data, 
                   .id = "kilder") %>%
  labelled::copy_labels_from(from = utv_data, .strict = FALSE) %>% 
  dplyr::mutate(
    kilder_integer = dplyr::case_when( 
      kilder == "Populasjon" ~ 0,
      kilder == "Utvalg" ~ 1
    ))

#################################################################################
### ALT DETTE BØR FIKSES I data_prep.R

hel_data <- 
  hel_data %>%
  dplyr::mutate(
    godkjent_integer = dplyr::if_else(godkjent == "Godkjent", 1, 0),
    resp = forcats::fct_recode(resp, 
                               "Skoleeier kommune" = "Skoleeier kommmune"),
    orgform = forcats::fct_recode(orgform, 
                                  "Offentlig" = "1",
                                  "Privat" = "2"),
    målform = forcats::fct_recode(målform, "Bokmål" = "Bokmaal"),
    fylke = as.factor(fylke),
    typevgs = factor(typevgs, levels=c("Studiespesialiserende", "Kombinert", "Yrkesfag")),
    sk4d = forcats::fct_recode(sk4d,
                               "Ungdomskole" = "Ungdomsskole",
                               "1-10 skole" = "1-10. skoler"),
    kstør_gs = forcats::fct_recode(kstør_gs, "Mer enn 20 000" = "Mer enn 20 0000"),
    kstør_vgs = forcats::fct_recode(kstør_vgs, "Mer enn 20 000" = "Mer enn 20 0000"),
    kstør_kom = forcats::fct_recode(kstør_kom, "Mer enn 20 000" = "Mer enn 20 0000")) %>% 
  labelled::set_variable_labels(
    landsdel = "Landsdel",
    landsdel_gs = "Landsdel",
    landsdel_vgs = "Landsdel",
    landsdel_kom = "Landsdel",
    landsdel_fylk = "Landsdel",
    fylke = "Fylke",
    fylke_gs = "Fylke",
    fylke_vgs = "Fylke",
    fylke_kom = "Fylke",
    fylke_fylk = "Fylke",
    s_3_1 = "(Fylkes)rådmann, assisterende rådmann og lignende",
    s_5_1 = "(Fylkes)rådmann, assisterende rådmann og lignende",
    s_3_2 = "Skolefaglig ansvarlig (eksempel utdanningsdirektør, skolesjef, oppvekstsjef, seksjonssjef for skole)",
    s_5_2 = "Skolefaglig ansvarlig (eksempel utdanningsdirektør, skolesjef, oppvekstsjef, seksjonssjef for skole)",
    s_3_3 = "Seksjonsleder, avdelingsleder og lignende stillinger på mellomledernivå",
    s_5_3 = "Seksjonsleder, avdelingsleder og lignende stillinger på mellomledernivå",
    s_3_4 = "Rådgiver, konsulent, førstesekretær, og lignende",
    s_5_4 = "Rådgiver, konsulent, førstesekretær, og lignende",
    s_3_5 = "Annet",
    s_5_5 = "Annet")


###################################################################################
tbl_2.1 <- 
  tibble::tibble(Tema = c("Respondenttype", "Skoletype 4-delt", 
                          "Skoletype 5-delt", "Størrelse grunnskole", 
                          "Type videregående", "Størrelse videregående",
                          "Skolestørrelse", "Landsdel", "Fylke", 
                          "Kommunestørrelse", "Sentralitetsindeks"), 
                 Grunnskole = c(rep(T,4),F,F,rep(T,5)), 
                 Videregående = c(rep(T,3),F,rep(T,7)), 
                 Kommune = c(T,rep(F,6),rep(T,4)), 
                 Fylkeskommune = c(T,rep(F,6),T,rep(F,3))) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ifelse(.x, "X", "")))

# Bør erstatte crosstable med en mer populær pakke/stabil løsning
tbl_2.2 <- 
  hel_data %>%
  dplyr::filter(kilder == "Utvalg") %>%
  crosstable::crosstable(cols = resp,
                         by = svarstatus, showNA = "no",
                         percent_pattern = list(body="{n}", total_row="{n}", total_col="{n}", total_all="{n}"),
                         total = "both") %>%
  dplyr::rename_with(.cols = variable, .fn = function(x) .$label[1]) %>% 
  # dplyr::rename(Godkjent = godkjent) %>% 
  dplyr::select(-.id, -label) 


tbl_2.3 <- 
  hel_data %>%
  dplyr::filter(kilder == "Utvalg") %>%
  dplyr::summarize(Gjennomsnitt = round(mean(`Antall svar`, na.rm = TRUE), digits = 1),
                   Median = median(`Antall svar`, na.rm = TRUE),
                   Maks = max(`Antall svar`, na.rm = TRUE),
                   .by = c(resp, godkjent)) %>%
  saros::swap_label_colnames()


tbl_2.4 <- 
  hel_data %>%
  dplyr::group_by(resp) %>%
  dplyr::mutate(`Maks populasjon` = sum(kilder == "Populasjon")) %>%
  dplyr::group_by(kilder, resp) %>% #group_by må stå to ganger
  dplyr::mutate(Bruttoutvalg = dplyr::n()) %>%
  dplyr::summarize(`Godkjente svar` = sum(godkjent == "Godkjent", na.rm = TRUE),
                   Bruttoutvalg = unique(Bruttoutvalg),
                   `Maks populasjon` = unique(`Maks populasjon`),
                   `Svarprosent bruttoutvalg` = (`Godkjente svar` / Bruttoutvalg) * 100,
                   `Andel populasjon deltatt` = ifelse(`Maks populasjon` == 0, 0, (`Godkjente svar` / `Maks populasjon`) * 100),
                   .groups = "drop") %>%
  tidyr::pivot_longer(cols = -c(kilder, resp),
                      names_to = " ", values_to = "Value") %>%
  tidyr::pivot_wider(names_from = resp, values_from = Value, names_expand = TRUE) %>%
  dplyr::filter(kilder == "Utvalg") %>% #Her filtrerer jeg rader som er null, for eksempel Populasjon og godkjente svar (Ingen fra populasjonen har svart på spørsmål så ingen kan være godkjent)
  dplyr::mutate(` ` = forcats::fct_relevel(` `, "Bruttoutvalg", "Maks populasjon", "Godkjente svar", "Svarprosent bruttoutvalg", "Andel populasjon deltatt"),
                dplyr::across(.cols = tidyselect::where(is.numeric), .fns = ~round(., digits = 1)),
                kilder = NULL) %>% 
  dplyr::arrange(as.integer(` `)) 
  

# Litt andre tall på grunn av de fire skolene som ble lagt ned, de er ikke med her, men tatt med i rapporten
tbl_2.5 <- 
  hel_data %>% 
  dplyr::filter(resp == "Skoleleder grunnskole") %>%
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_b(y = "fylke")

tbl_2.6 <-
  hel_data %>% 
  dplyr::filter(!is.na(landsdel_gs)) %>% 
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_c(x = "sk4d", y = "landsdel_gs")

tbl_2.7 <-
  hel_data %>% 
  dplyr::filter(!is.na(landsdel_gs)) %>% 
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_c(x = "gs3d", y = "landsdel_gs")

tbl_2.8 <-
  hel_data %>% 
  dplyr::filter(!is.na(sk4d) & sk4d != "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_gs", y="sk4d") %>% 
  gt::gt() %>% 
  gt::tab_spanner_delim(columns = -1, delim = "_")

tbl_2.9 <-
  hel_data %>% 
  dplyr::filter(sk4d != "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_gs", y="gs3d") %>% 
  gt::gt() %>% 
  gt::tab_spanner_delim(delim = "_")

tbl_2.10 <- 
  dplyr::bind_rows(
    hel_data %>%
      dplyr::filter(resp == "Skoleleder grunnskole") %>%
      dplyr::mutate(kilder = kilder == "Populasjon") %>% 
      tab_type_a(y="orgform"), 
    hel_data %>%
      dplyr::filter(resp == "Skoleleder grunnskole") %>%
      dplyr::mutate(kilder = kilder == "Populasjon") %>% 
      tab_type_a(y="målform"))

tbl_2.11 <-
  hel_data %>%
  dplyr::filter(resp == "Skoleleder videregående") %>%
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_b(y = "fylke")


tbl_2.12 <-
  hel_data %>% 
  dplyr::filter(sk4d == "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_vgs", y="typevgs") %>% 
  gt::gt() %>% 
  gt::tab_spanner_delim(delim = "_")


tbl_2.13 <-
  hel_data %>% 
  dplyr::filter(sk4d == "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_vgs", y="vgs3d") %>% 
  gt::gt() %>% 
  gt::tab_spanner_delim(delim = "_")

tbl_2.14 <- 
  dplyr::bind_rows(
    hel_data %>%
      dplyr::filter(resp == "Skoleleder videregående") %>%
      dplyr::mutate(kilder = kilder == "Populasjon") %>% 
      tab_type_a(y="orgform"), 
    hel_data %>%
      dplyr::filter(resp == "Skoleleder videregående") %>%
      dplyr::mutate(kilder = kilder == "Populasjon") %>% 
      tab_type_a(y="målform"))

tbl_2.15 <- 
  hel_data %>%
  dplyr::filter(resp == "Skoleeier kommune") %>%
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_b(y = "fylke")


#Måtte endre kategorier her fra h2022
tbl_2.16 <- 
  hel_data %>% 
  dplyr::filter(!is.na(landsdel_kom)) %>% 
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_c(x = "kstør_kom", y = "landsdel_kom")

tbl_2.17 <-
  hel_data %>% 
  dplyr::filter(resp == "Skoleeier kommune") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_kom", y="kstør_kom") %>% 
  gt::gt() %>% 
  gt::tab_spanner_delim(delim = "_")

tbl_2.18 <- 
  hel_data %>%
  dplyr::filter(kilder == "Utvalg",
                godkjent == "Godkjent") %>%
  saros:::crosstable3.data.frame(dep=paste0("s_1_", 1:5), indep="sk4d", totals = TRUE, 
                                 translations = list(by_total = "Total")) %>% 
  dplyr::filter(.category == "Valgt") %>%
  dplyr::select(.variable_label, .proportion, sk4d) %>%
  dplyr::mutate(.proportion = round(.proportion*100, 1)) %>% 
  tidyr::pivot_wider(names_from = sk4d, values_from = .proportion, values_fill = 0) %>% 
  dplyr::rename_with(.cols = -.variable_label, ~stringr::str_c(.x, "\n%")) %>%
  dplyr::rename(`Hvem svarer?` = .variable_label) %>% 
  gt::gt()

#Kommuner
tbl_2.19k <-
  hel_data %>%
  dplyr::filter(kilder == "Utvalg",
                godkjent == "Godkjent",
                stringr::str_detect(resp, "Skoleeier kommune")) %>% 
  dplyr::mutate(resp = forcats::fct_drop(resp)) %>% 
  saros:::crosstable3.data.frame(dep=paste0("s_3_", 1:5), totals = TRUE) %>% 
  dplyr::filter(.category == "Valgt") %>% 
  dplyr::select(.variable_label, `Skoleeier kommune` = .proportion)
#Fylker, mutate må være etter crosstable. Fordi ellers finner den ikke "label".
tbl_2.19F <- 
  hel_data %>%
  dplyr::filter(kilder == "Utvalg",
                godkjent == "Godkjent",
                stringr::str_detect(resp, "Skoleeier fylke")) %>% 
  dplyr::mutate(resp = forcats::fct_drop(resp)) %>% 
  saros:::crosstable3.data.frame(dep=paste0("s_5_", 1:5), totals = TRUE) %>% 
  dplyr::filter(.category == "Valgt") %>% 
  dplyr::select(.variable_label, `Skoleeier fylke` = .count)

tbl_2.19 <- 
  dplyr::inner_join(tbl_2.19k, tbl_2.19F, by = ".variable_label") %>%
  # dplyr::relocate("Total", .after = last_col()) %>%
  dplyr::mutate(across(`Skoleeier kommune`, ~round(.x*100, 1))) %>%
  dplyr::rename_with(.cols = -.variable_label, ~paste0(.x, "\n%")) %>%
  dplyr::rename(`Hvem svarer?` = .variable_label) %>% 
  gt::gt()
