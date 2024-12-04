library(magrittr)

pop_data <-  # Inneholder populasjonen, med indikator for om respondent er også i utvalget
paths$data$population[[params$cycle]]$abs %>%
  readRDS() %>%
  labelled::unlabelled()
  
hel_data <- # Lag et datasett med populasjon, OG utvalget (som nå blir duplisert)
  bind_rows(Populasjon = dplyr::mutate(pop_data, godkjent_integer = NA),
            Utvalg = dplyr::filter(pop_data, kilder == "Utvalgt"),
            .id = "kilder") %>% 
  mutate(sk4d = forcats::fct_recode(sk4d, "Ungdomsskole" = "ungdomsskole")) %>% 
  labelled::copy_labels_from(pop_data) %>%
  rename(`Antall svar` = Antall_svar) %>% # For fine tabeller
  labelled::set_variable_labels( # Dette skal egentlig stå i dataprep
    landsdel = "landsdel",
    fylke = "fylke",
    sk4d = "firedelt skoletype",
    resp = "respondenttype") %>% 
  labelled::set_variable_labels(.strict = FALSE, # Omgjør alle til str_sentence for kun dette kapittelet
                                .labels = purrr::map(labelled::var_label(.),
                                                     ~stringr::str_to_sentence(.x)))


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
  dplyr::mutate(dplyr::across(dplyr::where(is.logical), ~ifelse(.x, "X", ""))) %>% 
  labelled::remove_var_label()

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
  dplyr::select(-.id, -label)  %>% 
  labelled::remove_var_label()


tbl_2.3 <- 
  hel_data %>%
  dplyr::filter(kilder == "Utvalg") %>%
  dplyr::summarize(Gjennomsnitt = round(mean(`Antall svar`, na.rm = TRUE), digits = 1),
                   Median = median(`Antall svar`, na.rm = TRUE),
                   Maks = max(`Antall svar`, na.rm = TRUE),
                   .by = c(resp, godkjent)) %>% 
  arrange(as.integer(resp), godkjent) %>%
  rename(Respondenttype = resp, Godkjent = godkjent) %>% 
  labelled::remove_var_label()


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
  dplyr::arrange(as.integer(` `))  %>% 
  labelled::remove_var_label()
  

# Litt andre tall på grunn av de fire skolene som ble lagt ned, de er ikke med her, men tatt med i rapporten
tbl_2.5 <- 
  hel_data %>% 
  dplyr::filter(resp == "Skoleleder grunnskole") %>%
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_b(y = "fylke") %>% 
  rename(Fylke = fylke) %>% 
  labelled::remove_var_label()

tbl_2.6 <-
  hel_data %>% 
  dplyr::filter(!is.na(landsdel_gs)) %>% 
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_c(x = "sk4d", y = "landsdel_gs")  %>% 
  labelled::remove_var_label()

tbl_2.7 <-
  hel_data %>% 
  dplyr::filter(!is.na(landsdel_gs)) %>% 
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_c(x = "gs3d", y = "landsdel_gs") %>% 
  labelled::remove_var_label()

tbl_2.8 <- 
  hel_data %>% 
  dplyr::filter(!is.na(sk4d) & sk4d != "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_gs", y="sk4d") %>%  
  labelled::remove_var_label() %>% 
  dplyr::rename_with(.fn = ~stringr::str_replace(.x, "Populasjon", "Pop.")) %>% 
  dplyr::rename(Landsdel = landsdel_gs) %>%
  gt::gt() %>% 
  gt::tab_spanner_delim(columns = -1, delim = "_")

tbl_2.9 <-
  hel_data %>% 
  dplyr::filter(sk4d != "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_gs", y="gs3d") %>%
  rename_with(.fn = ~stringr::str_replace(.x, "Populasjon", "Pop.")) %>% 
  labelled::remove_var_label() %>%
  dplyr::rename(Landsdel = landsdel_gs) %>%
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
      tab_type_a(y="målform")) %>% 
  labelled::remove_var_label()

tbl_2.11 <- # For få i pop? 
  hel_data %>%
  dplyr::filter(resp == "Skoleleder videregående") %>%
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_b(y = "fylke") %>%
  rename(Fylke = fylke) %>%
  labelled::remove_var_label()


tbl_2.12 <- 
  hel_data %>% 
  dplyr::filter(sk4d == "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_vgs", y="typevgs")  %>% 
  rename_with(.fn = ~stringr::str_replace(.x, "Populasjon", "Pop.")) %>% 
  labelled::remove_var_label() %>% 
  dplyr::rename(Landsdel = landsdel_vgs) %>%
  gt::gt() %>% 
  gt::tab_spanner_delim(delim = "_")


tbl_2.13 <- # Oslo
  hel_data %>% 
  dplyr::filter(sk4d == "Videregående") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_vgs", y="vgs3d")  %>% 
  rename_with(.fn = ~stringr::str_replace(.x, "Populasjon", "Pop.")) %>% 
  labelled::remove_var_label() %>% 
  dplyr::rename(Landsdel = landsdel_vgs) %>%
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
      tab_type_a(y="målform")) %>% 
  labelled::remove_var_label()

tbl_2.15 <- # For få i pop. 
  hel_data %>%
  dplyr::filter(resp == "Skoleeier kommune") %>%
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_b(y = "fylke") %>% 
  rename(Fylke = fylke) %>%
  labelled::remove_var_label()


#Måtte endre kategorier her fra h2022
tbl_2.16 <-  # Oslo mangler småskoler?
  hel_data %>% 
  dplyr::filter(!is.na(landsdel_kom)) %>% 
  dplyr::mutate(kilder = kilder == "Utvalg") %>% 
  tab_type_c(x = "kstør_kom", y = "landsdel_kom") %>% 
  labelled::remove_var_label()

tbl_2.17 <- # Oslo
  hel_data %>% 
  dplyr::filter(resp == "Skoleeier kommune") %>% 
  dplyr::mutate(godkjent = godkjent == "Godkjent") %>%
  tab_type_d2(x = "landsdel_kom", y="kstør_kom")  %>% 
  labelled::remove_var_label() %>%
  dplyr::rename(Landsdel = landsdel_kom) %>%  
  gt::gt() %>% 
  gt::tab_spanner_delim(delim = "_")

tbl_2.18 <- # Mangler N totalrad
  hel_data %>%
  dplyr::filter(kilder == "Utvalg",
                godkjent == "Godkjent") %>%
  saros:::crosstable3.data.frame(dep=paste0("hvem_leder_", 1:5), indep="sk4d", totals = TRUE, 
                                 translations = list(by_total = "Total")) %>% 
  dplyr::filter(.category == "Valgt") %>%
  dplyr::select(.variable_label, .proportion, sk4d) %>%
  dplyr::mutate(.proportion = round(.proportion*100, 1)) %>% 
  tidyr::pivot_wider(names_from = sk4d, values_from = .proportion, values_fill = 0) %>% 
  dplyr::rename_with(.cols = -.variable_label, ~stringr::str_c(.x, "\n%")) %>%
  dplyr::rename(`Hvem svarer?` = .variable_label)  %>% 
  labelled::remove_var_label() %>% 
  gt::gt()

#Kommuner
tbl_2.19k <-
  hel_data %>%
  dplyr::filter(kilder == "Utvalg",
                godkjent == "Godkjent",
                stringr::str_detect(resp, "Skoleeier kommune")) %>% 
  dplyr::mutate(resp = forcats::fct_drop(resp)) %>% 
  saros:::crosstable3.data.frame(dep=paste0("hvem_eier_", 1:5), totals = TRUE) %>% 
  dplyr::filter(.category == "Valgt") %>% 
  dplyr::select(.variable_label, `Skoleeier kommune` = .proportion)
#Fylker, mutate må være etter crosstable. Fordi ellers finner den ikke "label".
tbl_2.19F <- 
  hel_data %>%
  dplyr::filter(kilder == "Utvalg",
                godkjent == "Godkjent",
                stringr::str_detect(resp, "Skoleeier fylke")) %>% 
  dplyr::mutate(resp = forcats::fct_drop(resp)) %>% 
  saros:::crosstable3.data.frame(dep=paste0("hvem_eier_", 1:5), totals = TRUE) %>% 
  dplyr::filter(.category == "Valgt") %>% 
  dplyr::select(.variable_label, `Skoleeier fylke` = .count)

tbl_2.19 <- #Mangler N totalrad
  dplyr::inner_join(tbl_2.19k, tbl_2.19F, by = ".variable_label") %>%
  # dplyr::relocate("Total", .after = last_col()) %>%
  dplyr::mutate(across(`Skoleeier kommune`, ~round(.x*100, 1))) %>%
  dplyr::rename_with(.cols = -.variable_label, ~paste0(.x, "\n%")) %>%
  dplyr::rename(`Hvem svarer?` = .variable_label)  %>% 
  labelled::remove_var_label() %>% 
  gt::gt()
