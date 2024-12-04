library(dplyr, warn.conflicts = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)

  survey_data[[params$cycle]] <-
    paths$data$survey[[params$cycle]] %>%
    haven::read_dta() %>% # Om vi benytter parquet-formatet og rio-pakken så får vi alt det gode samt slipper unlabelled?
    labelled::unlabelled()
  

  survey_data[[params$cycle]] <-
    survey_data[[params$cycle]] %>%
    mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
           across(where(~rlang::is_string(labelled::var_label(.x)) &&
                          is.factor(.x) && 
                          (stringr::str_detect(labelled::var_label(.x), "I hvilken grad|^Hvor nyttig") ||
                             any(stringr::str_detect(levels(.x), "Helt enig|I stor grad")))), 
                  ~factor(.x, levels=levels(.x), ordered=TRUE)),
           
           fylke_gs = ifelse(!is.na(fylke_gs), fylke, NA),
           fylke_vgs = ifelse(!is.na(fylke_vgs), fylke, NA),
           fylke_kom = ifelse(!is.na(fylke_kom), fylke, NA),
           fylke_fylk = ifelse(!is.na(fylke_fylk), fylke, NA),
           across(matches("fylke"), ~factor(.x, levels=sort(unique(.x)))),
           across(matches("kstør_"), ~forcats::fct_relabel(.x, ~stringr::str_replace(., "0000", "000"))),
           across(matches("sk[45]d_"), ~forcats::fct_relabel(.x, ~stringr::str_replace(., "1-10\\. skoler", "1.-10. skole"))),
           across(matches("landsdel"), ~forcats::fct_relabel(.x, ~stringr::str_replace(., "Oslo-Området", "Oslo-området")))
    ) %>% 
    rename(s_712_1 = s_1, 
           s_712_2 = s_2, 
           s_107 = s_7, 
           s_900_3 = s_3, 
           s_900_4 = s_4, 
           s_900_5 = s_5, 
           s_900_6 = s_6) %>% 
    labelled::copy_labels_from(from = survey_data[[params$cycle]], .strict = FALSE) %>% # mutate kan fjerne labels, dette kopierer dem tilbake igjen
    # saros::sanitize_labels() %>% 
    labelled::set_variable_labels(
      landsdel_gs = "Grunnskoler, etter landsdel",
      landsdel_vgs = "Videregående skoler, etter landsdel",
      landsdel_kom = "Kommuner, etter landsdel",
      landsdel_fylk = "Fylker, etter landsdel",
      fylke_gs = "Grunnskoler, etter fylke",
      fylke_vgs = "Videregående skoler, etter fylke",
      fylke_kom = "Kommuner, etter fylke",
      fylke_fylk = "Fylker",
      sentind3d_gs = "Grunnskoler, etter sentralitet",
      sentind3d_vgs = "Videregående skoler, etter sentralitet",
      sentind3d_kom = "Kommuner, etter sentralitet",
      kstør_gs = "Grunnskoler, etter kommunestørrelse",
      kstør_kom = "Kommuner, etter kommunestørrelse",
      s_407_4 = "Kryss av for elementer som er inkludert i planen. Flere kryss mulig. - Annet",
      s_408_7 = "Kryss av for elementene som er inkludert i planen. Flere kryss mulig. - Annet",
      s_801 = "I hvilken grad har dere begynt å forberede dere på at det kommer ny opplæringslov høsten-2024")
  
  for(var in colnames(survey_data[[params$cycle]])) {
    labelled::var_label(survey_data[[params$cycle]][[var]]) <-
      stringr::str_replace_all(labelled::var_label(survey_data[[params$cycle]][[var]]),
                               pattern="([Ss])kole([n]*)[/]*[Kk]ommune[n]*[/]*[Ff]ylkeskommune[n]*", "\\1kole\\2/(fylkes)kommune\\2")
    labelled::var_label(survey_data[[params$cycle]][[var]]) <-
      stringr::str_replace_all(labelled::var_label(survey_data[[params$cycle]][[var]]),
                               pattern="[Kk]ommune[n]*[/]*([Ff])ylkeskommune[n]*", "(\\1ylkes)kommune")
    if(is.factor(survey_data[[params$cycle]][[var]])) {
      labelled::var_label(survey_data[[params$cycle]][[var]]) <-
        stringr::str_replace_all(labelled::var_label(survey_data[[params$cycle]][[var]]),
                                 pattern=", vennligst spesifiser:*|, spesifiser", "")
    }
    if(is.factor(survey_data[[params$cycle]][[var]])) {
      labelled::var_label(survey_data[[params$cycle]][[var]]) <-
        stringr::str_replace_all(labelled::var_label(survey_data[[params$cycle]][[var]]),
                                 pattern="Annet:", "Annet")
    }
    if(is.factor(survey_data[[params$cycle]][[var]])) {
      labelled::var_label(survey_data[[params$cycle]][[var]]) <-
        stringr::str_replace_all(labelled::var_label(survey_data[[params$cycle]][[var]]),
                                 pattern="Andre:", "Andre")
    }
    
  }
  
  check_labels[[params$cycle]] <-

    survey_data[[params$cycle]] %>%
    labelled::lookfor(details = TRUE) %>%
    select(variable, label, col_type) %>%
    tidyr::separate(label, into=c("label_prefix", "label_suffix", "label_surplus"), sep = " - ", remove = FALSE) %>%
    group_by(label, label_prefix, label_suffix, label_surplus, col_type) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n>1)
    stopifnot()
  
  qs::qsave(x = survey_data[[params$cycle]],
            file = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".qs")), nthreads = 4)
  survey_data[[params$cycle]] %>% 
    labelled::set_variable_labels(.strict = FALSE, .labels =
                                    purrr::map(labelled::get_variable_labels(.),
                                               ~{
                                                 if(length(.x)==0) return()
                                                 .x
                                                 })) %>%
    haven::write_dta(path = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".dta")))
