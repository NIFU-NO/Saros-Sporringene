library(dplyr, warn.conflicts = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)

  # Bør være unødvendig om alt gjøres i R
  df_labels[[params$cycle]] <-
    readxl::read_excel(paths$df_labels[[params$cycle]]) #name	varlab	vallab_full
  
  survey_data[[params$cycle]] <-
    paths$data$survey[[params$cycle]] %>%
    haven::read_dta() %>% # Om vi benytter parquet-formatet og rio-pakken så får vi alt det gode samt slipper unlabelled?
    labelled::unlabelled()
  
  # Mye av dette bør være unødvendig dersom vi omgår Stata, og variabellabels er satt riktig fra starten
  labelled::var_label(survey_data[[params$cycle]]) <-
    labelled::var_label(survey_data[[params$cycle]]) %>%
    purrr::map2(.y = names(.), .f = ~{
      if(.y %in% df_labels[[params$cycle]]$name) {
        label <-
          dplyr::filter(df_labels[[params$cycle]], name==.y) %>%
          dplyr::pull("vallab_full") %>%
          stringr::str_trim() %>%
          stringr::str_replace_all(pattern = "[[:space:]]+", replacement = " ") %>%
          stringr::str_remove_all(pattern = ", spesifiser.*|, vennligst .*")
        if(!is.na(label) && stringr::str_count(label, pattern = " - ") > 1) {
          label <- stringi::stri_replace_first(label, fixed = " - ", replacement = ": ")
        }
        if(!is.na(label) && label == "sentind3d gs") return("Sentralitetsindeks grunnskole")
        if(!is.na(label) && label == "sentind3d vgs") return("Sentralitetsindeks videregående")
        if(!is.na(label) && label == "sentind3d kom") return("Sentralitetsindeks kommune")
        label
        
      }  else .x
    })
  
  survey_data[[params$cycle]] <-
    survey_data[[params$cycle]] %>%
    mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
           resp_skoleleder = factor(resp, levels=c("Skoleleder grunnskole", "Skoleleder videregående")),
           across(matches("^resp_[suk]"), ~forcats::fct_relabel(.x, function(x) stringr::str_wrap(x, width=10))),
           across(where(~rlang::is_string(labelled::var_label(.x)) && 
                          stringr::str_detect(labelled::var_label(.x), "I hvilken grad|^Hvor nyttig") &&
                          is.factor(.x)), ~factor(.x, levels=levels(.x), ordered=TRUE))) %>% 
    labelled::copy_labels_from(from = survey_data[[params$cycle]], .strict = FALSE) %>% # mutate kan fjerne labels, dette kopierer dem tilbake igjen
    labelled::set_variable_labels(resp_skoleleder = "Skoleledere")
  
  for(var in colnames(survey_data[[params$cycle]])) {
    labelled::var_label(survey_data[[params$cycle]][[var]]) <-
      stringr::str_replace_all(labelled::var_label(survey_data[[params$cycle]][[var]]),
                               pattern="([Ss])kole([n]*)[/]*[Kk]ommune[n]*[/]*[Ff]ylkeskommune[n]*", "\\1kole\\2/(fylkes)kommune\\2")
    labelled::var_label(survey_data[[params$cycle]][[var]]) <-
      stringr::str_replace_all(labelled::var_label(survey_data[[params$cycle]][[var]]),
                               pattern="([Kk])ommune[n]*[/]*[Ff]ylkeskommune[n]*", "(\\1ylkes)kommune")
  }
  
  qs::qsave(x = survey_data[[params$cycle]],
            file = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".qs")), nthreads = 4)
  haven::write_dta(data = survey_data[[params$cycle]],
                   path = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".dta")))
  
