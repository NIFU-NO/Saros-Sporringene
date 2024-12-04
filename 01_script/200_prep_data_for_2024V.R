library(dplyr, warn.conflicts = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)

survey_data[[params$cycle]] <-
  paths$data$survey[[params$cycle]] %>%
  readRDS()

survey_data[[params$cycle]] <-
  survey_data[[params$cycle]] %>%
  mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
         across(where(~rlang::is_string(labelled::var_label(.x)) &&
                        is.factor(.x) && 
                        (stringr::str_detect(labelled::var_label(.x), "I hvilken grad|^Hvor nyttig") ||
                           any(stringr::str_detect(levels(.x), "Helt enig|I stor grad")))), 
                ~factor(.x, levels=levels(.x), ordered=TRUE)),
         Q10.8_10 = forcats::fct_recode(Q10.8_10,"Valgt" = "Vet ikke"),
         Q10.8_10 = forcats::fct_expand(Q10.8_10, "Ikke valgt", after = 0)
  ) %>% 
  rowwise() %>% 
  mutate(Q9.13_99 = sum(as.integer(c_across(c(Q9.13_1:Q9.13_5))), na.rm=TRUE),
         across(matches("Q9.13_[1-9]+$"), ~factor(ifelse(Q9.13_99==0, NA_character_, ifelse(is.na(.x), 1, 2)), levels=1:2, labels=c("Ikke valgt", "Valgt")))) %>% 
  select(-Q9.13_99) %>% 
  labelled::copy_labels_from(from = survey_data[[params$cycle]], .strict = FALSE) %>% # mutate kan fjerne labels, dette kopierer dem tilbake igjen
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern=" {2,}", " ")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern="([Ss])kole([n]*)[/]*[Kk]ommune[n]*[/]*[Ff]ylkeskommune[n]*", "\\1kole\\2/(fylkes)kommune\\2")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern=", vennligst spesifiser:*|, spesifiser", "")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern="[Aa]nnet:", "Annet")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern="[Aa]ndre:", "Andre")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern="Annen funksjon:", "Annen funksjon")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern="[Kk]ommune[n]*[/]*([Ff])ylkeskommune[n]*", "(\\1ylkes)kommune")) %>% 
  labelled::set_variable_labels(
    Q9.6 = paste0(labelled::var_label(.$Q9.6), " (Q9.6)"),
    Q9.8 = paste0(labelled::var_label(.$Q9.8), " (Q9.8)"),
    Q9.16 = paste0(labelled::var_label(.$Q9.16), " (Q9.16)"),
    Q9.18 = paste0(labelled::var_label(.$Q9.18), " (Q9.18)"),
    Antall_svar = "Antall svar",
    svarstatus = "Svarstatus",
    sk4d = "Firedelt skoletype",
    sk5d = "Femdelt skoletype"
  ) %>% 
  saros::sanitize_labels()

check_labels[[params$cycle]] <-
  survey_data[[params$cycle]] %>%
  labelled::lookfor(details = TRUE) %>%
  select(variable, label, col_type) %>%
  tidyr::separate(label, into=c("label_prefix", "label_suffix", "label_surplus"), sep = " - ", remove = FALSE) %>%
  group_by(label, label_prefix, label_suffix, label_surplus, col_type) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n>1)

if(nrow(check_labels[[params$cycle]]) != 0) {
  print(check_labels[[params$cycle]])
  cli::cli_abort(c(x="Unngå variabler av samme type som har identiske labels.", 
                   i="En mulighet er å legge til variabelnavnet i parentes på slutten av labelene."))
}

survey_data[[params$cycle]] %>%
  labelled::var_label() %>%
  unlist() %>% 
  unname() %>% 
  stringr::str_subset(pattern = ".*( - ).*( - ).*")

# survey_data[[params$cycle]] %>% 
#   select(
    # Q2.2_1:Q2.2_5, Q2.2_5_TEXT,
    # Q2.3_1:Q2.3_5, Q2.3_5_TEXT,
    # Q2.4, Q2.4_5_TEXT,
  #   Q4.2,
  #   Q4.3,
  #   Q4.4,
  #   Q4.6,
  #   Q5.2_1:Q5.2_3,
  #   Q9.9_1:Q9.9_5,
  #   fylke_gs,
  #   fylke_vgs,
  #   fylke_kom,
  #   fylke_fylk) %>% 
  # as_tibble() %>% 
  # select(where(~n_distinct(.x)<20)) %>% 
  # lapply(table)

# survey_data[[params$cycle]] %>% 
#   labelled::set_variable_labels(.strict = FALSE, .labels =
#                                   purrr::map(labelled::get_variable_labels(.),
#                                              ~{
#                                                if(length(.x)==0) return()
#                                                .x
#                                              })) %>%
#   haven::write_dta(path = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".dta")))

qs::qsave(x = survey_data[[params$cycle]],
          file = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".qs")), nthreads = 4)
