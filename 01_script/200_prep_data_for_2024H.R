library(dplyr, warn.conflicts = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)

survey_data[[params$cycle]] <-
  paths$data$survey[[params$cycle]] %>%
  readRDS()

survey_data[[params$cycle]] |>
  names() |>
  stringr::str_subset("^Q") |>
  stringr::str_subset("^Q[0-9]{1,2}\\.", negate = TRUE) %>% 
  {if(length(.)) cli::cli_abort("Variabelnavnene matcher ikke forventet mønster med QKK, der KK er kapittelnummeret (03, 04, osv): {(.)}")}

survey_data[[params$cycle]] <-
  survey_data[[params$cycle]] %>%
  mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
         across(where(~rlang::is_string(labelled::var_label(.x)) &&
                        is.factor(.x) && 
                        (stringr::str_detect(labelled::var_label(.x), "I hvilken grad|^Hvor nyttig") ||
                           any(stringr::str_detect(levels(.x), "Helt enig|I stor grad")))), 
                ~factor(.x, levels=levels(.x), ordered=TRUE)),
         across(where(~all(c("Valgt", "Ikke valgt") %in% levels(.x)) && is.factor(.x)),
                ~forcats::fct_relevel(.x, "Valgt", "Ikke valgt"))
         #Q10.8_10 = forcats::fct_recode(Q10.8_10,"Valgt" = "Vet ikke"),
         #Q10.8_10 = forcats::fct_expand(Q10.8_10, "Ikke valgt", after = 0)
  ) %>% 
  # rowwise() %>% 
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
                                                                        pattern="Annet, vennligst beskriv:", "Annet, vennligst beskriv")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern="Annet, vennligst spesifiser:", "Annet, vennligst spesifiser")) %>% 
  labelled::update_variable_labels_with(.fn = ~stringr::str_replace_all(.x,
                                                                        pattern="[Kk]ommune[n]*[/]*([Ff])ylkeskommune[n]*", "(\\1ylkes)kommune")) %>% 
  labelled::set_variable_labels(
    Antall_svar = "Antall svar",
    svarstatus = "Svarstatus",
    sk4d = "Firedelt skoletype",
    godkjent = "Godkjent",
    gsvg3d = "Skolestørrelse",
    sk5d = "Femdelt skoletype"
  ) %>% 
  saros.utils::remove_special_chars_in_labels()

check_labels[[params$cycle]] <-
  survey_data[[params$cycle]] %>%
  labelled::lookfor(details = TRUE) %>%
  select(variable, label, col_type) %>%
  tidyr::separate(label, into=c("label_prefix", "label_suffix", "label_surplus"), sep = " - ", remove = FALSE) %>%
  group_by(label, label_prefix, label_suffix, label_surplus, col_type) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n>1) |>
  suppressWarnings()

if(nrow(check_labels[[params$cycle]]) != 0) {
  print(check_labels[[params$cycle]])
  cli::cli_abort(c(x="Unngå variabler av samme type som har identiske labels.", 
                   i="En mulighet er å legge til variabelnavnet i parentes på slutten av labelene."))
}


survey_data[[params$cycle]] |>
  labelled::var_label() |>
  unlist() |>
  unname() |> 
  stringr::str_subset(pattern = ".*( - ).*( - ).*") %>% 
  {if(length(.)) cli::cli_warn("Du har variabeletiketter med flere bindestreker, dette vil trolig skape trøbbel.")}

fs::dir_create("_logs")
survey_data[[params$cycle]] |>
  labelled::look_for(details = "basic") |>
  dplyr::filter(col_type %in% c("ord", "fct")) |>
  dplyr::distinct(levels, .keep_all = TRUE) |> 
  dplyr::select(-value_labels, -missing, -pos) |> 
  tidyr::unnest(levels) |>
  dplyr::mutate(level_id = 1:dplyr::n(), .by = variable) |>
  tidyr::pivot_wider(names_from = level_id, values_from = levels) |>
  writexl::write_xlsx(fs::path("_logs", paste0("unike_svarkategorier_", params$cycle, ".xlsx")))
cli::cli_warn("Sjekk {.path {fs::path('_logs', paste0('unike_svarkategorier_', params$cycle, '.xlsx'))}}.")


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
