
# Bør være unødvendig om alt gjøres i R
df_labels <-
  readxl::read_excel(paths$df_labels[[cycle]]) #name	varlab	vallab_full

data <-
  paths$data_survey[[cycle]] %>%
  haven::read_dta() %>% # Om vi benytter parquet-formatet og rio-pakken så får vi alt det gode samt slipper unlabelled?
  labelled::unlabelled()

# Mye av dette bør være unødvendig dersom vi omgår Stata, og variabellabels er satt riktig fra starten
labelled::var_label(data) <-
  labelled::var_label(data) %>%
  purrr::map2(.y = names(.), .f = ~{
    if(.y %in% df_labels$name) {
      label <-
        dplyr::filter(df_labels, name==.y) %>%
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

data <-
  data %>%
  mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*")))) %>% 
  mutate(across(where(~!is.na(labelled::var_label(.x)) & 
                        stringr::str_detect(labelled::var_label(.x), "I hvilken grad|^Hvor nyttig") & 
                        is.factor(.x)), ~as.ordered(.x))) %>% 
  labelled::copy_labels_from(from = data, .strict = FALSE) # mutate kan fjerne labels, dette kopierer dem tilbake igjen
