library(dplyr, warn.conflicts = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)


survey_data[[params$cycle]] <-
  paths$data$survey[[params$cycle]] %>%
  readRDS()
# Lage/oppdatere kapitteldatasett på nytt uten å generere alle qmd-filene på nytt
paths$chapter_overview[[params$cycle]] |>
  readxl::read_excel() |>
  dplyr::rename(chapter = Tema, author = Kapittelforfatter) |>
  dplyr::rowwise() |>
  dplyr::group_map(.keep = TRUE, .f = ~{
      deps <- stringr::str_split(.x$dep, pattern = ", *")[[1]]
      deps <- deps[!is.na(deps)]
      indeps <- stringr::str_split(.x$indep, pattern = ", *")[[1]]
      indeps <- indeps[!is.na(indeps)]
      vars <- unique(c(deps, indeps, config_macro[[params$cycle]]$auxiliary_variables))
      vars <- saros.base:::eval_cols(vars, data=survey_data[[params$cycle]]) |> unlist() |> names()
      path <- here::here(paths$output[[params$cycle]], "Test", 
                         paste0("data_", stringr::str_pad(.x$Kapittelnr, 2, pad = "0"), "_", saros.base:::filename_sanitizer(.x$chapter, max_chars = 24), "_fylke", ".qs"))
      fs::dir_create(fs::path_dir(path))
      
      survey_data[[params$cycle]] |>
        dplyr::select(resp, tidyselect::all_of(vars)) |>
        dplyr::filter( resp %in% "Skoleeier fylkeskommune") |>
        qs::qsave(file = path)

      path <- here::here(paths$output[[params$cycle]], "Test", 
                         paste0("data_",  stringr::str_pad(.x$Kapittelnr, 2, pad = "0"), "_", saros.base:::filename_sanitizer(.x$chapter, max_chars = 24), ".qs"))

      survey_data[[params$cycle]] |>
        dplyr::select(resp, tidyselect::all_of(vars)) |>
        dplyr::filter(! resp %in% "Skoleeier fylkeskommune") |>
        qs::qsave(file = path)
      

  }) |> 
  unlist()
