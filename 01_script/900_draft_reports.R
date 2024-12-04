library(dplyr)

############################################################
## Felles for både ikke_fylke-output og fylke-output

survey_data[[params$cycle]] <-  # Read in survey_data from disk - to ease caching
  qs::qread(file = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".qs")),
            strict = TRUE, nthreads = 4)

config_macro[[params$cycle]] <-
  saros.base::read_default_draft_report_args(
    path = paths$organization_global_draft_report_settings
  ) |>
  utils::modifyList(val =   
                      saros.base::read_default_draft_report_args(
                        path = fs::path(paths$resources, "YAML", "_draft_report_settings.yaml")
                      ))

config_macro[[params$cycle]]$title <- params$cycle 
config_macro[[params$cycle]]$always_show_bi_for_indep <- "resp"
config_macro[[params$cycle]]$auxiliary_variables <-
  c("fylke",
    "fylke_gs",
    "fylke_vgs",
    "landsdel_gs",
    "landsdel_vgs",
    "landsdel_kom",
    "landsdel_fylk",
    "sk4d",
    "sk5d",
    "resp")
config_macro[[params$cycle]]$hide_chunk_if_n_below <- 0
config_macro[[params$cycle]]$hide_variable_if_all_na <- TRUE
config_macro[[params$cycle]]$organize_by <-
  c("chapter", ".variable_label_prefix_dep", ".variable_type_dep", ".variable_label_prefix_indep", ".template_name")
# tryCatch(fs::dir_delete(fs::path(paths$drafts_produced,
#                                  "Rapporter", params$response_group, params$cycle)), error=function(e) cli::cli_warn(e))
# tryCatch(fs::dir_delete(fs::path(paths$drafts_completed,
#                                  "Rapporter", params$response_group, params$cycle)), error=function(e) cli::cli_warn(e))

config_macro[[params$cycle]]$arrange_section_by <-
  c(chapter = FALSE, 
    .variable_name_dep = FALSE,
    .variable_type_dep = FALSE,
    # .variable_name_indep = FALSE, Defaults to order given in chapter_overview
    .template_name = FALSE)

########################################################
### Chapter overview (samme for ikke-fylke og fylke) ###
########################################################
chapter_overview[[params$cycle]] <-
  paths$chapter_overview[[params$cycle]] |>
  readxl::read_excel() |>
  dplyr::rename(chapter = Tema, author = Kapittelforfatter) |> 
  mutate(chapter_ord = as.integer(factor(chapter, levels = unique(chapter)))) 
# Tests
purrr::walk(c("dep", "indep", "Grunnskole", "Videregående", "Kommune", "Fylkeskommune"), 
            .f = ~if(!.x %in% names(chapter_overview[[params$cycle]])) {
              cli::cli_abort(c("Kapitteloversikt mangler kolonnen {.var {.x}} (eller omdøp dem slik at de stemmer med det overstående). :)"), call = rlang::caller_env(5))
            })
if(any(!is.na(chapter_overview[[params$cycle]]$dep) & stringr::str_detect(chapter_overview[[params$cycle]]$dep, pattern = "[^\\\\][\\\\]{1}\\."))) {
  cli::cli_abort("Kapitteloversiktens dep/indep-kolonner med {.fn matches} må spesifisere punktum med to backslasher (\\\\.), ikke en (\\.)")
}
if(!any(stringr::str_detect(chapter_overview[[params$cycle]]$chapter, pattern = "Innledning|Beskrivelse av utvalg"))) {
  cli::cli_abort("Har du glemt at 'Innledning' og 'Beskrivelse av utvalg' også skal med i kapitteloversikten?")
}

#######################################################
################ For ikke-fylke output ################
#######################################################

df_chunk_templates$ikke_fylke <-
  saros.base::get_chunk_template_defaults() |>
  dplyr::filter(.template_name %in% c("cat_plot_html", #"cat_table_html", 
                                      "chr_table"))
df_chunk_templates$ikke_fylke[df_chunk_templates$ikke_fylke$.template_name %in% "cat_plot_html" &
                           df_chunk_templates$ikke_fylke$.template_variable_type_indep %in% "fct;ord", 
                         ".template"] <-
  "::: {{#fig-{.chunk_name}}}

```{{r, fig.height = fig_height_h_barchart(n_y={.n_dep}, n_cats_y={.n_cats_dep}, max_chars_cats_y={.max_chars_dep}, n_x={.n_indep}, n_cats_x={.n_cats_indep}, max_chars_cats_x={.max_chars_indep})}}
{.obj_name} <- 
	data_{.chapter_foldername} |>
		makeme(dep = c({.variable_name_dep}), 
		indep = c({.variable_name_indep}), 
		type = 'cat_plot_html')
x <- stringi::stri_c('N = ', n_range2({.obj_name}))
#link <- make_link(data = {.obj_name}$data)
#link_plot <- make_link(data = {.obj_name}, file_suffix = '.png', link_prefix='[PNG](', save_fn = ggsaver)
#x <- I(paste0(c(x, link, link_plot), collapse=', '))
girafe(ggobj = {.obj_name})
```

_{.variable_label_prefix_dep}_ etter _{tolower(.variable_label_prefix_indep)}_. `{{r}} x`.

:::
"
df_chunk_templates$ikke_fylke[df_chunk_templates$ikke_fylke$.template_name %in% "cat_plot_html" &
                           df_chunk_templates$ikke_fylke$.template_variable_type_indep %in% NA, 
                         ".template"] <-
  "::: {{#fig-{.chunk_name}}}

```{{r, fig.height = fig_height_h_barchart(n_y={.n_dep}, n_cats_y={.n_cats_dep}, max_chars_cats_y={.max_chars_dep})}}
{.obj_name} <- 
	data_{.chapter_foldername} |>
		makeme(dep = c({.variable_name_dep}), 
		type = 'cat_plot_html')
x <- stringi::stri_c('N = ', n_range2({.obj_name}))
#link <- make_link(data = {.obj_name}$data)
#link_plot <- make_link(data = {.obj_name}, file_suffix = '.png', link_prefix='[PNG](', save_fn = ggsaver)
#x <- I(paste0(c(x, link, link_plot), collapse=', '))
girafe(ggobj = {.obj_name})
```

_{.variable_label_prefix_dep}_. `{{r}} x`.

:::"

# df_chunk_templates$ikke_fylke[df_chunk_templates$ikke_fylke$.template_name %in% "cat_table_html" &
#                            df_chunk_templates$ikke_fylke$.template_variable_type_indep %in% "fct;ord", 
#                          ".template"] <-
#   "::: {{#tbl-{.chunk_name}}}
# 
# ```{{r}}
# {.obj_name} <- 
# 	data_{.chapter_foldername} |>
# 		makeme(dep = c({.variable_name_dep}),  
# 		indep = c({.variable_name_indep}), 
# 		type = 'cat_table_html')
# x <- stringi::stri_c('N = ', n_range(data = data_{.chapter_foldername}, 
# 	dep = c({.variable_name_dep}), 
# 	indep = c({.variable_name_indep})))
# #link <- make_link(data={.obj_name})
# x <- I(paste0(c(x, link), collapse=', '))
# gt({.obj_name})
# ```
# 
# _{.variable_label_prefix_dep}_ etter _{tolower(.variable_label_prefix_indep)}_. `{{r}} x`.
# 
# :::"
# 
# df_chunk_templates$ikke_fylke[df_chunk_templates$ikke_fylke$.template_name %in% "cat_table_html" &
#                            df_chunk_templates$ikke_fylke$.template_variable_type_indep %in% NA, 
#                          ".template"] <-
#   "::: {{#tbl-{.chunk_name}}}
# 
# ```{{r}}
# {.obj_name} <- 
# 	data_{.chapter_foldername} |>
# 		makeme(dep = c({.variable_name_dep}),
# 		type = 'cat_table_html')
# x <- stringi::stri_c('N = ', n_range(data = data_{.chapter_foldername}, 
# 	dep = c({.variable_name_dep})))
# #link <- make_link(data={.obj_name})
# x <- I(paste0(c(x, link), collapse=', '))
# gt({.obj_name})
# ```
# 
# _{.variable_label_prefix_dep}_. `{{r}} x`.
# 
# :::"

config_macro$ikke_fylke <- config_macro[[params$cycle]]
config_macro$ikke_fylke$hide_bi_entry_if_sig_above <- 0.05


survey_data$ikke_fylke[[params$cycle]] <-  # Read in survey_data from disk - to ease caching
  qs::qread(file = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".qs")),
            strict = TRUE, nthreads = 4) |>
  dplyr::filter(resp != "Skoleeier fylkeskommune") |>
  dplyr::mutate(resp = droplevels(resp)) |>
  labelled::set_variable_labels(resp ="Respondenttype") # Pga mutate må denne på igjen


chapter_structure[[params$cycle]]$ikke_fylke <-
  rlang::inject(
  saros.base::refine_chapter_overview(
    chapter_overview = chapter_overview[[params$cycle]]  |>
      dplyr::mutate(indep = stringr::str_remove(indep, ",[[:space:]]*.+_fylk")),
    data = survey_data$ikke_fylke[[params$cycle]],
    chunk_templates = df_chunk_templates$ikke_fylke,
    !!!config_macro[[params$cycle]][c("always_show_bi_for_indep", "hide_chunk_if_n_below", "organize_by", "arrange_section_by")]
  )
) |>
  # The following fixes a bug in the sorting 
  left_join(chapter_overview[[params$cycle]]|> 
              select(chapter, chapter_ord)) |> 
  ungroup() |> 
  arrange(chapter_ord, .variable_label_prefix_dep, .variable_type_dep, .variable_label_prefix_indep, .template_name) |>
  mutate(chapter = forcats::fct_reorder(chapter, chapter_ord),
         .chapter_number = chapter_ord,
         chapter_number_text = sprintf(paste0("%0", 2, "d"),
                                  chapter_ord),
         chapter_foldername_clean = 
           saros.base:::filename_sanitizer(as.character(chapter),
                              max_chars = 24,
                              accept_hyphen = FALSE,
                              make_unique = FALSE),
         .chapter_foldername =
           stringi::stri_c(chapter_number_text, "_", chapter_foldername_clean,
                           ignore_null = TRUE)) |> 
  group_by(chapter, .variable_label_prefix_dep, .variable_type_dep, .variable_label_prefix_indep, .template_name)

saros.base::draft_report(
  data = survey_data$ikke_fylke[[params$cycle]],
  chapter_structure = chapter_structure[[params$cycle]]$ikke_fylke |> dplyr::filter(Kapittelnr == 10),
  !!!config_macro[[params$cycle]][stringr::str_detect(names(config_macro[[params$cycle]]), pattern="index|chapter|report|serialized|title|log_file|auxiliary_variables")],
  path = fs::path(paths$output[[params$cycle]])
  )


#######################################################
################## For fylke-output ###################
#######################################################

df_chunk_templates$fylke <-
  saros.base::get_chunk_template_defaults() |>
  dplyr::filter(.template_name %in% c("cat_plot_html", #"cat_table_html", 
                                      "chr_table"))
df_chunk_templates$fylke[df_chunk_templates$fylke$.template_name %in% "cat_plot_html" &
                           df_chunk_templates$fylke$.template_variable_type_indep %in% "fct;ord", 
                         ".template"] <-
  "::: {{#fig-{.chunk_name}}}

```{{r, fig.height = fig_height_h_barchart(n_y={.n_dep}, n_cats_y={.n_cats_dep}, max_chars_cats_y={.max_chars_dep}, n_x={.n_indep}, n_cats_x={.n_cats_indep}, max_chars_cats_x={.max_chars_indep})}}
{.obj_name} <- 
	data_{.chapter_foldername} |>
		makeme(dep = c({.variable_name_dep}), 
		indep = c({.variable_name_indep}), 
		type = 'cat_plot_html',
		data_label = 'count')
x <- stringi::stri_c('N = ', n_range2({.obj_name}))
#link <- make_link(data = {.obj_name}$data)
#link_plot <- make_link(data = {.obj_name}, file_suffix = '.png', link_prefix='[PNG](', save_fn = ggsaver)
#x <- I(paste0(c(x, link, link_plot), collapse=', '))
girafe(ggobj = {.obj_name})
```

_{.variable_label_prefix_dep}_ etter _{tolower(.variable_label_prefix_indep)}_. `{{r}} x`.

:::
"
df_chunk_templates$fylke[df_chunk_templates$fylke$.template_name %in% "cat_plot_html" &
                           df_chunk_templates$fylke$.template_variable_type_indep %in% NA, 
                         ".template"] <-
  "::: {{#fig-{.chunk_name}}}

```{{r, fig.height = fig_height_h_barchart(n_y={.n_dep}, n_cats_y={.n_cats_dep}, max_chars_cats_y={.max_chars_dep})}}
{.obj_name} <- 
	data_{.chapter_foldername} |>
		makeme(dep = c({.variable_name_dep}), 
		type = 'cat_plot_html',
		data_label = 'count')
x <- stringi::stri_c('N = ', n_range2({.obj_name}))
#link <- make_link(data = {.obj_name}$data)
#link_plot <- make_link(data = {.obj_name}, file_suffix = '.png', link_prefix='[PNG](', save_fn = ggsaver)
#x <- I(paste0(c(x, link, link_plot), collapse=', '))
girafe(ggobj = {.obj_name})
```

_{.variable_label_prefix_dep}_. `{{r}} x`.

:::"

# df_chunk_templates$fylke[df_chunk_templates$fylke$.template_name %in% "cat_table_html" &
#                            df_chunk_templates$fylke$.template_variable_type_indep %in% "fct;ord", 
#                          ".template"] <-
#   "::: {{#tbl-{.chunk_name}}}
# 
# ```{{r}}
# {.obj_name} <- 
# 	data_{.chapter_foldername} |>
# 		makeme(dep = c({.variable_name_dep}),  
# 		indep = c({.variable_name_indep}), 
# 		type = 'cat_table_html',
# 		data_label = 'count')
# x <- stringi::stri_c('N = ', n_range(data = data_{.chapter_foldername}, 
# 	dep = c({.variable_name_dep}), 
# 	indep = c({.variable_name_indep})))
# #link <- make_link(data={.obj_name})
# #x <- I(paste0(c(x, link), collapse=', '))
# gt({.obj_name})
# ```
# 
# _{.variable_label_prefix_dep}_ etter _{tolower(.variable_label_prefix_indep)}_. `{{r}} x`.
# 
# :::"
# 
# df_chunk_templates$fylke[df_chunk_templates$fylke$.template_name %in% "cat_table_html" &
#                            df_chunk_templates$fylke$.template_variable_type_indep %in% NA, 
#                          ".template"] <-
#   "::: {{#tbl-{.chunk_name}}}
# 
# ```{{r}}
# {.obj_name} <- 
# 	data_{.chapter_foldername} |>
# 		makeme(dep = c({.variable_name_dep}),
# 		type = 'cat_table_html',
# 		data_label = 'count')
# x <- stringi::stri_c('N = ', n_range(data = data_{.chapter_foldername}, 
# 	dep = c({.variable_name_dep})))
# #link <- make_link(data={.obj_name})
# #x <- I(paste0(c(x, link), collapse=', '))
# gt({.obj_name})
# ```
# 
# _{.variable_label_prefix_dep}_. `{{r}} x`.
# 
# :::"

config_macro$fylke <- config_macro[[params$cycle]]

config_macro$fylke$data_label <- "count"
config_macro$fylke["always_show_bi_for_indep"] <- list(NULL)
config_macro$fylke$hide_bi_entry_if_sig_above <- 1
config_macro$fylke$title <- "Fylke - antall"
config_macro$fylke$log_file <- "draft_generation_log_fylke.txt"
# config_macro$fylke[["arrange_section_by"]] <- c(Kapittelnr = FALSE, config_macro$fylke[["arrange_section_by"]])


survey_data$fylke[[params$cycle]] <-
  qs::qread(file = here::here(paste0(paths$data$saros_ready[[params$cycle]], ".qs")),
            strict = TRUE, nthreads = 4) |>
  dplyr::filter(resp == "Skoleeier fylkeskommune") |>
  dplyr::mutate(resp = droplevels(resp)) |> 
  labelled::set_variable_labels(resp ="Respondenttype")

chapter_structure[[params$cycle]]$fylke <-
  rlang::inject(
  saros.base::refine_chapter_overview(
    chapter_overview = chapter_overview[[params$cycle]] |>
      dplyr::mutate(indep = ifelse(!is.na(indep), "landsdel_fylk", NA)),
    data = survey_data$fylke[[params$cycle]],
    chunk_templates = df_chunk_templates$fylke,
    !!!config_macro$fylke[c("always_show_bi_for_indep", "hide_chunk_if_n_below", "organize_by", "arrange_section_by")]
  )) |>
  left_join(chapter_overview[[params$cycle]]|> 
              select(chapter, chapter_ord)) |> 
  ungroup() |> 
  arrange(chapter_ord, .variable_label_prefix_dep, .variable_type_dep, .variable_label_prefix_indep, .template_name) |>
  mutate(chapter = forcats::fct_reorder(chapter, chapter_ord),
         .chapter_number = chapter_ord,
         chapter_number_text = sprintf(paste0("%0", 2, "d"),
                                       chapter_ord),
         chapter_foldername_clean = 
           saros.base:::filename_sanitizer(as.character(chapter),
                              max_chars = 24,
                              accept_hyphen = FALSE,
                              make_unique = FALSE),
         .chapter_foldername =
           stringi::stri_c(chapter_number_text, "_", chapter_foldername_clean,
                           ignore_null = TRUE)) |> 
  group_by(chapter, .variable_label_prefix_dep, .variable_type_dep, .variable_label_prefix_indep, .template_name)
  
saros.base::draft_report(
  data = survey_data$fylke[[params$cycle]],
  chapter_structure = chapter_structure[[params$cycle]]$fylke |> dplyr::filter(Kapittelnr == 10), #|> dplyr::filter(stringr::str_detect(as.character(.variable_name_dep), "^Q12|^Q189|^Q190|^Q191|^Q192")),
  !!!config_macro[[params$cycle]][stringr::str_detect(names(config_macro[[params$cycle]]), pattern="index|chapter|report|serialized|title|log_file|auxiliary_variables")],
  path = fs::path(paths$output[[params$cycle]], "Fylke_antall")
  )





