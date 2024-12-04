################################################################################
# 1. In _fylke, rename datafiles to _fylke.qs and qmd-files to _fylke.qmd
################################################################################
## Gather and create paths
process_files <- 
  list(
    fylke_data =
      fs::dir_ls(path = fs::path(paths$output[[params$cycle]], "Fylke_antall"), 
                 recurse = TRUE, type = "file", regexp = "data_.+\\.qs|data_.+\\.rds") |>
      rlang::set_names() |>
      purrr::map_chr(~stringr::str_replace(.x, pattern = "(data_.+)(\\.qs)$", replacement = "\\1_fylke\\2")),
    fylke_qmd =
      fs::dir_ls(path = fs::path(paths$output[[params$cycle]], "Fylke_antall"), 
                 recurse = TRUE, type = "file", regexp = "[[:digit:]]+.*\\.qmd")  |>
      rlang::set_names() |> 
      purrr::map_chr(~stringr::str_replace(.x, pattern = "([[:digit:]]+.+)(\\.qmd)$", replacement = "\\1_fylke\\2")),
    main_qmd =
      fs::dir_ls(path = fs::path(paths$output[[params$cycle]]), 
                 recurse = FALSE, type = "file", regexp = "[[:digit:]]+.*\\.qmd"),
    chapter_drafting_folders =
      fs::dir_ls(path = paths$output[[params$cycle]], recurse = FALSE, type = "directory", regexp = paste0(paths$output[[params$cycle]], "/[0-9]+"))  |>
      purrr::map_chr(.f = ~stringr::str_c("Kap", .x))
  )
## Actually rename files to add _fylke to files in folder Fylke_antall
fs::file_move(names(process_files$fylke_data), unname(process_files$fylke_data))
fs::file_move(names(process_files$fylke_qmd), unname(process_files$fylke_qmd))

################################################################################
# 2. Replace data reading chunk in _fylke files with updated data file paths
################################################################################
fs::dir_ls(path = paths$output[[params$cycle]], recurse = TRUE, regexp = "_fylke\\.qmd") %>% 
  unname() %>% 
  purrr::walk(.f = ~{
    brio::read_lines(.x) %>% 
      stringr::str_replace_all(pattern = "(data_.+) <- qs::qread\\((.+)\\.qs'\\)",
                               replacement = "\\1_fylke <- qs::qread(\\2.qs')") %>% 
      stringr::str_replace_all(pattern = "(data_.+) <- readRDS\\((.+)\\.rds'\\)",
                               replacement = "\\1_fylke <- readRDS(\\2.rds')") %>% 
      brio::write_lines(path = .x)
  })

################################################################################
# 3. Insert data reading chunk to _fylke datafile in main qmd file
################################################################################
purrr::walk(unname(process_files$main_qmd), ~{
  brio::read_lines(.x) %>% 
    stringr::str_replace_all(pattern = "^(data_.+)( <- qs::qread\\(.+)(\\.qs'\\))$",
                             replacement = "\\1\\2\\3\n\\1_fylke\\2_fylke\\3") %>% 
    stringr::str_replace_all(pattern = "^(data_.+)( <- readRDS\\(.+)(\\.rds'\\))$",
                             replacement = "\\1\\2\\3\n\\1_fylke\\2_fylke\\3") %>% 
    brio::write_lines(path = .x)
})

################################################################################
# 4. Move _fylke-files to common folder?
################################################################################
fs::file_move(unname(process_files$fylke_data), 
              stringr::str_replace(unname(process_files$fylke_data), "Fylke_antall/", ""))
fs::file_move(unname(process_files$fylke_qmd), 
              stringr::str_replace(unname(process_files$fylke_qmd), "Fylke_antall/", ""))
## Delete empty folder Fylke_antall
fs::dir_delete(fs::path(paths$output[[params$cycle]], "Fylke_antall"))

################################################################################
# 5. Create Kap-folder for each chapter and move files/folders into them
################################################################################
fs::dir_ls(path = paths$output[[params$cycle]], recurse = FALSE, type = "directory", regexp = paste0("Rapporter/", params$cycle, "/[0-9]{2}_")) %>% 
  purrr::map_chr(~stringr::str_replace(.x, pattern = "/([0-9]{2}_)", "/Kap\\1")) %>% 
  unname() |>
  fs::dir_create()

## Make empty directories in these
fs::dir_ls(path = paths$output[[params$cycle]], recurse = FALSE, type = "directory", regexp = paste0("Rapporter/", params$cycle, "/Kap[0-9]{2}_")) %>% 
  purrr::walk(~{
    fs::dir_create(fs::path(.x, c("_1_til_udir", "_2_fra_udir", "_3_til_kvalitetssikrer", "_4_fra_kvalitetssikrer", "_5_ymse_utkast", "_6_html_til_utprøving")))
  })

## Move files into the Kap-folders (folders are experimental)
fs::dir_ls(path = paths$output[[params$cycle]], recurse = FALSE, 
           regexp = paste0("Rapporter/", params$cycle, "/Kap[0-9]{2}_")) %>% 
  purrr::walk(~{
    if(fs::is_file(.x)) {
      new_path <- 
        stringr::str_replace(.x, pattern = "(.+)/([0-9]{2}_.+)(\\.qmd)$", "\\1/Kap\\2/\\2\\3")
      fs::file_move(path = .x, new_path = new_path)
    }
    if(fs::is_dir(.x)) {
      new_path <- 
        stringr::str_replace(.x, pattern = "(.+)/([0-9]{2}_.+)$", "\\1/Kap\\2/\\2")
      fs::dir_copy(path = .x, new_path = new_path, overwrite = TRUE)
    }
  })



################################################################################
# 6. Set up Quarto extensions in each sub-folder for allowing single-file rendering
################################################################################
fs::dir_ls(path =  paths$output[[params$cycle]], recurse = FALSE, type = "directory", regexp = "/Kap[0-9]{2}_") %>% 
  c(., paths$site, paths$output[[params$cycle]]) %>% 
  unname() |>
  purrr::walk(.f =~{
    withr::with_dir(new = .x,
                    code = {
                      quarto::quarto_add_extension(extension = "NIFU-NO/nifutypst", no_prompt = TRUE, quiet = FALSE) # Leave on to get error messages
                      quarto::quarto_add_extension(extension = "NIFU-NO/nifudocx", no_prompt = TRUE, quiet = FALSE)
                      quarto::quarto_add_extension(extension = "NIFU-NO/rename_duplicate_labels", no_prompt = TRUE, quiet = FALSE)
                      quarto::quarto_add_extension(extension = "NIFU-NO/remove_empty_headings", no_prompt = TRUE, quiet = FALSE)
                    })
  })


################################################################################
# 7. Create general_formatting.R files in subdirectories which only run general_formatting.R in parent folder.
################################################################################
withr::with_dir(new = paths$site_drafts_completed, code = {
  fs::dir_ls(recurse = T, type = "directory", regexp = "_images|Tilbydere/|/[0-9]{1,2}_", invert = TRUE) %>%
    c(.) %>%
    fs::path(., "general_formatting.R") |>
    purrr::walk(.f = ~brio::write_lines(text='source("../general_formatting.R", chdir = TRUE)', path = .x))
}
)