##################################################################################
#####   Moving to temproary ("scratch") folder to avoid OneDrive sync problems ###
##################################################################################
# Using a local "temporary" scratch-folder instead of cloud services because there will be
# many files generated and the cloud services tend to create mysterious errors

library(dplyr)

fs::dir_copy(path = fs::path(paths$saros, c("_extensions", "_freeze", "Main")),
             new_path = fs::path(paths$site, c("_extensions", "_freeze", "Main")),
             overwrite = TRUE)
fs::dir_copy(path = fs::path(paths$resources, "_images"),
             new_path = fs::path(paths$site,  "_images"),
             overwrite = TRUE)

# Report files
fs::dir_ls(path = paths$drafts_completed,
           recurse = TRUE, type = c("file"),
           regexp = "\\.qmd$|\\.R$|\\.qs$|\\.rds$|\\.png|\\.xlsx", invert = FALSE) |>
  stringr::str_subset(pattern = "/_", negate = TRUE) |>
  fs::file_copy(path = .,
                new_path = stringi::stri_replace_all_fixed(.,
                                                           pattern = paths$saros,
                                                           replacement = paths$site),
                overwrite = TRUE)
fs::dir_ls(path = paths$site_drafts_completed,
           recurse = FALSE, type = "directory",
           regexp = "/_", invert = TRUE) |>
  fs::dir_ls(type="directory", pattern = "/Kap")


### Quarto Project files
new_project_config_files <-
  fs::path(paths$site, c("_quarto.yaml",
                         "_global.yaml",
                         "styles.css",
                         "styles.scss",
                         "bib_style.csl",
                         "general_formatting.R",
                         "index.qmd",
                         "index.qmd"))
fs::file_copy(path =
                c(fs::path(paths$resources, "YAML", c("_quarto.yaml",
                                                       "_global.yaml")),
                  fs::path(paths$resources, "CSS", c("styles.css",
                                                      "styles.scss")),
                  fs::path(paths$resources, "CitationStyles", "bib_style.csl"),
                  fs::path(paths$saros, "general_formatting.R"),
                  fs::path(paths$saros, "index.qmd"),
                  fs::path(paths$site_drafts_completed, "index.qmd")
                ),
              new_path = new_project_config_files,
              overwrite = TRUE)

### Should set all of these as hidden to avoid the temptation of modifying these copies rather than the originals
if(Sys.info()[["sysname"]] == "Windows") {
  for(file in new_project_config_files |> stringr::str_subset(pattern = "index\\.qmd", negate = TRUE)) {
    system(paste("attrib +h", shQuote(file)))
  }
}

### Install extensions in scratch folder
# withr::with_dir(new = paths$site,
#                 code = {
#                   quarto::quarto_add_extension(extension = "NIFU-NO/nifutypst", no_prompt = TRUE, quiet = F)
#                   quarto::quarto_add_extension(extension = "NIFU-NO/nifudocx", no_prompt = TRUE, quiet = F)
#                   quarto::quarto_add_extension(extension = "NIFU-NO/rename_duplicate_labels", no_prompt = TRUE, quiet = F)
#                   quarto::quarto_add_extension(extension = "NIFU-NO/remove_empty_headings", no_prompt = TRUE, quiet = F)
#                 })


# Fjerner noen ting fra YAML-delen av kapitlene som var nødvendig under utkast-skriving,
# men som nå er uønsket fordi vi bare har PDF og DOCX versjoner av hele rapporten, ikke hver del
fs::dir_ls(path = paths$site, recurse = TRUE, type = "file", 
           regexp = "\\.qmd") |>
  grep(x = _, pattern = "/_|report\\.qmd|_arkiv|index\\.qmd|Main/|Kap", invert=TRUE, value=TRUE) |>
  purrr::walk(.f = ~{
    .x |>
      brio::read_lines() |>
      stringr::str_replace_all(pattern = "[[:space:]\n\r]+nifudocx-docx: default|[[:space:]\n\r]+nifu_pub-typst: default",
                               replacement = "") |>
      brio::write_lines(path = .x)
  })

