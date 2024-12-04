## Post-PISVEEP (rendering of website and report PDF)



### Render with Quarto
withr::with_envvar(new = c(LC_ALL="C"),
                   action = "replace",
                   code = system.time(
                     quarto::quarto_render(
                       input = if(is.null(paths$site)) cli::cli_abort("paths$site is NULL") else paths$site,
                       as_job = TRUE,
                       output_format = c("all")
                     )))


# Take a temporary backup in case post-processing fails
fs::dir_copy(path = paths$local_basepath,
             new_path = paste0(paths$local_basepath, "_orig_", Sys.Date()))

#### Post-processing HTML-files: replacing link to 0_report.pdf with index.html ####
saros.base::remove_element_from_sidebar(path = paths$local_basepath,
                                        filename_as_regex = c("report\\.pdf", "report\\.docx"))

