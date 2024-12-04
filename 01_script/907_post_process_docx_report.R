#### Post-processing DOCX-files:
# Attach cover page, colofon and last pages
library(dplyr)
rapport <- 
  officer::read_docx(fs::path(paths$saros, "_extensions", "NIFU-NO", "nifudocx", "frontmatter.docx"))

for(file in fs::dir_ls(path = fs::path(paths$local_basepath, params$cycle), 
                       recurse = FALSE, type = "file", regexp = "\\.docx", 
                       fail = TRUE) %>% 
    grep(x = ., pattern = "report\\.docx",invert = TRUE, value = TRUE)) {
  rapport <-
    officer::body_add_docx(x = rapport, src = file, pos = "after")
}
# rapport <-
officer::body_add_docx(x = rapport, 
                       src = fs::path(paths$saros, "_extensions", "NIFU-NO", "nifudocx", "backmatter.docx"), 
                       pos = "after") %>% 
  # officer::read_docx(fs::path(paths$site, "_extensions", "NIFU-NO", "nifudocx", "frontmatter.docx")) %>% 
  #   officer::body_add_docx(src = fs::path(paths$site, "_site", "Rapporter", "2024V", "0_report_0-9.docx"), pos = "after") %>% 
  # officer::body_add_docx(src = fs::path(paths$site, "_site", "Rapporter", "2024V", "0_report_10-11.docx"), pos = "after") %>% 
  # officer::body_add_docx(src = fs::path(paths$site, "_extensions", "NIFU-NO", "nifudocx", "backmatter.docx"), pos = "after") %>% 
  officer::set_doc_properties(title = "Spørsmål til Skole-Norge",
                              creator = params$all_authors) %>% 
  officer::body_replace_all_text(old_value = "Undertittel", new_value = paste0("Analyser og resultater fra Utdanningsdirektoratets spørreundersøkelse til skoler og skoleeiere ", params$gjennomforing)) %>% 
  officer::body_replace_all_text(old_value = "Skriv inn oppdragsgiver", new_value = params$projectowner) %>% 
  officer::body_replace_all_text(old_value = "Skriv inn adresse til oppdragsgiver", new_value = params$projectowneraddress) %>% 
  officer::body_replace_all_text(old_value = "20xxx", new_value = params$projectnumber) %>% 
  print(target = fs::path(paths$local_basepath, params$cycle, paste0("report", Sys.Date(), ".docx")))


officer::docx_summary(report) %>% View()

officer::docx_bookmarks(report)
# Set all tables to autofit to window ####
# Ikke mulig så lenge Quarto lager tabell i tabell.
# Må vente på t fix eller ruke flextable?


# Remove (or replace base URL for?) all xlsx-hyperlinks
# All table column headers set in bold
# All tables set in a table style (or table auto style)
#   

