## Post-PISVEEP (rendering of website)

# Kompiler nettstedet og all output. Alternativ 1: Lagre lokalt i en midlertidig (temp)-mappe.
# Fordeler:
#   - Viktigst: UNNGÅR SHAREPOINT/ONEDRIVE BEGRENSNING PÅ LENGDE PÅ FILBANER.
#   - Nettstedets filer (inkl Word-versjon, PDF-versjon, etc) er i seg selv ikke viktige - de kan kompileres på nytt. Trenger derfor ikke å lagres i skyen/Sharepoint
#   - Filene kan til sammen utgjøre mange MB og de trengs ikke etter at de er lagt ut på serveren.
# Ulemper:
#   - Litt vanskeligere å finne disse filene (men se nedenfor for funksjoner/skript)

render_website <- function(site_dir = paths$site,
                           drafts_completed_dir = paths$drafts_completed,
                           resources_dir = paths$resources,
                           site_drafts_completed_dir = paths$site_drafts_completed,
                           site_resources_dir = paths$site_resources,
                           quarto_yaml_path = fs::path(paths$resources, "YAML", "_quarto.yaml"),
                           netlify = TRUE,
                           global_username = "admin", 
                           global_password = "arturead",
                           cycle = params$cycle, 
                           response_group = params$response_group,
                           prompt = TRUE,
                           ...) {
  dots <- rlang::list2(...)
  # Check that drafts_completed_dir contains any subfolder Rapporter and files
  if(!fs::dir_exists(fs::path(drafts_completed_dir, "Rapporter", response_group, cycle))) cli::cli_warn("Nothing to render?")
  cli::cli_inform("Ensure your site_dir is not on a Sharepoint or OneDrive - it will likely fail due to long paths.")
  
  if(prompt) answer <- utils::menu(choices = c("Y", "N"), title = "Sure you want to overwrite the local site folder site_dir?")
  if(prompt && answer=="N") return()
  quarto_yaml <- yaml::read_yaml(quarto_yaml_path)
  if(isFALSE(quarto_yaml$execute$cache) && 
     isFALSE(quarto_yaml$execute$freeze)) {
    tryCatch(fs::dir_delete(site_dir), error=function(e) cli::cli_warn(e))
  }
  
  
  fs::dir_copy(path = c(drafts_completed_dir, resources_dir),
               new_path = c(site_drafts_completed_dir, site_resources_dir),
               overwrite = TRUE)
  fs::file_copy(c(quarto_yaml_path,
                  fs::path(resources_dir, "YAML", "_nifu_global.yaml")),
                c(fs::path(site_drafts_completed_dir, "_quarto.yaml"),
                  fs::path(site_drafts_completed_dir, "_nifu_global.yaml")),
                overwrite = TRUE)
  if(isTRUE(netlify)) {
    if(fs::file_exists(fs::path(resources_dir, "YAML", "_publish.yml"))) {
      fs::file_copy(fs::path(resources_dir, "YAML", "_publish.yml"),
                    fs::path(site_drafts_completed_dir, "_publish.yml"),
                    overwrite = TRUE)
    } else cli::cli_warn("File not found: {.file {fs::path(resources_dir, 'YAML', '_publish.yml')}}")
  }
  browseURL(site_dir) # Check that structure is correct
  ## Takes about 6328 sec (1 hr 45 min) for macro + mesos, without cache or freeze, only html
  ## Takes about 20197 sec (5 hr 38 min) for macro + mesos, with cache and freeze, all outputs?
  system.time(
    withr::with_envvar(
      new = c(LC_ALL = "C"),
      code = rlang::exec(quarto::quarto_render,
                         input = site_drafts_completed_dir,
                         execute_dir = getwd(),
                         as_job = FALSE, 
                         output_format = c("html", "docx"), # PDF not working yet
                         !!!dots))) 
  outpath <- fs::path(site_dir, "_site", "index.html")
  browseURL(outpath)
  # Logg inn på Netlify og kopier følgende mappe til riktig prosjekt der.
  
  if(fs::dir_exists(fs::path(site_dir, drafts_completed_dir, "_site", "Rapporter", response_group, cycle, "mesos"))) {
    saros::create__headers_file(site_path = fs::path(site_dir, drafts_completed_dir, "_site"),
                                mesos_paths = fs::path_dir(stringr::str_replace(output_files, paths$drafts_produced, "")),
                                mesos_usernames = if(exists("output_files")) stringr::str_extract(output_files, "mesos/(.*)/index.qmd", group = 1),
                                mesos_passwords = if(exists("output_files")) stringr::str_extract(output_files, "mesos/(.*)/index.qmd", group = 1),
                                global_username = global_username, 
                                global_password = global_password)
  }

  site_dir
}

render_website(prompt = FALSE)

# Kompiler nettstedet og all output. Alternativ 2
# Følgende laster opp råfilene til Github som så kompilerer og dytter nettstedet til Netlify
