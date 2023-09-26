library(magrittr)


antall_fylk_pop <-
  hel_data %>% 
  dplyr::filter(kilder == "Populasjon", resp == "Skoleeier fylke") %>%
  nrow() %>% 
  {if(.==0) "ingen" else tall_til_ord(.)}

antall_fylk_gjennomført <- 
  hel_data %>% 
  dplyr::filter(kilder == "Utvalg", resp == "Skoleeier fylke", svarstatus %in% c("Gjennomført", "Noen svar - godkjent")) %>%
  nrow() %>% 
  {if(.==0) "ingen" else tall_til_ord(.)}


antall_fylk_noen_svar <-
  hel_data %>% 
  dplyr::filter(kilder == "Utvalg", resp == "Skoleeier fylke", 
                svarstatus %in% c("Noen svar - godkjent",
                                  "Noen svar - ikke godkjent")) %>%
  nrow() %>% 
  {if(.==0) "ingen" else tall_til_ord(.)}


antall_fylk_noen_svar_godkjent <-
  hel_data %>% 
  dplyr::filter(kilder == "Utvalg", resp == "Skoleeier fylke", 
                svarstatus == "Noen svar - godkjent") %>%
  nrow() %>% 
  {if(.==0) "ingen" else tall_til_ord(.)}


antall_fylk_noen_svar_ikke_godkjent <-
  hel_data %>% 
  dplyr::filter(kilder == "Utvalg", resp == "Skoleeier fylke", 
                svarstatus == "Noen svar - ikke godkjent") %>%
  nrow() %>% 
  {if(.==0) "ingen" else tall_til_ord(.)}

antall_fylk_ikke_svart <-
  hel_data %>% 
  dplyr::filter(kilder == "Utvalg", resp == "Skoleeier fylke", 
                svarstatus == "Ikke svart") %>%
  nrow() %>% 
  {if(.==0) "ingen" else tall_til_ord(.)}


antall_fylk_frafalt <-
  hel_data %>% 
  dplyr::filter(kilder == "Utvalg", resp == "Skoleeier fylke", 
                svarstatus == "Frafalt") %>%
  nrow() %>% 
  {if(.==0) "ingen" else tall_til_ord(.)}


#######
antall_gs <-
  hel_data %>% 
  dplyr::filter(kilder=="Populasjon", 
                resp=="Skoleleder grunnskole") %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}

antall_gs_utvalg <-
  hel_data %>% 
  dplyr::filter(kilder=="Utvalg", resp=="Skoleleder grunnskole") %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}

antall_gs_gjennomført <- 
  hel_data %>% 
  dplyr::filter(kilder=="Utvalg", 
                resp=="Skoleleder grunnskole", 
                svarstatus=="Gjennomført") %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}

antall_gs_noen_svar <-
  hel_data %>% 
  dplyr::filter(kilder=="Utvalg", 
                resp=="Skoleleder grunnskole", 
                svarstatus %in% c("Noen svar - godkjent", "Noen svar - ikke godkjent")) %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}

antall_gs_noen_svar_godkjent <-
  hel_data %>% 
  dplyr::filter(kilder=="Utvalg", 
                resp=="Skoleleder grunnskole", 
                svarstatus == "Noen svar - godkjent") %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}


antall_gs_noen_svar_ikke_godkjent <-
  hel_data %>% 
  dplyr::filter(kilder=="Utvalg", 
                resp=="Skoleleder grunnskole", 
                svarstatus == "Noen svar - ikke godkjent") %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}

antall_gs_ikke_svart <-
  hel_data %>% 
  dplyr::filter(kilder=="Utvalg", 
                resp=="Skoleleder grunnskole", 
                svarstatus == "Ikke svart") %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}


antall_gs_frafalt <-
  hel_data %>% 
  dplyr::filter(kilder=="Utvalg", 
                resp=="Skoleleder grunnskole", 
                svarstatus == "Frafalt") %>% 
  nrow() %>% 
  {if(.==0) "ingen" else if(.>20) . else tall_til_ord(.)}


########

svarprosent_gs <-
  tbl_2.4$`_data` %>% 
  dplyr::filter(` ` == "Svarprosent bruttoutvalg") %>% 
  dplyr::pull(`Skoleleder grunnskole`)
