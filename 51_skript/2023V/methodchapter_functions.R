
tab_type_a <- function(data, 
                       y = "orgform", 
                       sampled_indicator = "kilder", 
                       valid_indicator = "godkjent_integer", 
                       population_string = "Populasjon\n%",
                       sampled_string = "Nettoutvalg\n%",
                       y_string = "Type") {
  data %>%
    dplyr::group_by(dplyr::pick(tidyselect::all_of(y))) %>%
    dplyr::summarize(population_string = sum(kilder, na.rm = TRUE),
                     sampled_string = sum(godkjent_integer, na.rm = TRUE)) %>%
    dplyr::mutate(population_string = round(population_string / sum(population_string)*100, digits = 1),
                  sampled_string = round(sampled_string / sum(sampled_string) *100, digits = 1)) %>%
    dplyr::ungroup() %>% 
    rlang::set_names(nm = c(y_string, population_string, sampled_string))
  
}




tab_type_b <- function(data, 
                       y = "fylke", 
                       sampled_indicator = "kilder", 
                       valid_indicator = "godkjent_integer", 
                       total_string="Totalt", 
                       population_string = "Populasjon", 
                       sample_string="Utvalg",
                       nresponses_string = "Antall svar", 
                       responsepercentage_string = "Svarprosent") {

  data2 <- data
  data2[[y]] <- forcats::fct_collapse(data2[[y]], other_level = total_string)
    dplyr::bind_rows(data, data2) %>% 
    dplyr::group_by(.data[[y]]) %>%
    dplyr::summarize(population_string = sum(!.data[[sampled_indicator]], na.rm = TRUE),
                     sample_string = sum(.data[[sampled_indicator]], na.rm = TRUE),
                     nresponses_string = sum(.data[[valid_indicator]], na.rm = TRUE),
                     responsepercentage_string = round(nresponses_string / sample_string * 100, digits = 1)) %>%
      rlang::set_names(nm = c(y, population_string, sample_string, nresponses_string, responsepercentage_string)) %>% 
      labelled::copy_labels_from(from = data) %>%
      saros::swap_label_colnames()
}

tab_type_c <- function(data, 
                       x = "sk4d",
                       y = "landsdel_gs", 
                       sampled_indicator = "kilder", 
                       valid_indicator = "godkjent_integer", 
                       total_string = "Totalt") {
  
  data[[x]] <- forcats::fct_drop(data[[x]])
  data[[y]] <- forcats::fct_drop(data[[y]])
  data_out <- data
  if(rlang::is_string(x) && any(colnames(data) == x)) {
    data2 <- data
    data2[[x]] <- forcats::fct_collapse(data2[[x]], other_level = total_string)
    data_out <- dplyr::bind_rows(data_out, data2) 
  }
  if(rlang::is_string(y) && any(colnames(data) == y)) {
    data2 <- data
    data2[[y]] <- forcats::fct_collapse(data2[[y]], other_level = total_string)
    data_out <- dplyr::bind_rows(data_out, data2) 
  }  

  if(rlang::is_string(x) && rlang::is_string(y) && all(c(x, y) %in% colnames(data))) {
    data2 <- data
    data2[[x]] <- forcats::fct_collapse(data2[[x]], other_level = total_string)
    data2[[y]] <- forcats::fct_collapse(data2[[y]], other_level = total_string)
    data_out <- dplyr::bind_rows(data_out, data2)
  }
  
  
  data_out %>% 
      dplyr::summarize(responsepercentage = ifelse(sum(.data[[sampled_indicator]], na.rm = TRUE) == 0, 0, 
                                          sum(.data[[valid_indicator]], na.rm = TRUE)/sum(.data[[sampled_indicator]], na.rm = TRUE)),
                     .by = tidyselect::all_of(c(y, x))) %>%
    dplyr::mutate(responsepercentage = round(responsepercentage*100, digits = 1)) %>% 
    dplyr::arrange(if(rlang::is_string(y)) as.integer(.data[[y]]), if(rlang::is_string(x)) as.integer(.data[[x]])) %>% 
    tidyr::pivot_wider(names_from = tidyselect::all_of(x), 
                       values_from = responsepercentage, names_expand = TRUE) %>%
    labelled::copy_labels_from(from = data) %>% 
    saros::swap_label_colnames()
}

tab_type_d <- function(data, x="landsdel_gs", y="sk4d",
                       sample_string = "Utvalg\n%", population_string = "Populasjon\n%",
                       sampled_indicator = "godkjent") {
  data[[y]] <- forcats::fct_drop(data[[y]])
  data[[x]] <- forcats::fct_drop(data[[x]])

  U <-
    data %>%
    dplyr::filter(dplyr::if_all(.cols = tidyselect::all_of(sampled_indicator), .fns=~.x)) %>%
    crosstable::crosstable(cols = tidyselect::all_of(x), by = tidyselect::all_of(y),
                           percent_pattern = list(body="{p_tot}",
                                                  total_row = "{p_col}",
                                                  total_col ="{p_row}",
                                                  total_all = "{p_col}"
                                                  ),
                           showNA = "no", percent_digits = 1, total = "both") %>%
    tidyr::pivot_longer(cols = -c(.id, label, variable), values_to = sample_string, names_to = y)
  
  P <- 
    data %>%
    crosstable::crosstable(cols = tidyselect::all_of(x), by = tidyselect::all_of(y),
                           percent_pattern = list(body="{p_tot}", total_row = "{p_col}",
                                                  total_col ="{p_row}", total_all = "{p_col}"),
                           showNA = "no", percent_digits = 1, total = "both") %>%
    tidyr::pivot_longer(cols = -tidyselect::all_of(c(".id", "label", "variable")), values_to = population_string, names_to = y)
  
  dplyr::inner_join(U, P, by = c(".id", "label", "variable", y)) %>%
    dplyr::mutate(variable = factor(variable, levels = unique(variable)), 
                  new_var = factor(.data[[y]], levels = unique(.data[[y]])),
                  dplyr::across(tidyselect::all_of(c(sample_string, population_string)), ~as.numeric(stringr::str_remove(.x, "%")))) %>% 
    dplyr::rename_with(.cols = "variable", function(x) .$label[1]) %>%
    dplyr::select(-tidyselect::all_of(c(".id", "label", y))) %>% 
    tidyr::pivot_wider(names_from = "new_var", 
                       values_from = tidyselect::all_of(c(sample_string, population_string)), 
                       names_glue = "{new_var}_{.value}", names_vary = "slowest")

}

tab_type_d2 <- function(data, x = "landsdel_gs", y = "sk4d",
                        sample_string = "Utvalg %", population_string = "Populasjon %",
                        sampled_indicator = "godkjent", totals = TRUE) {
  
  data[[y]] <- forcats::fct_drop(data[[y]])
  data[[x]] <- forcats::fct_drop(data[[x]])
  
  # Calculate percentages for sampled data
  U <- data %>%
    dplyr::filter(.data[[sampled_indicator]]) %>%
    dplyr::count(.data[[x]], .data[[y]], name = "count") %>%
    dplyr::mutate("{sample_string}" := count / sum(count) * 100, count = NULL) %>% 
    tidyr::pivot_longer(cols = tidyselect::all_of(sample_string), names_to = NULL, values_to = sample_string)
  # browser()
  if(totals) {
    # Row totals
    U_row_totals <- 
      data %>%
      dplyr::filter(.data[[sampled_indicator]]) %>%
      dplyr::count(.data[[y]],  name = "count") %>%
      dplyr::mutate("{sample_string}" := count / sum(count) * 100, count = NULL,
                    "{x}" := "Totalt")
    
    # Column totals
    U_col_totals <- 
      data %>%
      dplyr::filter(.data[[sampled_indicator]]) %>%
      dplyr::count(.data[[x]],  name = "count") %>%
      dplyr::mutate("{sample_string}" := count / sum(count) * 100, count = NULL,
                    "{y}" := "Totalt")
    
    # Overall total
    U_overall_total <- 
      data %>%
      dplyr::filter(.data[[sampled_indicator]]) %>%
      dplyr::count(name = "count") %>%
      dplyr::mutate("{sample_string}" := count / sum(count) * 100, count = NULL,
                    "{y}" := "Totalt", "{x}" := "Totalt")
    
    U <- dplyr::bind_rows(U, U_row_totals, U_col_totals, U_overall_total)
  }
  # Calculate percentages for the whole population
  P <- data %>%
    dplyr::count(.data[[x]], .data[[y]], name = "count") %>%
    dplyr::mutate("{population_string}" := count / sum(count) * 100, count = NULL) %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(population_string), names_to = NULL, values_to = population_string)
  
  if(totals) {
    # Row totals
    P_row_totals <- 
      data %>%
      dplyr::count(.data[[y]],  name = "count") %>%
      dplyr::mutate("{population_string}" := count / sum(count) * 100, count = NULL,
                    "{x}" := "Totalt")
    
    # Column totals
    P_col_totals <- 
      data %>%
      dplyr::count(.data[[x]],  name = "count") %>%
      dplyr::mutate("{population_string}" := count / sum(count) * 100, count = NULL,
                    "{y}" := "Totalt")
    
    # Overall total
    P_overall_total <- 
      data %>%
      dplyr::count(name = "count") %>%
      dplyr::mutate("{population_string}" := count / sum(count) * 100, count = NULL,
                    "{y}" := "Totalt", "{x}" := "Totalt")
    
    P <- dplyr::bind_rows(P, P_row_totals, P_col_totals, P_overall_total)
  }
  
  recombine <- function(x) {
    unlist(lapply(x, function(i) {
      paste0(stringr::str_split_1(i, "_")[2:1], collapse="_")
    }))
  }
  
  # Join the sampled and population data
  result <-
    dplyr::inner_join(U, P, by = c(x, y)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c(sample_string, population_string)), ~round(.x, 1))) %>% 
    tidyr::pivot_wider(names_from = tidyselect::all_of(y), 
                       values_from = tidyselect::all_of(c(sample_string, population_string)),
                       names_vary = "slowest") %>% 
    dplyr::rename_with(.cols = -1, .fn = ~recombine(.x))

  attr(result[[x]], "label") <- attr(data[[x]], "label")

  return(result)
}





tall_til_ord <- function(x, cap = FALSE, mellomrom = FALSE, and = TRUE) {
  if (!is.numeric(x)) stop('The input is not numeric.')
  if (any(abs(x) >= 1e15)) stop('The absolute value must be less than 1e15.')
  opts = options(scipen = 15, OutDec = '.')  # avoid scientific notation
  on.exit(options(opts), add = TRUE)
  
  zero_to_19 = c(
    'null', 'en', 'to', 'tre', 'fire', 'fem', 'seks', 'syv', 'åtte', 'ni', 'ti',
    'elleve', 'tolv', paste0(c('tre', 'fjor', 'fem', 'seks', 'syt', 'at', 'nit'), 'ten')
  )
  names(zero_to_19) = as.character(0:19)
  tens = c('tjue', 'tretti', 'førti', 'femti', 'seksti', 'sytti', 'åtti', 'nitti')
  names(tens) = as.character(seq(20, 90, 10))
  marks = c('', 'tusen,', 'million,', 'milliard,', 'trillion,')
  
  convert_1 = function(x_c) zero_to_19[x_c]  # 0 - 9
  
  # 10 - 99
  convert_2 = function(x_c) {
    x_cs = strsplit(x_c, split = '')[[1]]
    if (x_cs[1] == 1) return(zero_to_19[x_c])  # 10 - 19
    if (x_cs[2] == 0) return(tens[x_c])  # 20, 30, 40, ...
    # 21, 22, etc.
    paste(tens[as.integer(x_cs[1]) - 1], convert_1(x_cs[2]), sep = if (mellomrom) ' ' else '')
  }
  
  # 100 - 999
  convert_3 = function(x_c) {
    x_cs = strsplit(x_c, split = '')[[1]]
    n_hundreds = paste(if(x_cs[1]==1) "ett" else convert_1(x_cs[1]), 
                       'hundre', sep = ' ')
    out = if (x_cs[2] == '0') {
      if (x_cs[3] == '0') return(n_hundreds)  # x00
      convert_1(x_cs[3])  # x0x
    } else {
      convert_2(paste(x_cs[2:3], collapse = ''))  # xxx
    }
    paste(n_hundreds, out, sep = if (and) ' og ' else ' ')
  }
  
  convert_le3 = function(x_c) {
    x_c = gsub('^0+', '', x_c) # avoid something like 000, 001, 010; but also remove 0
    n = nchar(x_c)
    if (n == 0) return('')
    if (n == 1) return(convert_1(x_c))
    if (n == 2) return(convert_2(x_c))
    if (n == 3) return(convert_3(x_c))
  }
  
  convert_one = function(x) {
    minus = if (x >= 0) '' else {
      x = abs(x); 'minus '
    }
    if (x == 0) {
      out = 'null'  # because convert_le3 removed all 0s
    } else {
      x_marks = strsplit(format(floor(x), big.mark = ','), split = ',')[[1]]  # e.g. 123,456,789
      out = vapply(x_marks, convert_le3, character(1))  # group by 3 digits
      x_marks2 = marks[length(x_marks):1]  # units?
      x_marks2[which(out == '')] = ''  # e.g. 4,000,123, 000, remove millions
      out = paste(out, x_marks2, sep = ' ', collapse = ' ')  # zip together
    }
    out = paste0(minus, out)
    out = gsub('^ *|,? *$', '', out)  # trim heading/trailing space
    out = gsub(' {2,}', ' ', out)  # remove multiple spaces
    if (cap) out = sub('^([a-z])', '\\U\\1', out, perl = TRUE)
    if (x - floor(x) > 0) {
      frac = sub('^[0-9]+[.]', '', as.character(x))
      frac = convert_1(strsplit(frac, '')[[1]])
      out = paste(c(out, 'komma', frac), collapse = ' ')
    }
    out
  }
  
  if(length(x) > 1) vapply(x, convert_one, character(1)) else convert_one(x)
}

ranger <- function(data, number_col, name_col, n=NA, and_str = " og ") {
  # Check if the input data is a data frame
  if (!is.data.frame(data)) stop("data must be a data frame")
  
  # Check if number_col and name_col are present in the data
  if (!(number_col %in% names(data))) stop(paste(number_col, "is not present in the data"))
  if (!(name_col %in% names(data))) stop(paste(name_col, "is not present in the data"))
  
  # Check if n is a positive integer or NA
  if (!is.na(n) && (!is.numeric(n) || n <= 0 || (n %% 1 != 0))) stop("n must be a positive integer or NA")
  
  # If n is NA, select entries outside 1 SD of the mean
  if (is.na(n)) {
    mean_val <- mean(data[[number_col]], na.rm = TRUE)
    sd_val <- sd(data[[number_col]], na.rm = TRUE)
    selected_data <- subset(data, data[[number_col]] < mean_val - sd_val | data[[number_col]] > mean_val + sd_val)
  } else {
    selected_data <- data
  }
  
  # Rank the entries based on number_col
  selected_data <- selected_data[order(selected_data[[number_col]], decreasing = TRUE), ]
  
  # Define a helper function to concatenate entries with commas and the user-defined "and" string
  concatenate_entries <- function(entries, and_str) {
    if (length(entries) == 1) return(entries)
    paste(c(paste(head(entries, -1), collapse = ", "), tail(entries, 1)), collapse = and_str)
  }
  
  # Extract the top n and bottom n entries in name_col and concatenate them
  if (!is.na(n)) {
    top_n <- concatenate_entries(head(selected_data[[name_col]], n), and_str)
    bottom_n <- concatenate_entries(tail(selected_data[[name_col]], n), and_str)
    top_values <- concatenate_entries(head(selected_data[[number_col]], n), and_str)
    bottom_values <- concatenate_entries(tail(selected_data[[number_col]], n), and_str)
  } else {
    top_n <- concatenate_entries(selected_data[[name_col]], and_str)
    bottom_n <- NA
    top_values <- concatenate_entries(selected_data[[number_col]], and_str)
    bottom_values <- NA
  }
  
  # Return the concatenated top n and bottom n entries
  return(c(høy = top_n, høy_verdi = top_values,
           lav = bottom_n, lav_verdi = bottom_values))
}


tell_og_konverter <- function(kilder, resp, svarstatus = NULL, data = hel_data, threshold = 20) {
  filtered_data <- dplyr::filter(data, kilder == kilder, resp == resp)
  
  if(!is.null(svarstatus)) {
    filtered_data <- dplyr::filter(filtered_data, .data$svarstatus %in% svarstatus)
  }
  
  count <- nrow(filtered_data)
  text <- if(count == 0) "ingen" else if(count > threshold) as.character(count) else tall_til_ord(count)
  
  return(list(n = count, txt = text))
}


tell_og_konverter_mer = function(resp) {
  x <-
    list(
      pop = tell_og_konverter("Populasjon", unname(resp)),
      utvalg = tell_og_konverter("Utvalg", unname(resp)),
      gjennomført = tell_og_konverter("Utvalg", unname(resp), "Gjennomført"),
      noen_svar = tell_og_konverter("Utvalg", unname(resp), c("Noen svar - godkjent", "Noen svar - ikke godkjent")),
      noen_svar_godkjent = tell_og_konverter("Utvalg", unname(resp), "Noen svar - godkjent"),
      noen_svar_ikke_godkjent = tell_og_konverter("Utvalg", unname(resp), "Noen svar - ikke godkjent"),
      godkjent = tell_og_konverter("Utvalg", unname(resp), c("Gjennomført", "Noen svar - godkjent")),
      ikke_godkjent = tell_og_konverter("Utvalg", unname(resp), c("Ikke svart", "Noen svar - ikke godkjent")),
      ikke_svart = tell_og_konverter("Utvalg", unname(resp), "Ikke svart"),
      frafalt = tell_og_konverter("Utvalg", unname(resp), "Frafalt")
    )
  setNames(x, paste0(names(resp), "_", names(x)))
}