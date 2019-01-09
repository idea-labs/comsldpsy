#' Flextable font settings
#'
#' @export
get_font <- function() {
  font <- list()
  font[["normal"]] <- officer::fp_text(
    font.size = 12,
    font.family = "Times New Roman"
  )
  font[["superscript"]] <- update(font$normal, vertical.align = "superscript")
  font[["italic"]] <- update(font$normal, italic = TRUE)

  font
}

# # font settings
# text_normal <- officer::fp_text(
#   font.size = 12,
#   font.family = "Times New Roman"
# )
# text_superscript <- update(text_normal, vertical.align = "superscript")
# text_italic <- update(text_normal, italic = TRUE)

#' Flextable theme to mimic APA style
#'
#' @export
theme_apa <- function(ft) {
  ft %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::align(align = "left", part = "all") %>%
    flextable::rotate(rotation = "lrtb", align = "top", part = "header") %>%
    flextable::rotate(rotation = "lrtb", align = "top", part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(
      border = officer::fp_border(width = 2),
      part = "all"
    ) %>%
    flextable::hline_bottom(
      border = officer::fp_border(width = 2),
      part = "all"
    ) %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 12, part = "all") %>%
    flextable::autofit()
}

#' Table 1
#'
#' Description and representativeness of the sample.
#'
#' @param data tbl. The filtered data frame
#' @return `flextable` object. The table
#' @export
add_table_1 <- function(data){
  c(n_grade3, perc_grade3) %<-% get_n_perc_filter(
    data,
    "grade == '3. Klasse'"
  )
  c(n_grade4, perc_grade4) %<-% get_n_perc_filter(
    data,
    "grade == '4. Klasse'"
  )
  c(n_male, perc_male) %<-% get_n_perc_filter(
    data,
    "gender == 'male'"
  )
  c(n_female, perc_female) %<-% get_n_perc_filter(
    data,
    "gender == 'female'"
  )
  c(n_hesse, perc_hesse) %<-% get_n_perc_filter(
    data,
    "land == 'Hessen'"
  )
  c(n_bavaria, perc_bavaria) %<-% get_n_perc_filter(
    data,
    "land == 'Bayern'"
  )
  c(n_german, perc_german) %<-% get_n_perc_filter(
    data,
    "nationality == 'German'"
  )
  c(n_nongerman, perc_nongerman) %<-% get_n_perc_filter(
    data,
    "nationality == 'non-German'"
  )
  c(n_haupts, perc_haupts) %<-% get_n_perc_filter(
    data,
    "education_mother == 'kein/Hauptschulabschluss'"
  )
  c(n_reals, perc_reals) %<-% get_n_perc_filter(
    data,
    "education_mother == 'Mittlere Reife'"
  )
  c(n_abitur, perc_abitur) %<-% get_n_perc_filter(
    data,
    "education_mother == 'Abitur'"
  )

  tibble::tribble(
    ~A, ~B, ~C, ~D, ~E,
    "gender", "", "", "", "",
    "male", "", stringr::str_c(n_male, " (", perc_male, "%)"), "", "",
    "female", "", stringr::str_c(n_female, " (", perc_female, "%)"), "", "",
    "grade", "", "", "", "",
    "3", "rd", stringr::str_c(n_grade3, " (", perc_grade3, "%)"), "", "",
    "4", "th", stringr::str_c(n_grade4, " (", perc_grade4, "%)"), "", "",
    "state", "", "", "", "",
    "Hesse", "", stringr::str_c(n_hesse, " (", perc_hesse, "%)"), "", "",
    "Bavaria", "", stringr::str_c(n_bavaria, " (", perc_bavaria, "%)"), "", "",
    "mothers' education", "", "", "", "",
    "no degree/Hauptschule", "1",
      stringr::str_c(n_haupts, " (", perc_haupts, "%)"),
      "23.5", "Federal Office of Statistics, 2016",
    "Realschule", "2", stringr::str_c(n_reals, " (", perc_reals, "%)"),
      "34.3", "",
    "Gymnasium", "3", stringr::str_c(n_abitur, " (", perc_abitur, "%)"),
      "42.9", "",
    "nationality", "", "", "", "",
    "German", "", stringr::str_c(n_german, " (", perc_german, "%)"),
      "89.5", "Federal Office of Statistics, 2017a (p. 41), 2017b (p. 62)",
    "non-German", "", stringr::str_c(n_nongerman, " (", perc_nongerman, "%)"),
      "10.5", ""
  ) %>%
    flextable::flextable(col_keys = c("dummy_col", "C", "D", "E")) %>%
    flextable::display(
      col_key = "dummy_col",
      pattern = "{{variable_}}{{superscript_}}",
      formatters = list(variable_ ~ A, superscript_ ~ B),
      fprops = list(superscript_ = update(get_font()$superscript, italic = TRUE))
    ) %>%
    flextable::set_header_labels(
      dummy_col = "variable",
      C = "N (%) present study",
      D = "% reference value (if applicable)",
      E = "source"
    ) %>%
    theme_apa() %>%
    flextable::padding(
      i = c(2:3, 5:6, 8:9, 11:13, 15:16), j = 1, padding.left = 10
    ) %>%
    flextable::italic(i = c(2:3, 5:6, 8:9, 11:13, 15:16), j = 1) %>%
    flextable::merge_at(i = 11:13, j = 4) %>%
    flextable::merge_at(i = 15:16, j = 4) %>%
    flextable::width(j = 3, width = 1.3) %>%
    flextable::width(j = 4, width = 1.9)
}

#' Table 2
#'
#' Numbers and percentages of children with different types of SLD
#'   and average test scores.
#'
#' @inherit add_table_1
#' @export
add_table_2 <- function(data){
  data %>%
    dplyr::filter(!is.na(dsm5_cutoff_35)) %>%
    dplyr::group_by(dsm5_cutoff_35) %>%
    dplyr::summarize(
      freq = n(),
      perc = round(freq / nrow(data) * 100, 1),
      cft_mean = mean(cft_iq_own_kl),
      cft_sd = sd(cft_iq_own_kl),
      wrt_mean = mean(wrt_t_own),
      wrt_sd = sd(wrt_t_own),
      wllp_mean = mean(wllp_t_own),
      wllp_sd = sd(wllp_t_own),
      cody_mean = mean(cody_t_own),
      cody_sd = sd(cody_t_own)
    ) %>%
    # round all double to 1 decimal point
    dplyr::mutate_if(
      is.double,
      dplyr::funs(as.character(round(., 1)))
    ) %>%
    # combine freq and perc column
    dplyr::mutate(
      freq = stringr::str_c(as.character(freq), " (", perc, "%)")
    ) %>%
    dplyr::select(-perc) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      dsm5_cutoff_35 = "SLD group",
      freq = "freq. [%]",
      cft_mean = "CFT",
      wrt_mean = "WRT",
      wllp_mean = "WLLP-R",
      cody_mean = "CODY"
    ) %>%
    flextable::add_header(
      cft_mean = "(intelligence)",
      wrt_mean = "(spelling)",
      wllp_mean = "(reading)",
      cody_mean = "(arithmetic)",
      top = FALSE
    ) %>%
    flextable::add_header(
      cft_mean = "[IQ]",
      wrt_mean = "[T-score]",
      wllp_mean = "[T-score]",
      cody_mean = "[T-score]",
      top = FALSE
    ) %>%
    flextable::add_header(
      cft_mean = "M",
      cft_sd = "SD",
      wrt_mean = "M",
      wrt_sd = "SD",
      wllp_mean = "M",
      wllp_sd = "SD",
      cody_mean = "M",
      cody_sd = "SD",
      top = FALSE
    ) %>%
    flextable::merge_at(j = 3:4, i = 1, part = "header") %>%
    flextable::merge_at(j = 3:4, i = 2, part = "header") %>%
    flextable::merge_at(j = 3:4, i = 3, part = "header") %>%
    flextable::merge_at(j = 5:6, i = 1, part = "header") %>%
    flextable::merge_at(j = 5:6, i = 2, part = "header") %>%
    flextable::merge_at(j = 5:6, i = 3, part = "header") %>%
    flextable::merge_at(j = 7:8, i = 1, part = "header") %>%
    flextable::merge_at(j = 7:8, i = 2, part = "header") %>%
    flextable::merge_at(j = 7:8, i = 3, part = "header") %>%
    flextable::merge_at(j = 9:10, i = 1, part = "header") %>%
    flextable::merge_at(j = 9:10, i = 2, part = "header") %>%
    flextable::merge_at(j = 9:10, i = 3, part = "header") %>%
    flextable::padding(
      i = 1:2, j = 1:10, padding.bottom = 0, part = "header"
    ) %>%
    flextable::padding(
      i = 2:3, j = 1:10, padding.top = 0, part = "header"
    ) %>%
    flextable::padding(
      i = 3:4, j = 1:10, padding.top = 0, part = "header"
    ) %>%
    theme_apa() %>%
    flextable::align(j = 2:10, align = "right", part = "all") %>%
    flextable::align(j = 3:10, align = "center", part = "header") %>%
    flextable::width(j = 1, width = 2) %>%
    flextable::width(j = 2, width = 1) %>%
    flextable::width(j = 3:4, width = 0.5) %>%
    flextable::width(j = 5:8, width = 0.4) %>%
    flextable::width(j = 9:10, width = 0.5)
}

# helper function for add_table_3
sld_summary_onevar <- function(data, var, group_var) {
  var <- rlang::enquo(var)
  group_var <- rlang::enquo(group_var)

  data <- data %>%
    dplyr::filter(!is.na(!!group_var) & !is.na(!!var)) %>%
    dplyr::group_by(!!var, !!group_var) %>%
    dplyr::summarize(freq = n()) %>%
    dplyr::mutate(perc = round(freq / sum(freq) * 100, 1)) %>%
    dplyr::mutate_all(dplyr::funs(as.character(.))) %>%
    dplyr::mutate(perc = stringr::str_c("[", perc, "%]")) %>%
    dplyr::ungroup()

  df_freq <- data %>%
    dplyr::select(-perc) %>%
    tidyr::spread(!!group_var, freq) %>%
    dplyr::mutate(type = "freq") %>%
    tibble::rownames_to_column()

  df_perc <- data %>%
    dplyr::select(-freq) %>%
    tidyr::spread(!!group_var, perc) %>%
    dplyr::mutate(type = "perc") %>%
    tibble::rownames_to_column()

  dplyr::full_join(
    df_freq, df_perc,
    by = names(df_freq)
  )  %>%
    dplyr::arrange(rowname) %>%
    janitor::clean_names() %>%
    dplyr::select(-rowname)
}

# helper function for add_table_3
disorder_rows <- function(data, disorder_var, disorder_string){
  disorder_var <- rlang::enquo(disorder_var)

  row_names <- c(
    "group",
    stringr::str_c(disorder_string, "_yes"),
    stringr::str_c(disorder_string, "_no")
  )

  dsm5_35 <- data %>%
    sld_summary_onevar(dsm5_cutoff_35, !!disorder_var)

  dsm5_35_male <- data %>%
    dplyr::filter(gender == "male") %>%
    sld_summary_onevar(dsm5_cutoff_35, !!disorder_var) %>%
    dplyr::mutate_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(ifelse(is.na(.) & type == "freq", "0", .))
    ) %>%
    dplyr::rename_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(stringr::str_c(., "_male"))
    ) %>%
    dplyr::select(-type)

  dsm5_35_female <- data %>%
    dplyr::filter(gender == "female") %>%
    sld_summary_onevar(dsm5_cutoff_35, !!disorder_var) %>%
    dplyr::mutate_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(ifelse(is.na(.) & type == "freq", "0", .))
    ) %>%
    dplyr::rename_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(stringr::str_c(., "_female"))
    ) %>%
    dplyr::select(-type)

  dsm5_35 %<>%
    dplyr::bind_cols(dsm5_35_male) %>%
    dplyr::bind_cols(dsm5_35_female) %>%
    dplyr::rename(group = dsm5_cutoff_35) %>%
    dplyr::mutate(group = as.character(group))

  dsm5_35_01 <- data %>%
    sld_summary_onevar(dsm5_cutoff_35_01, !!disorder_var) %>%
    dplyr::slice(3:4)

  dsm5_35_01_male <- data %>%
    dplyr::filter(gender == "male") %>%
    sld_summary_onevar(dsm5_cutoff_35_01, !!disorder_var) %>%
    dplyr::slice(3:4) %>%
    dplyr::mutate_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(ifelse(is.na(.) & type == "freq", "0", .))
    ) %>%
    dplyr::rename_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(stringr::str_c(., "_male"))
    ) %>%
    dplyr::select(-type)

  dsm5_35_01_female <- data %>%
    dplyr::filter(gender == "female") %>%
    sld_summary_onevar(dsm5_cutoff_35_01, !!disorder_var) %>%
    dplyr::slice(3:4) %>%
    dplyr::mutate_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(ifelse(is.na(.) & type == "freq", "0", .))
    ) %>%
    dplyr::rename_at(
      dplyr::vars(indication_of_problems, no_problems),
      dplyr::funs(stringr::str_c(., "_female"))
    ) %>%
    dplyr::select(-type)

  dsm5_35_01 %<>%
    dplyr::bind_cols(dsm5_35_01_male) %>%
    dplyr::bind_cols(dsm5_35_01_female) %>%
    dplyr::rename(group = dsm5_cutoff_35_01) %>%
    dplyr::mutate(group = as.character(group))

  dplyr::bind_rows(
    dsm5_35,
    dsm5_35_01
  ) %>%
    dplyr::mutate(
      indication_of_problems = ifelse(
        type == "freq",
        stringr::str_c(
          indication_of_problems, " (",
          indication_of_problems_male, "/",
          indication_of_problems_female, ")"
        ),
        indication_of_problems
      ),
      no_problems = ifelse(
        type == "freq",
        stringr::str_c(
          no_problems, " (",
          no_problems_male, "/",
          no_problems_female, ")"
        ),
        no_problems
      ),
      group = ifelse(
        group == "indication of problems",
        "Total SLD (any disorder)",
        group
      )
    ) %>%
    dplyr::select(1:3) %>%
    stats::setNames(row_names)
}

#' Table 3
#'
#' Numbers and percentages of children with psychopathology for
#'   different types of SLD.
#'
#' @inherit add_table_1
#' @export
add_table_3 <- function(data){
  dplyr::bind_cols(
    disorder_rows(data, adhs_z_cat, "adhd"),
    disorder_rows(data, sca_e_z_cat, "anxiety"),
    disorder_rows(data, ssv_z_cat, "conduct"),
    disorder_rows(data, des_z_cat, "depression")
  ) %>%
    dplyr::select(-dplyr::matches("group[1-9]$")) %>%
    flextable::flextable() %>%
    flextable::set_header_labels(
      group = "",
      anxiety_yes = "yes",
      anxiety_no = "no",
      depression_yes = "yes",
      depression_no = "no",
      adhd_yes = "yes",
      adhd_no = "no",
      conduct_yes = "yes",
      conduct_no = "no"
    ) %>%
    flextable::add_header(
      group = "",
      anxiety_yes = "[%]",
      depression_yes = "[%]",
      adhd_yes = "[%]",
      conduct_yes = "[%]"
    ) %>%
    flextable::add_header(
      group = "",
      anxiety_yes = "freq. (male/female)",
      depression_yes = "freq. (male/female)",
      adhd_yes = "freq. (male/female)",
      conduct_yes = "freq. (male/female)"
    ) %>%
    flextable::add_header(
      group = "SLD group",
      anxiety_yes = "anxiety",
      depression_yes = "depression",
      adhd_yes = "ADHD",
      conduct_yes = "conduct problems"
    ) %>%
    flextable::merge_at(j = 2:3, i = 1, part = "header") %>%
    flextable::merge_at(j = 2:3, i = 2, part = "header") %>%
    flextable::merge_at(j = 2:3, i = 3, part = "header") %>%
    flextable::merge_at(j = 4:5, i = 1, part = "header") %>%
    flextable::merge_at(j = 4:5, i = 2, part = "header") %>%
    flextable::merge_at(j = 4:5, i = 3, part = "header") %>%
    flextable::merge_at(j = 6:7, i = 1, part = "header") %>%
    flextable::merge_at(j = 6:7, i = 2, part = "header") %>%
    flextable::merge_at(j = 6:7, i = 3, part = "header") %>%
    flextable::merge_at(j = 8:9, i = 1, part = "header") %>%
    flextable::merge_at(j = 8:9, i = 2, part = "header") %>%
    flextable::merge_at(j = 8:9, i = 3, part = "header") %>%
    flextable::merge_v(j = 1, part = "body") %>%
    theme_apa() %>%
    flextable::align(j = 2:9, align = "right", part = "all") %>%
    flextable::align(j = 2:9, align = "center", part = "header") %>%
    flextable::padding(
      i = 1:3, j = 1:9, padding.bottom = 0, part = "header"
    ) %>%
    flextable::padding(i = 16, j = 1:9, padding.bottom = 20) %>%
    flextable::width(j = 1, width = 1.4) %>%
    flextable::width(j = c(3, 5, 7, 9), width = 1.2) %>%
    flextable::width(j = c(2, 4, 6, 8), width = 1.1)
}

#' Table 4
#'
#' Fisher’s exact test results.
#'
#' @inherit add_table_1
#' @export
add_table_4 <- function(data){
  data %>%
    dplyr::select(
      x, y, fisher_test_p, fisher_test_p_fdr,
      fisher_test_or, fisher_test_ci_low, fisher_test_ci_up
    ) %>%
    stats::setNames(c("dis", "psy", "p", "fdr", "or", "cil", "ciu")) %>%
    dplyr::mutate(
      dis = dplyr::case_when(
        dis == "dsm5_cutoff_35_01" ~ "any disorder",
        dis == "dsm5_cutoff_35_read" ~ "reading disorder",
        dis == "dsm5_cutoff_35_spell" ~ "spelling disorder",
        dis == "dsm5_cutoff_35_math" ~ "arithmetic disorder",
        TRUE ~ NA_character_
      ),
      dis = forcats::fct_relevel(
        as.factor(dis),
        "any disorder",
        "reading disorder",
        "spelling disorder",
        "arithmetic disorder"
      ),
      psy = dplyr::case_when(
        psy == "adhs_z_cat" ~ "ADHD",
        psy == "des_z_cat" ~ "depression",
        psy == "sca_e_z_cat" ~ "anxiety",
        psy == "ssv_z_cat" ~ "conduct disorder",
        TRUE ~ NA_character_
      ),
      p = dplyr::case_when(
        as.numeric(p) < 0.001 ~ " < .001",
        TRUE ~ stringr::str_sub(as.character(round(as.numeric(p), 3)), 2, 5)
      ),
      fdr = dplyr::case_when(
        fdr == "yes" ~ "*",
        TRUE ~ " "
      )
    ) %>%
    dplyr::arrange(dis, psy) %>%
    dplyr::mutate_at(
      dplyr::vars(or, cil, ciu),
      dplyr::funs(as.character(round(as.numeric(.), 2)))
    ) %>%
    dplyr::mutate(
      or = stringr::str_c(or, " (", cil, "-", ciu, ")")
    ) %>%
    dplyr::select(-cil, -ciu) %>%
    flextable::flextable(col_keys = c("dis", "psy", "dummy_col", "or")) %>%
    flextable::display(
      col_key = "dummy_col",
      pattern = "{{variable_}}{{superscript_}}",
      formatters = list(variable_ ~ p, superscript_ ~ fdr),
      fprops = list(superscript_ = update(get_font()$superscript, italic = TRUE))
    ) %>%
    flextable::set_header_labels(
      dis = "SLD",
      psy = "psychopathology",
      dummy_col = "p",
      or = "OR (95% CI)"
    ) %>%
    theme_apa() %>%
    flextable::merge_v(j = 1) %>%
    flextable::align(j = 1:2, align = "left", part = "all") %>%
    flextable::align(j = 3:4, align = "right", part = "all")
}

#' Table footnote
#'
#' Adds a table footnote to the manuscript.
#'
#' @param manuscript `officer` rdocx object. The manuscript
#' @param number character. Number/symbol of the footnote
#' @param text character. Text of the footnote
#' @return `officer` rdocx object. The manuscript with added content
#' @export
body_add_footnote <- function(manuscript, number, text) {
  officer::body_add_fpar(
    manuscript,
    officer::fpar(
      officer::ftext(number, prop = get_font()$superscript),
      officer::ftext(text, prop = get_font()$normal)
    )
  )
}

#' Table caption
#'
#' Adds a table caption to the manuscript.
#'
#' @param manuscript `officer` rdocx object. The manuscript
#' @param number character. Number of the table
#' @param text character. Text of the caption
#' @return `officer` rdocx object. The manuscript with added content
#' @export
body_add_caption_table <- function(manuscript, number, text) {
  manuscript %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(
          stringr::str_c(
            "Table ", as.character(number)), prop = get_font()$normal
          )
      )
    ) %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(text, prop = get_font()$italic)
      )
    ) %>%
    officer::body_add_par("")
}

#' Add tables
#'
#' Add all tables and table captions to the manuscript.
#'
#' @param manuscript `officer` rdocx object. The manuscript
#' @param tab1 `flextable` object. Table 1
#' @param tab2 `flextable` object. Table 2
#' @param tab3 `flextable` object. Table 3
#' @param tab4 `flextable` object. Table 4
#' @return `officer` rdocx object. The manuscript with added content
#' @export
add_table_all <- function(manuscript, tab1, tab2, tab3, tab4){
  manuscript %>%

    # table 1
    officer::body_add_break() %>%
    body_add_caption_table(1, "Description and representativeness of the sample") %>%
    flextable::body_add_flextable(tab1, align = "left") %>%
    body_add_footnote("1 ", "Hauptschule, five years of school after four years of elementary school.") %>%
    body_add_footnote("2 ", "Realschule, six years of school after four years of elementary school, terminating with a secondary-school level-I certificate.") %>%
    body_add_footnote("3 ", "Gymnasium, eight years of school after four years of elementary school, terminating with a general qualification for university entrance.") %>%

    # table 2
    officer::body_add_break() %>%
    body_add_caption_table(2, "Numbers and percentages of children with different types of SLD and their average test scores") %>%
    flextable::body_add_flextable(tab2, align = "left") %>%

    # table 3
    officer::body_add_break() %>%
    officer::body_end_section_portrait() %>%
    body_add_caption_table(3, "Numbers and percentages of children with anxiety, depression, conduct disorder, and ADHD in children with different types of SLD") %>%
    flextable::body_add_flextable(tab3, align = "left") %>%
    officer::body_end_section_landscape() %>%

    # table 4
    body_add_caption_table(4, "Fisher’s exact test results for the difference in occurence of anxiety, depression, conduct disorder, and ADHD between children with and without SLD") %>%
    flextable::body_add_flextable(tab4, align = "left") %>%
    body_add_footnote("* ", "significant after FDR correction")
}
