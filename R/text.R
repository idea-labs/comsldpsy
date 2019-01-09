#' Mean and CI of intelligence score
#'
#' Computes the mean and confidence intervall of the IQ score for a
#'   selected subgroup of participants.
#'
#' @param data tbl. Input data
#' @param filter_cond character. Filter condition(s)
#' @return character. Mean (CI) of IQ value for selected subgroup
#' @export
get_mean_ci <- function(data, filter_cond) {
  data_selection <- data %>%
    dplyr::filter_(filter_cond)

  results <- data_selection$cft_iq_own_kl %>%
    Hmisc::smean.cl.normal() %>%
    round(digits = 2)

  stringr::str_c(
      "M = ", results[[1]],
      " (95% CI = ", results[[2]], "–", results[[3]], ")"
    )
}

#' Descriptive statistics for manuscript text
#'
#' Computes all descriptive statistics to be reported in the
#'   manuscript and stores them as list of character strings.
#'
#' @param data tbl. Filtered data frame
#' @return list. A list with named entries for the character
#'   strings
#' @export
add_text_descriptives <- function(data) {
  text <- list()

  # n participants
  text$n_filtered <- nrow(data)

  # age
  age_mean <- mean(data$age, na.rm = TRUE)
  text$age_mean_y <- floor(age_mean / 12)
  text$age_mean_m <- round(age_mean - text$age_mean_y * 12)
  text$age_sd <- round(sd(data$age, na.rm = TRUE))
  age_min <- min(data$age, na.rm = TRUE)
  text$age_min_y <- floor(age_min / 12)
  text$age_min_m <- round(age_min - text$age_min_y * 12)
  age_max <- max(data$age, na.rm = TRUE)
  text$age_max_y <- floor(age_max / 12)
  text$age_max_m <- round(age_max - text$age_max_y * 12)
  age_mean_grade3 <- data %>%
    dplyr::filter(grade == "3. Klasse") %>%
    .$age %>%
    mean(na.rm = TRUE)
  text$age_mean_y_grade3 <- floor(age_mean_grade3 / 12)
  text$age_mean_m_grade3 <- round(
    age_mean_grade3 - text$age_mean_y_grade3 * 12
  )
  age_mean_grade4 <- data %>%
    dplyr::filter(grade == "4. Klasse") %>%
    .$age %>%
    mean(na.rm = TRUE)
  text$age_mean_y_grade4 <- floor(age_mean_grade4 / 12)
  text$age_mean_m_grade4 <- round(
    age_mean_grade4 - text$age_mean_y_grade4 * 12
  )

  # observed SLDs
  c(text$n_iso_read, text$perc_iso_read) %<-% get_n_perc_filter(
    data,
    "dsm5_cutoff_35 == 'isolated reading disorder'"
  )
  c(text$n_iso_spell, text$perc_iso_spell) %<-% get_n_perc_filter(
    data,
    "dsm5_cutoff_35 == 'isolated spelling disorder'"
  )
  c(text$n_iso_math, text$perc_iso_math) %<-% get_n_perc_filter(
    data,
    "dsm5_cutoff_35 == 'isolated arithmetic disorder'"
  )

  text$n_sld_any <- get_n_perc_filter(
    data,
    "dsm5_cutoff_35_01 == 'indication of problems'"
  )[[1]]
  text$n_sld_read <- get_n_perc_filter(
    data,
    "dsm5_cutoff_35_read == 'indication of problems'"
  )[[1]]
  text$n_sld_spell <- get_n_perc_filter(
    data,
    "dsm5_cutoff_35_spell == 'indication of problems'"
  )[[1]]
  text$n_sld_math <- get_n_perc_filter(
    data,
    "dsm5_cutoff_35_math == 'indication of problems'"
  )[[1]]

  # observed comorbidities between SLD & psychopath.
  text$perc_com_read_spell_des <- get_n_perc_filter(
      dplyr::filter(data, dsm5_cutoff_35 == "comorbid reading & spelling"),
      "des_z_cat == 'indication of problems'"
    )[[2]] %>%
    round()
  text$perc_com_read_math_ssv <- get_n_perc_filter(
      dplyr::filter(data, dsm5_cutoff_35 == "comorbid reading & arithmetic"),
      "ssv_z_cat == 'indication of problems'"
    )[[2]] %>%
    round()
  text$perc_com_spell_math_adhs <- get_n_perc_filter(
      dplyr::filter(data, dsm5_cutoff_35 == "comorbid spelling & arithmetic"),
      "adhs_z_cat == 'indication of problems'"
    )[[2]] %>%
    round()
  text$perc_any_sca <- get_n_perc_filter(
      dplyr::filter(data, dsm5_cutoff_35_01 == "indication of problems"),
      "sca_e_z_cat == 'indication of problems'"
    )[[2]] %>%
    round()
  text$perc_any_des <- get_n_perc_filter(
    dplyr::filter(data, dsm5_cutoff_35_01 == "indication of problems"),
    "des_z_cat == 'indication of problems'"
    )[[2]] %>%
    round()
  text$perc_any_adhs <- get_n_perc_filter(
    dplyr::filter(data, dsm5_cutoff_35_01 == "indication of problems"),
    "adhs_z_cat == 'indication of problems'"
    )[[2]] %>%
    round()
  text$perc_any_ssv <- get_n_perc_filter(
    dplyr::filter(data, dsm5_cutoff_35_01 == "indication of problems"),
    "ssv_z_cat == 'indication of problems'"
    )[[2]] %>%
    round()

  # intelligence group differences
  text$iq_math <- get_mean_ci(
    data,
    "dsm5_cutoff_35 == 'isolated arithmetic disorder'"
  )
  text$iq_read <- get_mean_ci(
    data,
    "dsm5_cutoff_35 == 'isolated reading disorder'"
  )
  text$iq_spell <- get_mean_ci(
    data,
    "dsm5_cutoff_35 == 'isolated spelling disorder'"
  )

  # R version
  text$r_version <- stringr::str_c(
    R.version$major,
    ".",
    R.version$minor
  )

  text
}

#' Number of excluded participants
#'
#' Computes the number of participants that are excluded when applying the
#'   specified exclusion criterion.
#'
#' @param data tbl. The data frame to be filtered
#' @param criterion character. The exclusion criterion
#' @return double. The number of excluded participants
#' @export
count_filter <- function(data, criterion) {
  data %>%
    dplyr::filter_(stringr::str_c("!(", criterion, ")")) %>%
    nrow()
}

#' Number of excluded participants per exclusion criterion
#'
#' Computes the number of participants that are excluded for each
#'   exclusion criterion and totals for groups of exclusion criteria.
#'
#' @param data tbl. The data frame to be filtered
#' @param df_filter_cond tbl. A data frame with exclusion criteria
#' @param output character. "full" returns full data frame; "groups" returns
#'   excluded participants per group of exclusion criteria; "subgroups"
#'   returns excluded participants per exclusion criterion.
#' @return tbl. A data frame listing the number of excluded participants
#'   per exclusion criterion.
#' @export
count_filter_results <- function(data,
                                 filter_cond,
                                 output = c("full", "groups", "subgroups")) {
  df_all_filter <- filter_cond %>%
    dplyr::summarize(filter = paste(na.omit(filter), collapse = " & ")) %>%
    dplyr::mutate(
      group = "all conditions",
      condition = "all conditions"
    ) %>%
    dplyr::select(group, condition, filter)

  for (the_group in unique(filter_cond$group)) {
    df_group <- filter_cond %>%
      dplyr::filter(group == the_group)

    if (nrow(df_group) == 1) {
      df_all_filter %<>%
        dplyr::bind_rows(df_group)
    } else {
      df_group_sum <- df_group %>%
        dplyr::group_by(group) %>%
        dplyr::summarize(
          condition = the_group,
          filter = paste(na.omit(filter), collapse = " & ")
        )

      df_all_filter %<>%
        dplyr::bind_rows(df_group_sum, df_group)
    }
  }

  n <- c()
  for (filter in df_all_filter$filter) {
    n <- c(n, count_filter(data, filter))
  }

  df_all_filter %<>%
    dplyr::bind_cols(as.data.frame(n))

  output <- match.arg(output)
  if (output == "groups") {
    df_all_filter %<>%
      dplyr::filter(group == condition) %>%
      dplyr::select(-group, -filter)
  } else if (output == "subgroups") {
    df_all_filter %<>%
      dplyr::select(-filter)
  }

  df_all_filter
}

#' Numbers of excluded participants for manuscript text
#'
#' Computes number of excluded participants to be reported in the
#'   manuscript and stores them as list of character strings.
#'
#' @param data tbl. Transformed data frame
#' @return list. A list with named entries for the character
#'   strings
#' @export
add_text_exclusion <- function(data) {
  df_all_filter <- count_filter_results(data, get_filter_cond())

  text <- list()

  c(text$n_implausible, text$perc_implausible) %<-% get_n_perc_filter(
    data,
    dplyr::filter(df_all_filter, condition == "implausible data")$filter,
    TRUE
  )
  c(text$n_test_incompl, text$perc_test_incompl) %<-% get_n_perc_filter(
    data,
    dplyr::filter(df_all_filter, condition == "not all days completed")$filter,
    TRUE
  )
  c(text$n_question_incompl, text$perc_question_incompl) %<-% get_n_perc_filter(
    data,
    dplyr::filter(
      df_all_filter,
      condition == "parent questionnaires incomplete"
    )$filter,
    TRUE
  )
  c(text$n_siblings, not_reported) %<-% get_n_perc_filter(
    data,
    dplyr::filter(df_all_filter, condition == "no siblings")$filter,
    TRUE
  )
  c(text$n_iq_low, text$perc_iq_low) %<-% get_n_perc_filter(
    data,
    dplyr::filter(df_all_filter, condition == "IQ in normal range")$filter,
    TRUE
  )
  c(text$n_other_criteria, text$perc_other_criteria) %<-% get_n_perc_filter(
    data,
    dplyr::filter(df_all_filter, condition == "other criteria")$filter,
    TRUE
  )
  c(text$n_excl_total, text$perc_excl_total) %<-% get_n_perc_filter(
    data,
    dplyr::filter(df_all_filter, condition == "all conditions")$filter,
    TRUE
  )

  text
}

#' Odds ratio and CI for Fisher's exact test
#'
#' Outputs a character string with the odds ratio and CI for
#'   a specified combination of SLD and psychopathology
#'
#' @param data tbl. Data frame with results of Fisher's exact
#'   test
#' @param sld character. SLD variable name
#' @param psy character. Psychopathology variable name
#' @return character. String reporting odds ratio and CI,
#'   e.g., "1.25 (95% CI = 1.15–1.35)"
#'
#' @export
report_fisher_or <- function(data, sld, psy){
  data %>%
    dplyr::filter(x == sld & y == psy) %$%
    stringr::str_c(
      round(as.numeric(.$fisher_test_or), 2),
      " (95% CI = ",
      round(as.numeric(.$fisher_test_ci_low), 2),
      "–",
      round(as.numeric(.$fisher_test_ci_up), 2),
      ")"
    )
}

#' Range of odds ratios from Fisher's exact test for specific psychopathology
#'
#' Outputs a character string with the range of odds ratios for all
#'   combinations of a specific psychopathology and the different SLDs
#'
#' @param data tbl. Data frame with results of Fisher's exact
#'   test
#' @param psy character. Psychopathology variable name
#' @return character. String reporting odds ratio range,
#'   e.g., "2.5–3.5"
#'
#' @export
report_fisher_or_range <- function(data, psy){
  data %>%
    dplyr::filter(y == psy & x != "dsm5_cutoff_5_01") %>%
    dplyr::mutate(fisher_test_or = as.numeric(fisher_test_or)) %$%
    stringr::str_c(
      "range ",
      round(min(.$fisher_test_or), 2),
      "–",
      round(max(.$fisher_test_or), 2)
    )
}

#' Results of Fisher's exact test for manuscript text
#'
#' Stores results of Fisher's exact test as list of
#'   character strings.
#'
#' @inheritParams report_fisher_or
#' @return list. A list with named entries for the character
#'   strings
#' @export
add_text_fisher <- function(data) {
  text <- list()

  text$or_any_adhs <- report_fisher_or(data, "dsm5_cutoff_35_01", "adhs_z_cat")
  text$or_any_des <- report_fisher_or(data, "dsm5_cutoff_35_01", "des_z_cat")
  text$or_any_sca <- report_fisher_or(data, "dsm5_cutoff_35_01", "sca_e_z_cat")
  text$or_any_ssv <- report_fisher_or(data, "dsm5_cutoff_35_01", "ssv_z_cat")
  text$or_read_adhs <- report_fisher_or(
    data, "dsm5_cutoff_35_read", "adhs_z_cat"
  )
  text$or_spell_adhs <- report_fisher_or(
    data, "dsm5_cutoff_35_spell", "adhs_z_cat"
  )
  text$or_math_adhs <- report_fisher_or(
    data, "dsm5_cutoff_35_math", "adhs_z_cat"
  )
  text$or_range_des <- report_fisher_or_range(data, "des_z_cat")

  text
}

#' z-score and p-value for trend test
#'
#' Outputs a character string with z-score and p-value
#'   for a specified psychopathology
#'
#' @param data tbl. Data frame with results of the trend test
#' @param psy character. Psychopathology variable name
#' @return character. String reporting z-score and p-value,
#'   e.g., "z= 4.45, p < 0.001"
#' @export
report_trend_sign <- function(data, psy){
  data %>%
    dplyr::filter(y == psy) %>%
    dplyr::select(trend_wald_z, trend_wald_p_1sided) %>%
    stats::setNames(c("z", "p")) %>%
    dplyr::mutate(
      z = round(z, 2),
      p = dplyr::case_when(
        p < 0.001 ~ " < .001",
        TRUE ~ stringr::str_c(
          " = ", stringr::str_sub(as.character(round(p, 3)), 2, 5)
        )
      )
    ) %$%
    stringr::str_c("z = ", .$z, ", p", .$p)
}

#' Odds ratio and CI for trend test
#'
#' Outputs a character string with the odds ratio and CI for
#'   a specified psychopathology
#'
#' @inheritParams report_trend_sign
#' @return character. String reporting odds ratio and CI,
#'   e.g., "1.25 (95% CI = 1.15–1.35)"
#' @export
report_trend_or <- function(data, psy){
  data %>%
    dplyr::filter(y == psy) %>%
    dplyr::select(starts_with("estim")) %>%
    stats::setNames(c("or", "cil", "ciu")) %>%
    dplyr::mutate_all(dplyr::funs(round(., 2))) %$%
    stringr::str_c("OR = ", .$or, "; 95%-CI = ", .$cil, "–", .$ciu)
}

#' Results of trend test for manuscript text
#'
#' Stores results of the trend test as list of
#'   character strings.
#'
#' @inheritParams report_trend_sign
#' @return list. A list with named entries for the character
#'   strings
#' @export
add_text_trend <- function(data) {
  text <- list()

  text$trend_sign_adhs <- report_trend_sign(data, "adhs_z_cat")
  text$trend_sign_des <- report_trend_sign(data, "des_z_cat")
  text$trend_sign_sca <- report_trend_sign(data, "sca_e_z_cat")
  text$trend_sign_ssv <- report_trend_sign(data, "ssv_z_cat")

  text$trend_or_adhs <- report_trend_or(data, "adhs_z_cat")
  text$trend_or_des <- report_trend_or(data, "des_z_cat")
  text$trend_or_sca <- report_trend_or(data, "sca_e_z_cat")
  text$trend_or_ssv <- report_trend_or(data, "ssv_z_cat")

  text
}

#' Odds ratios, CIs, and p-values for post hoc tests
#'
#' Outputs a character string with the odds ratios, CIs, and
#'   p-values for a specified psychopathology
#'
#' @param data tbl. Data frame with results of the posthoc test
#' @param psy character. Psychopathology variable name
#' @return character vector. Vector of strings reporting odds ratios,
#'   p-value and CIs, e.g., "OR = 1.25, 95%-CI = 1.15–1.35, p < 0.001"
#' @export
report_posthoc <- function(data, psy){
  data %>%
    dplyr::filter(y == psy) %>%
    dplyr::select(3:10) %>%
    stats::setNames(c(
      "12_p", "12_or", "12_cil", "12_ciu",
      "23_p", "23_or", "23_cil", "23_ciu"
    )) %>%
    dplyr::mutate_at(
      dplyr::vars(ends_with("p")),
      dplyr::funs(dplyr::case_when(
        . < 0.001 ~ " < .001",
        TRUE ~ stringr::str_c(
          " = ", stringr::str_sub(as.character(round(., 3)), 2, 5)
        )
      ))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(ends_with("or"), ends_with("cil"), ends_with("ciu")),
      dplyr::funs(round(., 2))
    ) %$%
    c(
      stringr::str_c(
        "OR = ", .$`12_or`,
        ", 95%-CI = ", .$`12_cil`, "–", .$`12_ciu`, ", p", .$`12_p`
      ),
      stringr::str_c(
        "OR = ", .$`23_or`,
        ", 95%-CI = ", .$`23_cil`, "–", .$`23_ciu`, ", p", .$`23_p`
      )
    )
}

#' Results of the post hoc tests for manuscript text
#'
#' Stores results of the post hoc tests as list of
#'   character strings.
#'
#' @inheritParams report_posthoc
#' @return list. A list with named entries for the character
#'   strings
#' @export
add_text_posthoc <- function(data) {
  text <- list()

  c(text$posthoc_12_adhs, text$posthoc_23_adhs) %<-% report_posthoc(
    data, "adhs_z_cat"
  )
  c(text$posthoc_12_des, text$posthoc_23_des) %<-% report_posthoc(
    data, "des_z_cat"
  )
  c(text$posthoc_12_sca, text$posthoc_23_sca) %<-% report_posthoc(
    data, "sca_e_z_cat"
  )

  text
}

#' Parameter estimate, CI, and p-value for the poisson model
#'
#' Outputs a character string with the estimate, CI, and p-value
#'   for a specified term in the poisson model
#'
#' @param data tbl. Data frame with results of the poisson model
#' @param the_term character. Model term
#' @return character. Strings reporting parameter estimate, CI, and
#'   p-value, e.g., "1.65 (95-CI = 1.55–1.75, p < .001)"
#' @export
report_poisson <- function(data, the_term){
  data %>%
    dplyr::filter(term == the_term) %>%
    dplyr::select(2:5) %>%
    stats::setNames(c("p", "est", "cil", "ciu")) %>%
    dplyr::mutate(
      p = dplyr::case_when(
        p < 0.001 ~ " < .001",
        TRUE ~ stringr::str_c(" = ", stringr::str_sub(
          as.character(round(p, 3)), 2, 5)
        )
      )
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(est, cil, ciu),
      dplyr::funs(round(., 2))
    ) %$%
    stringr::str_c(.$est, " (95-CI = ", .$cil, "–", .$ciu, ", p", .$p, ")")
}

#' Results of the poisson model for manuscript text
#'
#' Stores results of the poisson model as list of
#'   character strings.
#'
#' @inheritParams report_poisson
#' @return list. A list with named entries for the character
#'   strings
#' @export
add_text_poisson <- function(data) {
  text <- list()

  text$poisson_intercept <- report_poisson(data, "(Intercept)")
  text$poisson_slope <- report_poisson(data, "dsm5_cutoff_35_n")

  text
}


#   ____________________________________________________________________________
#   add text to manuscript                                                  ####

#' Add text blocks to manuscript
#'
#' Iterates over list of named character strings and replaces placeholders
#'   ("<<...>>") with corresponding names in the Microsoft Word template of the
#'   manuscript.
#'
#' @param manuscript `officer` rdocx object. The manuscript template
#' @param text list. List of named character strings to be added to
#'   the manuscript
#' @return `officer` rdocx object. The manuscript with placeholders replaced
#' @seealso [officer::body_replace_all_text()]
#' @export
add_text <- function(manuscript, text) {
  for (name in names(text)){
    old_value <- stringr::str_c("<<", name, ">>")
    new_value <- as.character(text[[name]])

    manuscript %>%
      officer::body_replace_all_text(
        old_value, new_value,
        only_at_cursor = FALSE,
        ignore.case = TRUE
      )
  }

  manuscript
}
