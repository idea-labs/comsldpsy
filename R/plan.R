#'  Get the analysis plan
#'
#' Using the `drake` pipeline toolkit, sets up all analysis steps to go from
#'   the raw data to the finished manuscript.
#'
#' @return A `drake` plan.
#' @seealso [drake::drake()]
#' @export
get_plan <- function(){
  #   __________________________________________________________________________
  #   prepare data for analysis                                             ####

  plan_preparation <- drake::drake_plan(
    data_raw = readRDS(drake::file_in("analysis/data/data_orig.rds")),
    data_transformed = transform_data(data_raw),
    data_filtered = filter_data(data_transformed, get_filter_cond())
  )

  #   __________________________________________________________________________
  #   inferential statistics                                                ####

  disability_01 <- c(
    "dsm5_cutoff_35_01",
    "dsm5_cutoff_35_read",
    "dsm5_cutoff_35_spell",
    "dsm5_cutoff_35_math"
  )

  psychopaths <- c(
    "adhs_z_cat",
    "des_z_cat",
    "sca_e_z_cat",
    "ssv_z_cat"
  )

  ##  ..........................................................................
  ##  Fisher's exact tests                                                  ####

  args_fisher <- tidyr::crossing(disability_01, psychopaths) %>%
    stats::setNames(c("x", "y")) %>%
    dplyr::mutate(
      data = rlang::syms("data_filtered"),
      id = stringr::str_c("fisher", x, y, sep = "_")
    )

  plan_fisher <- drake::map_plan(args_fisher, fisher_test)
  plan_fisher_combined <- drake::gather_plan(
    plan_fisher, target = "df_fisher", gather = "rbind"
  )

  ##  ..........................................................................
  ##  trend tests                                                           ####

  args_trend <- tidyr::crossing("dsm5_cutoff_35_012", psychopaths) %>%
    stats::setNames(c("x", "y")) %>%
    dplyr::mutate(
      data = rlang::syms("data_filtered"),
      id = stringr::str_c("trend", x, y, sep = "_")
    )

  plan_trend <- drake::map_plan(args_trend, trend_test)
  plan_trend_combined <- drake::gather_plan(
    plan_trend, target = "df_trend", gather = "rbind"
  )

  args_posthoc <- tidyr::crossing("dsm5_cutoff_35_012", psychopaths) %>%
    stats::setNames(c("x", "y")) %>%
    dplyr::mutate(
      data = rlang::syms("data_filtered"),
      id = stringr::str_c("posthoc", x, y, sep = "_")
    )

  plan_posthoc <- drake::map_plan(args_posthoc, posthoc_test)
  plan_posthoc_combined <- drake::gather_plan(
    plan_posthoc, target = "df_posthoc", gather = "rbind"
  )

  ##  ..........................................................................
  ##  poisson model                                                         ####

  plan_poisson <- drake::drake_plan(
    df_poisson = glm_poisson(data_filtered)
  )

  ##  ..........................................................................
  ##  FDR correction                                                        ####

  plan_fdr_correction <- drake::drake_plan(
    df_fisher_fdr = correct_mult_comp(df_fisher, "fisher_test_p"),
    df_trend_fdr = correct_mult_comp(df_trend, "trend_wald_p_1sided"),
    df_posthoc_fdr = correct_mult_comp(
      df_posthoc, c("post_hoc_12_p", "post_hoc_23_p")
    )
  )

  #   __________________________________________________________________________
  #   add text to manuscript                                                ####

  plan_add_text <- drake::drake_plan(
    text_descriptives = add_text_descriptives(data_filtered),
    text_exclusion = add_text_exclusion(data_transformed),
    text_fisher = add_text_fisher(df_fisher_fdr),
    text_trend = add_text_trend(df_trend_fdr),
    text_posthoc = add_text_posthoc(df_posthoc_fdr),
    text_poisson = add_text_poisson(df_poisson),
    text_combined = c(
      text_descriptives,
      text_exclusion,
      text_fisher,
      text_trend,
      text_posthoc,
      text_poisson
    ),

    manuscript_in = officer::read_docx(
      file_in("analysis/templates/manuscript_template.docx")
    ),
    manuscript_text = add_text(manuscript_in, text_combined),

    supplemental_in = officer::read_docx(
      file_in("analysis/templates/supplemental_template.docx")
    ),
    supplemental_text = add_text(supplemental_in, text_combined)
  )


  #   __________________________________________________________________________
  #   add tables to manuscript & supplemental                               ####

  plan_add_tables <- drake::drake_plan(
    table_1 = add_table_1(data_filtered),
    table_2 = add_table_2(df_fisher_fdr),
    table_suppl = add_table_sup(data_filtered),
    manuscript_tables = add_table_manuscr(
      manuscript_text,
      table_1,
      table_2
    ),
    supplemental_tables = add_table_suppl(
      supplemental_text,
      table_suppl,
      text_descriptives
    ),
    supplemental_out = print(
      supplemental_tables,
      target = file_out("analysis/manuscript/supplemental.docx")
    )
  )

  #   __________________________________________________________________________
  #   export figures and add figure captions to manuscript                  ####

  plan_add_figures <- drake::drake_plan(
    figure_1 = add_figure_1(data_filtered),
    figure_2 = add_figure_2(data_filtered),
    figure_3 = add_figure_3(data_filtered),
    manuscript_figures = add_figure_all(manuscript_tables),
    manuscript_out = print(
      manuscript_figures,
      target = drake::file_out("analysis/manuscript/manuscript.docx")
    ),
    figure_1_out = export_tiff_figure(
      figure_1, drake::file_out("analysis/figures/figure_1.tiff"), 180, 90
    ),
    figure_2_out = export_tiff_figure(
      figure_2, drake::file_out("analysis/figures/figure_2.tiff"), 240, 160
    ),
    figure_3_out = export_tiff_figure(
      figure_3, drake::file_out("analysis/figures/figure_3.tiff"), 120, 110
    )
  )

  #   __________________________________________________________________________
  #   combine plans                                                         ####

  drake::bind_plans(
    plan_preparation,
    plan_fisher,
    plan_fisher_combined,
    plan_trend,
    plan_trend_combined,
    plan_posthoc,
    plan_posthoc_combined,
    plan_poisson,
    plan_fdr_correction,
    plan_add_text,
    plan_add_tables,
    plan_add_figures
  )
}
