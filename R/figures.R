#' Plot for Figure 1
#'
#' Plots line graphs representing percent of participants having a
#'   certain SLD over the number of areas with psychopathological
#'   problems
#'
#' @param data tbl. Input data
#' @param labels list. A list containing, for each SLD to plot, the variable
#'   name, the filter condition to filter the data frame and compute
#'   percentages, and the name to use in the plot legend
#' @return `ggplot` object. The plot
#' @export
plot_figure_1 <- function(data, labels) {
  data_plot <- tibble::tibble()
  for (i in 1:length(labels$vars)) {
    data_plot <- dplyr::bind_rows(
      data_plot,
      data %>%
        dplyr::filter_(
          stringr::str_c(labels$vars[[i]], " == ", labels$filters[[i]])
        ) %>%
        dplyr::group_by_(labels$vars[[i]], "psychopaths_n") %>%
        dplyr::summarize(n = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          disability = labels$names[[i]],
          percent = n / sum(n)
        ) %>%
        dplyr::select(disability, psychopaths_n, percent)
    )
  }

  data_plot %>%
    dplyr::mutate(
      disability = forcats::fct_relevel(
        disability,
        labels$names
      )
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        psychopaths_n, percent,
        group = disability,
        linetype = disability,
        shape = disability
      )
    ) +
    ggplot2::geom_line(size = 0.65) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 0.75)
    ) +
    jtools::theme_apa(
      legend.font.size = 10,
      legend.pos = "topright",
      remove.x.gridlines = FALSE,
      remove.y.gridlines = FALSE
    )
}

#' Figure 1
#'
#' Number of areas affected by psychopathology in children with and without
#'   different subtypes of SLD.
#'
#' @param data tbl. The filtered data frame
#' @return `ggmatrix` object. The figure
#' @seealso [GGally::ggmatrix()]
#' @export
add_figure_1 <- function(data) {
  figure_1a_labels <- list(
    vars = c(
      "dsm5_cutoff_35_01",
      "dsm5_cutoff_35_01"
    ),
    filters = c(
      "'no problems'",
      "'indication of problems'"
    ),
    names = c(
      "no SLD",
      "any SLD"
    )
  )

  figure_1b_labels <- list(
    vars = c(
      "dsm5_cutoff_35_read",
      "dsm5_cutoff_35_spell",
      "dsm5_cutoff_35_math"
    ),
    filters = c(
      "'indication of problems'",
      "'indication of problems'",
      "'indication of problems'"
    ),
    names = c(
      "reading disorder",
      "spelling disorder",
      "arithmetic disorder"
    )
  )

  plot_list <- list(
    plot_figure_1(data, figure_1a_labels),
    plot_figure_1(data, figure_1b_labels)
  )

  GGally::ggmatrix(
    plot_list, 1, 2,
    xlab = "Number of areas with psychopathology",
    ylab = "Percent"
  )
}

#' Plot for Figure 2
#'
#' Plots "Upset" graph visualizing the overlap between different
#'   psychopathological problems within a selected subgroup of participants
#'   having / not having a specific SLD
#'
#' @param data tbl. Input data
#' @param filter_var character. The SLD variable name
#' @param invert_filter logical. Whether to select children *without* the SLD,
#'   Default: FALSE
#' @param ymax double. The maximum value of the y-axis of the intersection
#'   size bar plot, Default: NULL
#' @return `ggplot` object. The plot
#' @seealso [UpSetR::upset()]
#' @export
plot_figure_2 <- function(data,
                          filter_var,
                          invert_filter = FALSE,
                          ymax = NULL) {
  if (invert_filter) {
    plot_data <- data %>%
      as.data.frame() %>%
      dplyr::filter_(
        stringr::str_c(filter_var, " == 'no problems'")
      )
  } else {
    plot_data <- data %>%
      as.data.frame() %>%
      dplyr::filter_(
        stringr::str_c(filter_var, " == 'indication of problems'")
      )
  }

  plot_data %>%
    dplyr::select(
      sca_e_z_cat,
      adhs_z_cat,
      des_z_cat,
      ssv_z_cat
    ) %>%
    dplyr::rename(
      anxiety = sca_e_z_cat,
      ADHD = adhs_z_cat,
      depression = des_z_cat,
      `conduct dis.` = ssv_z_cat
    ) %>%
    dplyr::mutate_all(
      dplyr::funs(redcapAPI::redcapFactorFlip(.))
    ) %>%
    dplyr::mutate_all(
      dplyr::funs(sjlabelled::remove_all_labels(.))
    ) %>%
    UpSetR::upset(
      nintersects = NA,
      sets = c("depression", "conduct dis.", "anxiety", "ADHD"),
      keep.order = TRUE,
      mainbar.y.max = ymax
    )
}

#' Figure 2
#'
#' "UpSet” graphs visualizing the overlap between areas with psychopathology
#'
#' @param data tbl. The filtered data frame
#' @return grid object. The figure
#' @export
add_figure_2 <- function(data) {
  upset_graphs <- list(
    var = c(
      "dsm5_cutoff_35_01",
      "dsm5_cutoff_35_01",
      "dsm5_cutoff_35_read",
      "dsm5_cutoff_35_spell",
      "dsm5_cutoff_35_math"
    ),
    invert = c(
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE
    )
  )

  plot_list_1 <- list()
  for (i in 1:2) {
    plot_figure_2(data, upset_graphs$var[[i]], upset_graphs$invert[[i]])
    grid::grid.edit("arrange", name = as.character(i))
    vp <- grid::grid.grab()
    plot_list_1[[as.character(i)]] <- vp
  }

  plot_list_2 <- list()
  for (i in 3:5) {
    plot_figure_2(data, upset_graphs$var[[i]], upset_graphs$invert[[i]], 20)
    grid::grid.edit("arrange", name = as.character(i))
    vp <- grid::grid.grab()
    plot_list_2[[as.character(i)]] <- vp
  }

  row_1 <- cowplot::plot_grid(
    plotlist = plot_list_1,
    nrow = 1,
    labels = c("no", "any")
  )
  row_2 <- cowplot::plot_grid(
    plotlist = plot_list_2,
    nrow = 1,
    labels = c("reading", "spelling", "arithmetic")
  )

  cowplot::plot_grid(row_1, row_2, ncol = 1, align = "v", axis = "l")
}

#' Plot for Figure 3
#'
#' Plots line graphs representing percentages of selected psychopathologies
#'   over the levels of an SLD variable.
#'
#' @param data tbl. Input data
#' @param disability character. SLD variable name
#' @param psychopaths character vector. Vector of psychopathology variable names
#' @return `ggplot` object. The plot
#' @export
plot_figure_3 <- function(data, disability, psychopaths) {
  data_plot <- tibble::tibble()
  for (psychopath in psychopaths) {
    data_plot <- dplyr::bind_rows(
      data_plot,
      data %>%
        dplyr::filter_(
          stringr::str_c("!is.na(", disability, ") & !is.na(", psychopaths, ")")
        ) %>%
        dplyr::group_by_(disability, psychopath) %>%
        dplyr::summarize(n = n()) %>%
        dplyr::mutate(
          psychopath = dplyr::case_when(
            psychopath == "adhs_z_cat" ~ "ADHD",
            psychopath == "sca_e_z_cat" ~ "anxiety",
            psychopath == "des_z_cat" ~ "depression",
            psychopath == "ssv_z_cat" ~ "conduct disorder"
          ),
          frequency = n / sum(n)
        ) %>%
        dplyr::filter_(
          stringr::str_c(psychopath, " == 'indication of problems'")
        ) %>%
        dplyr::select(c(disability, "psychopath", "frequency"))
    )
  }

  plot <- data_plot %>%
    ggplot2::ggplot(
      ggplot2::aes_string(
        disability, "frequency",
        group = "psychopath",
        linetype = "psychopath",
        shape = "psychopath"
      )
    ) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_line(size = 0.65) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      sec.axis = ggplot2::sec_axis(~. / (1 - .), name = "Odds")
    ) +
    jtools::theme_apa(
      legend.font.size = 10,
      legend.pos = "topleft",
      remove.x.gridlines = FALSE,
      remove.y.gridlines = FALSE
    ) +
    ggplot2::xlab("SLD comorbidity") +
    ggplot2::ylab("Percent")

  print(plot)
}

#' Figure 3
#'
#' Trend in the prevalence of psychopathologies over `dsm5_cutoff_35_012`
#'
#' @param data tbl. The filtered data frame
#' @return `ggplot` object. The figure
#' @export
add_figure_3 <- function(data) {
  plot_figure_3(
    data,
    "dsm5_cutoff_35_012",
    c("adhs_z_cat", "des_z_cat", "sca_e_z_cat", "ssv_z_cat")
  )
}

#' Figure caption
#'
#' Adds a figure caption to the manuscript.
#'
#' @param manuscript `officer` rdocx object. The manuscript
#' @param number character. Number of the figure
#' @param text character. Text of the caption
#' @return `officer` rdocx object. The manuscript with added content
#' @export
body_add_caption_figure <- function(manuscript, number, text) {
  manuscript %>%
    officer::body_add_par("") %>%
    officer::body_add_fpar(
      officer::fpar(
        officer::ftext(
          stringr::str_c("Figure ", as.character(number), ". "),
          prop = get_font()$italic
        ),
        officer::ftext(text, prop = get_font()$normal)
      )
    )
}

#' Add figures
#'
#' Add all figures and figure captions to the manuscript.
#'
#' @param manuscript `officer` rdocx object. The manuscript
#' @param fig1 plot object. Figure 1
#' @param fig2 plot object. Figure 2
#' @param fig3 plot object. Figure 3
#' @return `officer` rdocx object. The manuscript with added content
#' @export
add_figure_all <- function(manuscript, fig1, fig2, fig3) {
  manuscript %>%

    # figure 1
    officer::body_add_gg(fig1) %>%
    body_add_caption_figure(1, "Number of areas affected by psychopathology in children with and without different subtypes of SLD.") %>%

    # figure 2
    officer::body_end_section_portrait() %>%
    officer::body_add_gg(fig2, height = 5.7, width = 9.1) %>%
    body_add_caption_figure(2, "“UpSet” graphs visualizing the overlap between areas with psychopathology in children with no SLD, any SLD, reading disorder, spelling disorder, and arithmetic disorder. For each SLD group, the total number of children with the different psychopathologies (anxiety, depression, conduct disorder, and ADHD) is presented in the small horizontal graph on the left. In the graph on the right, the dots indicate the combinations of psychopathologies, and the bar above the respective dots indicates the number of children within this SLD-group affected by the respective psychopathologies. conduct dis. = conduct disorder.") %>%
    officer::body_end_section_landscape() %>%

    # figure 3
    officer::body_add_gg(fig3) %>%
    body_add_caption_figure(3, "Trend in the prevalence of psychopathologies over the groups of children without an SLD, with an isolated SLD, and with comorbid SLDs.")
}
