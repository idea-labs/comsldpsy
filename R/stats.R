#' Correlation table
#'
#' Computes correlations between given variables. Returns correlation table as a
#'   data frame.
#'
#' @param data Input data
#' @param vars named character vector. The variable names and labels of
#'   variables to compute correlations for
#'
#' @return tbl. Correlation table
#' @export
get_corr_table <- function(data, vars) {
  matrix <- data %>%
    dplyr::select(names(vars)) %>%
    cor(use = "pairwise.complete.obs")

  matrix <- round(matrix, 2)
  matrix[upper.tri(matrix, diag = TRUE)] <- ""

  matrix %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(~vars) %>%
    dplyr::mutate(rowname = vars) %>%
    dplyr::select(rowname, dplyr::everything()) %>%
    dplyr::filter(rowname != vars[1]) %>%
    dplyr::select(-c(vars[length(vars)]))
}

#' Confidence interval for odds ratios
#'
#' Computes the adjusted inverse hyperbolic sine confidence interval for the
#'   odds ratio.
#'
#' Procedure as described in Fagerland & Newcombe (2013) and Fagerland,
#'   Lydersen, & Laake (2017).
#'
#' The function is an adaptation of the `Matlab` script provided by Fagerland et
#'   al. (2017) on the book's
#'   [website](http://contingencytables.com/software-resources).
#'
#' @param data tbl. Input data
#' @param x,y character. Variable names
#' @param psi1,psi2 double. Pseudo-frequencies (both should be > 0),
#'   Default: 0.6, 0.4
#' @param alpha double. The nominal level, Default: 0.05
#' @return tbl. Data frame with odds ratio and CIs
#' @references
#'   Fagerland, M., Lydersen, S., & Laake, P. (2017). *Statistical analysis
#'   of contingency tables.* London: Chapman and Hall.
#'
#'   Fagerland, M. W., & Newcombe, R. G. (2013). Confidence intervals for
#'   odds ratio and relative risk based on the inverse hyperbolic sine
#'   transformation. *Statistics in Medicine, 32*(16), 2823–2836.
#' @export
adj_inv_sinh_ci_or <- function(data,
                               x,
                               y,
                               psi1 = 0.6,
                               psi2 = 0.4,
                               alpha = 0.05) {

  # make contingency table
  n <- data %>%
    dplyr::select(c(x, y)) %>%
    stats::xtabs(~., data = .)

  # estimate of the odds ratio (thetahat)
  estimate <- n[1, 1] * n[2, 2] / (n[1, 2] * n[2, 1])

  # adjusted estimate
  thetatilde <- (n[1, 1] + psi1) * (n[2, 2] + psi1) /
    ( (n[1, 2] + psi1) * (n[2, 1] + psi1) )

  # upper alpha/2 percentile of the standard normal distribution
  z <- qnorm(1 - alpha / 2, 0, 1)

  # confidence limits
  tmp <- asinh(0.5 * z * sqrt(1 / (n[1, 1] + psi2) + 1 / (n[1, 2] + psi2) +
                                1 / (n[2, 1] + psi2) + 1 / (n[2, 2] + psi2)))
  L <- exp(log(thetatilde) - 2 * tmp)
  U <- exp(log(thetatilde) + 2 * tmp)

  tibble::tibble(
    or = estimate,
    ci_low = L,
    ci_up = U
  )
}

#' Fisher's exact test
#'
#' Computes Fisher's exact test for 2 x 2 contingency tables.
#'
#' @param data tbl. Input data
#' @param x,y character. Variable names
#' @param alternative character. Alternative hypothesis, Default: "greater"
#' @return tbl. Data frame with p-value for the test, odds ratio, and adjusted
#'   inverse hyperbolic sine CIs
#' @seealso [adj_inv_sinh_ci_or()]
#' @export
fisher_test <- function(data, x, y, alternative = "greater") {
  p_value <- stats::fisher.test(
    data[[x]],
    data[[y]],
    alternative = alternative
  )$p.value

  or_cis <- adj_inv_sinh_ci_or(data, x, y)

  tibble::tibble(
    x = x,
    y = y,
    fisher_test_p = p_value,
    fisher_test_or = or_cis$or,
    fisher_test_ci_low = or_cis$ci_low,
    fisher_test_ci_up = or_cis$ci_up
  )
}

#' Trend test for ordered r x 2 contingency table
#'
#' Computes trend test based on the generalized linear model with logit link
#'   function (logistic regression) and the Wald test statistic.
#'
#' Procedure as described in Fagerland, Lydersen, & Laake (2017).
#'
#' The function is an adaptation of the `Matlab` script provided by Fagerland et
#'   al. (2017) on the book's
#'   [website](http://contingencytables.com/software-resources).
#'
#' @param data tbl. Input data
#' @param x character. Name of a categorical variable with r levels
#' @param y character. Name of a categorical variable with 2 levels
#' @param alpha double. The nominal level, Default: 0.05
#' @return tbl. Data frame with Wald test statistic and p-value as well as
#'   estimate for the trend (odds ratio) and associated Wald confidence
#'   intervals
#' @references
#'   Fagerland, M., Lydersen, S., & Laake, P. (2017). *Statistical analysis
#'   of contingency tables.* London: Chapman and Hall.
#' @export
trend_test <- function(data, x, y, alpha=0.05) {
  model <- stats::glm(
    stringr::str_c(y, " ~ as.numeric(", x, ")"),
    family = binomial(link = "logit"),
    data = data
  )

  betahat <- summary(model)$coefficients[2, 1]
  SEhat   <- summary(model)$coefficients[2, 2]

  Z_Wald <- betahat / SEhat
  P_Wald <- 2 * (1 - pnorm(abs(Z_Wald)))

  z <- qnorm(1 - alpha / 2)
  CI_Wald_low <- betahat - z * SEhat
  CI_Wald_up <- betahat + z * SEhat

  tibble::tibble(
    x = x,
    y = y,
    trend_wald_z = Z_Wald,
    trend_wald_p = P_Wald,
    estim_wald_beta_or = exp(betahat),
    estim_wald_ci_low_or = exp(CI_Wald_low),
    estim_wald_ci_up_or = exp(CI_Wald_up)
  ) %>%
    dplyr::mutate(
      trend_wald_p_1sided = dplyr::case_when(
        trend_wald_z < 0 ~ 1 - trend_wald_p / 2,
        TRUE ~ trend_wald_p / 2
      )
    )
}

#' Post hoc tests for a 3 x 2 contingency table trend test
#'
#' Computes one-sided Fisher’s exact tests comparing increasing levels of
#'   the categorical variable with 3 levels (1 vs. 2; 2 vs. 3)
#'
#' @inheritParams trend_test
#' @return tbl. Data frame with p-value for the test, odds ratio, and adjusted
#'   inverse hyperbolic sine CIs for both comparisons
#' @seealso [fisher_test()] [adj_inv_sinh_ci_or()]
#' @export
posthoc_test <- function(data, x, y) {
  posthoc_12 <- data %>%
    dplyr::filter_(stringr::str_c(x, " %in% levels(.[['", x, "']])[1:2]")) %>%
    dplyr::mutate(!!x := forcats::fct_drop(.data[[x]])) %>%
    fisher_test(x, y) %>%
    dplyr::rename_all(
      dplyr::funs(stringr::str_replace(., "fisher_test", "post_hoc_12"))
    )

  posthoc_23 <- data %>%
    dplyr::filter_(stringr::str_c(x, " %in% levels(.[['", x, "']])[2:3]")) %>%
    dplyr::mutate(!!x := forcats::fct_drop(.data[[x]])) %>%
    fisher_test(x, y) %>%
    dplyr::rename_all(
      dplyr::funs(stringr::str_replace(., "fisher_test", "post_hoc_23"))
    )

  dplyr::full_join(posthoc_12, posthoc_23, by = c("x", "y"))
}

#' Tidy GLM results
#'
#' Return a tidy data frame of GLM results.
#'
#' Wraps around [broom::tidy()] and [broom::confint_tidy()] to provide a
#'   concise overview of model parameters. Can exponentiate coefficients
#'   and CIs for binomial or poisson models.
#'
#' @param glm_model A GLM model object
#' @param exp Logical. Should coefficients and CIs be exponentiated for
#'   binomial or poisson models, Default: TRUE
#' @return tbl. A data frame with p-value, coefficients, and CIs
#' @seealso [broom::tidy()] [broom::confint_tidy()]
#' @export
tidy_glm <- function(glm_model, exp = TRUE, abbr = FALSE) {
  results <- dplyr::bind_cols(
    broom::tidy(glm_model),
    broom::confint_tidy(glm_model)
  ) %>%
    dplyr::select(term, p.value, estimate, starts_with("conf")) %>%
    tibble::as.tibble()

  if (exp) {
    if (glm_model$family$family %in% c("binomial", "poisson", "quasipoisson")){
      results %<>%
        dplyr::mutate_at(
          dplyr::vars(estimate, conf.low, conf.high),
          dplyr::funs(exp(.))
        )
    }
  }

  results
}

#' Poisson regression
#'
#' Computes a general linear model with logit link (poisson regression)
#'   with number of SLDs as predictor and the number of psychopathological
#'   areas as outcome variable.
#'
#' @param data Input data
#' @return tbl. A tidy data frame with p-value, coefficients, and CIs for
#'   the model
#' @seealso [stats::glm()] [tidy_glm()]
#' @export
glm_poisson <- function(data){
  stats::glm(
    psychopaths_n ~ dsm5_cutoff_35_n,
    family = "poisson",
    data = data
  ) %>%
    tidy_glm()
}

#' Multiple comparison correction
#'
#' Applies multiple comparison correction by controlling the false discovery
#'   rate (FDR) using the modified FDR procedure by Benjamini and
#'   Yekutieli (2001)
#'
#' @param data tbl. A tidy data frame with statistical test results
#' @param var_name character (vector). Column name(s) with p-values
#' @param threshold double. False discovery rate, Default: 0.05
#' @return tbl. The input data frame with an additional column indicating
#'   whether a p-value survived multiple comparison correction
#' @seealso [stats::p.adjust()]
#' @references
#'   Benjamini, Y., & Yekutieli, D. (2001). The control of the false discovery
#'   rate in multiple testing under dependency. *The Annals of Statistics, 29*
#'   (4), 1165–1188.
#' @export
correct_mult_comp <- function(data, var_name, threshold = 0.05) {
  p_vals <- c()
  for (i in 1:length(var_name)){
    p_vals <- c(p_vals, data[[var_name[[i]]]])
  }

  p_cor <- tibble::tibble(p_raw = p_vals) %>%
    dplyr::mutate(
      fdr = ifelse(p.adjust(p_raw, "BY") < threshold, "yes", "no")
    ) %>%
    unique()

  for (i in 1:length(var_name)){
    p_var <- var_name[[i]]
    fdr_var <- stringr::str_c(var_name[[i]], "_fdr")

    data %<>%
      dplyr::left_join(p_cor, by = stats::setNames(nm = p_var, "p_raw")) %>%
      dplyr::rename(
        !!fdr_var := fdr
      )
  }

  data
}
