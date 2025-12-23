library(psych)
library(dplyr)
library(purrr)
library(ggplot2)

## Load the processed dataset created in `script/proc_data.R`
## This runs the preprocessing script and then extracts the `data` object it creates.
proc_env <- new.env(parent = globalenv())
source(here::here("script/proc_data.R"), local = proc_env)
data <- proc_env$data
stopifnot(exists("data", envir = proc_env), !is.null(data))
# De la columna eliminado, los 1, quitarlos.

# Variables de interes
pre_obj_fonasa <- select(data, starts_with("fonasa_pre_comp_obj"))
pos_obj_fonasa <- select(data, starts_with("fonasa_pos_comp_obj"))
pre_subj_fonasa <- select(data, starts_with("fonasa_pre_comp_subj"))
pos_subj_fonasa <- select(data, starts_with("fonasa_pos_comp_subj"))
pre_trust_belief_reliab_fonasa <- select(data, starts_with("fonasa_pre_trust_belief_reliab")) # Moral Ethic - McKnight
pos_trust_belief_reliab_fonasa <- select(data, starts_with("fonasa_pos_trust_belief_reliab")) # Moral Ethic - McKnight
pre_trust_belief_func_fonasa <- select(data, starts_with("fonasa_pre_trust_belief_func")) # Moral Ethic - McKnight
pos_trust_belief_func_fonasa <- select(data, starts_with("fonasa_pos_trust_belief_func")) # Moral Ethic - McKnight
pre_trust_func_reliab_fonasa <- select(data, starts_with("fonasa_pre_trust_func_reliab")) 
pos_trust_func_reliab_fonasa <- select(data, starts_with("fonasa_pos_trust_func_reliab"))
pre_trust_func_capab_fonasa <- select(data, starts_with("fonasa_pre_trust_func_capab"))
pos_trust_func_capab_fonasa <- select(data, starts_with("fonasa_pos_trust_func_capab"))
pre_trust_moral_ethi_fonasa <- select(data, starts_with("fonasa_pre_trust_moral_ethi"))
pos_trust_moral_ethi_fonasa <- select(data, starts_with("fonasa_pos_trust_moral_ethi"))
pre_trust_moral_sinc_fonasa <- select(data, starts_with("fonasa_pre_trust_moral_sinc"))
pos_trust_moral_sinc_fonasa <- select(data, starts_with("fonasa_pos_trust_moral_sinc"))
pos_trust_belief_help_fonasa <- select(data, starts_with("fonasa_pos_trust_belief_help")) # Functional Ethic - McKnight

# Function to calculate descriptive statistics - returns a tibble with the statistics
describe_stats <- function(data) {
  # Calculate the mean for each respondent (row-wise mean)
  row_means <- rowMeans(data, na.rm = TRUE)
  
  # Calculate overall statistics from these row means
  overall_mean <- mean(row_means, na.rm = TRUE)
  overall_median <- median(row_means, na.rm = TRUE)
  overall_sd <- sd(row_means, na.rm = TRUE)
  overall_min <- min(row_means, na.rm = TRUE)
  overall_max <- max(row_means, na.rm = TRUE)
  
  # Calculate Cronbach's alpha (psych::alpha) and extract only the raw_alpha
  alpha_val <- psych::alpha(data)$total$raw_alpha

  tibble(
    cronbach_alpha = alpha_val,
    mean = overall_mean,
    median = overall_median,
    std = overall_sd,
    min = overall_min,
    max = overall_max
  )
}

# For each of the variables, calculate cronbach's alpha, mean, median, std, min, max
pre_stats_obj_fonasa <- describe_stats(pre_obj_fonasa)
pos_stats_obj_fonasa <- describe_stats(pos_obj_fonasa)
pre_stats_subj_fonasa <- describe_stats(pre_subj_fonasa)
pos_stats_subj_fonasa <- describe_stats(pos_subj_fonasa)
pre_stats_trust_belief_reliab_fonasa <- describe_stats(pre_trust_belief_reliab_fonasa)
pos_stats_trust_belief_reliab_fonasa <- describe_stats(pos_trust_belief_reliab_fonasa)
pre_stats_trust_belief_func_fonasa <- describe_stats(pre_trust_belief_func_fonasa)
pos_stats_trust_belief_func_fonasa <- describe_stats(pos_trust_belief_func_fonasa)
pre_stats_trust_func_reliab_fonasa <- describe_stats(pre_trust_func_reliab_fonasa)
pos_stats_trust_func_reliab_fonasa <- describe_stats(pos_trust_func_reliab_fonasa)
pre_stats_trust_func_capab_fonasa <- describe_stats(pre_trust_func_capab_fonasa)
pos_stats_trust_func_capab_fonasa <- describe_stats(pos_trust_func_capab_fonasa)
pre_stats_trust_moral_ethi_fonasa <- describe_stats(pre_trust_moral_ethi_fonasa)
pos_stats_trust_moral_ethi_fonasa <- describe_stats(pos_trust_moral_ethi_fonasa)
pre_stats_trust_moral_sinc_fonasa <- describe_stats(pre_trust_moral_sinc_fonasa)
pos_stats_trust_moral_sinc_fonasa <- describe_stats(pos_trust_moral_sinc_fonasa)
pos_stats_trust_belief_help_fonasa <- describe_stats(pos_trust_belief_help_fonasa)

# Create a table with the statistics for each variable
stats_table <- list(
  pre_obj_fonasa = pre_stats_obj_fonasa,
  pos_obj_fonasa = pos_stats_obj_fonasa,
  pre_subj_fonasa = pre_stats_subj_fonasa,
  pos_subj_fonasa = pos_stats_subj_fonasa,
  pre_trust_belief_reliab_fonasa = pre_stats_trust_belief_reliab_fonasa,
  pos_trust_belief_reliab_fonasa = pos_stats_trust_belief_reliab_fonasa,
  pre_trust_belief_func_fonasa = pre_stats_trust_belief_func_fonasa,
  pos_trust_belief_func_fonasa = pos_stats_trust_belief_func_fonasa,
  pre_trust_func_reliab_fonasa = pre_stats_trust_func_reliab_fonasa,
  pos_trust_func_reliab_fonasa = pos_stats_trust_func_reliab_fonasa,
  pre_trust_func_capab_fonasa = pre_stats_trust_func_capab_fonasa,
  pos_trust_func_capab_fonasa = pos_stats_trust_func_capab_fonasa,
  pre_trust_moral_ethi_fonasa = pre_stats_trust_moral_ethi_fonasa,
  pos_trust_moral_ethi_fonasa = pos_stats_trust_moral_ethi_fonasa,
  pre_trust_moral_sinc_fonasa = pre_stats_trust_moral_sinc_fonasa,
  pos_trust_moral_sinc_fonasa = pos_stats_trust_moral_sinc_fonasa,
  pos_trust_belief_help_fonasa = pos_stats_trust_belief_help_fonasa
) |>
  imap_dfr(\(stats, var_name) dplyr::mutate(stats, Variable = var_name)) |>
  dplyr::select(Variable, dplyr::everything()) |>
  dplyr::arrange(Variable) |>
  dplyr::rename(
    "Cronbach alpha" = cronbach_alpha,
    "Mean" = mean,
    "Median" = median,
    "Std" = std,
    "Min" = min,
    "Max" = max
  )

# Function to create a two way ANOVA plot for a pair of variable matrices
make_anova2_plot <- function(
  data1,
  data2,
  label1 = deparse(substitute(data1)),
  label2 = deparse(substitute(data2)),
  paired = FALSE,
  ci_level = 0.95,
  y_limits = c(1, 5),
  y_breaks = 1:5
) {
  # Coerce to numeric safely (handles character likert "1".."5"; avoids factor->integer codes)
  to_numeric_df <- function(x) {
    x <- as.data.frame(x)
    x[] <- lapply(x, function(col) suppressWarnings(as.numeric(as.character(col))))
    x
  }

  d1 <- to_numeric_df(data1)
  d2 <- to_numeric_df(data2)

  # Row-wise mean per respondent
  row_means1 <- rowMeans(d1, na.rm = TRUE)
  row_means2 <- rowMeans(d2, na.rm = TRUE)

  # Summary stats + CI for plotting
  summarize_with_ci <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    m <- mean(x)
    s <- stats::sd(x)
    se <- s / sqrt(n)
    alpha <- 1 - ci_level
    tcrit <- stats::qt(1 - alpha / 2, df = n - 1)
    ci <- tcrit * se
    tibble(n = n, mean = m, sd = s, se = se, ci = ci, ymin = m - ci, ymax = m + ci)
  }

  sum1 <- summarize_with_ci(row_means1) |> mutate(Variable = label1)
  sum2 <- summarize_with_ci(row_means2) |> mutate(Variable = label2)
  summary_df <- bind_rows(sum1, sum2) |>
    mutate(
      Variable = factor(Variable, levels = c(label1, label2)),
      label = sprintf("Mean=%.2f\nSD=%.2f", mean, sd)
    )

  # Statistical test (with 2 groups this is equivalent to 1-way ANOVA)
  # If you truly need a 2-way ANOVA (2 factors), we can extend this signature.
  test_df <- tibble(
    value = c(row_means1, row_means2),
    Variable = factor(c(rep(label1, length(row_means1)), rep(label2, length(row_means2))),
                      levels = c(label1, label2))
  ) |>
    filter(!is.na(value))

  # Use the vector form so `paired` is supported (formula method doesn't accept `paired`)
  rm1 <- test_df$value[test_df$Variable == label1]
  rm2 <- test_df$value[test_df$Variable == label2]

  test <- if (paired) {
    # paired requires equal-length vectors (same respondents in same order)
    stats::t.test(rm1, rm2, paired = TRUE)
  } else {
    stats::t.test(rm1, rm2, paired = FALSE)
  }

  # Nudge labels slightly above the error bar tops
  label_nudge_y <- if (!is.null(y_limits) && length(y_limits) == 2) {
    0.03 * diff(y_limits)
  } else {
    0.03 * (max(summary_df$ymax, na.rm = TRUE) - min(summary_df$ymin, na.rm = TRUE))
  }

  p <- ggplot(summary_df, aes(x = Variable, y = mean, group = 1)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.8) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.15, linewidth = 0.8) +
    geom_text(aes(y = ymax, label = label), vjust = 0, nudge_y = label_nudge_y, size = 3.3) +
    labs(x = NULL, y = "Mean (row-wise)") +
    theme_classic(base_size = 12)

  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits, clip = "off")
  }
  if (!is.null(y_breaks)) {
    p <- p + scale_y_continuous(breaks = y_breaks)
  }

  list(
    plot = p,
    summary = summary_df,
    test = test
  )
}

data$ai_disclosure_fonasa<-as.factor(data$ai_disclosure_fonasa)
data$performance_fonasa<-as.factor(data$performance_fonasa)
data$control_humano_fonasa<-as.factor(data$control_humano_fonasa)

# Create plots for each Hypothesis
# H1 - AI Disclosure
# Subjective 
ttest_H1_AID_subj <- t.test(fonasa_comp_subjetiva_pos ~ ai_disclosure_fonasa, data = data)
anova2_H1_AID_plot_subj <- local({
  plot(fonasa_comp_subjetiva_pos ~ ai_disclosure_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H1_AID_subj$statistic)),
      sprintf("df = %.2f", unname(ttest_H1_AID_subj$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H1_AID_subj$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

# Objective
ttest_H1_AID_obj <- t.test(fonasa_comp_objetiva_pos ~ ai_disclosure_fonasa, data = data)
anova2_H1_AID_plot_obj <- local({
  plot(fonasa_comp_objetiva_pos ~ ai_disclosure_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H1_AID_obj$statistic)),
      sprintf("df = %.2f", unname(ttest_H1_AID_obj$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H1_AID_obj$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

# Moral Trust
ttest_H1_ME_trust_McKnight <- t.test(fonasa_conf_belief_moral_pos ~ ai_disclosure_fonasa, data = data)
anova2_H1_ME_plot_trust_McKnight <- local({
  plot(fonasa_conf_belief_moral_pos ~ ai_disclosure_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H1_ME_trust_McKnight$statistic)),
      sprintf("df = %.2f", unname(ttest_H1_ME_trust_McKnight$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H1_ME_trust_McKnight$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

ttest_H1_ME_trust_MDMT <- t.test(fonasa_conf_word_moral_pos ~ ai_disclosure_fonasa, data = data)
anova2_H1_ME_plot_trust_MDMT <- local({
  plot(fonasa_conf_word_moral_pos ~ ai_disclosure_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H1_ME_trust_MDMT$statistic)),
      sprintf("df = %.2f", unname(ttest_H1_ME_trust_MDMT$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H1_ME_trust_MDMT$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})


# H3 - Performance
# Subjective 
ttest_H3_P_subj <- t.test(fonasa_comp_subjetiva_pos ~ performance_fonasa, data = data)
anova2_H3_P_plot_subj <- local({
  plot(fonasa_comp_subjetiva_pos ~ performance_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H3_P_subj$statistic)),
      sprintf("df = %.2f", unname(ttest_H3_P_subj$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H3_P_subj$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

# Objective
ttest_H3_P_obj <- t.test(fonasa_comp_objetiva_pos ~ performance_fonasa, data = data)
anova2_H3_P_plot_obj <- local({
  plot(fonasa_comp_objetiva_pos ~ performance_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H3_P_obj$statistic)),
      sprintf("df = %.2f", unname(ttest_H3_P_obj$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H3_P_obj$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})


# Moral Trust
ttest_H3_ME_trust_McKnight <- t.test(fonasa_conf_belief_moral_pos ~ performance_fonasa, data = data)
anova2_H3_ME_plot_trust_McKnight <- local({
  plot(fonasa_conf_belief_moral_pos ~ performance_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H3_ME_trust_McKnight$statistic)),
      sprintf("df = %.2f", unname(ttest_H3_ME_trust_McKnight$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H3_ME_trust_McKnight$p.value, digits = 3, eps = 0.001))
    ),  
    bty = "n"
  )
  recordPlot()
})

ttest_H3_ME_trust_MDMT <- t.test(fonasa_conf_word_moral_pos ~ performance_fonasa, data = data)
anova2_H3_ME_plot_trust_MDMT <- local({
  plot(fonasa_conf_word_moral_pos ~ performance_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H3_ME_trust_MDMT$statistic)),
      sprintf("df = %.2f", unname(ttest_H3_ME_trust_MDMT$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H3_ME_trust_MDMT$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

# H5 - Human control
# Subjective 
ttest_H5_HC_subj <- t.test(fonasa_comp_subjetiva_pos ~ control_humano_fonasa, data = data)
anova2_H5_HC_plot_subj <- local({
  plot(fonasa_comp_subjetiva_pos ~ control_humano_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H5_HC_subj$statistic)),
      sprintf("df = %.2f", unname(ttest_H5_HC_subj$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H5_HC_subj$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

# Objective
ttest_H5_HC_obj <- t.test(fonasa_comp_objetiva_pos ~ control_humano_fonasa, data = data)
anova2_H5_HC_plot_obj <- local({
  plot(fonasa_comp_objetiva_pos ~ control_humano_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H5_HC_obj$statistic)),
      sprintf("df = %.2f", unname(ttest_H5_HC_obj$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H5_HC_obj$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

# Moral Trust
ttest_H5_HC_trust_McKnight <- t.test(fonasa_conf_belief_moral_pos ~ control_humano_fonasa, data = data)
anova2_H5_HC_plot_trust_McKnight <- local({
  plot(fonasa_conf_belief_moral_pos ~ control_humano_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H5_HC_trust_McKnight$statistic)),
      sprintf("df = %.2f", unname(ttest_H5_HC_trust_McKnight$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H5_HC_trust_McKnight$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

ttest_H5_HC_trust_MDMT <- t.test(fonasa_conf_word_moral_pos ~ control_humano_fonasa, data = data)
anova2_H5_HC_plot_trust_MDMT <- local({
  plot(fonasa_conf_word_moral_pos ~ control_humano_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H5_HC_trust_MDMT$statistic)),
      sprintf("df = %.2f", unname(ttest_H5_HC_trust_MDMT$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H5_HC_trust_MDMT$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

# Functional Trust
ttest_H5_HC_trust_func <- t.test(fonasa_conf_belief_funcional_pos ~ control_humano_fonasa, data = data)
anova2_H5_HC_plot_trust_func <- local({
  plot(fonasa_conf_belief_funcional_pos ~ control_humano_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H5_HC_trust_func$statistic)),
      sprintf("df = %.2f", unname(ttest_H5_HC_trust_func$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H5_HC_trust_func$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

ttest_H5_HC_trust_func_MDMT <- t.test(fonasa_conf_word_funcional_pos ~ control_humano_fonasa, data = data)
anova2_H5_HC_plot_trust_func_McKnight <- local({
  plot(fonasa_conf_word_funcional_pos ~ control_humano_fonasa, data = data)
  legend(
    "topright",
    legend = c(
      sprintf("t = %.3f", unname(ttest_H5_HC_trust_func_MDMT$statistic)),
      sprintf("df = %.2f", unname(ttest_H5_HC_trust_func_MDMT$parameter)),
      sprintf("p-value = %s", format.pval(ttest_H5_HC_trust_func_MDMT$p.value, digits = 3, eps = 0.001))
    ),
    bty = "n"
  )
  recordPlot()
})

