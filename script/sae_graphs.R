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

# Variables de interes
sae_pre_comp_obj <- select(data, starts_with("sae_pre_comp_obj"))
sae_pos_comp_obj <- select(data, starts_with("sae_pos_comp_obj"))
sae_pre_comp_subj <- select(data, starts_with("sae_pre_comp_subj"))
sae_pos_comp_subj <- select(data, starts_with("sae_pos_comp_subj"))
sae_pre_trust_belief_reliab <- select(data, starts_with("sae_pre_trust_belief_reliab"))
sae_pos_trust_belief_reliab <- select(data, starts_with("sae_pos_trust_belief_reliab"))
sae_pre_trust_belief_func <- select(data, starts_with("sae_pre_trust_belief_func"))
sae_pos_trust_belief_func <- select(data, starts_with("sae_pos_trust_belief_func"))
sae_pre_trust_belief_help <- select(data, starts_with("sae_pre_trust_belief_help"))
sae_pos_trust_belief_help <- select(data, starts_with("sae_pos_trust_belief_help"))
sae_pre_trust_func_reliab <- select(data, starts_with("sae_pre_trust_func_reliab"))
sae_pos_trust_func_reliab <- select(data, starts_with("sae_pos_trust_func_reliab"))
sae_pre_trust_func_capab <- select(data, starts_with("sae_pre_trust_func_capab"))
sae_pos_trust_func_capab <- select(data, starts_with("sae_pos_trust_func_capab"))
sae_pre_trust_moral_ethi <- select(data, starts_with("sae_pre_trust_moral_ethi"))
sae_pos_trust_moral_ethi <- select(data, starts_with("sae_pos_trust_moral_ethi"))
sae_pre_trust_moral_sinc <- select(data, starts_with("sae_pre_trust_moral_sinc"))
sae_pos_trust_moral_sinc <- select(data, starts_with("sae_pos_trust_moral_sinc"))


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
pre_stats_obj_sae <- describe_stats(sae_pre_comp_obj)
pos_stats_obj_sae <- describe_stats(sae_pos_comp_obj)
pre_stats_subj_sae <- describe_stats(sae_pre_comp_subj)
pos_stats_subj_sae <- describe_stats(sae_pos_comp_subj)
pre_stats_trust_belief_reliab_sae <- describe_stats(sae_pre_trust_belief_reliab)
pos_stats_trust_belief_reliab_sae <- describe_stats(sae_pos_trust_belief_reliab)
pre_stats_trust_belief_func_sae <- describe_stats(sae_pre_trust_belief_func)
pos_stats_trust_belief_func_sae <- describe_stats(sae_pos_trust_belief_func)
pre_stats_trust_belief_help_sae <- describe_stats(sae_pre_trust_belief_help)
pos_stats_trust_belief_help_sae <- describe_stats(sae_pos_trust_belief_help)
pre_stats_trust_func_reliab_sae <- describe_stats(sae_pre_trust_func_reliab)
pos_stats_trust_func_reliab_sae <- describe_stats(sae_pos_trust_func_reliab)
pre_stats_trust_func_capab_sae <- describe_stats(sae_pre_trust_func_capab)
pos_stats_trust_func_capab_sae <- describe_stats(sae_pos_trust_func_capab)
pre_stats_trust_moral_ethi_sae <- describe_stats(sae_pre_trust_moral_ethi)
pos_stats_trust_moral_ethi_sae <- describe_stats(sae_pos_trust_moral_ethi)
pre_stats_trust_moral_sinc_sae <- describe_stats(sae_pre_trust_moral_sinc)
pos_stats_trust_moral_sinc_sae <- describe_stats(sae_pos_trust_moral_sinc)


# Create a table with the statistics for each variable
stats_table <- list(
  sae_pre_comp_obj = pre_stats_obj_sae,
  sae_pos_comp_obj = pos_stats_obj_sae,
  sae_pre_comp_subj = pre_stats_subj_sae,
  sae_pos_comp_subj = pos_stats_subj_sae,
  sae_pre_trust_belief_reliab = pre_stats_trust_belief_reliab_sae,
  sae_pos_trust_belief_reliab = pos_stats_trust_belief_reliab_sae,
  sae_pre_trust_belief_func = pre_stats_trust_belief_func_sae,
  sae_pos_trust_belief_func = pos_stats_trust_belief_func_sae,
  sae_pre_trust_belief_help = pre_stats_trust_belief_help_sae,
  sae_pos_trust_belief_help = pos_stats_trust_belief_help_sae,
  sae_pre_trust_func_reliab = pre_stats_trust_func_reliab_sae,
  sae_pos_trust_func_reliab = pos_stats_trust_func_reliab_sae,
  sae_pre_trust_func_capab = pre_stats_trust_func_capab_sae,
  sae_pos_trust_func_capab = pos_stats_trust_func_capab_sae,
  sae_pre_trust_moral_ethi = pre_stats_trust_moral_ethi_sae,
  sae_pos_trust_moral_ethi = pos_stats_trust_moral_ethi_sae,
  sae_pre_trust_moral_sinc = pre_stats_trust_moral_sinc_sae,
  sae_pos_trust_moral_sinc = pos_stats_trust_moral_sinc_sae
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


# Create plots for each Hypothesis
# H2 - algorithm disclosure
# Subjective 
pos_subj_sae_AlgD_0 <- data %>%
  filter(disclosure == 0) %>%
  select(starts_with("sae_pos_comp_subj"))
pos_subj_sae_AlgD_1 <- data %>%
  filter(disclosure == 1) %>%
  select(starts_with("sae_pos_comp_subj"))

anova2_plot <- make_anova2_plot(pos_subj_sae_AlgD_0, pos_subj_sae_AlgD_1)$plot


# Objective
pos_obj_sae_AlgD_0 <- data %>%
  filter(disclosure == 0) %>%
  select(starts_with("sae_pos_comp_obj"))
pos_obj_sae_AlgD_1 <- data %>%
  filter(disclosure == 1) %>%
  select(starts_with("sae_pos_comp_obj"))

anova2_plot <- make_anova2_plot(pos_obj_sae_AlgD_0, pos_obj_sae_AlgD_1)$plot

# Functional
pos_trustFunc_sae_AlgD_0 <- data %>%
  filter(disclosure == 0) %>%
  select(starts_with("sae_pos_trust_belief_reliab", "sae_pos_trust_belief_func"))
pos_trustFunc_sae_AlgD_1 <- data %>%
  filter(disclosure == 1) %>%
  select(starts_with("sae_pos_trust_belief_reliab", "sae_pos_trust_belief_func"))

anova2_plot <- make_anova2_plot(pos_trustFunc_sae_AlgD_0, pos_trustFunc_sae_AlgD_1)$plot

# Moral Ethics
pos_trustMoral_sae_AlgD_0 <- data %>%
  filter(disclosure == 0) %>%
  select(starts_with("sae_pos_trust_belief_help"))
pos_trustMoral_sae_AlgD_1 <- data %>%
  filter(disclosure == 1) %>%
  select(starts_with("sae_pos_trust_belief_help"))

anova2_plot <- make_anova2_plot(pos_trustMoral_sae_AlgD_0, pos_trustMoral_sae_AlgD_1)$plot


# H4 - situational information
# Subjective 
pos_subj_sae_SI_0 <- data %>%
  filter(inf_situacional == 0) %>%
  select(starts_with("sae_pos_comp_subj"))
pos_subj_sae_SI_1 <- data %>%
  filter(inf_situacional == 1) %>%
  select(starts_with("sae_pos_comp_subj"))

anova2_plot <- make_anova2_plot(pos_subj_sae_SI_0, pos_subj_sae_SI_1)$plot


# Objective
pos_obj_sae_SI_0 <- data %>%
  filter(inf_situacional == 0) %>%
  select(starts_with("sae_pos_comp_obj"))
pos_obj_sae_SI_1 <- data %>%
  filter(inf_situacional == 1) %>%
  select(starts_with("sae_pos_comp_obj"))

anova2_plot <- make_anova2_plot(pos_obj_sae_SI_0, pos_obj_sae_SI_1)$plot

# Functional
pos_trustFunc_sae_SI_0 <- data %>%
  filter(inf_situacional == 0) %>%
  select(starts_with("sae_pos_trust_belief_reliab", "sae_pos_trust_belief_func"))
pos_trustFunc_sae_SI_1 <- data %>%
  filter(inf_situacional == 1) %>%
  select(starts_with("sae_pos_trust_belief_reliab", "sae_pos_trust_belief_func"))

anova2_plot <- make_anova2_plot(pos_trustFunc_sae_SI_0, pos_trustFunc_sae_SI_1)$plot

# Moral Ethics
pos_trustMoral_sae_SI_0 <- data %>%
  filter(inf_situacional == 0) %>%
  select(starts_with("sae_pos_trust_belief_help"))
pos_trustMoral_sae_SI_1 <- data %>%
  filter(inf_situacional == 1) %>%
  select(starts_with("sae_pos_trust_belief_help"))

anova2_plot <- make_anova2_plot(pos_trustMoral_sae_SI_0, pos_trustMoral_sae_SI_1)$plot


# H5 - control humano
# Subjective 
pos_subj_sae_CH_0 <- data %>%
  filter(control_humano == 0) %>%
  select(starts_with("sae_pos_comp_subj"))
pos_subj_sae_CH_1 <- data %>%
  filter(control_humano == 1) %>%
  select(starts_with("sae_pos_comp_subj"))

anova2_plot <- make_anova2_plot(pos_subj_sae_CH_0, pos_subj_sae_CH_1)$plot


# Objective
pos_obj_sae_CH_0 <- data %>%
  filter(control_humano == 0) %>%
  select(starts_with("sae_pos_comp_obj"))
pos_obj_sae_CH_1 <- data %>%
  filter(control_humano == 1) %>%
  select(starts_with("sae_pos_comp_obj"))

anova2_plot <- make_anova2_plot(pos_obj_sae_CH_0, pos_obj_sae_CH_1)$plot

# Functional
pos_trustFunc_sae_CH_0 <- data %>%
  filter(control_humano == 0) %>%
  select(starts_with("sae_pos_trust_belief_reliab", "sae_pos_trust_belief_func"))
pos_trustFunc_sae_CH_1 <- data %>%
  filter(control_humano == 1) %>%
  select(starts_with("sae_pos_trust_belief_reliab", "sae_pos_trust_belief_func"))

anova2_plot <- make_anova2_plot(pos_trustFunc_sae_CH_0, pos_trustFunc_sae_CH_1)$plot

# Moral Ethics
pos_trustMoral_sae_CH_0 <- data %>%
  filter(control_humano == 0) %>%
  select(starts_with("sae_pos_trust_belief_help"))
pos_trustMoral_sae_CH_1 <- data %>%
  filter(control_humano == 1) %>%
  select(starts_with("sae_pos_trust_belief_help"))

anova2_plot <- make_anova2_plot(pos_trustMoral_sae_CH_0, pos_trustMoral_sae_CH_1)$plot




