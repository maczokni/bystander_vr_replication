# =============================================================================
# 05_physiology_rq5.R
# RQ5  Physiological arousal across intervention conditions (VR condition only)
#      HRV: HF(log), Figure 6     EDA: mean microsiemens, Figure 7
# =============================================================================

source(file.path("R", "00_setup.R"))

# Outcome for each measure: change from baseline to scenario video
# (experiment minus baseline). Set to "experiment_hf_log" / "eda_mean_exp" to
# analyse scenario-video levels instead of change scores.
hrv_outcome <- "hf_log_diff"
eda_outcome <- "eda_diff"

hrv <- read.csv(files$physio_hrv) %>% mutate(scenario = as_scenario(scenario))
eda <- read.csv(files$physio_eda) %>% mutate(scenario = as_scenario(scenario))

physio_anova <- function(data, outcome, label) {
  f <- reformulate("scenario", response = outcome)
  fit <- aov(f, data = data)
  cat("\n====", label, "====\n")
  print(data %>% group_by(scenario) %>%
          summarise(n = n(), mean = mean(.data[[outcome]], na.rm = TRUE),
                    sd = sd(.data[[outcome]], na.rm = TRUE), .groups = "drop"))
  print(summary(fit))
  cat("\nTukey HSD:\n")
  print(TukeyHSD(fit))
  cat("\nWelch one-way test (unequal variances):\n")
  print(oneway.test(f, data = data))
  fit
}

physio_box <- function(data, outcome, ylab) {
  ggplot(data, aes(x = scenario, y = .data[[outcome]])) +
    geom_boxplot() +
    theme_bw() +
    xlab("") +
    ylab(ylab)
}

# ---- HRV --------------------------------------------------------------------
hrv_fit <- physio_anova(hrv, hrv_outcome, "RQ5 / H5a: HF(log)")
fig6 <- physio_box(hrv, hrv_outcome,
                   "Difference in HF(log) between baseline and experiment")
save_fig(fig6, "figure_6_hrv")

s <- summary(hrv_fit)[[1]]
store_result("RQ5", "H5a: heart rate variability",
             sprintf("F(%d,%d) = %.2f, %s%s", s$Df[1], s$Df[2], s$`F value`[1],
                     fmt_p(s$`Pr(>F)`[1]),
                     ifelse(s$`Pr(>F)`[1] < settings$alpha_bonferroni, "",
                            "; ns after Bonferroni")))

# ---- EDA --------------------------------------------------------------------
eda_fit <- physio_anova(eda, eda_outcome, "RQ5 / H5b: mean EDA")
fig7 <- physio_box(eda, eda_outcome,
                   "Difference in mean EDA (µS) between baseline and experiment")
save_fig(fig7, "figure_7_eda")

s <- summary(eda_fit)[[1]]
store_result("RQ5", "H5b: EDA",
             sprintf("F(%d,%d) = %.2f, %s", s$Df[1], s$Df[2], s$`F value`[1],
                     fmt_p(s$`Pr(>F)`[1])))

# ---- Physiological descriptives ---------------------------------------------
physio_descriptives <- bind_rows(
  hrv %>% group_by(scenario) %>%
    summarise(measure = "HF(log) baseline",   mean = mean(baseline_hf_log),   sd = sd(baseline_hf_log), n = n()),
  hrv %>% group_by(scenario) %>%
    summarise(measure = "HF(log) experiment", mean = mean(experiment_hf_log), sd = sd(experiment_hf_log), n = n()),
  hrv %>% group_by(scenario) %>%
    summarise(measure = "HF(log) change",     mean = mean(hf_log_diff),       sd = sd(hf_log_diff), n = n()),
  eda %>% group_by(scenario) %>%
    summarise(measure = "EDA baseline (uS)",   mean = mean(eda_mean_baseline), sd = sd(eda_mean_baseline), n = n()),
  eda %>% group_by(scenario) %>%
    summarise(measure = "EDA experiment (uS)", mean = mean(eda_mean_exp),      sd = sd(eda_mean_exp), n = n()),
  eda %>% group_by(scenario) %>%
    summarise(measure = "EDA change (uS)",     mean = mean(eda_diff),          sd = sd(eda_diff), n = n())
) %>% relocate(measure)
write.csv(physio_descriptives, file.path(paths$tables, "physiology_descriptives.csv"),
          row.names = FALSE)
