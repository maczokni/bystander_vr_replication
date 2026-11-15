# =============================================================================
# 04_affect_rq4.R
# RQ4  Self-reported affect (PANAS) across intervention conditions
#      Figures 5a and 5b, Table 1
# =============================================================================

source(file.path("R", "00_setup.R"))

dat <- read.csv(files$analysis_data) %>%
  mutate(scenario = as_scenario(scenario))

# ---- Descriptives -----------------------------------------------------------
panas_descriptives <- dat %>%
  group_by(scenario) %>%
  summarise(n = n(),
            positive_n = sum(!is.na(pos_score)),
            positive_mean = mean(pos_score, na.rm = TRUE), positive_sd = sd(pos_score, na.rm = TRUE),
            negative_n = sum(!is.na(neg_score)),
            negative_mean = mean(neg_score, na.rm = TRUE), negative_sd = sd(neg_score, na.rm = TRUE),
            .groups = "drop")
print(panas_descriptives)
write.csv(panas_descriptives, file.path(paths$tables, "panas_descriptives.csv"),
          row.names = FALSE)

# ---- Figures 5a and 5b ------------------------------------------------------
fig5a <- ggplot(dat, aes(x = scenario, y = pos_score)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Positive PANAS score")
save_fig(fig5a, "figure_5a_panas_positive", width = 5, height = 4)

fig5b <- ggplot(dat, aes(x = scenario, y = neg_score)) +
  geom_boxplot() +
  theme_bw() +
  xlab("") +
  ylab("Negative PANAS score")
save_fig(fig5b, "figure_5b_panas_negative", width = 5, height = 4)

# ---- Table 1: linear models (Control is the reference category) ------------
panas_pos <- lm(pos_score ~ scenario, data = dat)
panas_neg <- lm(neg_score ~ scenario, data = dat)
summary(panas_pos)
summary(panas_neg)

model_table <- function(model, outcome) {
  g <- glance(model)
  tidy(model) %>%
    mutate(outcome = outcome,
           adj_r_squared = g$adj.r.squared,
           f_statistic = g$statistic,
           df1 = g$df, df2 = g$df.residual,
           model_p = g$p.value) %>%
    relocate(outcome)
}

table1 <- bind_rows(model_table(panas_pos, "Positive affect"),
                    model_table(panas_neg, "Negative affect"))
write.csv(table1, file.path(paths$tables, "table_1_panas_by_scenario.csv"), row.names = FALSE)

for (m in list(list(panas_pos, "H4a: positive affect"),
               list(panas_neg, "H4b: negative affect"))) {
  g <- glance(m[[1]])
  store_result("RQ4", m[[2]],
               sprintf("F(%d,%d) = %.2f, %s, adj. R2 = %.3f",
                       g$df, g$df.residual, g$statistic, fmt_p(g$p.value),
                       g$adj.r.squared))
}
