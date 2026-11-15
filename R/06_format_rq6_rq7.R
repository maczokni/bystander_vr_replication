# =============================================================================
# 06_format_rq6_rq7.R
# RQ6  Presence and realism: VR vs written scenarios   (Figure 8)
# RQ7  Presentation format, presence, realism and affect (Table 2)
# =============================================================================

source(file.path("R", "00_setup.R"))

dat <- read.csv(files$analysis_data) %>%
  mutate(format = relevel(factor(format), ref = "written"))

# ---- Descriptives -----------------------------------------------------------
format_descriptives <- dat %>%
  group_by(format) %>%
  summarise(across(c(realism_score, presence_score, pos_score, neg_score),
                   list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE),
                        n = ~ sum(!is.na(.x)))),
            .groups = "drop")
print(format_descriptives)
write.csv(format_descriptives, file.path(paths$tables, "format_descriptives.csv"),
          row.names = FALSE)

# ---- Figure 8 ---------------------------------------------------------------
p_realism <- ggplot(dat, aes(x = realism_score, y = format)) +
  geom_boxplot() + coord_flip() + ylab("") + xlab("Realism score") +
  theme_minimal() +
  ggtitle("Realism")

p_presence <- ggplot(dat, aes(x = presence_score, y = format)) +
  geom_boxplot() + coord_flip() + ylab("") + xlab("Presence score") +
  theme_minimal() +
  ggtitle("Presence")

fig8 <- p_realism + p_presence
save_fig(fig8, "figure_8_realism_presence", width = 8, height = 4.5)

# ---- RQ6: t-tests (equal variances) -----------------------------------------
t_presence <- t.test(presence_score ~ format, data = dat, var.equal = TRUE)
t_realism  <- t.test(realism_score ~ format, data = dat, var.equal = TRUE)
print(t_presence)
print(t_realism)

# t.test compares written - VR; report as VR - written
report_t <- function(tt) {
  sprintf("t = %.2f, df = %d, %s, 95%% CI [%.2f, %.2f]",
          -tt$statistic, as.integer(tt$parameter), fmt_p(tt$p.value),
          -tt$conf.int[2], -tt$conf.int[1])
}
store_result("RQ6", "H6a: presence", report_t(t_presence))
store_result("RQ6", "H6b: realism",  report_t(t_realism))

# ---- RQ7 / Table 2: affect ~ presence + realism + format ---------------------
emotion_pos <- lm(pos_score ~ presence_score + realism_score + format, data = dat)
emotion_neg <- lm(neg_score ~ presence_score + realism_score + format, data = dat)
summary(emotion_pos)
summary(emotion_neg)

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

table2 <- bind_rows(model_table(emotion_pos, "Positive affect"),
                    model_table(emotion_neg, "Negative affect"))
write.csv(table2, file.path(paths$tables, "table_2_panas_presence_realism_format.csv"),
          row.names = FALSE)

for (m in list(list(emotion_pos, "H7a: format and positive affect"),
               list(emotion_neg, "H7b: format and negative affect"))) {
  co <- tidy(m[[1]]) %>% filter(term == "formatVR")
  store_result("RQ7", m[[2]],
               sprintf("estimate = %.2f, t = %.2f, %s%s", co$estimate, co$statistic,
                       fmt_p(co$p.value),
                       ifelse(co$p.value < settings$alpha &
                                co$p.value >= settings$alpha_bonferroni,
                              "; ns after Bonferroni", "")))
}
