# =============================================================================
# 07_summary_table.R
# Table 3: summary of results for all hypotheses
# =============================================================================

source(file.path("R", "00_setup.R"))

order <- c("H1", "H2", "H3", "H4a", "H4b", "H5a", "H5b", "H6a", "H6b", "H7a", "H7b")

table3 <- readRDS(results_file) %>%
  mutate(h = sub(":.*", "", hypothesis),
         h = factor(h, levels = order)) %>%
  arrange(h) %>%
  select(rq, hypothesis, result)

print(table3, n = Inf)
write.csv(table3, file.path(paths$tables, "table_3_summary_of_results.csv"), row.names = FALSE)

cat(sprintf("\nBonferroni-corrected alpha across %d tests: %.4f\n",
            settings$n_hypothesis_tests, settings$alpha_bonferroni))
