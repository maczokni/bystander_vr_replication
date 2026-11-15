# =============================================================================
# 00_setup.R
# Packages, file locations, settings and helper functions used by all scripts.
# =============================================================================

# ---- Packages ---------------------------------------------------------------
required_packages <- c(
  "dplyr", "tidyr", "stringr", "readr", "purrr", "tibble", "ggplot2",
  "janitor", "vcd", "broom", "patchwork", "RColorBrewer"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                              logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Please install the following packages first: ",
       paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(broom)
})

# ---- File locations ---------------------------------------------------------
# All paths are relative to the project root (open the .Rproj file first).
# Raw and derived data are NOT included in this repository (see data/README.md).
paths <- list(
  raw          = file.path("data", "raw"),
  derived      = file.path("data", "derived"),
  figures      = file.path("output", "figures"),
  tables       = file.path("output", "tables")
)

files <- list(
  # inputs (place these in data/raw/)
  survey_vr       = file.path(paths$raw, "survey_vr.csv"),
  survey_written  = file.path(paths$raw, "survey_written.csv"),
  group_lookup    = file.path(paths$raw, "group_lookup.csv"),
  demographics    = file.path(paths$raw, "demographics.csv"),
  id_corrections  = file.path(paths$raw, "id_corrections.csv"),  # optional
  imotions_dir    = file.path(paths$raw, "imotions"),
  kubios_dir      = file.path(paths$raw, "kubios"),
  # derived files (created by the scripts)
  segments_dir    = file.path(paths$derived, "physio_segments"),
  for_kubios_dir  = file.path(paths$derived, "for_kubios"),
  physio_hrv      = file.path(paths$derived, "physio_hrv.csv"),
  physio_eda      = file.path(paths$derived, "physio_eda.csv"),
  analysis_data   = file.path(paths$derived, "analysis_data.csv")
)

for (p in c(paths$derived, paths$figures, paths$tables)) {
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
}

# ---- Settings ---------------------------------------------------------------
settings <- list(
  # Qualtrics exports: number of rows to drop from the top of each export once
  # read with read.csv(). These are the two Qualtrics header rows (question
  # text, import IDs) plus pilot/test responses recorded before data collection.
  skip_rows_vr      = 11,
  skip_rows_written = 3,

  # Number of tests treated as one family for the Bonferroni check (H1-H7b)
  n_hypothesis_tests = 11,
  alpha              = 0.05,

  # Scenario coding used in the group lookup file
  scenario_labels = c("1" = "Control", "2" = "Indirect", "3" = "Direct")
)
settings$alpha_bonferroni <- settings$alpha / settings$n_hypothesis_tests

# ---- Helper functions -------------------------------------------------------

# Recode Likert agreement items (realism, presence) to 1-5
agree_to_num <- function(x, reverse = FALSE) {
  out <- case_when(
    x == "Strongly disagree" ~ 1,
    x == "Disagree"          ~ 2,
    x == "Neutral"           ~ 3,
    x == "Agree"             ~ 4,
    x == "Strongly agree"    ~ 5,
    TRUE                     ~ NA_real_
  )
  if (reverse) out <- 6 - out
  out
}

# Recode PANAS intensity items to 1-5. Unanswered items are missing (NA), so
# a participant with an unanswered item has no score on that subscale.
panas_to_num <- function(x) {
  case_when(
    x == "Very slightly or not at all" ~ 1,
    x == "A little"                    ~ 2,
    x == "Moderately"                  ~ 3,
    x == "Quite a bit"                 ~ 4,
    x == "Extremely"                   ~ 5,
    TRUE                               ~ NA_real_
  )
}

# Order the intervention conditions consistently
as_scenario <- function(x) {
  factor(x, levels = c("Control", "Indirect", "Direct"))
}

# Save a ggplot as both PDF and PNG
save_fig <- function(plot, name, width = 7, height = 5) {
  ggsave(file.path(paths$figures, paste0(name, ".pdf")), plot,
         width = width, height = height)
  ggsave(file.path(paths$figures, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300)
  invisible(plot)
}

# Store headline results for the summary table (Table 3)
results_file <- file.path(paths$tables, "results_store.rds")
store_result <- function(rq, hypothesis, result) {
  store <- if (file.exists(results_file)) readRDS(results_file) else tibble()
  store <- store %>%
    filter(!(rq == !!rq & hypothesis == !!hypothesis)) %>%
    bind_rows(tibble(rq = rq, hypothesis = hypothesis, result = result))
  saveRDS(store, results_file)
}

fmt_p <- function(p) {
  ifelse(p < .001, "p < .001", paste0("p = ", sub("^0", "", sprintf("%.3f", p))))
}
