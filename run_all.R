# =============================================================================
# run_all.R
# Reproduces all analyses, figures and tables in the paper.
# Open bystander-vr-replication.Rproj in RStudio, then run this file.
# =============================================================================

# Set to TRUE to re-run the physiological pre-processing from the raw iMotions
# and Kubios exports. If FALSE, the scripts use the derived files already in
# data/derived/ (physio_hrv.csv, physio_eda.csv).
run_physiology_preprocessing <- FALSE

source(file.path("R", "00_setup.R"))
if (file.exists(results_file)) file.remove(results_file)

if (run_physiology_preprocessing) source(file.path("R", "01_process_physiology.R"))

source(file.path("R", "02_prepare_survey.R"))
source(file.path("R", "03_perceptions_rq1_rq3.R"))
source(file.path("R", "04_affect_rq4.R"))
source(file.path("R", "05_physiology_rq5.R"))
source(file.path("R", "06_format_rq6_rq7.R"))
source(file.path("R", "07_summary_table.R"))

writeLines(capture.output(sessionInfo()), file.path("output", "session_info.txt"))
message("Done. Figures are in output/figures, tables in output/tables.")
