# =============================================================================
# 01_process_physiology.R
# Physiological data processing (VR condition only).
#
#   Step A  Split each iMotions export into a baseline segment (neutral beach
#           video) and an experiment segment (scenario video), and write
#           ECG-only files for HRV analysis in Kubios.
#   Step B  [manual] Run the ECG files through Kubios HRV Scientific (v4.1.1)
#           and export the results for each segment as CSV to data/raw/kubios/
#           named <id>_baseline_hrv.csv and <id>_experiment_hrv.csv.
#   Step C  Extract HR / HRV statistics from the Kubios exports.
#   Step D  Compute mean EDA (microsiemens) for baseline and experiment.
#
# Inputs : data/raw/imotions/<id>.csv   one iMotions export per participant
#          data/raw/kubios/<id>_{baseline,experiment}_hrv.csv
#          data/raw/group_lookup.csv
# Outputs: data/derived/physio_hrv.csv, data/derived/physio_eda.csv
# =============================================================================

source(file.path("R", "00_setup.R"))

group_lookup <- read.csv(files$group_lookup, colClasses = "character") %>%
  mutate(id = toupper(id),
         scenario = settings$scenario_labels[scenario]) %>%
  distinct()

# ---- Stimulus timings -------------------------------------------------------
# Durations are frame counts / frame rate of each video (ms).
baseline_duration <- 3730 / (1497 / 50) * 1000
video_duration <- c(Control  = 3343 / (30000 / 1001) * 1000,
                    Indirect = 3785 / (30000 / 1001) * 1000,
                    Direct   = 3503 / (30000 / 1001) * 1000)
# Time (ms from scenario video start) at which the staring begins
staring_starts <- c(Control = 51.885, Indirect = 53.987, Direct = 53.386) * 1000
# Time at which the intervention begins (no intervention in Control)
intervention_starts <- c(Control  = unname(video_duration["Control"]),
                         Indirect = (60 + 41.734) * 1000,
                         Direct   = (60 + 37.754) * 1000)

# ---- Step A: segment iMotions exports --------------------------------------
segment_imotions <- function(file) {

  id <- toupper(tools::file_path_sans_ext(basename(file)))
  scenario <- group_lookup$scenario[group_lookup$id == id]
  if (length(scenario) != 1) {
    message("Skipping ", basename(file), ": no unique scenario in lookup")
    return(invisible(NULL))
  }

  raw <- read.csv(file, header = FALSE, colClasses = "character")
  # iMotions exports begin with a block of metadata; the column header row
  # is the first row whose first cell is "Row".
  header_row <- which(trimws(raw[[1]]) == "Row")[1]
  names(raw) <- raw[header_row, ]
  dat <- raw[(header_row + 1):nrow(raw), ] %>%
    janitor::clean_names() %>%
    mutate(video_play_active = na_if(video_play_active, ""),
           timestamp_num = as.numeric(timestamp),
           start_flag = video_play_active == "video play" &
             is.na(lag(video_play_active)))

  # first video onset = baseline video, second = scenario video
  starts <- which(dat$start_flag %in% TRUE)
  dat$event <- NA_character_
  dat$event[starts[1]] <- "BASELINE START"
  dat$event[starts[2]] <- "VIDEO START"

  baseline_start <- dat$timestamp_num[starts[1]]
  baseline_end   <- baseline_start + baseline_duration
  video_start    <- dat$timestamp_num[starts[2]]
  video_end      <- video_start + video_duration[[scenario]]

  dat <- dat %>%
    mutate(
      segment = case_when(
        event == "BASELINE START" ~ "BASELINE START",
        timestamp_num > baseline_start & timestamp_num < baseline_end ~ "BASELINE VIDEO"
      ),
      segment = case_when(
        is.na(segment) & lag(segment) == "BASELINE VIDEO" ~ "BASELINE END",
        TRUE ~ segment
      ),
      segment = case_when(
        event == "VIDEO START" ~ "VIDEO START",
        timestamp_num > video_start & timestamp_num < video_end ~ "MAIN VIDEO",
        TRUE ~ segment
      ),
      segment = case_when(
        is.na(segment) & lag(segment) == "MAIN VIDEO" ~ "VIDEO END",
        TRUE ~ segment
      )
    )

  video_end_observed <- dat$timestamp_num[which(dat$segment == "VIDEO END")[1]]

  baseline <- dat %>%
    filter(segment %in% c("BASELINE START", "BASELINE VIDEO", "BASELINE END"))

  experiment <- dat %>%
    filter(segment %in% c("VIDEO START", "MAIN VIDEO", "VIDEO END")) %>%
    mutate(phase = case_when(
      timestamp_num > video_start &
        timestamp_num < video_start + staring_starts[[scenario]] ~ "SETUP",
      timestamp_num >= video_start + staring_starts[[scenario]] &
        timestamp_num < video_start + intervention_starts[[scenario]] ~ "STARING",
      timestamp_num >= video_start + intervention_starts[[scenario]] &
        timestamp_num < video_end_observed ~ "INTERVENTION",
      TRUE ~ NA_character_
    ))

  dir.create(files$segments_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(files$for_kubios_dir, recursive = TRUE, showWarnings = FALSE)

  write.csv(baseline,   file.path(files$segments_dir, paste0(id, "_baseline.csv")),
            row.names = FALSE)
  write.csv(experiment, file.path(files$segments_dir, paste0(id, "_experiment.csv")),
            row.names = FALSE)

  # ECG only (timestamp + ECG channel) for Kubios
  write.csv(baseline %>% select(timestamp, channel_9_ecg100c),
            file.path(files$for_kubios_dir, paste0(id, "_baseline.csv")), row.names = FALSE)
  write.csv(experiment %>% select(timestamp, channel_9_ecg100c),
            file.path(files$for_kubios_dir, paste0(id, "_experiment.csv")), row.names = FALSE)

  invisible(id)
}

imotions_files <- list.files(files$imotions_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(imotions_files) > 0) {
  walk(imotions_files, function(f) {
    tryCatch(segment_imotions(f),
             error = function(e) message("Could not process ", basename(f), ": ",
                                         conditionMessage(e)))
  })
} else {
  message("No iMotions exports found in ", files$imotions_dir, "; skipping Step A.")
}

# ---- Step B: Kubios (manual) -----------------------------------------------
# Load each file in data/derived/for_kubios/ into Kubios HRV Scientific,
# apply artefact correction, and save the results report as CSV to
# data/raw/kubios/<id>_baseline_hrv.csv / <id>_experiment_hrv.csv.

# ---- Step C: HRV statistics from Kubios exports ----------------------------
read_kubios <- function(file) {
  dat <- read.csv(file, header = FALSE, colClasses = "character")
  get <- function(label) {
    v <- dat[trimws(dat$V1) == label, "V2"]
    if (length(v) == 0) NA_real_ else as.numeric(trimws(v[1]))
  }
  tibble(
    mean_heart_rate = get("Mean HR (beats/min):"),
    sd_heart_rate   = get("SD HR (beats/min):"),
    min_heart_rate  = get("Min HR (beats/min):"),
    max_heart_rate  = get("Max HR (beats/min):"),
    stress_index    = get("Stress index:"),
    hf_log          = get("HF (log):")
  )
}

kubios_files <- list.files(files$kubios_dir, pattern = "_baseline_hrv\\.csv$")
kubios_ids <- toupper(sub("_baseline_hrv\\.csv$", "", kubios_files))

hrv <- map_dfr(kubios_ids, function(id) {
  b <- file.path(files$kubios_dir, paste0(id, "_baseline_hrv.csv"))
  e <- file.path(files$kubios_dir, paste0(id, "_experiment_hrv.csv"))
  if (!file.exists(b) || !file.exists(e)) return(NULL)  # incomplete recording
  bind_cols(
    tibble(id = id),
    read_kubios(b) %>% rename_with(~ paste0("baseline_", .x)),
    read_kubios(e) %>% rename_with(~ paste0("experiment_", .x))
  )
})

if (nrow(hrv) > 0) {
  hrv <- hrv %>%
    mutate(hf_log_diff       = experiment_hf_log - baseline_hf_log,
           stress_index_diff = experiment_stress_index - baseline_stress_index) %>%
    left_join(group_lookup, by = "id")
  write.csv(hrv, files$physio_hrv, row.names = FALSE)
  message("HRV data written for ", nrow(hrv), " participants.")
}

# ---- Step D: mean EDA -------------------------------------------------------
segment_files <- list.files(files$segments_dir, pattern = "_baseline\\.csv$")
segment_ids <- sub("_baseline\\.csv$", "", segment_files)

mean_eda <- function(file) {
  x <- suppressWarnings(as.numeric(read.csv(file)$channel_13_eda100c))
  mean(x[!is.na(x)])
}

eda <- map_dfr(segment_ids, function(id) {
  b <- file.path(files$segments_dir, paste0(id, "_baseline.csv"))
  e <- file.path(files$segments_dir, paste0(id, "_experiment.csv"))
  if (!file.exists(b) || !file.exists(e)) return(NULL)
  tibble(id = id, eda_mean_baseline = mean_eda(b), eda_mean_exp = mean_eda(e))
})

if (nrow(eda) > 0) {
  eda <- eda %>%
    mutate(eda_diff = eda_mean_exp - eda_mean_baseline) %>%
    left_join(group_lookup, by = "id")
  write.csv(eda, files$physio_eda, row.names = FALSE)
  message("EDA data written for ", nrow(eda), " participants.")
}
