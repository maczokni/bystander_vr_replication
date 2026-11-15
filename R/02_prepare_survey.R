# =============================================================================
# 02_prepare_survey.R
# Combine the VR and written survey exports, attach condition and
# demographics, score the scales, and write one analysis dataset.
#
# Inputs : data/raw/survey_vr.csv, data/raw/survey_written.csv,
#          data/raw/group_lookup.csv, data/raw/demographics.csv,
#          data/raw/id_corrections.csv (optional)
# Output : data/derived/analysis_data.csv
#          (no free-text answers or Qualtrics metadata are retained)
# =============================================================================

source(file.path("R", "00_setup.R"))

# ---- Participant ID cleaning ------------------------------------------------
# Participants typed their own study ID into the survey. IDs are upper-cased,
# hyphens replaced with underscores, and known typos corrected using an
# optional lookup (columns: raw_id, id).
id_corrections <- if (file.exists(files$id_corrections)) {
  read.csv(files$id_corrections, colClasses = "character")
} else {
  tibble(raw_id = character(), id = character())
}

clean_id <- function(x) {
  x <- str_replace_all(toupper(trimws(x)), "-", "_")
  fix <- match(x, toupper(id_corrections$raw_id))
  ifelse(is.na(fix), x, id_corrections$id[fix])
}

# ---- Read the two survey exports -------------------------------------------
read_survey <- function(file, skip_rows, format) {
  dat <- read.csv(file, colClasses = "character") %>%
    janitor::clean_names()
  dat <- dat[(skip_rows + 1):nrow(dat), ]
  dat %>% mutate(format = format)
}

survey_vr <- read_survey(files$survey_vr, settings$skip_rows_vr, "VR") %>%
  # one participant has a duplicate, empty response; keep responses with data
  filter(nchar(q2_1) > 0)

survey_written <- read_survey(files$survey_written, settings$skip_rows_written, "written")

survey <- bind_rows(survey_vr, survey_written) %>%
  mutate(id = clean_id(q1_1))

# ---- Condition --------------------------------------------------------------
group_lookup <- read.csv(files$group_lookup, colClasses = "character") %>%
  mutate(id = toupper(id)) %>%
  distinct()

survey <- survey %>%
  left_join(group_lookup, by = "id") %>%
  mutate(scenario = as_scenario(settings$scenario_labels[scenario]))

# Two participants entered the same ID. Both are kept: the second occurrence
# is relabelled with the suffix "A", matching its record in the demographics
# file. It keeps the condition assigned to the original ID.
dup <- duplicated(survey$id)
survey$id[dup] <- paste0(survey$id[dup], "A")

# ---- Demographics -----------------------------------------------------------
demographics <- read.csv(files$demographics, colClasses = "character") %>%
  mutate(id = toupper(id),
         gender = ifelse(gender %in% c("m", "f"), toupper(gender), gender))

survey <- survey %>% left_join(demographics, by = "id")
# Every participant should have a demographic record
if (any(is.na(survey$gender))) {
  warning("No demographic record for: ",
          paste(survey$id[is.na(survey$gender)], collapse = ", "))
}

# ---- Q3.1 Classification as sexual harassment -------------------------------
survey <- survey %>%
  mutate(
    q3_1 = factor(q3_1, levels = c("Definitely not sexual harassment",
                                   "Probably not sexual harassment",
                                   "I'm not sure",
                                   "Probably sexual harassment",
                                   "Definitely sexual harassment")),
    harassment_grouped = case_when(
      q3_1 %in% c("Definitely not sexual harassment",
                  "Probably not sexual harassment") ~ "Not sexual harassment",
      q3_1 %in% c("Probably sexual harassment",
                  "Definitely sexual harassment")   ~ "Sexual harassment",
      q3_1 == "I'm not sure"                        ~ "I'm not sure"),
    harassment_grouped = factor(harassment_grouped,
                                levels = c("Not sexual harassment",
                                           "I'm not sure",
                                           "Sexual harassment"))
  )

# ---- Q4.1 / Q4.2 Bystander noticing and helpfulness -------------------------
survey <- survey %>%
  mutate(
    noticed = factor(q4_1, levels = union(c("Yes", "No", "Not sure"),
                                          sort(unique(q4_1)))),
    helped  = ifelse(q4_2 == "...Help in any way", "Yes", "No or Not sure"),
    helped  = factor(helped, levels = c("No or Not sure", "Yes"))
  )

# ---- PANAS ------------------------------------------------------------------
panas_items <- paste0("q5_1_", 1:20)
positive_items <- paste0("q5_1_", c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19))
negative_items <- paste0("q5_1_", c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20))

survey <- survey %>%
  mutate(across(all_of(panas_items), panas_to_num)) %>%
  mutate(pos_score = rowSums(across(all_of(positive_items))),
         neg_score = rowSums(across(all_of(negative_items))))

# ---- Realism (6 items, item 1 reversed) and presence (13 items) -------------
# Item q6_1_7 (nausea, VR only) is not part of the realism score.
presence_reversed <- paste0("q7_1_", c(3, 4, 9, 13))
presence_forward  <- setdiff(paste0("q7_1_", 1:13), presence_reversed)

survey <- survey %>%
  mutate(
    q6_1_1 = agree_to_num(q6_1_1, reverse = TRUE),
    across(paste0("q6_1_", 2:7), agree_to_num),
    across(all_of(presence_forward), agree_to_num),
    across(all_of(presence_reversed), ~ agree_to_num(.x, reverse = TRUE)),
    realism_score  = rowSums(across(paste0("q6_1_", 1:6))),
    presence_score = rowSums(across(paste0("q7_1_", 1:13)))
  )

# ---- Keep only the variables needed for the analysis ------------------------
analysis_data <- survey %>%
  select(id, format, scenario, gender, any_of("age"),
         q3_1, harassment_grouped, noticed, helped,
         all_of(panas_items), pos_score, neg_score,
         paste0("q6_1_", 1:7), realism_score,
         paste0("q7_1_", 1:13), presence_score)

write.csv(analysis_data, files$analysis_data, row.names = FALSE)

# ---- Sample description ----------------------------------------------------
sample_description <- analysis_data %>%
  group_by(format) %>%
  summarise(n = n(),
            female = sum(gender == "F", na.rm = TRUE),
            male = sum(gender == "M", na.rm = TRUE),
            other_gender = sum(!is.na(gender) & !gender %in% c("F", "M")),
            .groups = "drop") %>%
  bind_rows(analysis_data %>%
              summarise(format = "Total", n = n(),
                        female = sum(gender == "F", na.rm = TRUE),
                        male = sum(gender == "M", na.rm = TRUE),
                        other_gender = sum(!is.na(gender) & !gender %in% c("F", "M"))))

if ("age" %in% names(analysis_data)) {
  ages <- analysis_data %>%
    mutate(age = as.numeric(age)) %>%
    group_by(format) %>%
    summarise(mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE)) %>%
    bind_rows(analysis_data %>% mutate(age = as.numeric(age)) %>%
                summarise(format = "Total", mean_age = mean(age, na.rm = TRUE),
                          sd_age = sd(age, na.rm = TRUE)))
  sample_description <- left_join(sample_description, ages, by = "format")
}

write.csv(sample_description, file.path(paths$tables, "sample_description.csv"),
          row.names = FALSE)
print(sample_description)
print(table(analysis_data$format, analysis_data$scenario, useNA = "ifany"))
