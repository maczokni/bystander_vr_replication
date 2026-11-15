# Data

No data are included in this repository. The scripts expect the files below in
`data/raw/`. Everything in `data/raw/` and `data/derived/` is excluded from version
control by `.gitignore`.

Participant IDs only need to be consistent across files. They should be
study codes, not names, emails or any other identifying information.

## `survey_vr.csv` and `survey_written.csv`

Qualtrics exports (CSV, "use choice text") of the questionnaire, one for each
presentation format. Column names are standardised with `janitor::clean_names()`,
so `Q5.1_1` becomes `q5_1_1`, and so on. The items used are:

| Column | Item |
|---|---|
| `Q1.1` | Participant ID (typed by the participant) |
| `Q2.1` | Open-ended: what the scenario was about (used only to drop empty responses) |
| `Q3.1` | Would you classify the scenario as sexual harassment? (5 options, "Definitely not" to "Definitely") |
| `Q4.1` | Did any of the bystanders notice what was happening? (Yes / No / Not sure) |
| `Q4.2` | Did anyone who saw what was happening... ("...Help in any way", "...Make things worse", "...Both help and make things worse, or", "Didn't help and didn't make it worse") |
| `Q5.1_1`–`Q5.1_20` | PANAS items ("Very slightly or not at all" to "Extremely") |
| `Q6.1_1`–`Q6.1_7` | Realism items ("Strongly disagree" to "Strongly agree") |
| `Q7.1_1`–`Q7.1_13` | Presence items ("Strongly disagree" to "Strongly agree") |

The number of rows dropped from the top of each export (Qualtrics header rows
and pilot responses) is set in `R/00_setup.R` (`settings$skip_rows_vr`,
`settings$skip_rows_written`). Free-text answers and Qualtrics metadata are not
carried into the analysis dataset.

## `group_lookup.csv`

Experimental condition for every participant (both formats).

| Column | Description |
|---|---|
| `id` | Participant ID |
| `scenario` | 1 = control (no intervention), 2 = indirect intervention, 3 = direct intervention |

## `demographics.csv`

| Column | Description |
|---|---|
| `id` | Participant ID |
| `gender` | F, M, or other self-described gender |
| `age` | Age in years (optional) |

## `id_corrections.csv` (optional)

Corrections for participant IDs mistyped in the survey.

| Column | Description |
|---|---|
| `raw_id` | ID as entered (after upper-casing and replacing `-` with `_`) |
| `id` | Correct ID |

## Physiological data (VR condition only)

- `imotions/<id>.csv`: one iMotions export per participant, containing the
  columns `Timestamp`, `Channel 9 (ECG100C)`, `Channel 13 (EDA100C)` and
  `VideoPlayActive`. The export's column header row is the row whose first cell
  is `Row`. The first video onset is the baseline video; the second is the
  scenario video.
- `kubios/<id>_baseline_hrv.csv` and `kubios/<id>_experiment_hrv.csv`: Kubios HRV
  Scientific results reports (CSV) for the ECG segments written by
  `R/01_process_physiology.R` to `data/derived/for_kubios/`.

`R/01_process_physiology.R` writes `data/derived/physio_hrv.csv` and
`data/derived/physio_eda.csv`, which the analysis scripts read.
