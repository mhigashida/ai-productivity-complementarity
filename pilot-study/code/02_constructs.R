# 02_constructs.R
# PHDBA297B Pilot Study
# Purpose: Construct preregistered indices (AI intensity, task standardization),
#          compute reliability, and write constructed dataset.

source(here::here("code/00_setup.R"))

# -----------------------------
# Paths
# -----------------------------
in_path  <- here::here("data/processed/analysis_ready.csv")
out_path <- here::here("data/processed/analysis_constructed.csv")

dir.create(here::here("output/logs"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Read
# -----------------------------
df <- readr::read_csv(in_path, col_types = cols(.default = col_guess()))

# -----------------------------
# Helpers
# -----------------------------
z <- function(x) as.numeric(scale(x))

rev_1to5 <- function(x) {
  # reverse 1-5 Likert: 1<->5, 2<->4, 3 stays 3
  ifelse(is.na(x), NA_real_, 6 - x)
}

# Convert a column to numeric robustly:
# - if it's already numeric -> keep
# - if it's character like "4" -> as.numeric works
# - if it's factor -> as.character then as.numeric
to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(as.character(x)))
}

# -----------------------------
# 1) AI intensity index (preregistered)
# -----------------------------
# NOTE: you confirmed ai_percent_time is named ai_percent
ai_items <- c("ai_frequency", "ai_percent", "ai_integration")
missing_ai <- setdiff(ai_items, names(df))
if (length(missing_ai) > 0) {
  stop("Missing AI items in analysis_ready.csv: ", paste(missing_ai, collapse = ", "))
}

df <- df %>%
  mutate(across(all_of(ai_items), to_numeric))

# z-score components
df <- df %>%
  mutate(
    ai_z_freq = z(ai_frequency),
    ai_z_pct  = z(ai_percent),
    ai_z_int  = z(ai_integration)
  )

# Average z-scores requiring >= 2 observed components per respondent
df <- df %>%
  rowwise() %>%
  mutate(
    ai_intensity_index = {
      vals <- c(ai_z_freq, ai_z_pct, ai_z_int)
      if (sum(!is.na(vals)) >= 2) mean(vals, na.rm = TRUE) else NA_real_
    }
  ) %>%
  ungroup()

# Reliability for raw AI items (not z-scored)
ai_alpha <- psych::alpha(df %>% dplyr::select(all_of(ai_items)))
capture.output(ai_alpha, file = here::here("output/logs/alpha_ai_intensity.txt"))

# -----------------------------
# 2) Task standardization index (preregistered mapping)
# -----------------------------
# task_clarity + performance_criteria + solution_clarity + reverse(task_judgment)
ts_items_raw <- c("task_clarity", "performance_criteria", "solution_clarity", "task_judgment")
missing_ts <- setdiff(ts_items_raw, names(df))
if (length(missing_ts) > 0) {
  stop("Missing task standardization items in analysis_ready.csv: ", paste(missing_ts, collapse = ", "))
}

df <- df %>%
  mutate(across(all_of(ts_items_raw), to_numeric)) %>%
  mutate(task_judgment_rev = rev_1to5(task_judgment))

ts_items_final <- c("task_clarity", "performance_criteria", "solution_clarity", "task_judgment_rev")

df <- df %>%
  mutate(
    task_standardization_index = rowMeans(
      dplyr::select(., all_of(ts_items_final)),
      na.rm = TRUE
    )
  )

ts_alpha <- psych::alpha(df %>% dplyr::select(all_of(ts_items_final)))
capture.output(ts_alpha, file = here::here("output/logs/alpha_task_standardization.txt"))

# -----------------------------
# 3) DV as ordered factor (primary prereg DV)
# -----------------------------
if (!("relative_performance" %in% names(df))) stop("Missing DV: relative_performance")

df <- df %>%
  mutate(relative_performance = to_numeric(relative_performance)) %>%
  mutate(
    relative_performance_ord = ordered(relative_performance, levels = sort(unique(relative_performance)))
  )

# -----------------------------
# 4) QC logs (missingness)
# -----------------------------
qc <- tibble::tibble(
  n_total = nrow(df),
  n_ai_index_nonmissing = sum(!is.na(df$ai_intensity_index)),
  n_ts_index_nonmissing = sum(!is.na(df$task_standardization_index)),
  n_dv_nonmissing = sum(!is.na(df$relative_performance_ord))
)
readr::write_csv(qc, here::here("output/logs/constructs_qc.csv"))

message("Constructs created.")
print(qc)

# -----------------------------
# 5) Save constructed dataset
# -----------------------------
readr::write_csv(df, out_path)
message("Saved: ", out_path)