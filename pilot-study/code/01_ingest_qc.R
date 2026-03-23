# 01_ingest_qc.R
# PHDBA297B Pilot Study
# Purpose: Ingest raw Qualtrics export, apply preregistered exclusions,
#          and write analysis-ready dataset + exclusion log.

source(here::here("pilot-study/code/00_setup.R"))

# -----------------------------
# Paths
# -----------------------------
raw_path <- here::here("data/raw/raw_data.csv")

dir.create(here::here("data/processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output/logs"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Read raw export
# -----------------------------
raw <- readr::read_csv(raw_path, col_types = cols(.default = col_guess()))

n_raw <- nrow(raw)

# Qualtrics exports often include 2 metadata rows at the top:
# Row 1: question text, Row 2: ImportId
# (Your earlier exports had these.)
if (n_raw >= 2) {
  raw2 <- raw %>% dplyr::slice(-(1:2))
} else {
  stop("Raw file has fewer than 2 rows; cannot drop Qualtrics metadata rows.")
}

n_after_drop2 <- nrow(raw2)

# Standardize names
df <- raw2 %>% janitor::clean_names()

# Log column names for reproducibility/debugging
writeLines(names(df), con = here::here("output/logs/columns_after_clean_names.txt"))

# -----------------------------
# Exclusion step: preview/test
# -----------------------------
n_before_preview <- nrow(df)

# Try to find a Qualtrics "distribution channel" type column
# Common variants after clean_names():
candidate_cols <- c(
  "distributionchannel",
  "distribution_channel",
  "distribution_channel_",
  "channel",
  "responsechannel",
  "response_channel"
)
dc_col <- intersect(candidate_cols, names(df))

if (length(dc_col) >= 1) {
  dc <- dc_col[1]
  df <- df %>%
    mutate(.dc = tolower(trimws(as.character(.data[[dc]])))) %>%
    filter(.dc != "preview") %>%
    select(-.dc)
  used_preview_col <- dc
} else {
  # If the column truly doesn't exist, we continue but log it clearly.
  used_preview_col <- NA_character_
  warning(
    "No distribution channel column found; preview/test responses cannot be excluded automatically. ",
    "Check output/logs/columns_after_clean_names.txt"
  )
}

n_after_preview <- nrow(df)

# -----------------------------
# Exclusion step: attention checks
# -----------------------------
n_before_attn <- nrow(df)

# Helper: interpret check_01 robustly (TRUE / "True" / 1)
check01_ok <- function(x) {
  if (is.logical(x)) return(x %in% TRUE)
  if (is.numeric(x)) return(x == 1)
  if (is.character(x)) return(tolower(trimws(x)) %in% c("true", "t", "1", "yes"))
  return(FALSE)
}

if (!("check_01" %in% names(df))) stop("Missing required column: check_01")
if (!("attention_check" %in% names(df))) stop("Missing required column: attention_check")

df <- df %>%
  filter(check01_ok(check_01)) %>%
  filter(attention_check == 4)

n_after_attn <- nrow(df)

# Optional: keep only finished responses if present
n_before_finished <- nrow(df)
if ("finished" %in% names(df)) {
  # finished is often TRUE/FALSE or 1/0
  df <- df %>%
    filter(
      finished == TRUE |
        finished == 1 |
        tolower(as.character(finished)) %in% c("true", "1", "yes")
    )
}
n_after_finished <- nrow(df)

# -----------------------------
# Write outputs
# -----------------------------
out_path <- here::here("data/processed/analysis_ready.csv")
readr::write_csv(df, out_path)

# Exclusion log table
exclusion_log <- tibble::tibble(
  step = c(
    "raw_rows",
    "drop_first_2_metadata_rows",
    "exclude_preview",
    "exclude_attention_checks",
    "exclude_not_finished_optional",
    "final_usable"
  ),
  n = c(
    n_raw,
    n_after_drop2,
    n_after_preview,
    n_after_attn,
    n_after_finished,
    n_after_finished
  ),
  detail = c(
    "Raw Qualtrics export as read by read_csv()",
    "Dropped first two rows (question text + ImportId)",
    ifelse(is.na(used_preview_col),
           "No distribution channel column found (preview exclusion not applied)",
           paste0("Excluded preview using column: ", used_preview_col)),
    "Applied check_01 correct AND attention_check == 4",
    if ("finished" %in% names(raw2)) "Filtered to finished responses (if applicable)" else "No finished column in export",
    paste0("Wrote ", out_path)
  )
)

readr::write_csv(exclusion_log, here::here("output/logs/exclusion_counts.csv"))

# Print summary to console
message("========== Ingest + QC summary ==========")
print(exclusion_log)
message("Final usable N = ", n_after_finished)
message("Saved: ", out_path)
message("Columns logged to: output/logs/columns_after_clean_names.txt")
message("========================================")