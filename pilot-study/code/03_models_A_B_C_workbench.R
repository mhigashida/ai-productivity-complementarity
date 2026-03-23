# 03_confirmatory_models.R
# PHDBA297B Pilot Study
#
# What this script does (clean + reproducible):
#   1) Loads analysis_constructed.csv
#   2) Builds TWO ordinal DVs:
#        - relative_performance_ord
#        - evaluation_outcome_ord
#   3) Fits confirmatory ordinal logistic models (ordinal::clm):
#        - Model A: minimal controls (continuous covariates only)
#        - Model B: “full” prereg controls, with robust collapsing for sparse factors
#   4) Fits Model C (exploratory): 3-way interaction AI × TaskStd × DigitalComfort
#   5) Saves .txt + .html tables under output/tables/ and diagnostics under output/logs/
#
# Notes:
#   - This script ONLY uses columns that actually exist in the data.
#   - It avoids “age not estimable” by applying a deterministic rule:
#       if age, tenure_years, and experience_years all exist -> drop age (common collinearity in small samples)
#   - It collapses sparse factor levels (job_level, job_function) more aggressively for stability.
#   - If the “full” model still produces NA SEs, it automatically falls back to dropping the sparse factors
#     and logs that decision.

source(here::here("code/00_setup.R"))

# -----------------------------
# Paths + folders
# -----------------------------
in_path <- here::here("data/processed/analysis_constructed.csv")

dir.create(here::here("output/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output/logs"),   recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Read + clean names
# -----------------------------
df <- readr::read_csv(in_path, show_col_types = FALSE) |>
  janitor::clean_names()

# Harmonize hours column (you observed hours_week exists)
if ("hours_week" %in% names(df) && !("hours_per_week" %in% names(df))) {
  df <- dplyr::rename(df, hours_per_week = hours_week)
}

# -----------------------------
# Helpers
# -----------------------------
have <- function(x) x %in% names(df)

as_ordered_from_numeric <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  levs <- sort(unique(x_num[!is.na(x_num)]))
  if (length(levs) < 2) stop("DV has < 2 observed levels after numeric coercion.")
  ordered(x_num, levels = levs)
}

collapse_rare <- function(x, min_n = 10, other = "Other") {
  x <- as.factor(x)
  tab <- table(x, useNA = "no")
  rare <- names(tab)[tab < min_n]
  x2 <- as.character(x)
  x2[x2 %in% rare] <- other
  factor(x2)
}

safe_clm <- function(formula, data) {
  # Wrap clm so script never hard-stops; if it fails, you get a readable error.
  tryCatch(
    ordinal::clm(formula, data = data, link = "logit"),
    error = function(e) e
  )
}

save_model_outputs <- function(model, stem) {
  # model can be an error object; handle both
  txt_path  <- here::here("output/tables", paste0(stem, ".txt"))
  html_path <- here::here("output/tables", paste0(stem, ".html"))

  if (inherits(model, "error")) {
    cat("MODEL FAILED:\n", conditionMessage(model), "\n", file = txt_path)
    message("Saved failure log: ", txt_path)
    return(invisible(NULL))
  }

  capture.output(summary(model), file = txt_path)

  # modelsummary can error on some clm objects; guard it.
  tryCatch(
    modelsummary::modelsummary(
      list(stem = model),
      output = html_path,
      exponentiate = TRUE,
      statistic = "({std.error})",
      stars = TRUE
    ),
    error = function(e) {
      cat("MODEL FIT OK, but modelsummary FAILED:\n", conditionMessage(e), "\n",
          file = here::here("output/logs", paste0("modelsummary_fail_", stem, ".txt")))
    }
  )

  message("Saved: ", txt_path)
  message("Saved: ", html_path)
}

# -----------------------------
# Create ordinal DVs (CSV does not preserve factor classes)
# -----------------------------
if (!have("relative_performance")) stop("Missing column: relative_performance")
df$relative_performance_ord <- as_ordered_from_numeric(df$relative_performance)
message("relative_performance_ord levels used (ordered): ",
        paste(levels(df$relative_performance_ord), collapse = ", "))

if (!have("evaluation_outcome")) stop("Missing column: evaluation_outcome")
df$evaluation_outcome_ord <- as_ordered_from_numeric(df$evaluation_outcome)
message("evaluation_outcome_ord levels used (ordered): ",
        paste(levels(df$evaluation_outcome_ord), collapse = ", "))

# -----------------------------
# Predictors + prereg covariates (use only what exists)
# -----------------------------
required_main <- c("ai_intensity_index", "task_standardization_index")
missing_main <- setdiff(required_main, names(df))
if (length(missing_main) > 0) stop("Missing required predictors: ", paste(missing_main, collapse = ", "))

covars_prereg <- c(
  "age",
  "tenure_years",
  "experience_years",
  "hours_per_week",
  "digital_comfort",
  "job_level",
  "job_function"
)

covars_present <- intersect(covars_prereg, names(df))

# Deterministic collinearity rule (small-sample friendly):
# If you have age + tenure + experience, drop age to avoid rank deficiency.
if (all(c("age", "tenure_years", "experience_years") %in% covars_present)) {
  covars_present <- setdiff(covars_present, "age")
  cat("Dropped 'age' because tenure_years + experience_years are also present (common collinearity in small N).\n",
      file = here::here("output/logs", "decision_drop_age.txt"))
  message("NOTE: 'age' excluded by rule to reduce deterministic collinearity with tenure/experience in this pilot sample.")
}

# Minimal controls = continuous-ish only (no sparse factors)
controls_min <- intersect(c("tenure_years", "experience_years", "hours_per_week", "digital_comfort"), names(df))

# -----------------------------
# Diagnostics: sparsity tables for factors (raw)
# -----------------------------
if (have("job_level")) {
  capture.output(table(df$job_level, useNA = "ifany"),
                 file = here::here("output/logs/freq_job_level_raw.txt"))
}
if (have("job_function")) {
  capture.output(table(df$job_function, useNA = "ifany"),
                 file = here::here("output/logs/freq_job_function_raw.txt"))
}

# -----------------------------
# Model builder (A/B) for a given DV
# -----------------------------
fit_models_for_dv <- function(dv_ord_col, dv_tag) {
  # Model A: minimal controls (stable)
  rhs_A <- c(
    "ai_intensity_index",
    "task_standardization_index",
    "ai_intensity_index:task_standardization_index",
    controls_min
  )
  fml_A <- as.formula(paste(dv_ord_col, "~", paste(rhs_A, collapse = " + ")))
  mA <- safe_clm(fml_A, data = df)
  save_model_outputs(mA, paste0("modelA_", dv_tag, "_minimal_clm"))

  # Model B: full controls w/ collapsed sparse factor levels
  dfB <- df

  if (have("job_level"))    dfB$job_level    <- collapse_rare(dfB$job_level,    min_n = 10, other = "Other")
  if (have("job_function")) dfB$job_function <- collapse_rare(dfB$job_function, min_n = 10, other = "Other")

  # Diagnostics after collapsing
  if (have("job_level")) {
    capture.output(table(dfB$job_level, useNA = "ifany"),
                   file = here::here("output/logs/freq_job_level_collapsed.txt"))
  }
  if (have("job_function")) {
    capture.output(table(dfB$job_function, useNA = "ifany"),
                   file = here::here("output/logs/freq_job_function_collapsed.txt"))
  }

  rhs_B <- c(
    "ai_intensity_index",
    "task_standardization_index",
    "ai_intensity_index:task_standardization_index",
    intersect(covars_present, names(dfB))
  )
  fml_B <- as.formula(paste(dv_ord_col, "~", paste(rhs_B, collapse = " + ")))
  mB <- safe_clm(fml_B, data = dfB)

  # If clm fit returns NA SEs broadly, fall back by dropping sparse factors.
  if (!inherits(mB, "error") && any(is.na(stats::coef(summary(mB))[, "Std. Error"]))) {
    cat("Model B produced NA standard errors; refitting without job_level/job_function.\n",
        file = here::here("output/logs", paste0("fallback_", dv_tag, "_drop_sparse_factors.txt")))
    rhs_B2 <- setdiff(rhs_B, c("job_level", "job_function"))
    fml_B2 <- as.formula(paste(dv_ord_col, "~", paste(rhs_B2, collapse = " + ")))
    mB2 <- safe_clm(fml_B2, data = dfB)
    save_model_outputs(mB2, paste0("modelB_", dv_tag, "_full_collapsed_clm"))
  } else {
    save_model_outputs(mB, paste0("modelB_", dv_tag, "_full_collapsed_clm"))
  }
}

# -----------------------------
# Run confirmatory models for both DVs
# -----------------------------
fit_models_for_dv("relative_performance_ord", "relative_performance")
fit_models_for_dv("evaluation_outcome_ord",   "evaluation_outcome")

# -----------------------------
# Model C (EXPLORATORY): 3-way interaction on evaluation_outcome
# AI × TaskStd × DigitalComfort
# -----------------------------
needed_C <- c("ai_intensity_index", "task_standardization_index", "digital_comfort")
if (all(needed_C %in% names(df))) {

  dfC <- df |>
    dplyr::mutate(
      ai_c = as.numeric(scale(ai_intensity_index, center = TRUE, scale = FALSE)),
      ts_c = as.numeric(scale(task_standardization_index, center = TRUE, scale = FALSE)),
      dc_c = as.numeric(scale(digital_comfort, center = TRUE, scale = FALSE))
    )

  rhs_C <- c(
    "ai_c * ts_c * dc_c",
    intersect(c("tenure_years", "experience_years", "hours_per_week"), names(dfC))
  )
  fml_C <- as.formula(paste("evaluation_outcome_ord ~", paste(rhs_C, collapse = " + ")))

  mC <- safe_clm(fml_C, data = dfC)
  save_model_outputs(mC, "modelC_evaluation_outcome_exploratory_3way_clm")

} else {
  cat("Skipped Model C: missing one of ai_intensity_index, task_standardization_index, digital_comfort.\n",
      file = here::here("output/logs", "skipped_modelC_missing_predictors.txt"))
}

message("All models finished.")
message("Tables saved under output/tables/. Diagnostics saved under output/logs/.")