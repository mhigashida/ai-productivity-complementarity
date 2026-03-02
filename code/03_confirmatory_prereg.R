# 03_confirmatory_prereg.R
# PHDBA297B Pilot Study
#
# Purpose (preregistered confirmatory + clearly labeled exploratory):
#   1) Load: data/processed/analysis_constructed.csv
#   2) Construct two ordinal DVs:
#        - relative_performance_ord   (self-report)
#        - evaluation_outcome_ord     (formal evaluation outcome; more objective proxy)
#   3) Fit confirmatory ordinal logistic models (ordinal::clm):
#        - Model A (parsimonious): focal predictors + continuous controls only
#        - Model B (prereg/full): focal predictors + prereg controls, with deterministic stabilization:
#             * collapse sparse factor levels (job_level, job_function)
#             * if still unstable (NA SEs or singular Hessian), fall back to dropping sparse factors
#   4) Fit Model C (exploratory): 3-way AI × TaskStd × DigitalComfort on evaluation_outcome
#   5) Save outputs:
#        - output/tables/*.txt and *.html
#        - output/logs/* diagnostics + decision logs
#
# IMPORTANT:
#   - Uses ONLY columns that exist in your dataset (no gender, etc.)
#   - Applies a deterministic rule for age collinearity:
#       if age + tenure_years + experience_years exist -> drop age
#   - All deviations/stabilizations are logged under output/logs/

source(here::here("code/00_setup.R"))

# -----------------------------
# Paths + folders
# -----------------------------
in_path <- here::here("data/processed/analysis_constructed.csv")

dir.create(here::here("output/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output/figs"),   recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output/logs"),   recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Read + clean names
# -----------------------------
df <- readr::read_csv(in_path, show_col_types = FALSE) |>
  janitor::clean_names()

# Harmonize hours column (you have hours_week in the raw)
if ("hours_week" %in% names(df) && !("hours_per_week" %in% names(df))) {
  df <- dplyr::rename(df, hours_per_week = hours_week)
}

have <- function(x) x %in% names(df)

# -----------------------------
# Helpers
# -----------------------------
as_ordered_from_numeric <- function(x, dv_name = "dv") {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  levs  <- sort(unique(x_num[!is.na(x_num)]))
  if (length(levs) < 2) stop(dv_name, " has <2 observed levels after numeric coercion.")
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
  tryCatch(
    ordinal::clm(formula, data = data, link = "logit"),
    error = function(e) e
  )
}

is_unstable_clm <- function(model) {
  if (inherits(model, "error")) return(TRUE)

  # NA standard errors
  se <- tryCatch(stats::coef(summary(model))[, "Std. Error"], error = function(e) NA)
  if (any(is.na(se))) return(TRUE)

  # singular / huge condition number often signals trouble
  condH <- tryCatch(model$cond.H, error = function(e) NA)
  if (!is.na(condH) && is.finite(condH) && condH > 1e12) return(TRUE)

  FALSE
}

save_model_outputs <- function(model, stem) {
  txt_path  <- here::here("output/tables", paste0(stem, ".txt"))
  html_path <- here::here("output/tables", paste0(stem, ".html"))

  if (inherits(model, "error")) {
    cat("MODEL FAILED:\n", conditionMessage(model), "\n", file = txt_path)
    message("Saved failure log: ", txt_path)
    return(invisible(NULL))
  }

  capture.output(summary(model), file = txt_path)

  # modelsummary can fail for some clm objects; guard it
  tryCatch(
    modelsummary::modelsummary(
      setNames(list(model), stem),
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
# DVs: reconstruct ordered factors (CSV won't preserve factor class)
# -----------------------------
if (!have("relative_performance")) stop("Missing column: relative_performance")
df$relative_performance_ord <- as_ordered_from_numeric(df$relative_performance, "relative_performance")
message("relative_performance_ord levels used (ordered): ",
        paste(levels(df$relative_performance_ord), collapse = ", "))

if (!have("evaluation_outcome")) stop("Missing column: evaluation_outcome")
df$evaluation_outcome_ord <- as_ordered_from_numeric(df$evaluation_outcome, "evaluation_outcome")
message("evaluation_outcome_ord levels used (ordered): ",
        paste(levels(df$evaluation_outcome_ord), collapse = ", "))

# -----------------------------
# Focal prereg predictors
# -----------------------------
required_main <- c("ai_intensity_index", "task_standardization_index")
missing_main <- setdiff(required_main, names(df))
if (length(missing_main) > 0) stop("Missing required predictors: ", paste(missing_main, collapse = ", "))

# -----------------------------
# Preregistered covariates (use only what exists)
# -----------------------------
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

# Deterministic collinearity rule (pilot stability):
# If age + tenure + experience exist -> drop age
if (all(c("age", "tenure_years", "experience_years") %in% covars_present)) {
  covars_present <- setdiff(covars_present, "age")
  cat(
    "Dropped 'age' because tenure_years + experience_years are also present (deterministic collinearity rule for small N).\n",
    file = here::here("output/logs", "decision_drop_age.txt")
  )
  message("NOTE: 'age' excluded by rule to reduce deterministic collinearity with tenure/experience in this pilot sample.")
}

# Minimal controls: continuous-ish only (avoid sparse factors)
controls_min <- intersect(
  c("tenure_years", "experience_years", "hours_per_week", "digital_comfort"),
  names(df)
)

# -----------------------------
# Diagnostics: factor sparsity (raw)
# -----------------------------
if (have("job_level")) {
  capture.output(table(df$job_level, useNA = "ifany"),
                 file = here::here("output/logs", "freq_job_level_raw.txt"))
}
if (have("job_function")) {
  capture.output(table(df$job_function, useNA = "ifany"),
                 file = here::here("output/logs", "freq_job_function_raw.txt"))
}

# -----------------------------
# Fit Model A and Model B for a given DV
# -----------------------------
fit_models_for_dv <- function(dv_ord_col, dv_tag) {
  # ---- Model A: parsimonious confirmatory (stable benchmark)
  rhs_A <- c(
    "ai_intensity_index",
    "task_standardization_index",
    "ai_intensity_index:task_standardization_index",
    controls_min
  )
  fml_A <- as.formula(paste(dv_ord_col, "~", paste(rhs_A, collapse = " + ")))
  mA <- safe_clm(fml_A, data = df)
  save_model_outputs(mA, paste0("modelA_", dv_tag, "_minimal_clm"))

  # ---- Model B: prereg/full controls with deterministic stabilization
  dfB <- df

  # Collapse rare factor levels (deterministic)
  if (have("job_level"))    dfB$job_level    <- collapse_rare(dfB$job_level,    min_n = 10, other = "Other")
  if (have("job_function")) dfB$job_function <- collapse_rare(dfB$job_function, min_n = 10, other = "Other")

  # Diagnostics after collapsing
  if (have("job_level")) {
    capture.output(table(dfB$job_level, useNA = "ifany"),
                   file = here::here("output/logs", paste0("freq_", dv_tag, "_collapsed_job_level.txt")))
    capture.output(table(dfB$job_level, dfB[[dv_ord_col]], useNA = "ifany"),
                   file = here::here("output/logs", paste0("crosstab_", dv_tag, "_collapsed_job_level_by_", dv_ord_col, ".txt")))
  }
  if (have("job_function")) {
    capture.output(table(dfB$job_function, useNA = "ifany"),
                   file = here::here("output/logs", paste0("freq_", dv_tag, "_collapsed_job_function.txt")))
    capture.output(table(dfB$job_function, dfB[[dv_ord_col]], useNA = "ifany"),
                   file = here::here("output/logs", paste0("crosstab_", dv_tag, "_collapsed_job_function_by_", dv_ord_col, ".txt")))
  }

  rhs_B <- c(
    "ai_intensity_index",
    "task_standardization_index",
    "ai_intensity_index:task_standardization_index",
    intersect(covars_present, names(dfB))
  )

  fml_B <- as.formula(paste(dv_ord_col, "~", paste(rhs_B, collapse = " + ")))
  mB <- safe_clm(fml_B, data = dfB)

  # If unstable, fall back to dropping sparse factors (deterministic rule)
  if (is_unstable_clm(mB)) {
    cat(
      paste0(
        "Model B unstable for ", dv_tag,
        " (NA SEs and/or extreme cond.H). Refit without sparse factors job_level/job_function.\n"
      ),
      file = here::here("output/logs", paste0("fallback_", dv_tag, "_drop_sparse_factors.txt"))
    )

    rhs_B2 <- setdiff(rhs_B, c("job_level", "job_function"))
    fml_B2 <- as.formula(paste(dv_ord_col, "~", paste(rhs_B2, collapse = " + ")))
    mB2 <- safe_clm(fml_B2, data = dfB)

    save_model_outputs(mB2, paste0("modelB_", dv_tag, "_full_collapsed_clm"))
  } else {
    save_model_outputs(mB, paste0("modelB_", dv_tag, "_full_collapsed_clm"))
  }
}

# -----------------------------
# Run confirmatory models (both DVs)
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
  cat(
    "Skipped Model C: missing one of ai_intensity_index, task_standardization_index, digital_comfort.\n",
    file = here::here("output/logs", "skipped_modelC_missing_predictors.txt")
  )
}

message("All models finished.")
message("Tables saved under output/tables/. Diagnostics saved under output/logs/.")