# 04_prereg_outputs.R
# PHDBA297B Pilot Study
# Outputs for preregistration + presentation figures (ggplot2, grayscale)
#
# Generates:
#   1) Table 3 confirmatory model summary (txt + html)
#   2) Figure: coefficient plot (Model B prereg confirmatory)
#   3) Figure: interaction visualization (AI × TaskStd), predicted P(top category)
#
# Notes:
#   - Uses only columns present in analysis_constructed.csv.
#   - Drops age deterministically if age + tenure + experience all exist (small-N collinearity).
#   - Avoids Pandoc dependency (DOCX export is not required; HTML is saved).
#   - Uses ordinal::clm for ordinal logistic regression.
#   - Uses predict.clm(type="prob") for interaction plot (no ggeffects needed).

source(here::here("code/00_setup.R"))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(janitor)
  library(ordinal)
  library(ggplot2)
  library(modelsummary)
})

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
df <- read_csv(in_path, show_col_types = FALSE) %>%
  clean_names()

# Harmonize hours column (you observed hours_week exists)
if ("hours_week" %in% names(df) && !("hours_per_week" %in% names(df))) {
  df <- rename(df, hours_per_week = hours_week)
}

# -----------------------------
# Helpers
# -----------------------------
have <- function(x) x %in% names(df)

as_ordered_from_numeric <- function(x, label = "DV") {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  levs <- sort(unique(x_num[!is.na(x_num)]))
  if (length(levs) < 2) stop(label, " has <2 observed levels after numeric coercion.")
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

save_txt <- function(x, path) capture.output(x, file = path)

# -----------------------------
# Build ordinal DV: evaluation_outcome_ord
# -----------------------------
if (!have("evaluation_outcome")) stop("Missing column: evaluation_outcome")

df$evaluation_outcome_ord <- as_ordered_from_numeric(df$evaluation_outcome, "evaluation_outcome")
message("evaluation_outcome_ord levels used: ", paste(levels(df$evaluation_outcome_ord), collapse = ", "))

# -----------------------------
# Required predictors (prereg core)
# -----------------------------
required_main <- c("ai_intensity_index", "task_standardization_index")
missing_main <- setdiff(required_main, names(df))
if (length(missing_main) > 0) stop("Missing required predictors: ", paste(missing_main, collapse = ", "))

# -----------------------------
# Preregistered covariates (only keep those that exist)
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

# Deterministic small-N collinearity rule:
# If age + tenure + experience exist together, drop age.
if (all(c("age", "tenure_years", "experience_years") %in% covars_present)) {
  covars_present <- setdiff(covars_present, "age")
  writeLines(
    "Dropped 'age' (rule) to reduce deterministic collinearity with tenure_years + experience_years in small N.",
    con = here::here("output/logs", "decision_drop_age_prereg_outputs.txt")
  )
  message("NOTE: Dropped 'age' (rule) to reduce deterministic collinearity in small N.")
}

# -----------------------------
# Prepare factor covariates (collapse rare levels for stability)
# -----------------------------
dfB <- df

if ("job_level" %in% names(dfB)) {
  dfB$job_level <- collapse_rare(dfB$job_level, min_n = 10, other = "Other")
  save_txt(table(df$job_level, useNA = "ifany"),  here::here("output/logs/freq_job_level_raw_prereg.txt"))
  save_txt(table(dfB$job_level, useNA = "ifany"), here::here("output/logs/freq_job_level_collapsed_prereg.txt"))
}
if ("job_function" %in% names(dfB)) {
  dfB$job_function <- collapse_rare(dfB$job_function, min_n = 10, other = "Other")
  save_txt(table(df$job_function, useNA = "ifany"),  here::here("output/logs/freq_job_function_raw_prereg.txt"))
  save_txt(table(dfB$job_function, useNA = "ifany"), here::here("output/logs/freq_job_function_collapsed_prereg.txt"))
}

# -----------------------------
# Preregistered confirmatory model (Model B)
# DV: evaluation_outcome_ord
# -----------------------------
rhs_B <- c(
  "ai_intensity_index",
  "task_standardization_index",
  "ai_intensity_index:task_standardization_index",
  intersect(covars_present, names(dfB))
)

fml_B <- as.formula(paste("evaluation_outcome_ord ~", paste(rhs_B, collapse = " + ")))

mB <- safe_clm(fml_B, data = dfB)

# If NA SEs appear widely (often due to sparse factors), refit dropping the sparse factors.
if (!inherits(mB, "error")) {
  se_vec <- tryCatch(coef(summary(mB))[,"Std. Error"], error = function(e) NA)
  if (any(is.na(se_vec))) {
    writeLines(
      "Model produced NA SEs; refitting without job_level/job_function for stability (still prereg covariates where feasible).",
      con = here::here("output/logs", "fallback_drop_sparse_factors_prereg_outputs.txt")
    )
    rhs_B2 <- setdiff(rhs_B, c("job_level", "job_function"))
    fml_B2 <- as.formula(paste("evaluation_outcome_ord ~", paste(rhs_B2, collapse = " + ")))
    mB <- safe_clm(fml_B2, data = dfB)
  }
}

if (inherits(mB, "error")) {
  stop("Preregistered Model B failed: ", conditionMessage(mB))
}

# Make prereg model object explicit for downstream plotting
m_prereg <- mB

# -----------------------------
# Save Table 3 (txt + html)
# -----------------------------
table_txt <- here::here("output/tables/table3_confirmatory_model.txt")
save_txt(summary(m_prereg), table_txt)
message("Saved prereg model summary: ", table_txt)

# HTML table (journal-ish)
table_html <- here::here("output/tables/table3_confirmatory_model.html")
tryCatch(
  modelsummary::modelsummary(
    list("Confirmatory (preregistered)" = m_prereg),
    output = table_html,
    exponentiate = FALSE,
    statistic = "({std.error})",
    stars = TRUE,
    fmt = 3
  ),
  error = function(e) {
    writeLines(
      paste("HTML table export failed:", conditionMessage(e)),
      con = here::here("output/logs", "modelsummary_html_fail_prereg_modelB_eval.txt")
    )
  }
)

# -----------------------------
# Figure 1: Preregistered coefficient plot (Model B)
# Focus on H1 + H2 terms (AI, TaskStd, AI×TaskStd)
# -----------------------------
coef_mat <- coef(summary(m_prereg))
coef_df <- tibble(
  term = rownames(coef_mat),
  estimate = coef_mat[, "Estimate"],
  se = coef_mat[, "Std. Error"]
) %>%
  mutate(
    conf_low = estimate - 1.96 * se,
    conf_high = estimate + 1.96 * se
  )

# Keep only the prereg focal terms
focal_terms <- c("ai_intensity_index",
                 "task_standardization_index",
                 "ai_intensity_index:task_standardization_index")

coef_focal <- coef_df %>%
  filter(term %in% focal_terms) %>%
  mutate(
    term_label = case_when(
      term == "ai_intensity_index" ~ "AI intensity (H1)",
      term == "task_standardization_index" ~ "Task standardization",
      term == "ai_intensity_index:task_standardization_index" ~ "AI × TaskStd (H2)",
      TRUE ~ term
    ),
    term_label = factor(term_label, levels = c("AI intensity (H1)",
                                              "AI × TaskStd (H2)",
                                              "Task standardization"))
  )

p_coef <- ggplot(coef_focal, aes(x = estimate, y = term_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high),
                width = 0.15, linewidth = 0.6) +
  labs(
    x = "Log-odds coefficient (95% CI)",
    y = NULL,
    title = "Preregistered estimates (Model B)",
    subtitle = "Outcome: evaluation_outcome (ordinal)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(here::here("output/figs/fig_prereg_coef_modelB_eval.png"), p_coef,
       width = 7.2, height = 3.6, dpi = 300)
ggsave(here::here("output/figs/fig_prereg_coef_modelB_eval.pdf"), p_coef,
       width = 7.2, height = 3.6)
message("Saved coef plots: output/figs/fig_prereg_coef_modelB_eval.(png|pdf)")

# -----------------------------
# Figure 2: Interaction visualization (AI × TaskStd)
# Plot predicted probability of TOP outcome category across AI,
# with TaskStd at Low (25th pct) vs High (75th pct)
# -----------------------------
# Use the dataset used for model fit
df_plot <- dfB

# Choose low/high task standardization
ts_low  <- as.numeric(quantile(df_plot$task_standardization_index, 0.25, na.rm = TRUE))
ts_high <- as.numeric(quantile(df_plot$task_standardization_index, 0.75, na.rm = TRUE))

ai_seq <- seq(
  min(df_plot$ai_intensity_index, na.rm = TRUE),
  max(df_plot$ai_intensity_index, na.rm = TRUE),
  length.out = 60
)

# Helper for "typical" covariate values
mean_or_mode <- function(x) {
  if (is.numeric(x)) return(mean(x, na.rm = TRUE))
  # factor/character: mode
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Build baseline row with typical values for covariates in the model
needed_vars <- all.vars(formula(m_prereg))  # includes DV; we'll ignore DV
needed_vars <- setdiff(needed_vars, "evaluation_outcome_ord")

base_row <- lapply(needed_vars, function(v) mean_or_mode(df_plot[[v]]))
names(base_row) <- needed_vars
base_row <- as.data.frame(base_row, stringsAsFactors = FALSE)

# Ensure factor columns have correct type
for (v in names(base_row)) {
  if (v %in% c("job_level", "job_function") && v %in% names(df_plot)) {
    base_row[[v]] <- factor(base_row[[v]], levels = levels(as.factor(df_plot[[v]])))
  }
}

make_grid <- function(ts_value, ts_label) {
  g <- base_row[rep(1, length(ai_seq)), , drop = FALSE]
  g$ai_intensity_index <- ai_seq
  g$task_standardization_index <- ts_value
  g$taskstd_group <- ts_label
  g
}

grid_low  <- make_grid(ts_low,  "Low task standardization (25th pct)")
grid_high <- make_grid(ts_high, "High task standardization (75th pct)")
newdata <- bind_rows(grid_low, grid_high)

# predict probabilities (robust to different return shapes)
pred_obj <- predict(m_prereg, newdata = newdata, type = "prob")

# Some versions return a matrix; others return a list containing a matrix
prob_mat <- pred_obj
if (is.list(pred_obj) && !is.null(pred_obj$fit)) prob_mat <- pred_obj$fit
if (is.list(pred_obj) && !is.null(pred_obj$prob)) prob_mat <- pred_obj$prob

# Ensure we have a matrix with one column per outcome level
if (is.vector(prob_mat)) {
  # infer K from outcome levels
  ylev <- levels(df_plot$evaluation_outcome_ord)
  K <- length(ylev)
  if (K < 2) stop("Could not infer outcome levels for interaction plot.")
  if (length(prob_mat) != nrow(newdata) * K) {
    stop("predict() returned a vector of unexpected length; cannot reshape.")
  }
  prob_mat <- matrix(prob_mat, nrow = nrow(newdata), ncol = K, byrow = TRUE)
  colnames(prob_mat) <- ylev
} else {
  prob_mat <- as.matrix(prob_mat)
  if (is.null(colnames(prob_mat))) {
    colnames(prob_mat) <- levels(df_plot$evaluation_outcome_ord)
  }
}

# Combine categories 4 and 5 as "high performance"
levs <- colnames(prob_mat)

if (!all(c("4", "5") %in% levs)) {
  stop("Expected outcome levels 4 and 5 not found in probability matrix.")
}

newdata$prob_high_perf <- prob_mat[, "4"] + prob_mat[, "5"]

p_int <- ggplot(newdata, aes(x = ai_intensity_index, y = prob_high_perf, linetype = taskstd_group)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "AI intensity index",
    y = "Predicted probability of high evaluation (≥4)",
    title = "Preregistered interaction visualization",
    subtitle = "AI intensity × task standardization (holding covariates at typical values)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave(here::here("output/figs/fig_interaction_ai_x_taskstd_highperf.png"), p_int,
       width = 7.2, height = 4.0, dpi = 300)
ggsave(here::here("output/figs/fig_interaction_ai_x_taskstd_highperf.pdf"), p_int,
       width = 7.2, height = 4.0)
message("Saved interaction plots: output/figs/fig_interaction_ai_x_taskstd_topprob.(png|pdf)")

message("Done.")