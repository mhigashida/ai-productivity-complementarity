# ai_productivity_exploratory_models.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
  library(broom)
  library(gt)
  library(glue)
})

DATA_PATH <- "data/processed/analysis_ready.csv"
OUT_DIR_TABLES <- "output/tables"
dir.create(OUT_DIR_TABLES, recursive = TRUE, showWarnings = FALSE)

safe_numeric <- function(x) suppressWarnings(as.numeric(x))

zscore_na <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - m) / s
}

center_na <- function(x) {
  m <- mean(x, na.rm = TRUE)
  x - m
}

fit_polr <- function(formula, data) {
  MASS::polr(
    formula = formula,
    data = data,
    Hess = TRUE,
    method = "logistic",
    na.action = na.omit
  )
}

tidy_polr <- function(model, model_name, outcome_name) {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(!str_detect(term, "\\|")) %>%
    mutate(
      model = model_name,
      outcome = outcome_name,
      p.value = 2 * pnorm(abs(statistic), lower.tail = FALSE),
      sig = case_when(
        p.value < .001 ~ "***",
        p.value < .01  ~ "**",
        p.value < .05  ~ "*",
        TRUE ~ ""
      ),
      estimate_fmt = sprintf("%.3f", estimate),
      conf.low_fmt = sprintf("%.3f", conf.low),
      conf.high_fmt = sprintf("%.3f", conf.high),
      coef_ci = glue("{estimate_fmt} [{conf.low_fmt}, {conf.high_fmt}]")
    ) %>%
        dplyr::select(outcome, model, term, estimate, conf.low, conf.high, p.value, sig, coef_ci)
}

pretty_term <- function(x) {
  case_when(
    x == "ai_intensity" ~ "AI intensity",
    x == "task_standardization" ~ "Task standardization",
    x == "digital_comfort" ~ "Digital comfort",
    x == "ai_intensity:task_standardization" ~ "AI intensity × task standardization",
    x == "ai_intensity:digital_comfort" ~ "AI intensity × digital comfort",
    x == "task_standardization:digital_comfort" ~ "Task standardization × digital comfort",
    x == "ai_intensity:task_standardization:digital_comfort" ~ "AI intensity × task standardization × digital comfort",
    x == "tenure_years" ~ "Tenure",
    x == "experience_years" ~ "Experience",
    x == "hours_week" ~ "Weekly hours",
    TRUE ~ x
  )
}

df_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

df <- df_raw %>%
  mutate(
    ai_frequency = safe_numeric(ai_frequency),
    ai_percent = safe_numeric(ai_percent),
    ai_integration = safe_numeric(ai_integration),

    task_clarity = safe_numeric(task_clarity),
    performance_criteria = safe_numeric(performance_criteria),
    solution_clarity = safe_numeric(solution_clarity),
    task_judgment = safe_numeric(task_judgment),

    digital_comfort = safe_numeric(digital_comfort),
    tenure_years = safe_numeric(tenure_years),
    experience_years = safe_numeric(experience_years),
    hours_week = safe_numeric(hours_week),

    relative_performance = safe_numeric(relative_performance),
    evaluation_outcome = safe_numeric(evaluation_outcome),

    task_judgment_rev = 6 - task_judgment,

    ai_frequency_z   = zscore_na(ai_frequency),
    ai_percent_z     = zscore_na(ai_percent),
    ai_integration_z = zscore_na(ai_integration),

    ai_intensity = rowMeans(
      cbind(ai_frequency_z, ai_percent_z, ai_integration_z),
      na.rm = FALSE
    ),

    task_standardization = rowMeans(
      cbind(task_clarity, performance_criteria, solution_clarity, task_judgment_rev),
      na.rm = FALSE
    )
  )

# Collapse rare category in evaluation outcome: 2 -> 3
df <- df %>%
  mutate(
    evaluation_outcome_collapsed = case_when(
      evaluation_outcome == 2 ~ 3,
      TRUE ~ evaluation_outcome
    ),
    relative_performance_collapsed = case_when(
      relative_performance == 2 ~ 3,
      TRUE ~ relative_performance
    )
  )

# Primary sample
df_eval <- df %>%
  filter(!is.na(evaluation_outcome_collapsed)) %>%
  mutate(
    eval_outcome_ord = ordered(evaluation_outcome_collapsed)
  )

# Supplementary sample
df_rel <- df %>%
  filter(!is.na(relative_performance_collapsed)) %>%
  mutate(
    relative_performance_ord = ordered(relative_performance_collapsed)
  )

# Mean-center
center_vars <- c(
  "ai_intensity",
  "task_standardization",
  "digital_comfort",
  "tenure_years",
  "experience_years",
  "hours_week"
)

center_df <- function(dat) {
  dat %>%
    mutate(across(all_of(center_vars), center_na))
}

df_eval <- center_df(df_eval)
df_rel  <- center_df(df_rel)

cat("\nRows in eval sample:", nrow(df_eval), "\n")
cat("Rows in relative performance sample:", nrow(df_rel), "\n")

cat("\nEvaluation outcome distribution after collapsing:\n")
print(table(df_eval$eval_outcome_ord, useNA = "ifany"))

cat("\nRelative performance distribution after collapsing:\n")
print(table(df_rel$relative_performance_ord, useNA = "ifany"))

# Simpler controls only
controls <- c("tenure_years", "experience_years", "hours_week")

make_formula <- function(outcome, rhs_terms) {
  as.formula(paste(outcome, "~", paste(rhs_terms, collapse = " + ")))
}

rhs_eval_1 <- c("ai_intensity", "task_standardization", "ai_intensity:task_standardization", controls)
rhs_eval_2 <- c("ai_intensity", "task_standardization", "digital_comfort", "ai_intensity:digital_comfort", controls)
rhs_eval_3 <- c("ai_intensity * task_standardization * digital_comfort", controls)

rhs_rel_1 <- c("ai_intensity", "task_standardization", "ai_intensity:task_standardization", controls)
rhs_rel_2 <- c("ai_intensity", "task_standardization", "digital_comfort", "ai_intensity:digital_comfort", controls)
rhs_rel_3 <- c("ai_intensity * task_standardization * digital_comfort", controls)

m_eval_1 <- fit_polr(make_formula("eval_outcome_ord", rhs_eval_1), df_eval)
m_eval_2 <- fit_polr(make_formula("eval_outcome_ord", rhs_eval_2), df_eval)
m_eval_3 <- fit_polr(make_formula("eval_outcome_ord", rhs_eval_3), df_eval)

m_rel_1 <- fit_polr(make_formula("relative_performance_ord", rhs_rel_1), df_rel)
m_rel_2 <- fit_polr(make_formula("relative_performance_ord", rhs_rel_2), df_rel)
m_rel_3 <- fit_polr(make_formula("relative_performance_ord", rhs_rel_3), df_rel)

results_long <- bind_rows(
  tidy_polr(m_eval_1, "Model 1: Baseline", "Formal evaluation outcome"),
  tidy_polr(m_eval_2, "Model 2: AI × digital comfort", "Formal evaluation outcome"),
  tidy_polr(m_eval_3, "Model 3: Three-way interaction", "Formal evaluation outcome"),
  tidy_polr(m_rel_1,  "Model 1: Baseline", "Relative performance"),
  tidy_polr(m_rel_2,  "Model 2: AI × digital comfort", "Relative performance"),
  tidy_polr(m_rel_3,  "Model 3: Three-way interaction", "Relative performance")
) %>%
  mutate(term_pretty = pretty_term(term))

write_csv(results_long, file.path(OUT_DIR_TABLES, "tab_exploratory_models.csv"))

terms_to_keep <- c(
  "AI intensity",
  "Task standardization",
  "Digital comfort",
  "AI intensity × task standardization",
  "AI intensity × digital comfort",
  "Task standardization × digital comfort",
  "AI intensity × task standardization × digital comfort"
)

table_wide <- results_long %>%
  filter(term_pretty %in% terms_to_keep) %>%
  mutate(display = paste0(coef_ci, sig)) %>%
  dplyr::select(outcome, model, term_pretty, display) %>%
  pivot_wider(names_from = model, values_from = display) %>%
  arrange(outcome, match(term_pretty, terms_to_keep))

gt_tbl <- table_wide %>%
  gt(groupname_col = "outcome", rowname_col = "term_pretty") %>%
  tab_header(
    title = md("**Table 2**"),
    subtitle = md("**Exploratory ordinal logistic regression models**")
  ) %>%
  cols_label(
    `Model 1: Baseline` = "Model 1",
    `Model 2: AI × digital comfort` = "Model 2",
    `Model 3: Three-way interaction` = "Model 3"
  ) %>%
  tab_source_note(
    source_note = md(
      "Entries are log-odds coefficients with 95% confidence intervals in brackets. Model 1 includes AI intensity, task standardization, and their interaction. Model 2 adds digital comfort and the AI intensity × digital comfort interaction. Model 3 includes the full three-way interaction among AI intensity, task standardization, and digital comfort. Continuous predictors are mean-centered. The formal evaluation outcome collapses the sparse lowest category into the adjacent category for estimation stability. *p* < .05, **p** < .01, ***p*** < .001."
    )
  ) %>%
  opt_table_font(font = list(default_fonts())) %>%
  cols_align(align = "center", columns = everything())

gtsave(gt_tbl, file.path(OUT_DIR_TABLES, "tab_exploratory_models.png"))

message("Done.")