# code/fig_coef_matrix_eval_and_relative.R
# Journal-style coefficient "matrix" (two models side-by-side)
# - Model A: evaluation_outcome (ordinal)
# - Model B: relative_performance (ordinal)
# Points: log-odds coefficients; bars: 95% Wald CI

source(here::here("code/00_setup.R"))

# -----------------------------
# Paths
# -----------------------------
in_path <- here::here("data/processed/analysis_constructed.csv")
out_png <- here::here("output/figs/fig_coef_matrix_eval_relative.png")
out_pdf <- here::here("output/figs/fig_coef_matrix_eval_relative.pdf")

dir.create(here::here("output/figs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("output/logs"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Load data
# -----------------------------
df <- readr::read_csv(in_path, show_col_types = FALSE) |>
  janitor::clean_names()

if ("hours_week" %in% names(df) && !("hours_per_week" %in% names(df))) {
  df <- dplyr::rename(df, hours_per_week = hours_week)
}

# -----------------------------
# Helpers
# -----------------------------
as_ordered_from_numeric <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  levs <- sort(unique(x_num[!is.na(x_num)]))
  if (length(levs) < 2) stop("DV has <2 observed levels after numeric coercion.")
  ordered(x_num, levels = levs)
}

center_vars <- function(dat, vars) {
  for (v in vars) dat[[v]] <- dat[[v]] - mean(dat[[v]], na.rm = TRUE)
  dat
}

tidy_clm_slopes <- function(m, model_label) {
  ct <- summary(m)$coefficients |> as.data.frame()
  ct$term <- rownames(ct)

  ct |>
    dplyr::filter(!stringr::str_detect(term, "\\|")) |>
    dplyr::transmute(
      model = model_label,
      term  = term,
      estimate = Estimate,
      se = `Std. Error`,
      conf.low  = Estimate - 1.96 * `Std. Error`,
      conf.high = Estimate + 1.96 * `Std. Error`
    )
}

# -----------------------------
# Vars used in both models
# -----------------------------
preds <- c(
  "ai_intensity_index",
  "task_standardization_index",
  "digital_comfort",
  "tenure_years",
  "experience_years",
  "hours_per_week"
)

missing_preds <- setdiff(preds, names(df))
if (length(missing_preds) > 0) stop("Missing required columns: ", paste(missing_preds, collapse = ", "))

# -----------------------------
# DVs: ALWAYS coerce here (CSV drops factor class)
# -----------------------------
if (!("evaluation_outcome" %in% names(df))) stop("Missing DV: evaluation_outcome")
if (!("relative_performance" %in% names(df))) stop("Missing DV: relative_performance")

df <- df |>
  dplyr::mutate(
    evaluation_outcome_ord   = as_ordered_from_numeric(evaluation_outcome),
    relative_performance_ord = as_ordered_from_numeric(relative_performance)
  )

# Mean-center continuous predictors (for interaction interpretability)
df <- center_vars(df, preds)

# -----------------------------
# Model formulas
# -----------------------------
rhs <- paste(
  "ai_intensity_index",
  "task_standardization_index",
  "ai_intensity_index:task_standardization_index",
  "tenure_years",
  "experience_years",
  "hours_per_week",
  "digital_comfort",
  sep = " + "
)

fml_eval <- stats::as.formula(paste0("evaluation_outcome_ord ~ ", rhs))
fml_rel  <- stats::as.formula(paste0("relative_performance_ord ~ ", rhs))

m_eval <- ordinal::clm(fml_eval, data = df, link = "logit")
m_rel  <- ordinal::clm(fml_rel,  data = df, link = "logit")

# -----------------------------
# Collect coefficients
# -----------------------------
tab <- dplyr::bind_rows(
  tidy_clm_slopes(m_eval, "Evaluation outcome"),
  tidy_clm_slopes(m_rel,  "Relative performance")
)

term_map <- c(
  "ai_intensity_index" = "AI intensity",
  "task_standardization_index" = "Task standardization",
  "ai_intensity_index:task_standardization_index" = "AI × Task standardization",
  "digital_comfort" = "Digital comfort",
  "tenure_years" = "Tenure (years)",
  "experience_years" = "Experience (years)",
  "hours_per_week" = "Hours/week"
)

tab <- tab |>
  dplyr::mutate(term_label = dplyr::recode(term, !!!term_map, .default = term))

row_order <- c(
  "AI intensity",
  "Task standardization",
  "AI × Task standardization",
  "Digital comfort",
  "Tenure (years)",
  "Experience (years)",
  "Hours/week"
)

tab$term_label <- factor(tab$term_label, levels = rev(row_order))
tab$model <- factor(tab$model, levels = c("Evaluation outcome", "Relative performance"))

# -----------------------------
# Plot
# -----------------------------
p <- ggplot2::ggplot(tab, ggplot2::aes(x = model, y = estimate)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.6) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = conf.low, ymax = conf.high),
    width = 0.18,
    linewidth = 0.9
  ) +
  ggplot2::geom_point(size = 2.6) +
  ggplot2::facet_grid(rows = ggplot2::vars(term_label), switch = "y") +
  ggplot2::labs(x = NULL, y = "Coefficient (log-odds)") +
  ggplot2::theme_classic(base_size = 16) +
  ggplot2::theme(
    strip.background = ggplot2::element_blank(),
    strip.placement = "outside",
    strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0),
    axis.text.x = ggplot2::element_text(size = 12),
    plot.margin = ggplot2::margin(10, 18, 10, 10)
  )

ggplot2::ggsave(out_png, p, width = 11.5, height = 7.2, dpi = 300)
ggplot2::ggsave(out_pdf, p, width = 11.5, height = 7.2)

message("Saved: ", out_png)
message("Saved: ", out_pdf)

cat("\nN (Evaluation outcome model): ", nrow(model.frame(m_eval)), "\n", sep = "")
cat("N (Relative performance model): ", nrow(model.frame(m_rel)), "\n", sep = "")