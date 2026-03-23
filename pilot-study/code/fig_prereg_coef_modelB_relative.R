# code/fig_prereg_coef_modelB_relative.R
# Vertical coefficient plot (FOCAL TERMS ONLY) for relative performance DV
# Mirrors fig_coef_prereg_modelB_eval.R

source(here::here("code/00_setup.R"))

library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(ordinal)

# -----------------------------
# Paths
# -----------------------------
in_path <- here::here("data/processed/analysis_constructed.csv")
out_dir <- here::here("output/figs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_png <- file.path(out_dir, "fig_prereg_coef_modelB_relative_focal_vertical.png")
out_pdf <- file.path(out_dir, "fig_prereg_coef_modelB_relative_focal_vertical.pdf")

# -----------------------------
# Load + clean
# -----------------------------
df <- read_csv(in_path, show_col_types = FALSE) |>
  clean_names()

# Harmonize hours column name if needed
if ("hours_week" %in% names(df) && !("hours_per_week" %in% names(df))) {
  df <- rename(df, hours_per_week = hours_week)
}

# -----------------------------
# Ordered DV helper
# -----------------------------
as_ordered_from_numeric <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  levs <- sort(unique(x_num[!is.na(x_num)]))
  ordered(x_num, levels = levs)
}

# Relative performance as ordered DV
df$relative_performance_ord <- as_ordered_from_numeric(df$relative_performance)

# -----------------------------
# "Model B" specification (same RHS)
# -----------------------------
fml <- relative_performance_ord ~
  ai_intensity_index +
  task_standardization_index +
  ai_intensity_index:task_standardization_index +
  tenure_years +
  experience_years +
  hours_per_week +
  digital_comfort

m <- clm(fml, data = df, link = "logit")

# -----------------------------
# Extract focal coefficients
# -----------------------------
sm <- summary(m)
coef_tbl <- as.data.frame(sm$coefficients)
coef_tbl$term <- rownames(coef_tbl)
rownames(coef_tbl) <- NULL

focal_terms <- c(
  "ai_intensity_index",
  "task_standardization_index",
  "ai_intensity_index:task_standardization_index"
)

coef_focal <- coef_tbl |>
  filter(term %in% focal_terms) |>
  mutate(
    estimate = Estimate,
    se = `Std. Error`,
    conf_low = Estimate - 1.96 * `Std. Error`,
    conf_high = Estimate + 1.96 * `Std. Error`,
    term_label = case_when(
      term == "ai_intensity_index" ~ "AI intensity",
      term == "task_standardization_index" ~ "Task standardization",
      term == "ai_intensity_index:task_standardization_index" ~ "AI × Task standardization"
    )
  )

coef_focal$term_label <- factor(
  coef_focal$term_label,
  levels = c("AI intensity", "Task standardization", "AI × Task standardization")
)

# -----------------------------
# Plot (vertical coefficient plot)
# -----------------------------
p <- ggplot(coef_focal, aes(x = term_label, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_point(size = 3) +
  labs(
    x = NULL,
    y = "Coefficient (log-odds)"
  ) +
  theme_classic(base_size = 17) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

ggsave(out_png, p, width = 11, height = 6, dpi = 300)
ggsave(out_pdf, p, width = 11, height = 6)

message("Saved clean coefficient plot for relative performance.")