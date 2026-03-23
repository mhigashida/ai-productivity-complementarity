# code/fig_configuration_3way_facets.R
# Configuration Patterns in AI Productivity
# Predicted Pr(high evaluation >= 4) across AI intensity,
# faceted by Task Standardization (low/high), with lines for Digital Comfort (low/high).
# Ordinal logit (ordinal::clm) with centered predictors and 3-way interaction.
#
# Outputs:
#   output/figs/fig_config_ai_task_dc_facets.(png|pdf)

source(here::here("pilot-study/code/00_setup.R"))

# -----------------------------
# Paths
# -----------------------------
in_path <- here::here("data/processed/analysis_constructed.csv")
out_png <- here::here("output/figs/fig_config_ai_task_dc_facets.png")
out_pdf <- here::here("output/figs/fig_config_ai_task_dc_facets.pdf")
dir.create(here::here("output/figs"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Load data
# -----------------------------
df <- readr::read_csv(in_path, show_col_types = FALSE) |>
  janitor::clean_names()

# Harmonize hours column
if ("hours_week" %in% names(df) && !("hours_per_week" %in% names(df))) {
  df <- dplyr::rename(df, hours_per_week = hours_week)
}

# -----------------------------
# Ordinal DV
# -----------------------------
as_ordered_from_numeric <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  levs <- sort(unique(x_num[!is.na(x_num)]))
  if (length(levs) < 2) stop("DV has <2 observed levels after numeric coercion.")
  ordered(x_num, levels = levs)
}

stopifnot("evaluation_outcome" %in% names(df))
df$evaluation_outcome_ord <- as_ordered_from_numeric(df$evaluation_outcome)

# High-performance definition: outcome >= 4
levs_num <- suppressWarnings(as.numeric(as.character(levels(df$evaluation_outcome_ord))))
high_cols <- which(levs_num >= 4)
if (length(high_cols) == 0) stop("No outcome levels >= 4 found; check coding.")

# -----------------------------
# Required columns
# -----------------------------
required <- c(
  "ai_intensity_index",
  "task_standardization_index",
  "digital_comfort",
  "tenure_years",
  "experience_years",
  "hours_per_week"
)
missing <- setdiff(required, names(df))
if (length(missing) > 0) stop("Missing required columns: ", paste(missing, collapse = ", "))

# Drop age if present alongside tenure/experience (stability in small N)
if (all(c("age", "tenure_years", "experience_years") %in% names(df))) {
  message("NOTE: Dropped 'age' to reduce deterministic collinearity in small N.")
}

# -----------------------------
# Center predictors for 3-way model
# -----------------------------
dfC <- df |>
  dplyr::mutate(
    ai_c = as.numeric(scale(ai_intensity_index, center = TRUE, scale = FALSE)),
    ts_c = as.numeric(scale(task_standardization_index, center = TRUE, scale = FALSE)),
    dc_c = as.numeric(scale(digital_comfort, center = TRUE, scale = FALSE))
  )

# -----------------------------
# Fit 3-way model (exploratory)
# -----------------------------
fml3 <- evaluation_outcome_ord ~ ai_c * ts_c * dc_c +
  tenure_years + experience_years + hours_per_week

m3 <- ordinal::clm(fml3, data = dfC, link = "logit")

# -----------------------------
# Helper: compute category probabilities from eta + thresholds
# For clm: P(Y <= j) = logistic(theta_j - eta)
# -----------------------------
probs_from_eta <- function(eta, theta) {
  K <- length(theta) + 1
  # cum probs for j=1..K-1
  cum <- sapply(theta, function(th) stats::plogis(th - eta))
  p <- matrix(NA_real_, nrow = length(eta), ncol = K)
  p[, 1] <- cum[, 1]
  if (K > 2) {
    for (j in 2:(K - 1)) p[, j] <- cum[, j] - cum[, j - 1]
  }
  p[, K] <- 1 - cum[, K - 1]
  p
}

# -----------------------------
# Build prediction grid
#   - AI varies across observed range
#   - TaskStd: low = 25th pct, high = 75th pct
#   - DigitalComfort: low = 25th pct, high = 75th pct
#   - Controls held at mean
# -----------------------------
ai_min <- min(dfC$ai_intensity_index, na.rm = TRUE)
ai_max <- max(dfC$ai_intensity_index, na.rm = TRUE)

ts_low_raw  <- as.numeric(stats::quantile(dfC$task_standardization_index, 0.25, na.rm = TRUE))
ts_high_raw <- as.numeric(stats::quantile(dfC$task_standardization_index, 0.75, na.rm = TRUE))

dc_low_raw  <- as.numeric(stats::quantile(dfC$digital_comfort, 0.25, na.rm = TRUE))
dc_high_raw <- as.numeric(stats::quantile(dfC$digital_comfort, 0.75, na.rm = TRUE))

grid <- tidyr::expand_grid(
  ai_intensity_index = seq(ai_min, ai_max, length.out = 120),
  ts_level = c("Low task structure", "High task structure"),
  dc_level = c("Low digital comfort", "High digital comfort")
) |>
  dplyr::mutate(
    task_standardization_index = dplyr::if_else(ts_level == "Low task structure", ts_low_raw, ts_high_raw),
    digital_comfort = dplyr::if_else(dc_level == "Low digital comfort", dc_low_raw, dc_high_raw),
    tenure_years = mean(dfC$tenure_years, na.rm = TRUE),
    experience_years = mean(dfC$experience_years, na.rm = TRUE),
    hours_per_week = mean(dfC$hours_per_week, na.rm = TRUE)
  ) |>
  # center using the SAME centering used in dfC (mean of raw variables)
  dplyr::mutate(
    ai_c = ai_intensity_index - mean(dfC$ai_intensity_index, na.rm = TRUE),
    ts_c = task_standardization_index - mean(dfC$task_standardization_index, na.rm = TRUE),
    dc_c = digital_comfort - mean(dfC$digital_comfort, na.rm = TRUE)
  )

# -----------------------------
# Point predictions
# -----------------------------
theta_hat <- as.numeric(m3$alpha)         # thresholds
beta_hat  <- m3$beta                      # slopes (NO thresholds)

# model.matrix for RHS (includes intercept); drop intercept to align with beta_hat
X_full <- stats::model.matrix(stats::delete.response(stats::terms(m3)), grid)
if ("(Intercept)" %in% colnames(X_full)) {
  X <- X_full[, setdiff(colnames(X_full), "(Intercept)"), drop = FALSE]
} else {
  X <- X_full
}

# Ensure X columns match beta names exactly
missing_in_X <- setdiff(names(beta_hat), colnames(X))
missing_in_b <- setdiff(colnames(X), names(beta_hat))
if (length(missing_in_X) > 0) stop("Design matrix is missing beta terms: ", paste(missing_in_X, collapse = ", "))
if (length(missing_in_b) > 0) {
  # harmless if extra columns appear; but safest to enforce exact match
  X <- X[, names(beta_hat), drop = FALSE]
} else {
  X <- X[, names(beta_hat), drop = FALSE]
}

eta_hat <- as.numeric(X %*% beta_hat)
p_mat_hat <- probs_from_eta(eta_hat, theta_hat)
p_hat_ge4 <- rowSums(p_mat_hat[, high_cols, drop = FALSE])

# -----------------------------
# 95% CI ribbon via beta simulation (Wald approx)
#   - simulate only slope coefficients (beta), hold thresholds fixed at theta_hat
# -----------------------------
V_all <- stats::vcov(m3)

# vcov(m3) often includes thresholds + slopes; extract slope block by name
V_beta <- V_all[names(beta_hat), names(beta_hat), drop = FALSE]

if (any(!is.finite(diag(V_beta)))) stop("Non-finite slope vcov; cannot compute CI.")

set.seed(20260301)
beta_sims <- MASS::mvrnorm(n = 1000, mu = as.numeric(beta_hat), Sigma = V_beta)
colnames(beta_sims) <- names(beta_hat)

eta_sims <- X %*% t(beta_sims)  # n_grid x n_sims

p_ge4_sims <- apply(eta_sims, 2, function(eta) {
  pm <- probs_from_eta(as.numeric(eta), theta_hat)
  rowSums(pm[, high_cols, drop = FALSE])
})

p_lo <- apply(p_ge4_sims, 1, stats::quantile, probs = 0.025, na.rm = TRUE)
p_hi <- apply(p_ge4_sims, 1, stats::quantile, probs = 0.975, na.rm = TRUE)

plot_data <- grid |>
  dplyr::mutate(
    p_hat = p_hat_ge4,
    p_lo  = p_lo,
    p_hi  = p_hi
  )

message(
  "plot_data columns: ",
  paste(names(plot_data), collapse = ", ")
)

# -----------------------------
# Plot (journal-ish grayscale)
#   - single color (black), linetype distinguishes digital comfort
# -----------------------------
plot_data$ts_level <- factor(
  plot_data$ts_level,
  levels = c("High task structure", "Low task structure")
)

p <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = ai_intensity_index, y = p_hat, linetype = dc_level)
) +
  ggplot2::geom_ribbon(
    data = plot_data,
    mapping = ggplot2::aes(
      x = ai_intensity_index,
      ymin = p_lo,
      ymax = p_hi,
      group = dc_level
    ),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  ggplot2::geom_line(color = "black", linewidth = 1.1) +
  ggplot2::facet_wrap(~ ts_level, ncol = 1) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(
    x = "AI intensity",
    y = "Predicted Pr(high performance)",
    linetype = NULL
  ) +
  ggplot2::theme_classic(base_size = 17) +
  ggplot2::theme(
    legend.position = "bottom",
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave(out_png, p, width = 8, height = 9, dpi = 300)
ggplot2::ggsave(out_pdf, p, width = 8, height = 9)

message("Saved: ", out_png)
message("Saved: ", out_pdf)