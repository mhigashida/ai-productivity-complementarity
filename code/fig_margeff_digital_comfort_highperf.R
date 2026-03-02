# code/fig_margeff_digital_comfort_highperf.R
# Marginal effect of digital comfort on Pr(high performance >= 4)
# Using ordinal::clm + simulation-based 95% CI (Wald approx)

source(here::here("code/00_setup.R"))

# -----------------------------
# Paths
# -----------------------------
in_path <- here::here("data/processed/analysis_constructed.csv")
out_png <- here::here("output/figs/fig_margeff_digital_comfort_highperf.png")
out_pdf <- here::here("output/figs/fig_margeff_digital_comfort_highperf.pdf")

dir.create(here::here("output/figs"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Load data
# -----------------------------
df <- readr::read_csv(in_path, show_col_types = FALSE) |>
  janitor::clean_names()

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

# -----------------------------
# Model (same spec you’ve been using)
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

fml <- evaluation_outcome_ord ~ ai_intensity_index +
  task_standardization_index +
  ai_intensity_index:task_standardization_index +
  tenure_years + experience_years + hours_per_week + digital_comfort

m <- ordinal::clm(fml, data = df, link = "logit")

# -----------------------------
# Helpers
# -----------------------------
# Given eta and thresholds alpha, return matrix of category probabilities
probs_from_eta_alpha <- function(eta, alpha) {
  # alpha length = K-1 thresholds
  K <- length(alpha) + 1
  cum <- sapply(alpha, function(a) stats::plogis(a - eta))  # P(Y <= j)
  p <- matrix(NA_real_, nrow = length(eta), ncol = K)

  p[, 1] <- cum[, 1]
  if (K > 2) {
    for (j in 2:(K - 1)) p[, j] <- cum[, j] - cum[, j - 1]
  }
  p[, K] <- 1 - cum[, K - 1]
  p
}

# Compute Pr(Y >= 4) given beta/alpha and newdata grid
pr_geq4 <- function(beta, alpha, grid_df, high_cols) {
  X <- stats::model.matrix(stats::delete.response(stats::terms(m)), grid_df)

  # Drop intercept if present (clm beta has no intercept)
  if ("(Intercept)" %in% colnames(X) && !("(Intercept)" %in% names(beta))) {
    X <- X[, setdiff(colnames(X), "(Intercept)"), drop = FALSE]
  }

  # Align X columns to beta names
  miss <- setdiff(names(beta), colnames(X))
  if (length(miss) > 0) stop("Design matrix missing columns: ", paste(miss, collapse = ", "))
  X <- X[, names(beta), drop = FALSE]

  eta <- as.numeric(X %*% beta)
  pmat <- probs_from_eta_alpha(eta, alpha)
  rowSums(pmat[, high_cols, drop = FALSE])
}

# -----------------------------
# Define which outcome levels count as "high" (>= 4)
# -----------------------------
levs_num <- suppressWarnings(as.numeric(as.character(levels(df$evaluation_outcome_ord))))
high_cols <- which(levs_num >= 4)
if (length(high_cols) == 0) stop("No outcome levels >=4 found; check coding.")

# -----------------------------
# Prediction grid (vary digital comfort; hold others typical)
# -----------------------------
dc_min <- min(df$digital_comfort, na.rm = TRUE)
dc_max <- max(df$digital_comfort, na.rm = TRUE)

grid <- tibble::tibble(
  ai_intensity_index = mean(df$ai_intensity_index, na.rm = TRUE),
  task_standardization_index = mean(df$task_standardization_index, na.rm = TRUE),
  tenure_years = mean(df$tenure_years, na.rm = TRUE),
  experience_years = mean(df$experience_years, na.rm = TRUE),
  hours_per_week = mean(df$hours_per_week, na.rm = TRUE),
  digital_comfort = seq(dc_min, dc_max, length.out = 101)
)

# -----------------------------
# Simulation draws for CI (beta + thresholds)
# -----------------------------
set.seed(20260301)

coef_hat <- stats::coef(m)     # includes thresholds + beta
V <- stats::vcov(m)

beta_names <- names(m$beta)
alpha_names <- names(m$alpha)

# Sanity
stopifnot(all(beta_names %in% names(coef_hat)))
stopifnot(all(alpha_names %in% names(coef_hat)))

# Draw full parameter vector, then split
n_sims <- 2000
sims <- MASS::mvrnorm(n = n_sims, mu = coef_hat, Sigma = V)
colnames(sims) <- names(coef_hat)

# -----------------------------
# Marginal effect via central difference
# -----------------------------
h <- 0.05
grid_plus <- grid |> dplyr::mutate(digital_comfort = pmin(dc_max, digital_comfort + h))
grid_minus <- grid |> dplyr::mutate(digital_comfort = pmax(dc_min, digital_comfort - h))

# Point estimate (using fitted params)
p_plus_hat  <- pr_geq4(m$beta, m$alpha, grid_plus, high_cols)
p_minus_hat <- pr_geq4(m$beta, m$alpha, grid_minus, high_cols)
me_hat <- (p_plus_hat - p_minus_hat) / (2 * h)

# Simulation distribution of marginal effects
me_sims <- matrix(NA_real_, nrow = nrow(grid), ncol = n_sims)

for (s in seq_len(n_sims)) {
  beta_s <- sims[s, beta_names]
  alpha_s <- sims[s, alpha_names]

  p_plus  <- pr_geq4(beta_s, alpha_s, grid_plus, high_cols)
  p_minus <- pr_geq4(beta_s, alpha_s, grid_minus, high_cols)

  me_sims[, s] <- (p_plus - p_minus) / (2 * h)
}

me_lo <- apply(me_sims, 1, stats::quantile, probs = 0.025, na.rm = TRUE)
me_hi <- apply(me_sims, 1, stats::quantile, probs = 0.975, na.rm = TRUE)

plot_df <- grid |>
  dplyr::mutate(me = me_hat, lo = me_lo, hi = me_hi)

# -----------------------------
# Plot (journal-ish grayscale)
# -----------------------------
p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = digital_comfort, y = me)) +
  ggplot2::geom_hline(yintercept = 0, linewidth = 0.6) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), fill = "grey70", alpha = 0.5) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::labs(
    x = "Digital comfort",
    y = "Marginal effect on Pr(high ≥4)"
  ) +
  ggplot2::theme_classic(base_size = 18) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
    plot.margin  = ggplot2::margin(10, 20, 10, 10)
  )

ggplot2::ggsave(out_png, p, width = 10, height = 6, dpi = 300)
ggplot2::ggsave(out_pdf, p, width = 10, height = 6)

message("Saved: ", out_png)
message("Saved: ", out_pdf)