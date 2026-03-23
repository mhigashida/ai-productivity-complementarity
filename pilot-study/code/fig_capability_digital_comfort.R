# code/fig_capability_digital_comfort.R
# Capability Matters figure: predicted Pr(high performance >= 4) vs digital comfort
# Style: journal grayscale, point + 95% CI whiskers (no ribbon)

source(here::here("code/00_setup.R"))

in_path <- here::here("data/processed/analysis_constructed.csv")
out_png <- here::here("output/figs/fig_capability_digital_comfort_highperf_ci_whiskers.png")
out_pdf <- here::here("output/figs/fig_capability_digital_comfort_highperf_ci_whiskers.pdf")
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
# Model spec (stable prereg-style without sparse factors)
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

if (all(c("age", "tenure_years", "experience_years") %in% names(df))) {
  message("NOTE: Dropped 'age' to reduce deterministic collinearity in small N.")
}

fml <- evaluation_outcome_ord ~ ai_intensity_index +
  task_standardization_index +
  ai_intensity_index:task_standardization_index +
  tenure_years + experience_years + hours_per_week + digital_comfort

m <- ordinal::clm(fml, data = df, link = "logit")

# -----------------------------
# Define "high performance" = outcome >= 4
# -----------------------------
levs_num <- suppressWarnings(as.numeric(as.character(levels(df$evaluation_outcome_ord))))
high_cols <- which(levs_num >= 4)
if (length(high_cols) == 0) stop("No outcome levels >=4 found; check coding.")

prob_matrix_from_predict <- function(pred_obj) {
  if (is.matrix(pred_obj)) return(pred_obj)
  if (is.data.frame(pred_obj)) return(as.matrix(pred_obj))
  if (is.list(pred_obj) && !is.null(pred_obj$fit)) {
    if (is.matrix(pred_obj$fit)) return(pred_obj$fit)
    if (is.data.frame(pred_obj$fit)) return(as.matrix(pred_obj$fit))
  }
  stop("Could not coerce predict(..., type='prob') output into a numeric matrix.")
}

probs_from_eta <- function(eta, theta) {
  K <- length(theta) + 1
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
# Prediction grid: digital comfort at 1..5; hold others at typical values
# -----------------------------
grid <- tibble::tibble(
  ai_intensity_index = mean(df$ai_intensity_index, na.rm = TRUE),
  task_standardization_index = mean(df$task_standardization_index, na.rm = TRUE),
  tenure_years = mean(df$tenure_years, na.rm = TRUE),
  experience_years = mean(df$experience_years, na.rm = TRUE),
  hours_per_week = mean(df$hours_per_week, na.rm = TRUE),
  digital_comfort = 1:5
)

# Point estimates
pred0 <- predict(m, newdata = grid, type = "prob")
prob0 <- prob_matrix_from_predict(pred0)
p_hat <- rowSums(prob0[, high_cols, drop = FALSE])

# -----------------------------
# 95% CI via coefficient simulation (Wald approx) -- SLOPES ONLY
# -----------------------------
set.seed(20260301)

beta_hat <- as.numeric(m$beta)
names(beta_hat) <- names(m$beta)

V_all <- stats::vcov(m)

# vcov(m) often contains thresholds + slopes. Extract slope block only:
beta_names <- names(beta_hat)
missing_vcov <- setdiff(beta_names, rownames(V_all))
if (length(missing_vcov) > 0) {
  stop("vcov(m) is missing slope terms: ", paste(missing_vcov, collapse = ", "))
}
V_beta <- V_all[beta_names, beta_names, drop = FALSE]

if (any(!is.finite(diag(V_beta)))) stop("Non-finite vcov(beta); cannot compute CI.")

beta_sims <- MASS::mvrnorm(n = 1000, mu = beta_hat, Sigma = V_beta)

# Build RHS design matrix and align to slope terms
X_full <- stats::model.matrix(
  stats::delete.response(stats::terms(m)),
  data = grid
)

# clm uses thresholds (alpha) instead of an intercept, so drop intercept if present
if ("(Intercept)" %in% colnames(X_full)) {
  X_full <- X_full[, setdiff(colnames(X_full), "(Intercept)"), drop = FALSE]
}

missing_X <- setdiff(beta_names, colnames(X_full))
if (length(missing_X) > 0) {
  stop(
    "Design matrix is missing required slope columns: ",
    paste(missing_X, collapse = ", "),
    "\nHave: ", paste(colnames(X_full), collapse = ", "),
    "\nNeed: ", paste(beta_names, collapse = ", ")
  )
}

X <- X_full[, beta_names, drop = FALSE]

theta <- as.numeric(m$alpha)

eta_sims <- X %*% t(beta_sims)  # n_grid x n_sims

p_ge4_sims <- apply(eta_sims, 2, function(eta) {
  p_mat <- probs_from_eta(eta, theta)
  rowSums(p_mat[, high_cols, drop = FALSE])
})

p_lo <- apply(p_ge4_sims, 1, stats::quantile, probs = 0.025, na.rm = TRUE)
p_hi <- apply(p_ge4_sims, 1, stats::quantile, probs = 0.975, na.rm = TRUE)

plot_data <- grid |>
  dplyr::mutate(p_hat = p_hat, p_lo = p_lo, p_hi = p_hi)

# -----------------------------
# Plot: whiskers (journal-like), no ribbon
# -----------------------------
p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = digital_comfort, y = p_hat)) +
  ggplot2::geom_pointrange(
    ggplot2::aes(ymin = p_lo, ymax = p_hi),
    linewidth = 0.8
  ) +
  ggplot2::scale_x_continuous(breaks = 1:5) +
  ggplot2::scale_y_continuous(limits = c(0, 1)) +
  ggplot2::labs(
    x = "Digital comfort",
    y = "Pr(high performance ≥ 4)"
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