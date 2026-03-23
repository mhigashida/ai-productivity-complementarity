# 00_setup.R
# PHDBA297B Pilot Study
# Author: Masaki Higashida
# Purpose: Reproducible package setup and environment initialization

# ------------------------------------------------------------
# 1. Set CRAN mirror for non-interactive Rscript execution
# ------------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))

# ------------------------------------------------------------
# 2. Required packages
# ------------------------------------------------------------
required_packages <- c(
  "tidyverse",
  "readr",
  "janitor",
  "here",
  "psych",        # Cronbach's alpha
  "ordinal",      # clm for ordinal logistic regression
  "broom",
  "broom.mixed",
  "modelsummary",
  "ggeffects",
  "patchwork"
)

# ------------------------------------------------------------
# 3. Install missing packages (non-interactive safe)
# ------------------------------------------------------------
installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install) > 0) {
  message("Installing missing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, dependencies = TRUE)
} else {
  message("All required packages already installed.")
}

# ------------------------------------------------------------
# 4. Load libraries
# ------------------------------------------------------------
invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------------------------------------------
# 5. Reproducibility settings
# ------------------------------------------------------------
set.seed(20260225)

options(
  stringsAsFactors = FALSE,
  scipen = 999  # avoid scientific notation in outputs
)

# ------------------------------------------------------------
# 6. Save session info (Open Science best practice)
# ------------------------------------------------------------
dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)

capture.output(
  sessionInfo(),
  file = "output/logs/session_info.txt"
)

message("Environment setup complete.")