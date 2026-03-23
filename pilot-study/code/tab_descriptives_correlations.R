# code/tab_descriptives_correlations.R
# Descriptives + correlation matrix (AMJ-style) -> CSV + PNG

source(here::here("pilot-study/code/00_setup.R"))

library(dplyr)
library(readr)
library(janitor)
library(psych)
library(grid)
library(gridExtra)
library(gtable)

# -----------------------------
# Paths
# -----------------------------
in_path  <- here::here("data/processed/analysis_constructed.csv")
out_csv  <- here::here("output/tables/descriptives_corr_table.csv")
out_png  <- here::here("output/tables/descriptives_corr_table.png")
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# Load + clean
# -----------------------------
df <- read_csv(in_path, show_col_types = FALSE) |>
  clean_names()

# ---- IMPORTANT: update names to match your dataset ----
# You previously hit: "Missing columns: hours_per_week"
# In your raw data you have "hours_week". Your constructed file likely uses:
#   hours_week (not hours_per_week)
# Adjust here if your constructed file uses a different name.
vars_map <- c(
  ai_intensity_index         = "AI intensity (index)",
  task_standardization_index = "Task standardization (index)",
  digital_comfort            = "Digital comfort",
  relative_performance       = "Relative performance",
  evaluation_outcome         = "Evaluation outcome",
  tenure_years               = "Tenure (years)",
  experience_years           = "Experience (years)",
  hours_week                 = "Hours/week"
)

missing <- setdiff(names(vars_map), names(df))
if (length(missing) > 0) {
  stop("Missing columns in analysis_constructed.csv: ", paste(missing, collapse = ", "))
}

df_sub <- df |>
  select(all_of(names(vars_map))) |>
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.x))))

# -----------------------------
# Descriptives + correlations
# -----------------------------
desc <- psych::describe(df_sub)[, c("n", "mean", "sd")]
desc <- as.data.frame(desc)

cor_mat <- suppressWarnings(cor(df_sub, use = "pairwise.complete.obs"))
p_mat   <- psych::corr.test(df_sub, use = "pairwise")$p

# Format correlations with stars
fmt_r <- function(r, p) {
  if (is.na(r)) return(NA_character_)
  star <- ifelse(is.na(p), "",
                 ifelse(p < 0.01, "**",
                        ifelse(p < 0.05, "*", "")))
  # AMJ-ish: no leading zero
  s <- sprintf("%.2f", r)
  s <- sub("^0\\.", ".", s)
  s <- sub("^-0\\.", "-.", s)
  paste0(s, star)
}

k <- ncol(df_sub)
r_disp <- matrix(NA_character_, nrow = k, ncol = k)
for (i in seq_len(k)) {
  for (j in seq_len(k)) {
    if (i > j) r_disp[i, j] <- fmt_r(cor_mat[i, j], p_mat[i, j])
  }
}

# Build df_out exactly once (so PNG block never sees missing df_out)
var_labels <- unname(vars_map)
df_out <- data.frame(
  Variable = paste0(seq_len(k), ". ", var_labels),
  Mean     = sprintf("%.2f", desc$mean),
  SD       = sprintf("%.2f", desc$sd),
  stringsAsFactors = FALSE
)

# Add numbered correlation columns: 1..k
for (j in seq_len(k)) df_out[[as.character(j)]] <- NA_character_
for (i in seq_len(k)) {
  for (j in seq_len(k)) {
    if (i > j) df_out[i, as.character(j)] <- r_disp[i, j]
  }
}

# Write CSV
write_csv(df_out, out_csv)

# -----------------------------
# PNG export (robust, no clipping)
# -----------------------------
stopifnot(exists("df_out"))

library(grid)
library(gridExtra)
library(gtable)

out_png <- here::here("output/tables/descriptives_corr_table.png")

# Display: blank out NA
df_disp <- df_out
df_disp[is.na(df_disp)] <- ""

# Theme
tbl_theme <- gridExtra::ttheme_minimal(
  base_size = 18,
  core = list(
    fg_params = list(hjust = 0.5, x = 0.5),
    padding = unit(c(6, 6), "pt")
  ),
  colhead = list(
    fg_params = list(fontface = "bold", hjust = 0.5, x = 0.5),
    padding = unit(c(8, 8), "pt")
  )
)

tg <- gridExtra::tableGrob(df_disp, rows = NULL, theme = tbl_theme)

# Left-align Variable column (header + body)
var_col <- which(colnames(df_disp) == "Variable")
stopifnot(length(var_col) == 1)

for (i in seq_along(tg$grobs)) {
  g <- tg$grobs[[i]]
  if (!inherits(g, "text")) next

  in_var_col <- tg$layout$l[i] == var_col
  is_header  <- grepl("^colhead-fg", tg$layout$name[i])
  is_body    <- grepl("^core-fg", tg$layout$name[i])

  if (in_var_col && (is_header || is_body)) {
    g$just <- "left"
    g$x    <- unit(2, "mm")  # small inset from left edge of the cell
    tg$grobs[[i]] <- g
  }
}

# Column widths (edit these freely)
var_w  <- unit(4.0, "cm")
mean_w <- unit(2.5,  "cm")
sd_w   <- unit(2.5,  "cm")
num_w  <- unit(1.35, "cm")

new_widths <- rep(num_w, ncol(df_disp))
new_widths[var_col] <- var_w

mean_col <- which(colnames(df_disp) == "Mean")
sd_col   <- which(colnames(df_disp) == "SD")
if (length(mean_col) == 1) new_widths[mean_col] <- mean_w
if (length(sd_col)   == 1) new_widths[sd_col]   <- sd_w

tg$widths <- new_widths

# Note + spacing
note_text <- "** p < .01, * p < .05 (two-tailed). Pairwise-complete correlations."
note_g <- grid::textGrob(note_text, x = 0, just = "left",
                         gp = grid::gpar(fontsize = 14))

spacer <- grid::rectGrob(gp = grid::gpar(col = NA))  # invisible spacer

g <- gridExtra::arrangeGrob(
  tg,
  spacer,
  note_g,
  ncol = 1,
  heights = grid::unit.c(
    unit(1, "null"),
    unit(2.2, "lines"),                  # more gap between table and note
    grobHeight(note_g) + unit(1, "lines")
  )
)

# ---- CRITICAL FIX: draw in a LEFT-ANCHORED viewport with padding ----
# If the table is wide, centering causes left-side clipping. This prevents it.
pad_left  <- unit(12, "mm")
pad_right <- unit(6,  "mm")
pad_top   <- unit(6,  "mm")
pad_bot   <- unit(8,  "mm")

# Big enough canvas so it doesn't need to clip
grDevices::png(out_png, width = 3600, height = 1200, res = 200)
grid::grid.newpage()

vp <- grid::viewport(
  x = 0, y = 1, just = c("left", "top"),
  width  = unit(1, "npc") - pad_left - pad_right,
  height = unit(1, "npc") - pad_top  - pad_bot
)

grid::pushViewport(grid::viewport(x = pad_left, y = unit(1, "npc") - pad_top,
                                  just = c("left", "top")))
grid::pushViewport(vp)

grid::grid.draw(g)

grid::popViewport(2)
grDevices::dev.off()

message("Saved: ", out_png)
