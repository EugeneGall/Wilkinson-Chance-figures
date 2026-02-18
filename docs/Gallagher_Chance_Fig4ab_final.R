# Gallagher_Chance_Fig4ab_final.R
# Figure 4ab: Alcohol-related Automobile Fatality Rate vs Ethanol, 2023 data
# Two panels: (a) Observed, (b) EB-shrunken
# NOTE: Panels (a) and (b) are saved separately by this script.
# For the published 2026 Chance figure layout, graphic artist Catherine Laplace
# (https://www.catherinelaplace.com/Index.html) combined the EPS outputs.
# Modular helpers for Figures 2, 3, 4 using ggMarginal + EB shrinkage, short
# for Empirical Bayes shrinkage, as implemented by Fay & Herriot (1979).
# Fay, R. E. and R. A. Herriot 1979. Estimates of Income for Small Places:
# An Application of James-Stein Procedures to Census Data
# Journal of the American Statistical Association, Vol. 74, No.366 (Jun., 1979),
# pp.269-277. Code last updated: 17 Feb 2026
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(ggExtra)
  library(ggrepel)
})


# ---- Repo-root detection (robust on Windows + RStudio + "source()") ----
# Goal: make these scripts runnable whether you:
#   (a) click "Source" in RStudio,
#   (b) run with Rscript,
#   (c) run from an arbitrary working directory.
#
# Strategy:
#   1) Try to locate this script's directory (Rscript --file, or RStudio "ofile").
#   2) From that start dir (and as a fallback, getwd()), walk UP a few levels
#      looking for a repo root marker: required inputs under /data or /models.
#   3) If still not found, fall back to the start dir.

get_script_dir <- function() {
  this_file <- NULL
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args)
  if (length(hit) > 0) {
    this_file <- sub("^--file=", "", args[hit[1]])
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    # Works when using source() or RStudio "Source"
    this_file <- sys.frames()[[1]]$ofile
  }
  if (!is.null(this_file) && nzchar(this_file) && file.exists(this_file)) {
    return(normalizePath(dirname(this_file), winslash = "/", mustWork = TRUE))
  }
  NULL
}

find_repo_root <- function(required_paths = character(), max_up = 6) {
  start_dirs <- c(get_script_dir(), normalizePath(getwd(), winslash = "/", mustWork = TRUE))
  start_dirs <- unique(start_dirs[!is.na(start_dirs) & nzchar(start_dirs)])

  is_root <- function(dir) {
    ok <- TRUE
    for (rp in required_paths) {
      ok <- ok && file.exists(file.path(dir, rp))
    }
    ok
  }

  for (start in start_dirs) {
    d <- start
    for (k in 0:max_up) {
      if (is_root(d)) return(d)
      parent <- normalizePath(file.path(d, ".."), winslash = "/", mustWork = TRUE)
      if (identical(parent, d)) break
      d <- parent
    }
  }

  # Fallback
  start_dirs[1]
}

repo_root <- find_repo_root(required_paths = c('data/WilkinsonData2023_b.xlsx'))

# Helper: if a file is missing where expected, try to locate it anywhere under repo_root.
locate_file <- function(filename, root = repo_root) {
  hits <- list.files(root, pattern = paste0("^", filename, "$"), recursive = TRUE,
                     full.names = TRUE, ignore.case = TRUE)
  if (length(hits) > 0) normalizePath(hits[1], winslash = "/", mustWork = TRUE) else NA_character_
}


data_dir   <- file.path(repo_root, "data")
images_dir <- file.path(repo_root, "images", "final")
models_dir <- file.path(repo_root, "models")

if (!dir.exists(images_dir)) dir.create(images_dir, recursive = TRUE)
if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE)

cat("Repo root:  ", repo_root, "\n")
cat("Data dir:   ", data_dir, "\n")
cat("Images dir: ", images_dir, "\n")
cat("Models dir: ", models_dir, "

")
# -----------------------------
# 1) Read + compute observed Y
# -----------------------------
in_path <- file.path(data_dir, "WilkinsonData2023_b.xlsx")

if (!file.exists(in_path)) {
  alt <- locate_file("WilkinsonData2023_b.xlsx")
  if (!is.na(alt)) {
    message("Note: expected Excel file not found at ", in_path, "
      Using: ", alt)
    in_path <- alt
  } else {
    stop("Data file not found: ", in_path,
         "
Fix: ensure WilkinsonData2023_b.xlsx is under /data at the repo root.")
  }
}

sheet_nm <- "WilkinsonData2023"
out_dir <- images_dir

df <- read_excel(in_path, sheet = sheet_nm) %>%
  filter(!is.na(State)) %>%
  rename(
    Ethanol         = `Ethanol Consumption (Gal)`,
    p_alc_raw       = `Alcohol-related Driving Fatalities (%)`,
    traf_deaths_tot = Deaths,
    pop_census      = `Census Population`
  ) %>%
  mutate(
    # Defensive: percent vs proportion
    p_alc = ifelse(p_alc_raw > 1, p_alc_raw/100, p_alc_raw),
    
    # Expected alcohol-related deaths (can be non-integer; OK)
    traf_deaths_alc = traf_deaths_tot * p_alc,
    
    # Observed alcohol-related fatality rate per 100k
    AlcFatality_obs = 1e5 * traf_deaths_alc / pop_census
  )

# -----------------------------
# 2) EB shrinkage for Y (Fay–Herriot style)
# -----------------------------
eb_shrink_vec <- function(y, deaths_like, pop, scale = 1e5, death_floor = 0.5) {
  
  D  <- pmax(deaths_like, death_floor)
  se <- scale * sqrt(D) / pop
  v  <- se^2
  
  mu_hat <- weighted.mean(y, w = 1/v, na.rm = TRUE)
  
  m <- sum(is.finite(y) & is.finite(v))
  Q <- sum((y - mu_hat)^2 / v, na.rm = TRUE)
  C <- sum(1/v, na.rm = TRUE) - sum(1/v^2, na.rm = TRUE) / sum(1/v, na.rm = TRUE)
  
  tau2_hat <- max(0, (Q - (m - 1)) / C)
  
  B    <- tau2_hat / (tau2_hat + v)
  y_EB <- mu_hat + B * (y - mu_hat)
  
  list(y_EB = y_EB, tau2_hat = tau2_hat, mu_hat = mu_hat, v = v)
}

eb <- eb_shrink_vec(
  y           = df$AlcFatality_obs,
  deaths_like  = df$traf_deaths_alc,
  pop         = df$pop_census
)

df <- df %>% mutate(AlcFatality_EB = eb$y_EB)

message(sprintf("EB hyper-variance tau^2_hat = %.6f", eb$tau2_hat))

# -----------------------------
# 3) Plot settings (match your July Fig04 look)
# -----------------------------
no_tax_states <- c("Alaska", "Delaware", "Montana", "New Hampshire", "Oregon")

# Assumes you already have:
# df with columns: State, Ethanol, AlcFatality_obs, AlcFatality_EB
# and you have loaded: ggplot2, ggExtra, dplyr, ggrepel

library(ggplot2)
library(ggExtra)
library(dplyr)
library(ggrepel)

# -----------------------------
# Output directory (make it unambiguous)
# -----------------------------
out_dir <- images_dir

message("Working directory: ", getwd())
message("Saving to:        ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))

# -----------------------------
# Plot settings
# -----------------------------
no_tax_states <- c("Alaska", "Delaware", "Montana", "New Hampshire", "Oregon")

label_states <- c(
  "South Carolina", "Montana", "Wyoming", "Delaware",
  "Nevada", "New Hampshire", "New Jersey", "New Mexico",
  "New York", "Massachusetts", "Utah", "Minnesota",
  "North Dakota", "West Virginia"
)

label_right <- c("Massachusetts", "Nevada", "North Dakota")
label_left  <- setdiff(label_states, label_right)

x_limits <- c(1.0, 5.0)
y_limits <- range(c(df$AlcFatality_obs, df$AlcFatality_EB), na.rm = TRUE)

# -----------------------------
# Helper: safe save for ggMarginal objects (gtables)
# -----------------------------

library(gtable)
library(grid)

add_gap_above_panel <- function(g, gap_mm = 2) {
  # g is the ggMarginal output (a gtable)
  panel_rows <- unique(g$layout$t[grepl("^panel", g$layout$name)])
  if (length(panel_rows) < 1) return(g)
  
  panel_t <- panel_rows[1]  # top row index of panel
  gtable::gtable_add_rows(g, grid::unit(gap_mm, "mm"), pos = panel_t - 1)
}

save_marginal <- function(grob_obj, file_stub, width = 7, height = 7) {
  
  # PNG
  png(filename = file.path(out_dir, paste0(file_stub, ".png")),
      width = width, height = height, units = "in", res = 300)
  print(grob_obj)
  dev.off()
  
  # JPG
  jpeg(filename = file.path(out_dir, paste0(file_stub, ".jpg")),
       width = width, height = height, units = "in", res = 300, quality = 95)
  print(grob_obj)
  dev.off()
  
  # TIFF (T&F-friendly)
  tiff(filename = file.path(out_dir, paste0(file_stub, ".tiff")),
       width = width, height = height, units = "in", res = 600, compression = "lzw")
  print(grob_obj)
  dev.off()
}

# -----------------------------
# Main function (July-style ggMarginal, but with proper spacing + Tukey whiskers)
# -----------------------------
# -----------------------------
# Main function (July-style ggMarginal, plus:
#   - Kendall's tau printout
#   - Panel label (a/b) in upper-right
# -----------------------------
# -----------------------------
# Main function (July-style ggMarginal, plus:
#   - Kendall's tau printout
#   - Panel label (a/b) in upper-right
# -----------------------------
make_fig04_marginal <- function(dat, ycol, title_txt, outfile_stub,
                                panel_label = NULL,
                                marginal_size = 18,
                                gap_mm = 2) {
  
  # --- Kendall's tau (printed for your log / console) ---
  ok <- is.finite(dat$Ethanol) & is.finite(dat[[ycol]])
  kt <- suppressWarnings(cor.test(dat$Ethanol[ok], dat[[ycol]][ok], method = "kendall", exact = FALSE))
  
  message(sprintf(
    "Kendall's tau for %s (%s vs Spirits): tau = %.4f, p = %.4g, n = %d",
    outfile_stub, ycol, unname(kt$estimate), kt$p.value, sum(ok)
  ))
  
  dat2 <- dat %>%
    mutate(PointShape = if_else(State %in% no_tax_states, 16, 1))
  
  labL <- dat2 %>% filter(State %in% label_left)
  labR <- dat2 %>% filter(State %in% label_right)
  
  # --- Panel label coordinates (upper-right, inside border) ---
  xr <- x_limits[2] - 0.02 * diff(x_limits)
  yr <- y_limits[2] - 0.03 * diff(y_limits)
  
  # 1) Central scatterplot
  p <- ggplot(dat2, aes(x = Ethanol, y = .data[[ycol]])) +
    geom_point(aes(shape = factor(PointShape)), size = 2, show.legend = FALSE) +
    scale_shape_manual(values = c(`1` = 1, `16` = 16)) +
    geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits) +
    labs(
      x = "Consumption of Spirits",
      y = "Alcohol-related Automobile Fatality Rate",
      title = title_txt
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title       = element_text(hjust = 0),
      plot.margin      = margin(t = 8, r = 8, b = 8, l = 8)
    ) +
    geom_text_repel(
      data = labL, aes(label = State),
      nudge_x = -0.18, hjust = 1, size = 3.1,
      min.segment.length = 0, seed = 1
    ) +
    geom_text_repel(
      data = labR, aes(label = State),
      nudge_x = +0.35, hjust = 0, size = 3.1,
      min.segment.length = 0, seed = 1
    )
  
  # Add panel label (a/b) if provided
  if (!is.null(panel_label) && nzchar(panel_label)) {
    p <- p + annotate(
      "text", x = xr, y = yr, label = panel_label,
      hjust = 1, vjust = 1, size = 5, fontface = "bold"
    )
  }
  
  # 2) Add marginals
  g <- ggMarginal(
    p,
    type = "boxplot",
    margins = "both",
    size = marginal_size,
    groupColour = FALSE,
    groupFill   = FALSE,
    xparams = list(coef = 1.5, outlier.size = 1.4, size = 0.35),
    yparams = list(coef = 1.5, outlier.size = 1.4, size = 0.35)
  )
  
  # 3) Insert gap above the panel
  g <- add_gap_above_panel(g, gap_mm = gap_mm)
  
  # 4) Save
  save_marginal(g, outfile_stub, width = 7, height = 7)
  
  invisible(g)
}

# -----------------------------
# Produce the two separate figures (panel labels only; no titles)
# -----------------------------
fig4_a <- make_fig04_marginal(
  df, "AlcFatality_obs",
  title_txt = NULL,                    # <- removes "Observed"
  outfile_stub = "Fig04_Observed_ggMarginal",
  panel_label = "a",
  marginal_size = 18, gap_mm = 2
)

fig4_b <- make_fig04_marginal(
  df, "AlcFatality_EB",
  title_txt = NULL,                    # <- removes "EB-shrunken"
  outfile_stub = "Fig04_EB_ggMarginal",
  panel_label = "b",
  marginal_size = 18, gap_mm = 2
)

fig4_a
fig4_b

ggsave(file.path(out_dir, "g_fig4_a.png"), plot = fig4_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig4_a.eps"), plot = fig4_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig4_a.svg"), plot = fig4_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig4_a.pdf"), plot = fig4_a, width = 7, height = 7, dpi = 300)

ggsave(file.path(out_dir, "g_fig4_b.png"), plot = fig4_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig4_b.eps"), plot = fig4_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig4_b.svg"), plot = fig4_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig4_b.pdf"), plot = fig4_b, width = 7, height = 7, dpi = 300)
