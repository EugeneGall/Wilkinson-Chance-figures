# Gallagher_Chance_Fig3ab_final.R
# Author: Eugene.Gallagher@umb.edu, School for the Environment, UMass Boston,
# retired
# Figure 3ab: Automobile Fatality Rate vs Ethanol, 2023 data
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


# ---- Repo-root detection ----
# Figures live under /docs in your repo, while inputs live under /data at the repo root.
# So: start from this script's folder, then walk UP until we find a repo marker.

get_script_dir <- function() {
  # Works for: Rscript --file=..., and (most of the time) RStudio "Source"
  this_file <- NULL
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  hit <- grep(file_arg, args)
  if (length(hit) > 0) {
    this_file <- sub(file_arg, "", args[hit[1]])
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    this_file <- sys.frames()[[1]]$ofile
  }
  
  if (!is.null(this_file) && nzchar(this_file) && file.exists(this_file)) {
    return(normalizePath(dirname(this_file), winslash = "/", mustWork = TRUE))
  }
  
  return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
}

find_repo_root <- function(start_dir, must_contain_relpath, max_up = 6) {
  d <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  for (i in 0:max_up) {
    marker <- file.path(d, must_contain_relpath)
    if (file.exists(marker)) return(d)
    parent <- normalizePath(file.path(d, ".."), winslash = "/", mustWork = TRUE)
    if (identical(parent, d)) break
    d <- parent
  }
  return(normalizePath(start_dir, winslash = "/", mustWork = TRUE))
}

script_dir <- get_script_dir()
repo_root  <- find_repo_root(script_dir, file.path("data", "WilkinsonData2023_b.xlsx"))

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
# Paths
# -----------------------------
in_path  <- file.path(data_dir, "WilkinsonData2023_b.xlsx")

if (!file.exists(in_path)) {
  stop('Data file not found: ', in_path, '\nFix: put the required Excel file in ', data_dir, ' (repo root /data).')
}

sheet_nm <- "WilkinsonData2023"

out_dir <- images_dir

message("Working directory: ", getwd())
message("Saving to:        ", normalizePath(out_dir, winslash = "/", mustWork = FALSE))

# -----------------------------
# Read data
# -----------------------------
df <- read_excel(in_path, sheet = sheet_nm) %>%
  filter(!is.na(State)) %>%
  rename(
    Ethanol         = 'Ethanol Consumption (Gal)',
    traf_deaths_tot = 'Deaths',
    pop_census      = `Census Population`
  ) %>%
  mutate(
    # Observed automobile death rate per 100k
#    AutoDeathRate_obs = 1e5 * traf_deaths_tot / pop_census
    AutoDeathRate_obs = Deaths_per_100000
  )

# -----------------------------
# EB shrinkage helper (rate per 100k from counts + population)
# -----------------------------
eb_shrink_rate <- function(y_rate, deaths_like, pop, scale = 1e5, death_floor = 0.5) {
  # deaths_like can be non-integer; for pure death counts it's integer.
  # death_floor prevents SE=0 for tiny/zero counts.
  D  <- pmax(deaths_like, death_floor)
  se <- scale * sqrt(D) / pop
  v  <- se^2
  
  mu_hat <- weighted.mean(y_rate, w = 1/v, na.rm = TRUE)
  
  m <- sum(is.finite(y_rate) & is.finite(v))
  Q <- sum((y_rate - mu_hat)^2 / v, na.rm = TRUE)
  C <- sum(1/v, na.rm = TRUE) - (sum(1/v^2, na.rm = TRUE) / sum(1/v, na.rm = TRUE))
  
  tau2_hat <- max(0, (Q - (m - 1)) / C)
  
  B    <- tau2_hat / (tau2_hat + v)            # shrinkage weight (0..1)
  y_EB <- mu_hat + B * (y_rate - mu_hat)       # EB posterior mean
  
  list(y_EB = y_EB, tau2_hat = tau2_hat, mu_hat = mu_hat, v = v, B = B)
}

eb <- eb_shrink_rate(
  y_rate      = df$AutoDeathRate_obs,
  deaths_like = df$traf_deaths_tot,
  pop         = df$pop_census
)

df <- df %>% mutate(AutoDeathRate_EB = eb$y_EB)
message(sprintf("Fig03 EB hyper-variance tau^2_hat = %.6f", eb$tau2_hat))

# -----------------------------
# Kendall tau helper (for caption + console)
# -----------------------------

kendall_tau_print <- function(dat, xcol, ycol, tag = "") {
  x <- dat[[xcol]]
  y <- dat[[ycol]]
  ok <- is.finite(x) & is.finite(y)
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "kendall", exact = FALSE))
  msg <- sprintf(
    "%s Kendall's tau(%s vs %s): tau = %.3f, p = %.3g (n=%d)",
    tag, xcol, ycol, unname(ct$estimate), ct$p.value, sum(ok)
  )
  message(msg)
  invisible(ct)
}

# -----------------------------
# Label selection helper (automatic, small, reproducible)
# Picks states farthest from the bivariate median (robust-ish)
# -----------------------------
choose_labels <- function(dat, xcol, ycol, n = 14) {
  x <- dat[[xcol]]
  y <- dat[[ycol]]
  ok <- is.finite(x) & is.finite(y)
  
  mx <- median(x[ok]); my <- median(y[ok])
  sx <- mad(x[ok]);    sy <- mad(y[ok])
  if (!is.finite(sx) || sx == 0) sx <- sd(x[ok])
  if (!is.finite(sy) || sy == 0) sy <- sd(y[ok])
  
  score <- ((x - mx) / sx)^2 + ((y - my) / sy)^2
  dat %>%
    mutate(.score = score) %>%
    arrange(desc(.score)) %>%
    slice_head(n = n) %>%
    pull(State)
}

# -----------------------------
# Safe save for ggMarginal (gtables)
# Includes EPS via cairo_ps (best for ggplot-ish output)
# -----------------------------
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
  
  # TIFF (journal-friendly)
  tiff(filename = file.path(out_dir, paste0(file_stub, ".tiff")),
       width = width, height = height, units = "in", res = 600, compression = "lzw")
  print(grob_obj)
  dev.off()
  
  # EPS (Chance guidelines)
  # cairo_ps generally produces cleaner text than base postscript.
  # If cairo is missing on a machine, comment this and let Catherine generate EPS.
  grDevices::cairo_ps(filename = file.path(out_dir, paste0(file_stub, ".eps")),
                      width = width, height = height, onefile = FALSE, fallback_resolution = 600)
  print(grob_obj)
  dev.off()
}

# -----------------------------
# Figure style settings (match Fig02/Fig05)
# -----------------------------
no_tax_states <- c("Alaska", "Delaware", "Montana", "New Hampshire", "Oregon")

x_limits <- c(1.0, 5.0)  # REQUIRED common abscissa range
y_limits <- range(c(df$AutoDeathRate_obs, df$AutoDeathRate_EB), na.rm = TRUE)

# If you want a fixed y-range to mimic the July 2025 manuscript look, uncomment:
# y_limits <- c(4.5, 25.25)

# -----------------------------
# (Optional) Add a small gap above the main panel (helps top marginal breathing room)
# -----------------------------
suppressPackageStartupMessages({
  library(gtable)
  library(grid)
})
add_gap_above_panel <- function(g, gap_mm = 2) {
  panel_rows <- unique(g$layout$t[grepl("^panel", g$layout$name)])
  if (length(panel_rows) < 1) return(g)
  panel_t <- panel_rows[1]
  gtable::gtable_add_rows(g, grid::unit(gap_mm, "mm"), pos = panel_t - 1)
}

# -----------------------------
# Main plot builder (a/b panel tags, auto labels, Fig02/Fig05 aesthetics)
# -----------------------------
make_fig03_marginal <- function(dat, ycol, panel_tag = "a", outfile_stub,
                                label_n = 14, marginal_size = 18) {
  
  dat2 <- dat %>%
    mutate(PointShape = if_else(State %in% no_tax_states, 16, 1))
  
  # Choose label set for THIS figure (separate from Fig02/Fig05)
  label_states <- choose_labels(dat2, "Ethanol", ycol, n = label_n)
  
  # Left vs right: simple rule based on x relative to median
  x_med <- median(dat2$Ethanol, na.rm = TRUE)
  label_right <- dat2 %>% filter(State %in% label_states, Ethanol >= x_med) %>% pull(State)
  label_left  <- setdiff(label_states, label_right)
  
  labL <- dat2 %>% filter(State %in% label_left)
  labR <- dat2 %>% filter(State %in% label_right)
  # Print Kendall tau for caption use
  kendall_tau_print(dat2, "Ethanol", ycol, tag = paste0("Fig03(", panel_tag, "):"))
  
  # Panel tag coordinates (match Fig02/Fig04: slightly inset from top-right)
  xr <- x_limits[2] - 0.02 * diff(x_limits)
  yr <- y_limits[2] - 0.03 * diff(y_limits)
  
  p <- ggplot(dat2, aes(x = Ethanol, y = .data[[ycol]])) +
    geom_point(aes(shape = factor(PointShape)), size = 2, show.legend = FALSE) +
    scale_shape_manual(values = c(`1` = 1, `16` = 16)) +
    geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits) +
    labs(
      x = "Consumpton of Spirits",
      y = "Automobile Fatality Rate"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin      = margin(t = 8, r = 8, b = 8, l = 8)
    ) +
    # Panel tag (a/b) inset from upper-right (consistent with Fig02/Fig04)
    annotate("text", x = xr, y = yr, label = panel_tag,
             hjust = 1, vjust = 1, size = 6, fontface = "bold") +
    geom_text_repel(
      data = labL, aes(label = State),
      nudge_x = -0.20, hjust = 1, size = 3.1,
      min.segment.length = 0, seed = 1
    ) +
    geom_text_repel(
      data = labR, aes(label = State),
      nudge_x = +0.25, hjust = 0, size = 3.1,
      min.segment.length = 0, seed = 1
    )
  
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
  
  # Optional: small extra gap above the panel (helps top boxplot clearance)
  g <- add_gap_above_panel(g, gap_mm = 2)
  
  save_marginal(g, outfile_stub, width = 7, height = 7)
  invisible(g)
}

# -----------------------------
# Build both panels
# -----------------------------
fig3_a <- make_fig03_marginal(df, "AutoDeathRate_obs", panel_tag = "a",
                              outfile_stub = "Fig03_AutoDeaths_Observed",
                              label_n = 14, marginal_size = 18)

fig3_b <- make_fig03_marginal(df, "AutoDeathRate_EB",  panel_tag = "b",
                              outfile_stub = "Fig03_AutoDeaths_EB",
                              label_n = 14, marginal_size = 18)

# View in RStudio if desired
fig3_a
fig3_b

ggsave(file.path(out_dir, "g_fig3_a.png"), plot = fig3_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig3_a.eps"), plot = fig3_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig3_a.svg"), plot = fig3_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig3_a.pdf"), plot = fig3_a, width = 7, height = 7, dpi = 300)

ggsave(file.path(out_dir, "g_fig3_b.png"), plot = fig3_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig3_b.eps"), plot = fig3_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig3_b.svg"), plot = fig3_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig3_b.pdf"), plot = fig3_b, width = 7, height = 7, dpi = 300)