# Gallagher_Chance_Fig2ab_final.R
# Author: Eugene.Gallagher@umb.edu, School for the Environment, UMass Boston,
# retired
# Figure 2ab: Liver Mortality Rate vs Ethanol Consumption, 2023 data
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

# ============================================================
# 0) Packages + simple, reproducible paths (NO setwd() needed)
# ============================================================

pkgs <- c("readxl","dplyr","ggplot2","ggExtra","ggrepel","gtable","grid")
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(ggExtra)
  library(ggrepel)
  library(gtable)
  library(grid)
})

# ---- Repo-root detection ----
# If you keep scripts in repo/docs/, this finds repo root robustly.
# If run interactively in RStudio and it can't detect the script path,
# it falls back to getwd().

get_repo_root <- function() {
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
    return(normalizePath(file.path(dirname(this_file), ".."),
                         winslash = "/", mustWork = TRUE))
  }
  
  return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
}

repo_root <- get_repo_root()

# -----------------------------
# 0) Paths
# -----------------------------
in_path  <- file.path(repo_root, "data", "WilkinsonData2023_b.xlsx")
sheet_nm <- "WilkinsonData2023"
out_dir  <- file.path(repo_root, "images", "final")

if (!file.exists(in_path)) {
  stop('Data file not found: ', in_path, '\nFix: put the required Excel file in ', data_dir, ' (repo root /data).')
}

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

message("Repo root:        ", repo_root)
message("Data file:        ", in_path)
message("Saving to:        ", out_dir)

# -----------------------------
# 1) EB shrinkage helper (Fay–Herriot style)
#    y is a RATE (per 100k), deaths_like is count-like, pop is population
# -----------------------------
eb_shrink_vec <- function(y, deaths_like, pop, scale = 1e5, death_floor = 0.5) {
  
  D  <- pmax(deaths_like, death_floor)  # avoid 0 in sqrt
  se <- scale * sqrt(D) / pop
  v  <- se^2
  
  mu_hat <- weighted.mean(y, w = 1/v, na.rm = TRUE)
  
  ok <- is.finite(y) & is.finite(v)
  m  <- sum(ok)
  Q  <- sum((y[ok] - mu_hat)^2 / v[ok])
  C  <- sum(1/v[ok]) - sum(1/(v[ok]^2)) / sum(1/v[ok])
  
  tau2_hat <- max(0, (Q - (m - 1)) / C)
  
  B    <- tau2_hat / (tau2_hat + v)
  y_EB <- mu_hat + B * (y - mu_hat)
  
  list(y_EB = y_EB, tau2_hat = tau2_hat, mu_hat = mu_hat, v = v)
}

# -----------------------------
# 2) Kendall tau helper (prints nicely)
# -----------------------------
kendall_report <- function(x, y, label = "") {
  ct <- suppressWarnings(cor.test(x, y, method = "kendall", exact = FALSE))
  tau <- unname(ct$estimate)
  p   <- ct$p.value
  message(sprintf("Kendall tau %s: tau = %.3f, p = %.4g", label, tau, p))
  invisible(ct)
}

# -----------------------------
# 3) ggMarginal saving helper (ggMarginal returns a gtable, not a ggplot)
# -----------------------------
save_marginal <- function(grob_obj, file_stub, width = 7, height = 7) {
  
  # PNG
  png(filename = file.path(out_dir, paste0(file_stub, ".png")),
      width = width, height = height, units = "in", res = 300)
  print(grob_obj)
  dev.off()
  
  # JPG (quick review)
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
# 4) Gap helpers for ggMarginal gtable
#    - add_gap_above_panel(): space between top marginal and panel border
#    - add_gap_right_of_panel(): space between right marginal and panel border
# -----------------------------
add_gap_above_panel <- function(g, gap_mm = 2) {
  panel_rows <- unique(g$layout$t[grepl("^panel", g$layout$name)])
  if (length(panel_rows) < 1) return(g)
  panel_t <- panel_rows[1]
  gtable::gtable_add_rows(g, grid::unit(gap_mm, "mm"), pos = panel_t - 1)
}

add_gap_right_of_panel <- function(g, gap_mm = 2) {
  panel_cols <- unique(g$layout$l[grepl("^panel", g$layout$name)])
  if (length(panel_cols) < 1) return(g)
  panel_l <- panel_cols[1]
  panel_r <- unique(g$layout$r[grepl("^panel", g$layout$name)])[1]
  # Insert a slim spacer column immediately AFTER the panel
  gtable::gtable_add_cols(g, grid::unit(gap_mm, "mm"), pos = panel_r)
}

# -----------------------------
# 5) Standard data reader + derived variables used across figures
# -----------------------------
read_wilkinson_2023 <- function(path, sheet) {
  
  d <- read_excel(path, sheet = sheet) %>%
    filter(!is.na(State)) %>%
    rename(
      Ethanol         = `Ethanol Consumption (Gal)`,
      p_alc_raw       = `Alcohol-related Driving Fatalities (%)`,
      traf_deaths_tot = Deaths,
      pop_census      = `Census Population`,
      liver_deaths_23 = `Liver Disease Deaths 2023`
    ) %>%
    mutate(
      # Defensive: percent vs proportion
      p_alc = ifelse(p_alc_raw > 1, p_alc_raw/100, p_alc_raw),
      
      # Expected alcohol-related traffic deaths (can be non-integer; OK)
      traf_deaths_alc = traf_deaths_tot * p_alc,
      
      # Observed alcohol-related fatality rate per 100k (Figure 5 Y)
      AlcFatality_obs = 1e5 * traf_deaths_alc / pop_census,
      
      # Observed liver disease mortality rate per 100k (Figure 2 Y)
      LiverRate_obs   = 1e5 * liver_deaths_23 / pop_census
    )
  
  d
}

# ============================================================
# CLEAN DROP-IN FIX (replace your entire make_marginal_figure())
# - Keeps your left/right labeling scheme
# - Replaces labels for AI states with parentheses, e.g. "(Alaska)"
# - Uses ONE labeling layer so nothing “falls off the chain”
# - Works with ggMarginal as-is
# ============================================================

make_marginal_figure <- function(dat,
                                 xcol,
                                 ycol,
                                 panel_tag = NULL,      # "a" or "b"
                                 file_stub,
                                 x_limits = c(1, 5),
                                 y_limits = NULL,
                                 x_lab = "Consumption of Spirits",
                                 y_lab = "Deaths from Liver Disease",
                                 no_tax_states = c("Alaska", "Delaware", "Montana", "New Hampshire", "Oregon"),
                                 label_left = character(0),
                                 label_right = character(0),
                                 ai_states = c("Alaska","Oklahoma","New Mexico","South Dakota","Montana","North Dakota","Wyoming"),
                                 marginal_size = 18,
                                 gap_top_mm = 2,
                                 gap_right_mm = 2) {
  
  dat2 <- dat %>%
    mutate(
      PointShape = if_else(State %in% no_tax_states, 16, 1),
      x_plot = .data[[xcol]],
      y_plot = .data[[ycol]]
    )
  
  # Kendall tau for THIS panel
  kendall_report(dat2$x_plot, dat2$y_plot, label = paste0("(panel ", panel_tag %||% ")", " ", ycol))
  
  if (is.null(y_limits)) {
    y_limits <- range(dat2$y_plot, na.rm = TRUE)
  }
  
  # ---- ONE label table, with conditional label text
  label_states_all <- union(union(label_left, label_right), ai_states)
  
  lab_all <- dat2 %>%
    filter(State %in% label_states_all) %>%
    mutate(
      # Put parentheses ONLY on AI states
      label_txt = if_else(State %in% ai_states, paste0("(", State, ")"), State),
      
      # Preserve your left/right layout rules
      side = if_else(State %in% label_right, "R", "L"),
      nudge_x = if_else(side == "R", +0.35, -0.18),
      hjust   = if_else(side == "R", 0, 1)
    )
  
  p <- ggplot(dat2, aes(x = x_plot, y = y_plot)) +
    geom_point(aes(shape = factor(PointShape)), size = 2, show.legend = FALSE) +
    scale_shape_manual(values = c(`1` = 1, `16` = 16)) +
    geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.8) +
    scale_x_continuous(limits = x_limits) +
    scale_y_continuous(limits = y_limits) +
    labs(x = x_lab, y = y_lab, title = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    geom_text_repel(
      data = lab_all,
      aes(label = label_txt),
      nudge_x = lab_all$nudge_x,
      hjust   = lab_all$hjust,
      size = 3.1,
      min.segment.length = 0,
      seed = 1,
      max.overlaps = Inf
    )
  
  # Panel tag
  if (!is.null(panel_tag) && nzchar(panel_tag)) {
    p <- p + annotate(
      "text",
      x = x_limits[2], y = y_limits[2],
      label = panel_tag,
      hjust = 1.15, vjust = 1.15,
      size = 6
    )
  }
  
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
  
  g <- add_gap_above_panel(g, gap_mm = gap_top_mm)
  g <- add_gap_right_of_panel(g, gap_mm = gap_right_mm)
  
  save_marginal(g, file_stub, width = 7, height = 7)
  invisible(g)
}


# helper for "%||%" used above
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ============================================================
# FIGURE 2 (Observed + EB-shrunken liver mortality)
# ============================================================

df <- read_wilkinson_2023(in_path, sheet_nm)

# EB shrink the LIVER RATE (Figure 2 y)
eb_liver <- eb_shrink_vec(
  y          = df$LiverRate_obs,
  deaths_like = df$liver_deaths_23,
  pop        = df$pop_census
)
df <- df %>% mutate(LiverRate_EB = eb_liver$y_EB)
message(sprintf("Figure 2 liver EB tau^2_hat = %.6f", eb_liver$tau2_hat))

# ----- Labels for Figure 2 (edit freely; separate from Fig4/Fig5)
fig2_label_states <- c("Alaska","Colorado","Delaware","Hawaii", "Massachusetts",
                       "Maryland", "Massachusetts","Montana",
                       "Nevada","New Hampshire","New Jersey","New Mexico",
                       "New York","North Dakota","Oklahoma","Pennsylvania",
                       "South Dakota","Utah","Vermont", "West Virginia",
                       "Wyoming"
)
# Added Vermont, because it is the 2nd least populous state and F-H shifts
fig2_label_right <- c("Colorado", "Massachusetts","Nevada", "North Dakota", 
                      "Vermont", "Wyoming")
fig2_label_left  <- setdiff(fig2_label_states, fig2_label_right)

# Use common x-range (1,5) and common y-range across observed/EB so panels compare directly
y2_limits <- range(c(df$LiverRate_obs, df$LiverRate_EB), na.rm = TRUE)

ai_states <- c("Alaska","Oklahoma","New Mexico","South Dakota","Montana","North Dakota","Wyoming")

fig2_label_states <- setdiff(fig2_label_states, ai_states)
fig2_label_right  <- setdiff(fig2_label_right,  ai_states)
fig2_label_left   <- setdiff(fig2_label_left,   ai_states)

# Observed
# Panel a: Observed
g_fig2_a <- make_marginal_figure(
  dat        = df,
  xcol       = "Ethanol",
  ycol       = "LiverRate_obs",
  panel_tag  = "a",
  file_stub  = "Fig02a_Liver_Observed_ggMarginal",
  x_limits   = c(1, 5),
  y_limits   = y2_limits,
  x_lab      = "Consumption of Spirits",
  y_lab      = "Deaths from Liver Disease",
  label_left = fig2_label_left,
  label_right= fig2_label_right,
  marginal_size = 18,
  gap_top_mm = 2,
  gap_right_mm = 2
)

# Panel b: EB-shrunken
g_fig2_b <- make_marginal_figure(
  dat        = df,
  xcol       = "Ethanol",
  ycol       = "LiverRate_EB",
  panel_tag  = "b",
  file_stub  = "Fig02b_Liver_EB_ggMarginal",
  x_limits   = c(1, 5),
  y_limits   = y2_limits,
  x_lab      = "Consumption of Spirits",
  y_lab      = "Deaths from Liver Disease",
  label_left = fig2_label_left,
  label_right= fig2_label_right,
  marginal_size = 18,
  gap_top_mm = 2,
  gap_right_mm = 2
)
# View (optional)
g_fig2_a
g_fig2_b

ggsave(file.path(out_dir, "g_fig2_a.png"), plot = g_fig2_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig2_a.eps"), plot = g_fig2_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig2_a.svg"), plot = g_fig2_a, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig2_a.pdf"), plot = g_fig2_a, width = 7, height = 7, dpi = 300)

ggsave(file.path(out_dir, "g_fig2_b.png"), plot = g_fig2_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig2_b.eps"), plot = g_fig2_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig2_b.svg"), plot = g_fig2_b, width = 7, height = 7, dpi = 300)
ggsave(file.path(out_dir, "g_fig2_b.pdf"), plot = g_fig2_b, width = 7, height = 7, dpi = 300)
