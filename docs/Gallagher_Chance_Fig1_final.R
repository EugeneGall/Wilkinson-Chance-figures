# Gallagher_Chance_Fig1_final.R
# Author: Eugene.Gallagher@umb.edu, School for the Environment, UMass Boston,
# retired
# Data extracted from Wilkinson's original graphic which can be found here:
#   https://www.researchgate.net/publication/228717480_Presentation_Graphics/figures
#   https://www.sci.utah.edu/~kpotter/Library/Papers/wilkinson:2001:PG/wilkinson_2001_PG_05.png
# Code last updated: 17 February 2026

# ============================================================
# 0) Packages + simple, reproducible paths 
# ============================================================

pkgs <- c("dplyr", "ggplot2", "ggExtra", "readxl")
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

library(dplyr)
library(ggplot2)
library(ggExtra)
library(readxl)

# ---- Repo-root detection ----
# Option A (recommended): run from repo root; script finds root via this file's location
# Option B: if you run interactively and it can't find the script path, it falls back to getwd()

get_repo_root <- function() {
  # Try to locate this script on disk (works when sourcing the file)
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
    # Expect script in repo/docs/
    return(normalizePath(dirname(this_file), winslash = "/", mustWork = TRUE))
  }
  
  # Fallback: assume user is already at repo root
  return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
}

repo_root <- get_repo_root()

data_file <- file.path(repo_root, "data", "Fig01_Wilkinson_Extracted_Data.xlsx")
out_dir   <- file.path(repo_root, "images", "final")

if (!file.exists(data_file)) {
  stop('Data file not found: ', data_file, '\nFix: put the required Excel file in ', data_dir, ' (repo root /data).')
}

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

cat("Repo root:  ", repo_root, "\n")
cat("Data file:  ", data_file, "\n")
cat("Output dir: ", out_dir, "\n\n")

# =================
# 1) Read points
# =================

df <- read_excel(data_file, sheet = "WilkinsonData2023") %>%
  mutate(
    State = as.character(State),
    label = if_else(State %in% c("Nevada", "New Hampshire"), State, NA_character_)
  )

# ==========================
# 2) Quartiles (optional)
# ==========================

qx <- quantile(df$Ethanol,  probs = c(.25, .50, .75), na.rm = TRUE)
qy <- quantile(df$Fatality, probs = c(.25, .50, .75), na.rm = TRUE)
cat("\n--- Spirits (x) quartiles ---\n"); print(qx)
cat("\n--- Liver deaths (y) quartiles ---\n"); print(qy)

# ==========================================================
# 3) Kendall's tau helper (use across Figures 1–5, etc.)
# ==========================================================

print_kendall <- function(x, y, xname, yname) {
  kt <- cor.test(x, y, method = "kendall", exact = FALSE)
  cat(sprintf(
    "Kendall's tau (%s vs. %s): tau = %.3f, p-value = %.4g\n",
    xname, yname, unname(kt$estimate), kt$p.value
  ))
}

print_kendall(df$Ethanol, df$Fatality,
              "Ethanol Consumption", "Liver Disease Fatality")

# ======================
# 4) Label fine-tuning
# ======================

dx_nh <- -1.05; dy_nh <- -0.60
dx_nv <- -0.45; dy_nv <-  0.25

df_labs <- df %>%
  filter(label %in% c("Nevada", "New Hampshire")) %>%
  mutate(
    dx = if_else(label == "New Hampshire", dx_nh, dx_nv),
    dy = if_else(label == "New Hampshire", dy_nh, dy_nv)
  )

# ==========================================================
# 5) Optional kernel regression (kept, but not drawn by default)
# ==========================================================

x_trunc <- 3.5
kernel_type <- "normal"
bw_kernel   <- 1.75

df_left <- df %>% filter(Ethanol <= x_trunc)
xgrid <- seq(1, x_trunc, length.out = 250)

kfit <- ksmooth(
  x = df_left$Ethanol,
  y = df_left$Fatality,
  kernel = kernel_type,
  bandwidth = bw_kernel,
  x.points = xgrid
)

df_kernel <- data.frame(Ethanol = kfit$x, Fatality = kfit$y)

# ======================
# 6) Plot
# ======================

p <- ggplot(df, aes(x = Ethanol, y = Fatality)) +
  geom_point(shape = 1, size = 2, stroke = 0.6) +
  
  # LOESS truncated at x = 3.5 (fit uses only df_left; drawn only over df_left range)
  geom_smooth(
    data = df_left,
    method = "loess", se = FALSE,
    color = "black", linewidth = 0.8,
    span = 1.0,
    method.args = list(degree = 1)
  ) +
  
  # Optional dashed kernel curve (commented out)
  # geom_line(
  #   data = df_kernel,
  #   linewidth = 0.9,
  #   linetype = "dashed"
  # ) +
  
  geom_text(
    data = df_labs,
    aes(label = label, x = Ethanol + dx, y = Fatality + dy),
    size = 3.8, hjust = 0, vjust = 0
  ) +
  
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(5, 21), breaks = c(5, 10, 15, 20)) +
  labs(x = "Consumption of Spirits", y = "Deaths from Liver Disease") +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.title = element_text(size = 13),
    aspect.ratio = 1
  )

final_plot <- ggMarginal(
  p,
  type = "boxplot",
  margins = "both",
  size = 18,
  groupColour = FALSE,
  groupFill = FALSE
)

print(final_plot)

# ======================
# 7) Save outputs
# ======================

# Filenames
f_png <- file.path(out_dir, "Fig01_WilkinsonGraphic.png")
f_pdf <- file.path(out_dir, "Fig01_WilkinsonGraphic.pdf")
f_svg <- file.path(out_dir, "Fig01_WilkinsonGraphic.svg")
f_tif <- file.path(out_dir, "Fig01_WilkinsonGraphic.tif")
f_eps <- file.path(out_dir, "Fig01_WilkinsonGraphic.eps")

ggsave(f_png, plot = final_plot, width = 7, height = 7, dpi = 300)

# PDF/SVG are usually reliable
ggsave(f_pdf, plot = final_plot, width = 7, height = 7)
ggsave(f_svg, plot = final_plot, width = 7, height = 7)

# TIFF: best to use ragg if available; otherwise fall back
if (requireNamespace("ragg", quietly = TRUE)) {
  ggsave(f_tif, plot = final_plot, width = 7, height = 7, dpi = 300, device = ragg::agg_tiff)
} else {
  ggsave(f_tif, plot = final_plot, width = 7, height = 7, dpi = 300)
}

# EPS: requires a PostScript device; cairo_ps is common on Windows but not guaranteed
# If this fails on your system, it's not your analysis—it's the device.
try({
  ggsave(f_eps, plot = final_plot, width = 7, height = 7, device = cairo_ps)
}, silent = TRUE)

cat("\nSaved:\n  ", f_png, "\n  ", f_pdf, "\n  ", f_svg, "\n  ", f_tif, "\n  ", f_eps, "\n")
