# Gallagher_Chance_Fig5_Final.R
# Author: Eugene.Gallagher@umb.edu, School for the Environment, UMass Boston,
# retired
# DiagrammeR model
# Code last updated: 17 Feb 2026


# ---- Repo-root detection ----
# Recommended: run from repo root; script finds root via this file's location.
# Fallback: if script location can't be detected, uses getwd().

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
    return(normalizePath(dirname(this_file), winslash = "/", mustWork = TRUE))
  }

  return(normalizePath(getwd(), winslash = "/", mustWork = TRUE))
}

repo_root <- get_repo_root()

data_dir   <- file.path(repo_root, "data")
images_dir <- file.path(repo_root, "images", "final")
models_dir <- file.path(repo_root, "models")

if (!dir.exists(images_dir)) dir.create(images_dir, recursive = TRUE)
if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE)

cat("Repo root:  ", repo_root, "\n")
cat("Data dir:   ", data_dir, "\n")
cat("Images dir: ", images_dir, "\n")
out_dir <- images_dir

cat("Models dir: ", models_dir, "

")
setwd("C:/manus/Wilkinson Graphics/R/Github docs")

library(DiagrammeR)
library(DiagrammeRsvg)
library(magick)
library(rsvg)

dag5 <- grViz("
digraph Revised_DAG_Figure6 {
  graph [layout = dot, rankdir = LR]
  node [shape = ellipse, style = filled, fontname = Helvetica, fontsize = 12, width = 1.8]

  Sales_Tax           [label = 'Sales Tax', fillcolor = orange]
  Excise_Tax          [label = 'Excise Tax', fillcolor = orange]
  Alcohol_Purchase    [label = 'Alcohol\\nPurchase', fillcolor = lightblue]
  Alcohol_Consumption [label = 'Alcohol\\nConsumption', fillcolor = lightgrey]
  Liver_Disease       [label = 'Liver\\nDisease', fillcolor = lightpink]
  Auto_Fatalities     [label = 'Auto Fatalities\\n(per 100,000)', fillcolor = lightpink]
  ARAF_per_100k       [label = 'Alcohol-Related\\nAuto Fatalities\\n(per 100,000)', fillcolor = lightpink]
  Speed_Limit         [label = 'Speed Limit', fillcolor = green]
  Native_American     [label = 'Native American\\nProportion', fillcolor = plum]
  Vehicle_Ownership   [label = 'Vehicle\\nOwnership', fillcolor = orange]

  Sales_Tax            -> Alcohol_Purchase     [label = 'to be estimated']
  Excise_Tax           -> Alcohol_Purchase     [label = 'to be estimated']
  Sales_Tax            -> Alcohol_Consumption  [label = 'assumed']
  Excise_Tax           -> Alcohol_Consumption  [label = 'assumed']
  Alcohol_Purchase     -> Alcohol_Consumption  [label = 'assumed']
  Alcohol_Consumption  -> Liver_Disease        [label = 'latent (not to be estimated)']
  Alcohol_Consumption  -> ARAF_per_100k        [label = 'proxy path']
  Speed_Limit          -> Auto_Fatalities      [label = 'to be estimated']
  Auto_Fatalities      -> ARAF_per_100k        [label = 'to be estimated']
  Native_American      -> Liver_Disease        [label = 'to be estimated']
  Native_American      -> Alcohol_Purchase     [label = 'to be estimated']
  Native_American      -> Auto_Fatalities      [label = 'to be estimated']
  Vehicle_Ownership    -> Auto_Fatalities      [label = 'to be estimated']
  Vehicle_Ownership    -> ARAF_per_100k        [label = 'to be estimated']
}
")
dag5
dag_svg <- export_svg(dag5)
writeLines(dag_svg, file.path(out_dir, "Figure5.svg"))

# High-resolution raster PNG (width in pixels)
rsvg_png(charToRaw(dag_svg), file.path(out_dir, "Figure5.png"), width = 1800)

# Vector formats (width in inches)
rsvg_pdf(charToRaw(dag_svg), file.path(out_dir, "Figure5.pdf"), width = 10)
rsvg_eps(charToRaw(dag_svg), file.path(out_dir, "Figure5.eps"), width = 10)

# Convert high-res PNG to TIFF for journal submission
img <- image_read(file.path(out_dir, "Figure5.png"))

image_write(
  img,
  path        = file.path(out_dir, "Figure5.tif"),
  format      = "tiff",
  compression = "lzw",
  density     = "600x600"
)


# NOTE ON WIDTH UNITS AND EXPORT TO TIFF:

# rsvg_* does not provide a TIFF exporter, so we:
#  1) Render the DAG to high-resolution PNG with rsvg_png()
#  2) Convert that PNG to TIFF using the magick package.
# Also note that rsvg_png() interprets `width` in PIXELS (e.g., 1800),
# while rsvg_pdf()/rsvg_eps() interpret `width` in INCHES (e.g., 10).

