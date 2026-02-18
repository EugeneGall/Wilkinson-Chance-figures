# Gallagher__Chance_Fig8_final.R
# Figure 8 SEM diagram using FH-shrunken outcomes + raw Ethanol
# Auto-inserts standardized coefficients (std) + significance stars (p)
# Key fix: show 3 decimals when |std| < 0.01 so NA -> Auto_Fatalities... is not "0.00"
# Revised: 5 Jan 2026 10:56 AM, 1/7/2026 1:26 PM

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(DiagrammeR)
  library(DiagrammeRsvg)
  library(rsvg)
  library(magick)
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

repo_root <- find_repo_root(required_paths = c('models/SEM_FH_excise_Fig08b_edges.csv'))

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
out_dir <- images_dir

cat("Models dir: ", models_dir, "

")
# -----------------------------
# 0) Paths
# -----------------------------
edges_path <- file.path(models_dir, "SEM_FH_excise_Fig08b_edges.csv")

if (!file.exists(edges_path)) {
  alt <- locate_file("SEM_FH_excise_Fig08b_edges.csv")
  if (!is.na(alt)) {
    message("Note: expected model edges CSV not found at ", edges_path, "
      Using: ", alt)
    edges_path <- alt
  } else {
    stop("Model output not found: ", edges_path,
         "
Fix: run Gallagher_Chance_Fig6_7_lavaan_final.R first (it writes /models),
",
         "     or include the precomputed CSV in the repo under /models.")
  }
}

# -----------------------------
# 1) Read edges table (CSV)
# -----------------------------
edges <- readr::read_csv(edges_path, show_col_types = FALSE)
stopifnot(all(c("to","from","p","std") %in% names(edges)))

# -----------------------------
# 2) Formatting helpers (VECTOR-SAFE)
# -----------------------------
sig_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01, "**",
                       ifelse(p < 0.05, "*", "")
                )
         )
  )
}

fmt_std_one <- function(x) {
  if (is.na(x)) return("")
  digits <- ifelse(abs(x) < 0.01, 3, 2)
  out <- sprintf(paste0("%.", digits, "f"), x)
  if (x >= 0) paste0("+", out) else out
}

fmt_std <- function(x) vapply(x, fmt_std_one, character(1))

make_label <- function(std, p) paste0(fmt_std(std), sig_stars(p))

# Lookup table: key = "from->to"  value = formatted label
edge_labels_df <- edges %>%
  mutate(label = make_label(std, p)) %>%
  transmute(key = paste0(from, "->", to), label)

edge_labels <- setNames(edge_labels_df$label, edge_labels_df$key)

lab <- function(from, to) {
  key <- paste0(from, "->", to)
  if (!is.na(edge_labels[key])) edge_labels[key] else ""
}

# -----------------------------
# 3) Build the diagram (USE lavaan variable names as node IDs)
# -----------------------------
dag_txt <- sprintf("
digraph SEM_DAG_Fig08_FH {
  graph [layout = dot, rankdir = LR]
  node [shape = ellipse, style = filled, fontname = Helvetica, fontsize = 12, width = 1.9]

  # Node IDs match lavaan variables (for automatic labeling)
  Native_American           [label = 'Native American\\nProportion', fillcolor = plum]
  Sales_Tax                 [label = 'Sales Tax', fillcolor = orange]
  Excise_Tax                [label = 'Excise Tax', fillcolor = orange]
  Alcohol_Purchase          [label = 'Alcohol\\nPurchase', fillcolor = lightblue]
  Liver_Disease             [label = 'Liver\\nDisease', fillcolor = lightpink]
  Speed_Limit               [label = 'Speed Limit', fillcolor = green]
  Auto_Fatalities_per_100k  [label = 'Auto Fatalities\\n(per 100,000)', fillcolor = lightpink]
  ARAF_per_100k             [label = 'Alcohol-Related\\nAuto Fatalities\\n(per 100,000)', fillcolor = lightpink]
  Vehicles                  [label = 'Vehicle\\nOwnership', fillcolor = orange]

  # Edge labels are pulled from the exported edges.csv (standardized std + stars)
  Native_American -> Alcohol_Purchase          [label = '%s']
  Sales_Tax       -> Alcohol_Purchase          [label = '%s']
  Excise_Tax      -> Alcohol_Purchase          [label = '%s']

  Alcohol_Purchase -> Liver_Disease            [label = '%s']
  Native_American  -> Liver_Disease            [label = '%s']

  Native_American          -> Auto_Fatalities_per_100k  [label = '%s']
  Speed_Limit              -> Auto_Fatalities_per_100k  [label = '%s']
  Vehicles                 -> Auto_Fatalities_per_100k  [label = '%s']

  Alcohol_Purchase         -> ARAF_per_100k     [label = '%s']
  Auto_Fatalities_per_100k -> ARAF_per_100k     [label = '%s']
  Native_American          -> ARAF_per_100k     [label = '%s']
}
",
lab("Native_American","Alcohol_Purchase"),
lab("Sales_Tax","Alcohol_Purchase"),
lab("Excise_Tax","Alcohol_Purchase"),
lab("Alcohol_Purchase","Liver_Disease"),
lab("Native_American","Liver_Disease"),
lab("Native_American","Auto_Fatalities_per_100k"),
lab("Speed_Limit","Auto_Fatalities_per_100k"),
lab("Vehicles","Auto_Fatalities_per_100k"),
lab("Alcohol_Purchase","ARAF_per_100k"),
lab("Auto_Fatalities_per_100k","ARAF_per_100k"),
lab("Native_American","ARAF_per_100k")
)

dag8 <- grViz(dag_txt)
dag8

# -----------------------------
# 4) Export
# -----------------------------

dag_svg <- export_svg(dag8)
writeLines(dag_svg, file.path(out_dir, "Figure8.svg"))

# High-resolution raster PNG (width in pixels)
rsvg_png(charToRaw(dag_svg), file.path(out_dir, "Figure8.png"), width = 1800)

# Vector formats (width in inches)
rsvg_pdf(charToRaw(dag_svg), file.path(out_dir, "Figure8.pdf"), width = 10)
rsvg_eps(charToRaw(dag_svg), file.path(out_dir, "Figure8.eps"), width = 10)

# Convert high-res PNG to TIFF for journal submission
img <- image_read(file.path(out_dir, "Figure8.png"))

image_write(
  img,
  path        = file.path(out_dir, "Figure8.tif"),
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