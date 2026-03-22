# Outliers & Alcohol (Chance, March 2026) — Reproducible R code

This repository contains the R scripts used to generate the figures in:

> Gallagher, E.D. (2026). *Outliers & Alcohol*. **Chance** (March issue).

It is designed so you can clone/download the repo and run each figure script
**without editing paths**, as long as the two Excel files are placed in `/data`.

This repository uses a compact, project-specific univariate empirical-Bayes
(Fay–Herriot-style) shrinkage function for the state-level graphics and
analyses; it is not intended as a full small-area-estimation implementation like
the broader `sae` framework or the multivariate Fay–Herriot package `msae`. 

## Folder layout

```
repo_root/
  Gallagher_Chance_Fig1_final.R
  Gallagher_Chance_Fig2ab_final.R
  Gallagher_Chance_Fig3ab_final.R
  Gallagher_Chance_Fig4ab_final.R
  Gallagher_Chance_Fig5_final.R
  Gallagher_Chance_Fig6_7_lavaan_final.R
  Gallagher_Chance_Fig8_final.R

  data/
    Fig01_Wilkinson_Extracted_Data.xlsx
    WilkinsonData2023_b.xlsx

  images/
    final/      # scripts write figures here

  models/
    SEM_FH_excise_Fig08b_edges.csv   # produced by Fig6_7 script (needed by Fig8)
```

## Quick start

1. Put the two Excel files into `data/`:
   - `Fig01_Wilkinson_Extracted_Data.xlsx`
   - `WilkinsonData2023_b.xlsx`

2. Run any figure script from the repo root, e.g. in RStudio:

```r
source("Gallagher_Chance_Fig1_final.R")
```

All output figures are written to `images/final/`.

### Figure 8 dependency

`Gallagher_Chance_Fig8_final.R` reads an edges table produced by the SEM step:

- Run `Gallagher_Chance_Fig6_7_lavaan_final.R` first; it writes `models/SEM_FH_excise_Fig08b_edges.csv`.
- Then run `Gallagher_Chance_Fig8_final.R`.

(If you already have the CSV in `models/`, you can run Figure 8 directly.)

---

## GitHub sidebar: what “shrinkage” means here (short)

Several figures use **shrinkage**: estimates for small or noisy units (e.g., states with small populations) are *partially pulled toward* a common, model-based expectation.

- In **Fay–Herriot (FH) shrinkage**, each area’s estimate is a weighted average of:
  1) the *direct* estimate for that area, and
  2) a *regression/synthetic* estimate predicted from covariates.

- The weight depends on **uncertainty**:
  - **High-variance / small-area** estimates get **more** shrinkage.
  - **Low-variance / large-area** estimates get **less** shrinkage.

- This is the same core idea as **multilevel modeling** (and Bayesian partial pooling): borrowing strength across groups improves stability, especially for small groups.

In short: shrinkage is a principled way to reduce over-interpretation of noisy small-area rates while preserving genuine signal where the data are precise.
