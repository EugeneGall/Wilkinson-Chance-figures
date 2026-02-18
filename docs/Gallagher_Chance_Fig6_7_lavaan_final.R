# Gallagher_Chance_Fig6_7_lavaan_final.R
# Author: Eugene.Gallagher@umb.edu, School for the Environment, UMass Boston,
# retired
# One program that produces:
#   Figure 6: 4x1 facets vs Native American proportion
#            (LD, AFR, DF are FH-shrunken; Ethanol is raw)
#   Figure 7: 2x1 facets vs Speed Limit
#            (AFR and DF are FH-shrunken)
#   Runs the lavaan structural equation model, writing output to /models
# 
# Revised: 17 Feb 2026

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(metafor)
})


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
# ---- Ensure output folders exist ----
# dir.create("../images", showWarnings = FALSE, recursive = TRUE)


# -----------------------------
# 0) Helpers
# -----------------------------
pick_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    stop("None of these candidate columns were found:\n  ",
         paste(candidates, collapse = "\n  "),
         "\n\nAvailable columns include:\n  ",
         paste(names(df), collapse = "\n  "))
  }
  hit[1]
}

to_abb <- function(state_name_or_abb) {
  ifelse(state_name_or_abb %in% state.name,
         state.abb[match(state_name_or_abb, state.name)],
         as.character(state_name_or_abb))
}

# Fay–Herriot helper:
# IMPORTANT: theta_hat = blup(m1)$pred (do NOT add fitted()).
fh_fit <- function(df, y, v, mods_formula) {
  f_mods <- as.formula(mods_formula)
  
  m0 <- metafor::rma.uni(yi = df[[y]], vi = df[[v]],
                         method = "REML", data = df)
  
  m1 <- metafor::rma.uni(yi = df[[y]], vi = df[[v]],
                         mods = f_mods, method = "REML", data = df)
  
  b <- metafor::blup(m1)
  theta_hat <- as.numeric(b$pred)
  
  R2_tau <- ifelse(m0$tau2 > 0, 1 - (m1$tau2 / m0$tau2), NA_real_)
  
  list(m0 = m0, m1 = m1, theta_hat = theta_hat, R2_tau = R2_tau)
}

# -----------------------------
# 1) Read data (use the _b file because it has counts for FH variances)
# -----------------------------
infile_b <- file.path(data_dir, "WilkinsonData2023_b.xlsx")

if (!file.exists(infile_b)) {
  stop('Data file not found: ', infile_b, '\nFix: put the required Excel file in ', data_dir, ' (repo root /data).')
}

sheet_name <- "WilkinsonData2023"  # adjust only if your sheet name differs

dat <- read_excel(infile_b, sheet = sheet_name)

# --- Identify key columns robustly (handles small name variations) ---
col_state <- pick_col(dat, c("State", "state", "STATE", names(dat)[1]))

col_pop   <- pick_col(dat, c("Census Population", "Population", "pop", "Census_Population"))
col_ai    <- pick_col(dat, c("AI_prop", "AI%", "AI %", "% Native American", "Native American Proportion",
                             "Native American proportion", "Native American (%)"))

col_eth   <- pick_col(dat, c("Ethanol Consumption (Gal)", "Ethanol", "Ethanol Consumption (Gal/Person)",
                             "Ethanol Consumption (Gal) ", "Ethanol Consumption"))

col_speed <- pick_col(dat, c("Speed_Limit", "Speed Limit", "Interstate Highway Speed Limit (mph)"))
col_tax   <- pick_col(dat, c("Sales_Tax", "Sales Tax", "Sales tax (%)", "SalesTax"))

# Counts needed for FH for AFR and total driving fatalities
col_ard_ct <- pick_col(dat, c("Alc_Auto_Deaths", "Alc-related auto deaths", "Alcohol_related_auto_deaths",
                              "ARD_count", "AlcAutoDeaths"))
col_df_ct  <- pick_col(dat, c("Deaths", "Total auto deaths", "Auto_Deaths", "DF_count", "TotalDeaths"))

# Liver disease rate (per 100,000) – exact name varies in your files
col_ld_rate <- pick_col(dat, c("Liver Disease Death Rate 2022", "Liver Disease Death Rate per 100,000",
                               "Liver Disease Deaths per 100,000",
                               "Liver Disease 2022", "Liver Disease Deaths", "LD", "Liver Disease Rate 2022"))

# -----------------------------
# 2) Clean/standardize variables
# -----------------------------
dat <- dat %>%
  mutate(
    State = as.character(.data[[col_state]]),
    StateAbb = to_abb(State),
    
    pop_census = as.numeric(.data[[col_pop]]),
    
    # AI proportion (0–1). Your Figure 6 uses "Native American Proportion".
    AI_prop = as.numeric(.data[[col_ai]]),
    AI_prop = ifelse(AI_prop > 1, AI_prop/100, AI_prop),
    
    Ethanol = as.numeric(.data[[col_eth]]),
    Speed_Limit = as.numeric(.data[[col_speed]]),
    Sales_Tax = as.numeric(.data[[col_tax]]),
    
    # Direct rates per 100,000 from counts
    ARD_direct = 1e5 * as.numeric(.data[[col_ard_ct]]) / pop_census,
    v_ARD      = (1e5 / pop_census)^2 * pmax(as.numeric(.data[[col_ard_ct]]), 1e-6),
    
    DF_direct  = 1e5 * as.numeric(.data[[col_df_ct]]) / pop_census,
    v_DF       = (1e5 / pop_census)^2 * pmax(as.numeric(.data[[col_df_ct]]), 1e-6),
    
    # Liver disease rate is given as a rate; to FH-shrink it we need an approximate sampling variance.
    # Use implied deaths count = rate * pop / 1e5 (rounded), then Poisson var(count)=count.
    LD_direct = as.numeric(.data[[col_ld_rate]]),
    LD_deaths_est = pmax(round(LD_direct * pop_census / 1e5), 1),
    v_LD = (1e5 / pop_census)^2 * LD_deaths_est
  )

# -----------------------------
# 3) FH models (matches lavaan covariate set)
# -----------------------------
mods_FH <- "~ Ethanol + Speed_Limit + AI_prop + Sales_Tax"

fit_ARD <- fh_fit(dat, y = "ARD_direct", v = "v_ARD", mods_formula = mods_FH)
fit_DF  <- fh_fit(dat, y = "DF_direct",  v = "v_DF",  mods_formula = mods_FH)
fit_LD  <- fh_fit(dat, y = "LD_direct",  v = "v_LD",  mods_formula = mods_FH)

dat <- dat %>%
  mutate(
    ARD_fh = fit_ARD$theta_hat,
    DF_fh  = fit_DF$theta_hat,
    LD_fh  = fit_LD$theta_hat
  )

cat("\nPseudo-R2 (tau^2 reduction):\n")
cat(sprintf("  LD : %.3f\n", fit_LD$R2_tau))
cat(sprintf("  ARD: %.3f\n", fit_ARD$R2_tau))
cat(sprintf("  DF : %.3f\n", fit_DF$R2_tau))

# -----------------------------
# 4) Label set (your pared list, de-duplicated)
# -----------------------------
state_labels <- unique(c(
  "Alaska", "Arizona", "Delaware", "Hawaii","Idaho", "Massachusetts", "Montana",
  "Nevada", "New Hampshire", "Mississippi", "New Mexico",
  "Oklahoma", "South Dakota", "New York", "North Dakota","Rhode Island",
  "South Carolina", "Utah", "Vermont", "Wyoming"
))


dat <- dat %>%
  mutate(Label = ifelse(State %in% state_labels, State, NA_character_))

# ============================================================
# FIGURE 6 (4x1): vs Native American proportion
#   Panels: LD (FH), ARD (FH), DF (FH), Ethanol (raw)
# ============================================================
fig6_long <- dat %>%
  transmute(
    State, Label, AI_prop,
    `Liver Disease Death Rate per 100,000`           = LD_fh,
    `Alcohol-related Driving Fatalities per 100,000` = ARD_fh,
    `Driving Fatalities per 100,000`                 = DF_fh,
    `Ethanol Consumption (Gal/Person)`               = Ethanol
  ) %>%
  pivot_longer(
    cols = c(`Liver Disease Death Rate per 100,000`,
             `Alcohol-related Driving Fatalities per 100,000`,
             `Driving Fatalities per 100,000`,
             `Ethanol Consumption (Gal/Person)`),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c("Liver Disease Death Rate per 100,000",
                 "Alcohol-related Driving Fatalities per 100,000",
                 "Driving Fatalities per 100,000",
                 "Ethanol Consumption (Gal/Person)")
    )
  )

p6 <- ggplot(fig6_long, aes(x = AI_prop, y = Value)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(
    data = fig6_long %>% filter(!is.na(Label)),
    aes(label = Label),
    size = 4.2, color = "black",
    max.overlaps = Inf,
    min.segment.length = 0
  ) +
  facet_wrap(~ Variable, ncol = 1, scales = "free_y") +
  labs(
    x = "Native American Proportion",
    y = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey85")
  )

print(p6)

ggsave(file.path(out_dir, "Figure6.png"), p6, width = 7.5, height = 10.5, dpi = 600)
ggsave(file.path(out_dir, "Figure6.tif"), p6, width = 7.5, height = 10.5, dpi = 600)
ggsave(file.path(out_dir, "Figure6.eps"), p6, width = 7.5, height = 10.5, dpi = 600)
ggsave(file.path(out_dir, "Figure6.pdf"), p6, width = 7.5, height = 10.5, dpi = 600)

# ============================================================
# FIGURE 7 (2x1): vs Speed limit, using FH-shrunken outcomes
#   Panels: ARD_fh and DF_fh
#   (This is your old Fig07 style, but now with FH y-values.)
# ============================================================
fig7_long <- dat %>%
  transmute(
    State, Label,
    Speed_Limit,
    `Alcohol-related Driving Fatalities per 100,000` = ARD_fh,
    `Driving Fatalities per 100,000`                 = DF_fh
  ) %>%
  pivot_longer(
    cols = c(`Alcohol-related Driving Fatalities per 100,000`,
             `Driving Fatalities per 100,000`),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c("Alcohol-related Driving Fatalities per 100,000",
                 "Driving Fatalities per 100,000")
    )
  )

min_speed <- min(dat$Speed_Limit, na.rm = TRUE)

p7 <- ggplot(fig7_long, aes(x = Speed_Limit, y = Value)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fullrange = TRUE) +
  geom_text_repel(
    data = fig7_long %>% filter(!is.na(Label) & Label != "Hawaii"),
    aes(label = Label),
    size = 4.2, color = "black",
    max.overlaps = Inf,
    min.segment.length = 0
  ) +
  geom_text(
    data = fig7_long %>% filter(Label == "Hawaii"),
    aes(label = Label),
    size = 4.2, color = "black",
    hjust = 0, nudge_x = 0.2
  ) +
  facet_wrap(~ Variable, ncol = 1, scales = "free_y") +
  labs(
    x = "Interstate Highway Speed Limit (mph)",
    y = NULL
  ) +
  scale_x_continuous(limits = c(59, NA)) +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey85")
  )

print(p7)

ggsave(file.path(out_dir, "Figure7.png"), p7, width = 7.5, height = 10.5, dpi = 600)
ggsave(file.path(out_dir, "Figure7.tif"), p7, width = 7.5, height = 10.5, dpi = 600)
ggsave(file.path(out_dir, "Figure7.eps"), p7, width = 7.5, height = 10.5, dpi = 600)
ggsave(file.path(out_dir, "Figure7.pdf"), p7, width = 7.5, height = 10.5, dpi = 600)
# ============================================================
# (Optional) regression summaries to paste into captions
# ============================================================
lm_panel <- function(df, y, x) {
  m <- lm(df[[y]] ~ df[[x]])
  sm <- summary(m)
  c(R2 = unname(sm$r.squared),
    p  = unname(coef(sm)[2,4]))
}

cat("\n=== Figure 6 simple lm summaries (y ~ AI_prop) ===\n")
print(rbind(
  LD  = lm_panel(dat, "LD_fh",  "AI_prop"),
  ARD = lm_panel(dat, "ARD_fh", "AI_prop"),
  DF  = lm_panel(dat, "DF_fh",  "AI_prop"),
  Eth = lm_panel(dat, "Ethanol","AI_prop")
))

cat("\n=== Figure 7 simple lm summaries (y ~ Speed_Limit) ===\n")
print(rbind(
  ARD = lm_panel(dat, "ARD_fh", "Speed_Limit"),
  DF  = lm_panel(dat, "DF_fh",  "Speed_Limit")
))

# ============================================================
# Figure 8 (SEM): lavaan model using FH-shrunken 2023 rates
#   Outcomes: LD_fh, ARD_fh, DF_fh  (FH)
#   Ethanol:  raw (Ethanol)
#   Adds Excise tax term (as in July)
#
# Suggested filename:
#   lavaan_SEM_FH_Excise_Chance_Fig08b.R
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lavaan)
  library(readxl)
})

# ---- If dat already exists from Fig06/07 code, keep it.
#      Otherwise you can uncomment the next line to load it.
# dat <- readxl::read_excel("../data/WilkinsonData2023_b.xlsx", sheet = "WilkinsonData2023")

# ---- Minimal harmonization / naming (adjust only if your column names differ)
# We keep your Figure-6/7 conventions:
#   AI_prop = Native American proportion (0-1)
#   Ethanol = `Ethanol Consumption (Gal)` (raw)
#   LD_fh, ARD_fh, DF_fh = FH shrunken rates per 100,000

# Make sure the needed predictors exist (rename if necessary)
dat_sem <- dat %>%
  mutate(
    Alcohol_Purchase = as.numeric(Ethanol),            # raw
    Native_American  = as.numeric(AI_prop),            # 0-1
    Speed_Limit      = as.numeric(Speed_Limit),
    Sales_Tax        = as.numeric(Sales_Tax),
    Vehicles         = as.numeric(Vehicles_per_person),
    
    Excise_Tax = dplyr::coalesce(
      suppressWarnings(as.numeric(`Excise_Tax_per_Gallon`))
    ),
    # FH outcomes (per 100k)
    Liver_Disease = as.numeric(LD_fh),
    ARAF_per_100k = as.numeric(ARD_fh),
    Auto_Fatalities_per_100k = as.numeric(DF_fh)
  )

# Wyoming special case (same logic you used last July)
if ("State" %in% names(dat_sem)) {
  dat_sem <- dat_sem %>%
    mutate(Sales_Tax = if_else(State %in% c("WY"), 0, Sales_Tax))
}

# ---- Safety checks
need <- c("Alcohol_Purchase","Native_American","Sales_Tax","Excise_Tax",
          "Speed_Limit","Vehicles",
          "Liver_Disease","ARAF_per_100k","Auto_Fatalities_per_100k")
missing <- setdiff(need, names(dat_sem))
if (length(missing) > 0) stop("Missing required SEM columns: ", paste(missing, collapse = ", "))

# ---- Model (same structure as your July model3, but with FH outcomes)
model_fh_excise <- '
  # Liver Disease (FH)
  Liver_Disease ~ Alcohol_Purchase + Native_American

  # Alcohol Purchase (raw)
  Alcohol_Purchase ~ Native_American + Sales_Tax + Excise_Tax

  # Auto Fatalities per 100k (FH)
  Auto_Fatalities_per_100k ~ Speed_Limit + Native_American + Vehicles

  # Alcohol-Related Fatalities per 100k (FH)
  ARAF_per_100k ~ Alcohol_Purchase + Auto_Fatalities_per_100k + Native_American
'

fit_fh_excise <- sem(model_fh_excise, data = dat_sem, meanstructure = TRUE)

cat("\n=== Figure 8 SEM (FH outcomes): fit summary ===\n")
print(summary(fit_fh_excise, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE))

# ---- Output tables you can reuse for DiagrammeR (no hand-copying)
pe <- parameterEstimates(fit_fh_excise, standardized = TRUE) %>%
  as_tibble()

edges <- pe %>%
  filter(op == "~") %>%
  transmute(
    to   = lhs,
    from = rhs,
    est  = est,
    se   = se,
    z    = z,
    p    = pvalue,
    std  = std.all
  )

cat("\n=== SEM edges (regressions), unstandardized + standardized ===\n")
print(edges)

# R^2 for endogenous variables
r2 <- lavInspect(fit_fh_excise, "r2")
cat("\n=== SEM R^2 (endogenous variables) ===\n")
print(r2)

# Save edges/r2 for your DiagrammeR script
saveRDS(list(fit = fit_fh_excise, edges = edges, r2 = r2),
        "../models/SEM_FH_excise_Fig08b.rds")
write.csv(edges, "../models/SEM_FH_excise_Fig08b_edges.csv", row.names = FALSE)

cat("\nSaved: ../models/SEM_FH_excise_Fig08b.rds and ..._edges.csv\n")
