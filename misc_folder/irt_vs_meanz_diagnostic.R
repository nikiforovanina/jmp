## ============================================================
## irt_vs_meanz_diagnostic.R
##
## QUESTION: how close are our pooled mean-of-z scores to a
## proper latent-trait estimate from a 2PL/graded-response IRT
## model fit on the same items?
##
## METHOD:
##   1. Source the pooled GESIS scoring script in a sandbox env to
##      get `long_all` (rev-coded raw ordinal values, one row per
##      candidate × item) and the final per-year scored datasets.
##   2. For each dimension (ec, ae):
##        a. Pivot to wide candidate × concept matrix. Items that
##           appear in multiple years share a column (rev_scale was
##           already applied per-year, so polarity is consistent).
##           ec_c11 (only 2021, 11-category) is its own column.
##        b. Fit a unidimensional `mirt` graded response model.
##        c. Extract θ (EAP factor scores).
##   3. Merge θ back to candidates by (year, wknr, partei) and
##      correlate with the current `econ_lr_score` /
##      `anti_elite_score` overall and per party.
##
## NOTE: IRT θ is on its own scale (mean ~0, sd ~1 by construction).
## We z-score it within-dim before plotting/comparing levels, but
## correlations are scale-free so unaffected.
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(haven)
  if (!requireNamespace("mirt", quietly = TRUE)) {
    install.packages("mirt", repos = "https://cloud.r-project.org")
  }
  library(mirt)
})

set.seed(42)

# ---------- run pooled scoring in sandbox env to grab long_all + scored ----------
scoring_dir <- "/Users/ninanikiforova/Desktop/jmp/0_data_construction/1_cand_scores_GESIS"
env <- new.env()
old_wd <- getwd()
setwd(scoring_dir)
source("1_compute_cand_scores_pooled.R", local = env, echo = FALSE)
setwd(old_wd)

long_all     <- env$long_all
party_lut    <- env$party_lut_codes
party_labels <- env$party_labels

scored_all <- bind_rows(
  env$cand13_scored %>% mutate(year = 2013L),
  env$cand17_scored %>% mutate(year = 2017L),
  env$cand21_scored %>% mutate(year = 2021L)
) %>%
  select(year, wknr, partei, party, econ_lr_score, anti_elite_score) %>%
  mutate(
    wknr   = as.numeric(haven::zap_labels(wknr)),
    partei = as.numeric(haven::zap_labels(partei))
  )

# ---------- helper: fit IRT for one dimension and return θ per candidate ----------
fit_irt_dim <- function(dim_code) {
  ldim <- long_all %>%
    filter(dim == dim_code, !is.na(value)) %>%
    mutate(value = as.integer(round(value)))   # mirt needs integer categories

  # Wide: rows = (year, wknr, partei), cols = concept, cells = ordinal value
  wide <- ldim %>%
    distinct(year, wknr, partei, concept, .keep_all = TRUE) %>%
    select(year, wknr, partei, concept, value) %>%
    pivot_wider(names_from = concept, values_from = value)

  ids   <- wide %>% select(year, wknr, partei)
  items <- wide %>% select(-year, -wknr, -partei)

  cat(sprintf("\n[%s] candidates: %d   items (concepts): %d\n",
              dim_code, nrow(items), ncol(items)))
  cat(sprintf("[%s] missingness per item:\n", dim_code))
  print(round(colMeans(is.na(items)), 3))

  # Drop rows with no items answered (mirt errors on all-NA rows)
  keep <- rowSums(!is.na(items)) > 0
  ids_k   <- ids[keep, ]
  items_k <- items[keep, , drop = FALSE]

  # Fit graded response model, unidimensional. mirt handles NA + variable n_categories.
  mod <- mirt(items_k, model = 1, itemtype = "graded",
              SE = FALSE, verbose = FALSE,
              technical = list(NCYCLES = 500))

  theta <- as.numeric(fscores(mod, method = "EAP",
                              full.scores = TRUE, full.scores.SE = FALSE))
  out <- ids_k %>% mutate(theta = theta)
  list(model = mod, theta = out)
}

cat("\n=========== Fitting IRT (graded response) for ec ===========\n")
ec_res <- fit_irt_dim("ec")
cat("\n=========== Fitting IRT (graded response) for ae ===========\n")
ae_res <- fit_irt_dim("ae")

# ---------- merge θ with mean-of-z scores ----------
joined <- scored_all %>%
  left_join(ec_res$theta %>% rename(theta_ec = theta),
            by = c("year","wknr","partei")) %>%
  left_join(ae_res$theta %>% rename(theta_ae = theta),
            by = c("year","wknr","partei"))

# Some candidates may have no answers in a dim → theta NA. Drop for correlations.
cat(sprintf("\nJoined rows: %d   non-NA theta_ec: %d   non-NA theta_ae: %d\n",
            nrow(joined), sum(!is.na(joined$theta_ec)),
            sum(!is.na(joined$theta_ae))))

# ---------- overall correlations ----------
cor_overall <- joined %>%
  summarise(
    cor_ec_pearson  = cor(econ_lr_score,    theta_ec, use = "complete.obs", method = "pearson"),
    cor_ec_spearman = cor(econ_lr_score,    theta_ec, use = "complete.obs", method = "spearman"),
    cor_ae_pearson  = cor(anti_elite_score, theta_ae, use = "complete.obs", method = "pearson"),
    cor_ae_spearman = cor(anti_elite_score, theta_ae, use = "complete.obs", method = "spearman")
  )
cat("\n=== Overall correlation: mean-of-z vs IRT θ ===\n")
print(cor_overall)

# ---------- correlations by year ----------
cor_by_year <- joined %>%
  group_by(year) %>%
  summarise(
    n          = dplyr::n(),
    cor_ec     = cor(econ_lr_score,    theta_ec, use = "complete.obs"),
    cor_ae     = cor(anti_elite_score, theta_ae, use = "complete.obs"),
    .groups = "drop"
  )
cat("\n=== Correlation by year ===\n")
print(cor_by_year)

# ---------- correlations by party (overall) ----------
cor_by_party <- joined %>%
  group_by(party) %>%
  summarise(
    n          = dplyr::n(),
    cor_ec     = cor(econ_lr_score,    theta_ec, use = "complete.obs"),
    cor_ae     = cor(anti_elite_score, theta_ae, use = "complete.obs"),
    .groups = "drop"
  )
cat("\n=== Correlation by party (across all years) ===\n")
print(cor_by_party)

# ---------- per-party means (level comparison, in z units) ----------
joined_z <- joined %>%
  mutate(theta_ec_z = scale(theta_ec)[, 1],
         theta_ae_z = scale(theta_ae)[, 1])

party_levels <- joined_z %>%
  filter(year == 2021) %>%
  group_by(party) %>%
  summarise(
    mean_econ_meanz   = mean(econ_lr_score,    na.rm = TRUE),
    mean_econ_irt_z   = mean(theta_ec_z,       na.rm = TRUE),
    mean_anti_meanz   = mean(anti_elite_score, na.rm = TRUE),
    mean_anti_irt_z   = mean(theta_ae_z,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_econ_meanz))
cat("\n=== 2021 per-party levels: mean-of-z vs IRT θ (both z-scaled) ===\n")
print(party_levels, width = Inf)

# ---------- save ----------
out <- list(
  cor_overall   = cor_overall,
  cor_by_year   = cor_by_year,
  cor_by_party  = cor_by_party,
  party_levels  = party_levels,
  joined        = joined
)
save(out, file = "/Users/ninanikiforova/Desktop/jmp/misc_folder/irt_vs_meanz_diagnostic.Rdata")
cat("\n[saved] /Users/ninanikiforova/Desktop/jmp/misc_folder/irt_vs_meanz_diagnostic.Rdata\n")
