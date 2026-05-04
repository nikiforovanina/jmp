## ============================================================
## 1_compute_cand_scores_pooled.R
##
## Estimates anti-elite and economic L-R candidate positions
## on a scale comparable across 2013, 2017, and 2021.
##
## Normalization strategy:
##   SD is always pooled across the years where an item exists.
##   Centering depends on whether the 2021 wave physically flipped
##   the agree/disagree response endpoints for that item:
##     flipped_in_2021 = TRUE  → subtract YEAR-SPECIFIC mean
##                               (absorbs response-order / primacy
##                                effect from the wave-level flip)
##     flipped_in_2021 = FALSE → subtract POOLED grand mean
##                               (preserves real cross-year level
##                                differences for anchor items
##                                whose scale stayed identical)
##
## All z-scores are averaged within each dimension to yield
## the final score.  NA imputation: per-state median first,
## then global median fallback (same logic as per-year scripts).
##
## Output:  cand13_scored / cand17_scored / cand21_scored
##   (same structure as 0_compute_cand_score_20XX.R outputs)
##   saved as data/cand{XX}_scored_pooled.Rdata
##
## NOTE: Two cross-year variable mappings are ASSUMED – verify
##   against the survey codebook before using:
##   ec_lr_self   : c3(2013) = c3(2017) = c5(2021)
##                  [Left–Right self-placement, 1–11]
##   ec_immigration: c9(2013) = c6(2017) = c8(2021)
##                  [Immigration restriction scale, 1–11]
## ============================================================

.in_source <- any(vapply(sys.calls(), function(x) {
  fn <- tryCatch(as.character(x[[1]])[1], error = function(e) "")
  identical(fn, "source") || identical(fn, "sys.source")
}, logical(1)))
if (interactive() && !.in_source && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
rm(.in_source)
set.seed(42)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})


## ---- Helpers -----------------------------------------------

to_num <- function(x) {
  if (inherits(x, c("haven_labelled", "labelled", "labelled_spss")))
    x <- haven::as_factor(x, levels = "values")
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

rev_scale     <- function(x, lo, hi) {
  x <- to_num(x); ifelse(is.finite(x), (lo + hi) - x, NA_real_)
}
validate_range <- function(x, lo, hi) {
  x <- to_num(x); x[!(x >= lo & x <= hi)] <- NA_real_; x
}


## ---- Party lookup ------------------------------------------

party_lut_codes <- tibble::tribble(
  ~orig, ~party_id,
  2,     1,    # CDU
  3,     1,    # CSU → CDU bucket
  4,     2,    # SPD
  6,     3,    # GRUENE
  5,     4,    # FDP
  7,     5,    # LINKE
  322,   6     # AfD
)

party_labels <- c("1" = "CDU/CSU", "2" = "SPD",    "3" = "GRUENE",
                  "4" = "FDP",     "5" = "LINKE",   "6" = "AfD")
party_colors <- c("CDU/CSU" = "#000000", "SPD" = "#E3000F", "GRUENE" = "#46962B",
                  "FDP"     = "#FFED00", "LINKE"= "#BE3075", "AfD"   = "#009EE0")


## ============================================================
## ITEM CATALOG
##
## Each entry maps one conceptual item to its year-specific
## variable name and reversal flag.
##
## Direction convention (after applying transforms):
##   higher value  →  more anti-elite (ae dimension)
##   higher value  →  more right-wing (ec dimension)
##
## Cross-year items: all three y20XX keys provided → pooled z.
## Two-year items:   two keys provided             → pooled z.
## Year-specific:    one key provided              → within-year z.
## ============================================================

## flipped_in_2021 = TRUE  ⇒ the 2021 wave physically reversed the
## response endpoints for this item (1=agree…5=disagree → 1=disagree…5=agree).
## The rev flag handles direction; flipped_in_2021 triggers within-year
## centering downstream so wave-level response-order shifts are absorbed.
## It is FALSE for items whose 2021 scale stayed identical to 2013/2017
## (1–4 democracy satisfaction, the 1–11 anchored scales) and for
## year-specific items (single year, centering is moot).

item_catalog <- list(

  ## ---- Anti-Elite (ae) dimension ----
  ## All 7 concepts present in all 3 years (d5x → d6x → d7x).
  ## In 2021 the raw coding polarity was reversed, so d7x items
  ## carry rev=TRUE while the 2013/2017 equivalents carry rev=FALSE.

  list(concept = "ae_a", dim = "ae", flipped_in_2021 = TRUE,
       y2013 = list(var = "d5a", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6a", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7a", rev = TRUE,  lo = 1, hi = 5)),

  list(concept = "ae_b", dim = "ae", flipped_in_2021 = TRUE,
       y2013 = list(var = "d5b", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6b", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7b", rev = TRUE,  lo = 1, hi = 5)),

  list(concept = "ae_c", dim = "ae", flipped_in_2021 = TRUE,
       y2013 = list(var = "d5c", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6c", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7c", rev = TRUE,  lo = 1, hi = 5)),

  list(concept = "ae_g", dim = "ae", flipped_in_2021 = TRUE,
       y2013 = list(var = "d5g", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6g", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7g", rev = TRUE,  lo = 1, hi = 5)),

  list(concept = "ae_h", dim = "ae", flipped_in_2021 = TRUE,
       y2013 = list(var = "d5h", rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "d6h", rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "d7h", rev = FALSE, lo = 1, hi = 5)),

  list(concept = "ae_f", dim = "ae", flipped_in_2021 = TRUE,
       y2013 = list(var = "d5f", rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "d6f", rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "d7f", rev = FALSE, lo = 1, hi = 5)),

  ## Satisfaction with democracy: c20 in 2013/2017, c23 in 2021
  ## 1–4 scale; endpoints kept identical across all 3 waves → no flip.
  list(concept = "ae_dem", dim = "ae", flipped_in_2021 = FALSE,
       y2013 = list(var = "c20", rev = FALSE, lo = 1, hi = 4),
       y2017 = list(var = "c20", rev = FALSE, lo = 1, hi = 4),
       y2021 = list(var = "c23", rev = FALSE, lo = 1, hi = 4)),

  ## ---- Economic Left–Right (ec) dimension ----
  ## c2x items: same variable name in all 3 years.
  ## The raw coding polarity was reversed for many items in 2021,
  ## so rev flags differ between 2013/2017 and 2021.

  # Keep government out of economy (agree = right-wing)
  list(concept = "ec_c2b", dim = "ec", flipped_in_2021 = TRUE,
       y2013 = list(var = "c2b", rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "c2b", rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "c2b", rev = FALSE, lo = 1, hi = 5)),

  # Reduce income inequalities (agree = left-wing → flip to right)
  list(concept = "ec_c2h", dim = "ec", flipped_in_2021 = TRUE,
       y2013 = list(var = "c2h", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2h", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2h", rev = TRUE,  lo = 1, hi = 5)),

  # Social security as government goal (agree = left-wing → flip)
  list(concept = "ec_c2g", dim = "ec", flipped_in_2021 = TRUE,
       y2013 = list(var = "c2g", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2g", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2g", rev = TRUE,  lo = 1, hi = 5)),

  # Ban same-sex marriage (agree = conservative/right)
  list(concept = "ec_c2d", dim = "ec", flipped_in_2021 = TRUE,
       y2013 = list(var = "c2d", rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "c2d", rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "c2d", rev = FALSE, lo = 1, hi = 5)),

  # Immigration good for economy (agree = left/liberal → flip)
  list(concept = "ec_c2i", dim = "ec", flipped_in_2021 = TRUE,
       y2013 = list(var = "c2i", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2i", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2i", rev = TRUE,  lo = 1, hi = 5)),

  # Self-determination on abortion (agree = left/liberal → flip)
  list(concept = "ec_c2j", dim = "ec", flipped_in_2021 = TRUE,
       y2013 = list(var = "c2j", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2j", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2j", rev = TRUE,  lo = 1, hi = 5)),

  # Immigrants should adapt culturally (agree = right/restrictive)
  list(concept = "ec_c2a", dim = "ec", flipped_in_2021 = TRUE,
       y2013 = list(var = "c2a", rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "c2a", rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "c2a", rev = FALSE, lo = 1, hi = 5)),

  # Left-Right self-placement (1=left, 11=right; ASSUMED: c3≡c5)
  # 1–11 anchored scale, endpoints identical across waves → no flip.
  list(concept = "ec_lr_self", dim = "ec", flipped_in_2021 = FALSE,
       y2013 = list(var = "c3", rev = FALSE, lo = 1, hi = 11),
       y2017 = list(var = "c3", rev = FALSE, lo = 1, hi = 11),
       y2021 = list(var = "c5", rev = FALSE, lo = 1, hi = 11)),

  # Immigration restriction (1=ease, 11=restrict; ASSUMED: c9≡c6≡c8)
  # 1–11 anchored scale, endpoints identical across waves → no flip.
  list(concept = "ec_immigration", dim = "ec", flipped_in_2021 = FALSE,
       y2013 = list(var = "c9", rev = FALSE, lo = 1, hi = 11),
       y2017 = list(var = "c6", rev = FALSE, lo = 1, hi = 11),
       y2021 = list(var = "c8", rev = FALSE, lo = 1, hi = 11)),

  ## Year-specific economic items (single year → centering is moot,
  ## within-year mean = grand mean by construction; flag set FALSE).

  # 2013 only: Economic stimulus funds (eurozone crisis)
  list(concept = "ec_c19e", dim = "ec", flipped_in_2021 = FALSE,
       y2013 = list(var = "c19e", rev = FALSE, lo = 1, hi = 5),
       y2017 = NULL,
       y2021 = NULL),

  # 2013 only: Financial support from Germany (eurozone crisis)
  list(concept = "ec_c19f", dim = "ec", flipped_in_2021 = FALSE,
       y2013 = list(var = "c19f", rev = FALSE, lo = 1, hi = 5),
       y2017 = NULL,
       y2021 = NULL),

  # 2017 only: Social benefits / redistribution item A
  list(concept = "ec_c18c", dim = "ec", flipped_in_2021 = FALSE,
       y2013 = NULL,
       y2017 = list(var = "c18c", rev = TRUE,  lo = 1, hi = 5),
       y2021 = NULL),

  # 2017 only: Social benefits / redistribution item B
  list(concept = "ec_c18h", dim = "ec", flipped_in_2021 = FALSE,
       y2013 = NULL,
       y2017 = list(var = "c18h", rev = FALSE, lo = 1, hi = 5),
       y2021 = NULL),

  # 2021 only: Taxes vs. social spending (1=taxes/right, 11=benefits/left → rev)
  # 1–11 anchored scale, single year → no within-year centering needed.
  list(concept = "ec_c11", dim = "ec", flipped_in_2021 = FALSE,
       y2013 = NULL,
       y2017 = NULL,
       y2021 = list(var = "c11", rev = TRUE, lo = 1, hi = 11))

)


## ============================================================
## LOAD DATA
## ============================================================

message("Loading data...")
cand13_raw <- read_dta("candidates_2013.dta")
cand17_raw <- read_dta("candidates_2017.dta")
cand21_raw <- read_dta("candidates_2021.dta")

names(cand13_raw) <- tolower(names(cand13_raw))
names(cand17_raw) <- tolower(names(cand17_raw))
names(cand21_raw) <- tolower(names(cand21_raw))

# Standardise key columns and create a unified state variable
cand13_raw <- cand13_raw %>%
  mutate(
    wknr   = to_num(if ("wkname" %in% names(.)) wkname else wknr),
    partei = to_num(if ("a1"     %in% names(.)) a1     else partei),
    state  = to_num(bundesland)
  ) %>%
  filter(!is.na(wknr) & wknr > 0)

cand17_raw <- cand17_raw %>%
  mutate(
    wknr   = to_num(wknr),
    partei = to_num(if ("a1" %in% names(.)) a1 else partei),
    state  = to_num(if      ("bundesland" %in% names(.)) bundesland
                    else if ("bula"       %in% names(.)) bula
                    else NA_real_)
  ) %>%
  filter(!is.na(wknr) & wknr > 0)


cand21_raw <- cand21_raw %>%
  mutate(
    wknr   = to_num(if ("wkname" %in% names(.)) wkname else wknr),
    partei = partei, 
    state  = to_num(bula)
  ) %>%
  filter(!is.na(wknr) & wknr > 0)

raw_list  <- list(cand13_raw, cand17_raw, cand21_raw)
year_ints <- c(2013L, 2017L, 2021L)
year_keys <- c("y2013", "y2017", "y2021")


## ============================================================
## STEP 1: Extract and transform items → long format
##
## For each concept × year, apply:
##   validate_range  → NAs for out-of-range responses
##   rev_scale       → flip polarity if needed
##   to_num          → ensure numeric
## Result: one row per candidate × concept, with the
##   harmonised (direction-consistent) value.
## ============================================================

extract_long <- function(dat, year_int, year_key, catalog) {
  out <- list()
  for (item in catalog) {
    spec <- item[[year_key]]
    if (is.null(spec)) next
    v <- spec$var
    if (!v %in% names(dat)) {
      message(sprintf("  [%d] '%s' (concept '%s') not found – skipped",
                      year_int, v, item$concept))
      next
    }
    vals <- validate_range(dat[[v]], spec$lo, spec$hi)
    vals <- if (spec$rev) rev_scale(vals, spec$lo, spec$hi) else to_num(vals)

    out[[length(out) + 1]] <- tibble(
      year             = year_int,
      wknr             = dat$wknr,
      partei           = dat$partei,
      state            = dat$state,
      concept          = item$concept,
      dim              = item$dim,
      flipped_in_2021  = isTRUE(item$flipped_in_2021),
      value            = vals
    )
  }
  bind_rows(out)
}

message("Extracting and transforming items...")
long_all <- bind_rows(
  mapply(extract_long,
         dat      = raw_list,
         year_int = year_ints,
         year_key = year_keys,
         MoreArgs = list(catalog = item_catalog),
         SIMPLIFY = FALSE)
)


## ============================================================
## STEP 2: Compute normalization parameters
##
## SD is always pooled across all years where the concept exists
## (grand_sd) — keeps the relative weight of each item stable.
## Centering depends on flipped_in_2021:
##   FALSE → use pooled grand_mean (anchor items, single-year items)
##   TRUE  → use year-specific mean (year_mean), so the wave-level
##           response-order shift introduced when 2021 reversed the
##           agree/disagree endpoints is absorbed.
## Year-specific items therefore use within-year statistics
## automatically (only one year contributes).
## ============================================================

message("Computing pooled normalization parameters...")
norm_params <- long_all %>%
  group_by(concept) %>%
  summarise(
    grand_mean  = mean(value, na.rm = TRUE),
    grand_sd    = sd(value,   na.rm = TRUE),
    n_pooled    = sum(!is.na(value)),
    years_used  = paste(sort(unique(year[!is.na(value)])), collapse = ","),
    .groups     = "drop"
  )

year_means <- long_all %>%
  group_by(concept, year) %>%
  summarise(year_mean = mean(value, na.rm = TRUE), .groups = "drop")

message("\nNormalization parameters:")
print(norm_params, n = Inf)

message("\nYear-specific means (used for items where flipped_in_2021=TRUE):")
print(year_means %>% arrange(concept, year), n = Inf)


## ============================================================
## STEP 3: Z-score each observation
##   center = year_mean      if flipped_in_2021
##          = grand_mean     otherwise
##   scale  = grand_sd       (pooled across years in both cases)
## ============================================================

# Optional per-item |z| cap. Set via CAND_Z_CAP env var by 0_data_construction.R
# when CAND_SCORE_MODE is "z_capped_1" or "z_capped_1_5". Empty string ⇒ no cap.
.z_cap_raw <- Sys.getenv("CAND_Z_CAP", unset = "")
.z_cap     <- if (nzchar(.z_cap_raw)) as.numeric(.z_cap_raw) else NA_real_
if (!is.na(.z_cap)) {
  message(sprintf(">>> Applying per-item |z| cap at %.2f", .z_cap))
}

long_normed <- long_all %>% filter(!is.na(value)) %>%
  left_join(norm_params %>% dplyr::select(concept, grand_mean, grand_sd),
            by = "concept") %>%
  left_join(year_means, by = c("concept", "year")) %>%
  mutate(
    center = ifelse(flipped_in_2021, year_mean, grand_mean),
    z_raw = ifelse(
      is.finite(grand_sd) & grand_sd > 0,
      (value - center) / grand_sd,
      NA_real_
    ),
    z = if (is.na(.z_cap)) z_raw else pmin(pmax(z_raw, -.z_cap), .z_cap)
  ) %>%
  dplyr::select(-z_raw)


## ============================================================
## STEP 3b: DIAGNOSTIC – per-item z-distribution check
##
## For each (concept × year), summarise the post-z distribution
## and compare against sibling items in the same (year × dim).
##
## Three flags:
##   flag_sd    : sd_z is <0.5x or >2x the mean sd_z of peers
##                (item is contributing far more / less spread)
##   flag_mean  : |mean_z| exceeds peers' mean |mean_z| by >0.5
##                (item is systematically off-center vs siblings)
##   flag_corr  : leave-one-out correlation with the average of
##                OTHER items in the same (candidate × year × dim)
##                is < 0.10 (item moves independently of the rest)
##
## Use: items flagged in multiple ways are candidates to drop or
## investigate (different construct, scale anomaly, or coding bug).
## ============================================================

item_z_stats <- long_normed %>%
  group_by(year, dim, concept) %>%
  summarise(
    n      = sum(!is.na(z)),
    mean_z = mean(z, na.rm = TRUE),
    sd_z   = sd(z,   na.rm = TRUE),
    p10_z  = quantile(z, 0.10, na.rm = TRUE),
    p90_z  = quantile(z, 0.90, na.rm = TRUE),
    min_z  = min(z, na.rm = TRUE),
    max_z  = max(z, na.rm = TRUE),
    .groups = "drop"
  )

# Peer reference: mean sd_z and mean |mean_z| of OTHER items
# in the same (year, dim)
item_z_stats <- item_z_stats %>%
  group_by(year, dim) %>%
  mutate(
    n_peers       = n() - 1L,
    peer_sd_z     = ifelse(n_peers > 0, (sum(sd_z) - sd_z) / n_peers, NA_real_),
    sd_z_ratio    = sd_z / peer_sd_z,
    peer_mean_abs = ifelse(n_peers > 0,
                           (sum(abs(mean_z)) - abs(mean_z)) / n_peers,
                           NA_real_)
  ) %>%
  ungroup()

# Leave-one-out correlation: each candidate's z on this item vs.
# the average z of all OTHER items in the same dimension that year.
loo_corr <- long_normed %>%
  group_by(year, wknr, partei, dim) %>%
  mutate(
    n_in_dim = sum(!is.na(z)),
    sum_z    = sum(z, na.rm = TRUE),
    z_others = ifelse(
      n_in_dim > 1 & !is.na(z),
      (sum_z - z) / (n_in_dim - 1L),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  group_by(year, dim, concept) %>%
  summarise(
    cor_with_peers = suppressWarnings(
      cor(z, z_others, use = "pairwise.complete.obs")
    ),
    .groups = "drop"
  )

item_z_stats <- item_z_stats %>%
  left_join(loo_corr, by = c("year", "dim", "concept")) %>%
  mutate(
    flag_sd   = !is.na(sd_z_ratio)    & (sd_z_ratio < 0.5 | sd_z_ratio > 2.0),
    flag_mean = !is.na(peer_mean_abs) & (abs(mean_z) > peer_mean_abs + 0.5),
    flag_corr = !is.na(cor_with_peers) & cor_with_peers < 0.10,
    any_flag  = flag_sd | flag_mean | flag_corr
  )

message("\nPer-item z-distribution diagnostics:")
print(
  item_z_stats %>%
    dplyr::select(year, dim, concept, n,
                  mean_z, sd_z, sd_z_ratio,
                  cor_with_peers,
                  flag_sd, flag_mean, flag_corr) %>%
    arrange(year, dim, concept),
  n = Inf
)

flagged <- item_z_stats %>% filter(any_flag)
if (nrow(flagged) > 0) {
  message("\n*** Flagged items (review): ***")
  print(
    flagged %>%
      dplyr::select(year, dim, concept,
                    mean_z, sd_z, sd_z_ratio,
                    cor_with_peers,
                    flag_sd, flag_mean, flag_corr) %>%
      arrange(year, dim, concept),
    n = Inf
  )
} else {
  message("\nNo items flagged by distribution diagnostics.")
}

# Persist diagnostics for later inspection
if (!dir.exists("data")) dir.create("data", showWarnings = FALSE)
save(item_z_stats, file = "data/item_z_diagnostics.Rdata")


## ============================================================
## STEP 4: Average z-scores within dimension → final score
## ============================================================

scores_wide <- long_normed %>%
  group_by(year, wknr, partei, state, dim) %>%
  summarise(
    score   = mean(z, na.rm = TRUE),
    n_items = sum(!is.na(z)),
    .groups = "drop"
  ) %>%
  mutate(score = ifelse(is.nan(score), NA_real_, score)) %>%
  pivot_wider(names_from = dim, values_from = c(score, n_items)) %>%
  rename(
    econ_lr_score    = score_ec,
    anti_elite_score = score_ae,
    n_items_ec       = n_items_ec,
    n_items_ae       = n_items_ae
  )


## ============================================================
## STEP 5: NA imputation
##   1. Per-state median within each year
##   2. Global median within each year (fallback)
## ============================================================

scores_imputed <- scores_wide %>%
  group_by(year, state) %>%
  mutate(
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score,    na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score,    na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  ungroup()


## ============================================================
## STEP 6: Attach party IDs and labels
## ============================================================

scores_with_party <- scores_imputed %>%
  left_join(party_lut_codes, by = c("partei" = "orig")) %>%
  filter(!is.na(party_id)) %>%
  mutate(party = factor(party_id,
                        levels = names(party_labels),
                        labels = party_labels))


## ============================================================
## STEP 7: Produce year-specific scored datasets
##         (same structure as 0_compute_cand_score_20XX.R)
## ============================================================

make_scored <- function(raw_dat, yr, scores_df) {
  sc <- scores_df %>%
    filter(year == yr) %>%
    dplyr::select(wknr, partei, econ_lr_score, anti_elite_score, party_id, party)
  raw_dat %>%
    left_join(sc, by = c("wknr", "partei"))
}

cand13_scored <- make_scored(cand13_raw, 2013L, scores_with_party) %>%
  filter(!is.na(party))
cand17_scored <- make_scored(cand17_raw, 2017L, scores_with_party) %>%
  filter(!is.na(party))
cand21_scored <- make_scored(cand21_raw, 2021L, scores_with_party) %>%
  filter(!is.na(party))

message(sprintf(
  "\nCandidates scored: 2013 = %d, 2017 = %d, 2021 = %d",
  nrow(cand13_scored), nrow(cand17_scored), nrow(cand21_scored)
))

# Quick score summaries
for (yr in c(2013, 2017, 2021)) {
  d <- get(sprintf("cand%s_scored", substr(yr, 3, 4)))
  message(sprintf(
    "\n%d  econ: mean=%.3f sd=%.3f | anti-elite: mean=%.3f sd=%.3f",
    yr,
    mean(d$econ_lr_score,    na.rm = TRUE),
    sd(d$econ_lr_score,      na.rm = TRUE),
    mean(d$anti_elite_score, na.rm = TRUE),
    sd(d$anti_elite_score,   na.rm = TRUE)
  ))
}


## ============================================================
## STEP 8: Save outputs
## ============================================================

save(cand13_scored, file = "data/cand13_scored_pooled.Rdata")
save(cand17_scored, file = "data/cand17_scored_pooled.Rdata")
save(cand21_scored, file = "data/cand21_scored_pooled.Rdata")
message("\nSaved: data/cand{13,17,21}_scored_pooled.Rdata")


## ============================================================
## STEP 9: Diagnostic plots
## ============================================================

all_scored <- bind_rows(
  cand13_scored %>% mutate(year = 2013L),
  cand17_scored %>% mutate(year = 2017L),
  cand21_scored %>% mutate(year = 2021L)
) %>%
  filter(!is.na(econ_lr_score) & !is.na(anti_elite_score))

# Faceted overview: all 3 years side-by-side
p_all <- ggplot(all_scored,
                aes(econ_lr_score, anti_elite_score, color = party)) +
  geom_point(size = 1.5, alpha = 0.6, na.rm = TRUE) +
  scale_color_manual(values = party_colors, name = "Party") +
  facet_wrap(~year) +
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
  labs(
    title    = "Candidate positions – pooled cross-year scale",
    subtitle = "Scores z-normed on grand mean/SD across all available years per item",
    x        = "Economic Left–Right (pooled z-score)",
    y        = "Anti-Elite (pooled z-score)"
  ) +
  theme_minimal(base_size = 13)

ggsave("graphs/plot_cand_pooled_all_years.png",
       p_all, width = 14, height = 5, dpi = 300)

# Individual year plots (matching style of original scripts)
for (yr in c(2013, 2017, 2021)) {
  d  <- all_scored %>% filter(year == yr)
  yy <- substr(yr, 3, 4)
  p  <- ggplot(d, aes(econ_lr_score, anti_elite_score, color = party)) +
    geom_point(size = 2, alpha = 0.8, na.rm = TRUE) +
    scale_color_manual(values = party_colors, name = "Party") +
    coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
    labs(
      title    = sprintf("%d – pooled cross-year scale", yr),
      subtitle = "z-scores anchored to grand mean/SD across all years",
      x        = "Economic Left–Right",
      y        = "Anti-Elite (↑)"
    ) +
    theme_minimal(base_size = 14)
  ggsave(sprintf("graphs/plot_cand_%d_pooled.png", yr),
         p, width = 7, height = 5, dpi = 300)
}

# Party-level medians across years (comparability check)
party_medians <- all_scored %>%
  group_by(year, party) %>%
  summarise(
    econ_med  = median(econ_lr_score,    na.rm = TRUE),
    elite_med = median(anti_elite_score, na.rm = TRUE),
    .groups   = "drop"
  )

p_party_trend <- ggplot(party_medians,
                        aes(x = factor(year), y = econ_med,
                            color = party, group = party)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = party_colors, name = "Party") +
  labs(
    title    = "Party median economic L-R score across years",
    subtitle = "Pooled cross-year z-scale; should be broadly stable if comparability holds",
    x        = "Year", y = "Median economic L-R score"
  ) +
  theme_minimal(base_size = 13)

ggsave("graphs/plot_party_econ_trend_pooled.png",
       p_party_trend, width = 8, height = 5, dpi = 300)

message("\nAll plots saved to graphs/")
message("Done.")
