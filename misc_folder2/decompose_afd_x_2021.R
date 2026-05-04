## ============================================================
## decompose_afd_x_2021.R
##
## Decomposes the 2021 economic L-R score (x) per item to see
## WHICH GESIS items push each party (and AfD especially) where
## they end up. Replicates the pooled-z normalization of
## 1_compute_cand_scores_pooled.R for the ec dimension.
##
## Output:
##   - Per-item average z by party (2021 only) → who is extreme on what
##   - Per-item average z by party x region   → where each item bites
##   - Item-level contribution to AfD's x mean vs population mean
##     (the "AfD lift" attributable to each item)
## ============================================================

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

GESIS_DIR <- "/Users/ninanikiforova/Desktop/jmp/0_data_construction/1_cand_scores_GESIS"

## ---- Helpers (mirroring 1_compute_cand_scores_pooled.R) ----
to_num <- function(x) {
  if (inherits(x, c("haven_labelled", "labelled", "labelled_spss")))
    x <- haven::as_factor(x, levels = "values")
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}
rev_scale <- function(x, lo, hi) {
  x <- to_num(x); ifelse(is.finite(x), (lo + hi) - x, NA_real_)
}
validate_range <- function(x, lo, hi) {
  x <- to_num(x); x[!(x >= lo & x <= hi)] <- NA_real_; x
}

party_lut <- tibble::tribble(
  ~orig, ~party,
  2,  "CDU/CSU", 3, "CDU/CSU",
  4,  "SPD",
  6,  "GRUENE",
  5,  "FDP",
  7,  "LINKE",
  322,"AfD"
)
party_levels <- c("CDU/CSU","SPD","GRUENE","FDP","LINKE","AfD")

## ---- ec items in 2021 (from item_catalog) ------------------
## name = concept, var2021 = 2021 variable, rev = reversal flag,
## flipped = flipped_in_2021 (drives centering rule)
ec_items_2021 <- tibble::tribble(
  ~concept,           ~var,   ~rev,  ~lo, ~hi, ~flipped,
  "ec_c2b",           "c2b",  FALSE, 1L,  5L,  TRUE,
  "ec_c2h",           "c2h",  TRUE,  1L,  5L,  TRUE,
  "ec_c2g",           "c2g",  TRUE,  1L,  5L,  TRUE,
  "ec_c2d",           "c2d",  FALSE, 1L,  5L,  TRUE,
  "ec_c2i",           "c2i",  TRUE,  1L,  5L,  TRUE,
  "ec_c2j",           "c2j",  TRUE,  1L,  5L,  TRUE,
  "ec_c2a",           "c2a",  FALSE, 1L,  5L,  TRUE,
  "ec_lr_self",       "c5",   FALSE, 1L, 11L,  FALSE,
  "ec_immigration",   "c8",   FALSE, 1L, 11L,  FALSE,
  "ec_c11",           "c11",  TRUE,  1L, 11L,  FALSE
)

## ---- Load all 3 years (we need pooled stats) ---------------
message("Loading GESIS waves 2013/2017/2021...")
load_dta <- function(y) {
  d <- read_dta(file.path(GESIS_DIR, sprintf("candidates_%d.dta", y)))
  names(d) <- tolower(names(d))
  d
}
d13 <- load_dta(2013); d17 <- load_dta(2017); d21 <- load_dta(2021)

## standardise key cols (same as in pooled script)
d13 <- d13 %>% mutate(
  wknr   = to_num(if ("wkname" %in% names(.)) wkname else wknr),
  partei = to_num(if ("a1"     %in% names(.)) a1     else partei),
  state  = to_num(bundesland)
) %>% filter(!is.na(wknr) & wknr > 0)
d17 <- d17 %>% mutate(
  wknr   = to_num(wknr),
  partei = to_num(if ("a1" %in% names(.)) a1 else partei),
  state  = to_num(if      ("bundesland" %in% names(.)) bundesland
                   else if ("bula"      %in% names(.)) bula else NA_real_)
) %>% filter(!is.na(wknr) & wknr > 0)
d21 <- d21 %>% mutate(
  wknr   = to_num(if ("wkname" %in% names(.)) wkname else wknr),
  state  = to_num(bula)
) %>% filter(!is.na(wknr) & wknr > 0)

## ---- For each ec item: pull harmonised values across years ----
## (replicates extract_long → grand_mean / grand_sd)
extract_one_item <- function(dat, year_int, var, rev, lo, hi) {
  if (!var %in% names(dat)) return(tibble())
  v <- validate_range(dat[[var]], lo, hi)
  if (rev) v <- rev_scale(v, lo, hi)
  tibble(year = year_int, value = v)
}

# Map of which year contains which 2021-equivalent var:
# (c2X items share name across years; c5(2021) ≡ c3(2013/2017);
#  c8(2021) ≡ c6(2017) ≡ c9(2013); c11 is 2021-only)
year_var_map <- list(
  ec_c2b = c(`2013`="c2b", `2017`="c2b", `2021`="c2b"),
  ec_c2h = c(`2013`="c2h", `2017`="c2h", `2021`="c2h"),
  ec_c2g = c(`2013`="c2g", `2017`="c2g", `2021`="c2g"),
  ec_c2d = c(`2013`="c2d", `2017`="c2d", `2021`="c2d"),
  ec_c2i = c(`2013`="c2i", `2017`="c2i", `2021`="c2i"),
  ec_c2j = c(`2013`="c2j", `2017`="c2j", `2021`="c2j"),
  ec_c2a = c(`2013`="c2a", `2017`="c2a", `2021`="c2a"),
  ec_lr_self     = c(`2013`="c3", `2017`="c3", `2021`="c5"),
  ec_immigration = c(`2013`="c9", `2017`="c6", `2021`="c8"),
  ec_c11         = c(                         `2021`="c11")
)
# rev flags by year (only differ for flipped_in_2021 items where 2021 reversed coding)
year_rev_map <- list(
  ec_c2b = c(`2013`=TRUE,  `2017`=TRUE,  `2021`=FALSE),
  ec_c2h = c(`2013`=FALSE, `2017`=FALSE, `2021`=TRUE),
  ec_c2g = c(`2013`=FALSE, `2017`=FALSE, `2021`=TRUE),
  ec_c2d = c(`2013`=TRUE,  `2017`=TRUE,  `2021`=FALSE),
  ec_c2i = c(`2013`=FALSE, `2017`=FALSE, `2021`=TRUE),
  ec_c2j = c(`2013`=FALSE, `2017`=FALSE, `2021`=TRUE),
  ec_c2a = c(`2013`=TRUE,  `2017`=TRUE,  `2021`=FALSE),
  ec_lr_self     = c(`2013`=FALSE,`2017`=FALSE,`2021`=FALSE),
  ec_immigration = c(`2013`=FALSE,`2017`=FALSE,`2021`=FALSE),
  ec_c11         = c(                          `2021`=TRUE)
)

dat_by_year <- list(`2013`=d13, `2017`=d17, `2021`=d21)
items_long_all_years <- bind_rows(lapply(ec_items_2021$concept, function(cn) {
  vs <- year_var_map[[cn]]
  rs <- year_rev_map[[cn]]
  spec <- ec_items_2021 %>% filter(concept == cn)
  bind_rows(lapply(names(vs), function(yr) {
    dat <- dat_by_year[[yr]]
    if (!vs[[yr]] %in% names(dat)) return(tibble())
    extract_one_item(dat, as.integer(yr),
                     vs[[yr]], rs[[yr]], spec$lo, spec$hi) %>%
      mutate(concept = cn)
  }))
}))

norm_params <- items_long_all_years %>%
  group_by(concept) %>%
  summarise(grand_mean = mean(value, na.rm = TRUE),
            grand_sd   = sd(value,   na.rm = TRUE), .groups="drop")

year_means <- items_long_all_years %>%
  group_by(concept, year) %>%
  summarise(year_mean = mean(value, na.rm = TRUE), .groups="drop")

## ---- Build the 2021 candidate-level item matrix -----------
items_2021_cand <- bind_rows(lapply(ec_items_2021$concept, function(cn) {
  spec <- ec_items_2021 %>% filter(concept == cn)
  v_name <- year_var_map[[cn]][["2021"]]
  rev_yr <- year_rev_map[[cn]][["2021"]]
  if (!v_name %in% names(d21)) return(tibble())
  v <- validate_range(d21[[v_name]], spec$lo, spec$hi)
  if (rev_yr) v <- rev_scale(v, spec$lo, spec$hi)
  tibble(wknr = d21$wknr, partei = d21$partei,
         state = d21$state, concept = cn, value = v, flipped = spec$flipped)
}))

z_2021 <- items_2021_cand %>%
  left_join(norm_params, by = "concept") %>%
  left_join(year_means %>% filter(year == 2021) %>% select(-year),
            by = "concept") %>%
  mutate(
    center = ifelse(flipped, year_mean, grand_mean),
    z      = (value - center) / grand_sd
  ) %>%
  left_join(party_lut, by = c("partei" = "orig")) %>%
  filter(!is.na(party)) %>%
  mutate(party  = factor(party, levels = party_levels),
         region = ifelse(state <= 10, "West", "East"))

## ---- 1) Per-party mean z per item -------------------------
cat("==== 1) 2021: mean z-score per item, by party ====\n")
per_item_party <- z_2021 %>%
  group_by(concept, party) %>%
  summarise(mean_z = mean(z, na.rm = TRUE),
            n      = sum(!is.na(z)),
            .groups = "drop") %>%
  pivot_wider(names_from = party, values_from = mean_z) %>%
  arrange(concept)
print(per_item_party, width = Inf)

## ---- 2) AfD vs population mean per item -------------------
cat("\n==== 2) AfD lift per item (mean_z_AfD - mean_z_all_parties) ====\n")
pop_means <- z_2021 %>%
  group_by(concept) %>%
  summarise(pop_mean_z = mean(z, na.rm = TRUE), .groups = "drop")

afd_means <- z_2021 %>% filter(party == "AfD") %>%
  group_by(concept) %>%
  summarise(afd_mean_z = mean(z, na.rm = TRUE),
            n_afd      = sum(!is.na(z)),
            .groups    = "drop")

afd_lift <- afd_means %>%
  left_join(pop_means, by = "concept") %>%
  mutate(lift = afd_mean_z - pop_mean_z) %>%
  arrange(desc(abs(lift)))
print(afd_lift, width = Inf)

cat("\n  Sum of lifts (= AfD mean x − population mean x, per item average):\n")
cat(sprintf("    sum(lift)/n_items = %.4f  | direct AfD mean x  = %.4f  | pop mean x = %.4f\n",
    sum(afd_lift$lift) / nrow(afd_lift),
    mean(z_2021$z[z_2021$party == "AfD"]),
    mean(z_2021$z)))

## ---- 3) AfD east vs west ----------------------------------
cat("\n==== 3) AfD: per-item mean z by region ====\n")
afd_by_region <- z_2021 %>% filter(party == "AfD") %>%
  group_by(concept, region) %>%
  summarise(mean_z = mean(z, na.rm = TRUE),
            n      = sum(!is.na(z)), .groups = "drop") %>%
  pivot_wider(names_from = region, values_from = c(mean_z, n)) %>%
  arrange(concept)
print(afd_by_region, width = Inf)

## ---- 4) Pooled-z vs within-2021-z difference, per item ----
##
## This isolates how much each item changes between a hypothetical
## "within-2021-only" normalization (what an old-style year-only
## script would do) and the new pooled normalization.
## For flipped=FALSE multi-year items, this is where the shift lives.
cat("\n==== 4) Per-item z impact: pooled-norm vs 2021-only-norm ====\n")
within_2021 <- items_2021_cand %>%
  group_by(concept) %>%
  summarise(y21_mean = mean(value, na.rm = TRUE),
            y21_sd   = sd(value,   na.rm = TRUE),
            .groups = "drop")

z_compare <- items_2021_cand %>%
  left_join(within_2021, by = "concept") %>%
  left_join(norm_params, by = "concept") %>%
  left_join(year_means %>% filter(year == 2021) %>% select(-year),
            by = "concept") %>%
  mutate(
    center_pooled  = ifelse(flipped, year_mean, grand_mean),
    z_pooled       = (value - center_pooled) / grand_sd,
    z_within_2021  = (value - y21_mean) / y21_sd,
    delta          = z_pooled - z_within_2021
  ) %>%
  left_join(party_lut, by = c("partei" = "orig")) %>%
  filter(!is.na(party)) %>%
  mutate(party = factor(party, levels = party_levels))

per_item_party_delta <- z_compare %>%
  group_by(concept, party) %>%
  summarise(mean_delta = mean(delta, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = party, values_from = mean_delta)

cat("Mean (pooled z − 2021-only z) per (item, party). Negative = pooled\n",
    "scoring shrinks this item's contribution; positive = pooled scoring\n",
    "amplifies it. Items with biggest |AfD| values are the ones most\n",
    "responsible for AfD's shift between schemes.\n\n", sep="")
print(per_item_party_delta %>% arrange(desc(abs(AfD))), width = Inf)

cat("\n==== 5) Mean delta on x = mean of mean_delta across items, per party ====\n")
# This approximates the score-level shift (since x = mean of item z's).
score_shift <- z_compare %>%
  group_by(party, wknr, partei) %>%
  summarise(x_pooled  = mean(z_pooled, na.rm = TRUE),
            x_within  = mean(z_within_2021, na.rm = TRUE),
            .groups   = "drop") %>%
  group_by(party) %>%
  summarise(n              = dplyr::n(),
            mean_x_pooled  = mean(x_pooled, na.rm = TRUE),
            mean_x_within  = mean(x_within, na.rm = TRUE),
            shift          = mean_x_pooled - mean_x_within,
            .groups = "drop")
print(score_shift, width = Inf)

cat("\nIf AfD's row in (5) shows a large positive 'shift', the pooled\n",
    "normalization is exactly what's pushing AfD rightward in the new\n",
    "data vs the old data.\n", sep="")
