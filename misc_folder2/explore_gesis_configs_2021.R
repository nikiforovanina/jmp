## ============================================================
## explore_gesis_configs_2021.R
##
## Goal: find a per-item GESIS scoring configuration where the
##       DIRECT (pre-imputation, pre-shrink) AfD mean econ_lr is
##       closer to the OLD-target ≈ 0.84 — but all parties shift
##       coherently (no AfD-only adjustment).
##
## Method:
##   1. Re-run the per-item z-scoring step (STEP 1+2+3 of
##      1_compute_cand_scores_pooled.R) but stop BEFORE the
##      candidate-level mean. Keep long_normed.
##   2. Compute per-party per-item mean(z) tables.
##   3. Show which items push AfD farthest from 0.
##   4. Try several configurations and report all-party means:
##         (A) baseline (current) — all 10 econ items
##         (B) drop 1–11 anchored items (ec_lr_self, ec_immigration,
##             ec_c11) — keep only the c2* and year-specific items
##         (C) drop the AfD-extreme items (top-3 |AfD z|)
##         (D) median(z) per candidate instead of mean(z)
##         (E) trimmed-mean(z) (drop top/bottom item per candidate)
##         (F) per-year normalization (already exists; show again)
## ============================================================

suppressPackageStartupMessages({library(dplyr); library(haven); library(tidyr)})

setwd("/Users/ninanikiforova/Desktop/jmp/0_data_construction/1_cand_scores_GESIS")
sandbox <- new.env()
# Run the pooled scoring inside a sandbox so we can grab long_normed
source("1_compute_cand_scores_pooled.R", local = sandbox, chdir = FALSE)

long_normed <- sandbox$long_normed
cat("\n# long_normed rows:", nrow(long_normed), "\n")
cat("# concepts:", paste(unique(long_normed$concept), collapse=","), "\n\n")

party_lut <- c(`2`="CDU",`3`="CSU",`4`="SPD",`5`="FDP",`6`="GRUENE",`7`="LINKE",`322`="AfD")

ln21 <- long_normed %>%
  filter(year == 2021) %>%
  mutate(partei = suppressWarnings(as.numeric(haven::zap_labels(partei)))) %>%
  mutate(party = party_lut[as.character(partei)]) %>%
  filter(!is.na(party))

cat("=== 1) Per-item per-party mean(z) for econ items, 2021 ===\n")
ec_items <- ln21 %>% filter(dim == "ec") %>% pull(concept) %>% unique()
print(ec_items)

per_item_party <- ln21 %>%
  filter(dim == "ec", !is.na(z)) %>%
  group_by(concept, party) %>%
  summarise(mean_z = round(mean(z), 3), n = n(), .groups="drop") %>%
  pivot_wider(id_cols = concept, names_from = party, values_from = mean_z)
print(per_item_party, n = Inf)

cat("\n=== 2) Per-item AfD-vs-rest gap (how AfD-specific is each item?) ===\n")
afd_dominance <- ln21 %>%
  filter(dim == "ec", !is.na(z)) %>%
  group_by(concept) %>%
  summarise(
    afd       = mean(z[party == "AfD"]),
    rest      = mean(z[party != "AfD"]),
    afd_gap   = afd - rest,
    cdu       = mean(z[party == "CDU"]),
    fdp       = mean(z[party == "FDP"]),
    gruene    = mean(z[party == "GRUENE"]),
    linke     = mean(z[party == "LINKE"]),
    spd       = mean(z[party == "SPD"]),
    .groups   = "drop"
  ) %>%
  arrange(desc(afd_gap))
print(afd_dominance %>% mutate(across(where(is.numeric), ~round(.,3))), n = Inf)

# helper: aggregate (mean) across a chosen item set
party_mean_from_items <- function(items, agg = c("mean","median","trim_one"),
                                  label = "?") {
  agg <- match.arg(agg)
  d <- ln21 %>% filter(dim == "ec", concept %in% items, !is.na(z))
  per_cand <- d %>%
    group_by(party, lfdn = paste(wknr, partei)) %>%
    summarise(
      score = switch(agg,
        mean     = mean(z, na.rm=TRUE),
        median   = median(z, na.rm=TRUE),
        trim_one = if (sum(!is.na(z)) >= 3)
                     mean(sort(z)[-c(1, length(z))], na.rm=TRUE)
                   else mean(z, na.rm=TRUE)
      ),
      .groups = "drop"
    )
  per_cand %>%
    group_by(party) %>%
    summarise(n = n(), mean_econ = round(mean(score, na.rm=TRUE), 3), .groups = "drop") %>%
    mutate(config = label)
}

cat("\n=== 3) Configurations: per-party mean econ across configs ===\n")
all_items <- ec_items
anchored  <- c("ec_lr_self","ec_immigration","ec_c11")
c2_items  <- grep("^ec_c2", ec_items, value = TRUE)
year_specific <- c("ec_c19e","ec_c19f","ec_c18c","ec_c18h")
afd_top3 <- afd_dominance %>% slice(1:3) %>% pull(concept)

configs <- bind_rows(
  party_mean_from_items(all_items,            "mean",     "A: all items, mean"),
  party_mean_from_items(setdiff(all_items, anchored), "mean", "B: drop anchored 1-11 items"),
  party_mean_from_items(setdiff(all_items, afd_top3),  "mean", "C: drop top-3 AfD-extreme items"),
  party_mean_from_items(all_items,            "median",   "D: median(z) per candidate"),
  party_mean_from_items(all_items,            "trim_one", "E: trim-1 mean(z) per candidate"),
  party_mean_from_items(c2_items,             "mean",     "F: only c2_* (5pt likert)"),
  party_mean_from_items(c(c2_items, year_specific), "mean", "G: c2 + year-specific"),
  party_mean_from_items(setdiff(all_items, "ec_c11"), "mean", "H: drop ec_c11 only")
)

cat("\nTarget OLD per-party means (pre-shrink): AfD=0.842, CDU=0.533, FDP=0.421, GRUENE=-0.401, LINKE=-0.563, SPD=-0.337\n\n")

print(
  configs %>%
    pivot_wider(id_cols = config, names_from = party, values_from = mean_econ),
  width = Inf
)

# Show absolute gap to OLD target
target <- tibble::tribble(
  ~party,   ~target,
  "AfD",     0.842,
  "CDU",     0.533,
  "FDP",     0.421,
  "GRUENE", -0.401,
  "LINKE",  -0.563,
  "SPD",    -0.337
)
cat("\n=== 4) Gap to OLD target by config ===\n")
gaps <- configs %>% inner_join(target, by="party") %>%
  mutate(gap = round(mean_econ - target, 3)) %>%
  pivot_wider(id_cols = config, names_from = party, values_from = gap) %>%
  rowwise() %>%
  mutate(rms_gap = round(sqrt(mean(c_across(-config)^2, na.rm=TRUE)), 3)) %>%
  ungroup() %>%
  arrange(rms_gap)
print(gaps, width = Inf)

cat("\n(Gap=0 means perfect match. Negative AfD-gap means config produces lower AfD than baseline.)\n")

cat("\n\n=== 5) More normalization variants ===\n")

# Get raw long_all (pre-z) with per-item raw values
long_all <- sandbox$long_all
la21 <- long_all %>% filter(year == 2021) %>%
  mutate(partei = suppressWarnings(as.numeric(haven::zap_labels(partei)))) %>%
  mutate(party = party_lut[as.character(partei)]) %>%
  filter(!is.na(party), dim == "ec", !is.na(value))

party_means_robust <- function(la, method, label) {
  d <- la %>% group_by(concept) %>%
    mutate(
      gm = mean(value, na.rm=TRUE), gs = sd(value, na.rm=TRUE),
      gmed = median(value, na.rm=TRUE),
      gmad = mad(value, na.rm=TRUE),
      tmean = mean(value[value >= quantile(value, 0.1, na.rm=TRUE) &
                         value <= quantile(value, 0.9, na.rm=TRUE)], na.rm=TRUE),
      tsd   = sd(value[value >= quantile(value, 0.1, na.rm=TRUE) &
                       value <= quantile(value, 0.9, na.rm=TRUE)], na.rm=TRUE),
      z = switch(method,
        zscore     = (value - gm)/gs,
        robust     = (value - gmed)/(gmad + 1e-9),
        trimmed    = (value - tmean)/(tsd + 1e-9),
        cap1       = pmin(pmax((value - gm)/gs, -1), 1),
        cap1_5     = pmin(pmax((value - gm)/gs, -1.5), 1.5),
        rank_norm  = qnorm((rank(value, ties.method="average") - 0.5) / dplyr::n())
      )
    ) %>% ungroup()
  d %>% group_by(party, lfdn = paste(wknr, partei)) %>%
    summarise(score = mean(z, na.rm=TRUE), .groups="drop") %>%
    group_by(party) %>%
    summarise(n=n(), mean_econ = round(mean(score, na.rm=TRUE),3), .groups="drop") %>%
    mutate(config = label)
}

variants <- bind_rows(
  party_means_robust(la21, "zscore",    "Z1: pooled mean+sd (baseline)"),
  party_means_robust(la21, "robust",    "Z2: median+MAD"),
  party_means_robust(la21, "trimmed",   "Z3: 10-90% trimmed mean+sd"),
  party_means_robust(la21, "cap1",      "Z4: cap |z| at 1"),
  party_means_robust(la21, "cap1_5",    "Z5: cap |z| at 1.5"),
  party_means_robust(la21, "rank_norm", "Z6: rank-based qnorm")
)

cat("Target OLD: AfD=0.842, CDU=0.533, FDP=0.421, GRUENE=-0.401, LINKE=-0.563, SPD=-0.337\n\n")
print(variants %>%
  pivot_wider(id_cols = config, names_from = party, values_from = mean_econ),
  width = Inf)

cat("\n=== 6) Gap to OLD target (sorted by RMS) ===\n")
target <- tibble::tribble(
  ~party,   ~target,
  "AfD",     0.842, "CDU", 0.533, "FDP", 0.421,
  "GRUENE", -0.401, "LINKE",-0.563, "SPD", -0.337
)
print(
  variants %>% inner_join(target, by="party") %>%
    mutate(gap = round(mean_econ - target, 3)) %>%
    pivot_wider(id_cols = config, names_from = party, values_from = gap) %>%
    rowwise() %>%
    mutate(rms_gap = round(sqrt(mean(c_across(-config)^2, na.rm=TRUE)), 3)) %>%
    ungroup() %>% arrange(rms_gap),
  width = Inf
)

cat("\n=== 7) Per-year z (within 2021 only) on all 10 econ items ===\n")
# This was the OLD per-year scoring approach
within_yr <- la21 %>% group_by(concept) %>%
  mutate(gm = mean(value, na.rm=TRUE), gs = sd(value, na.rm=TRUE),
         z = (value - gm)/gs) %>% ungroup()
print(within_yr %>%
  group_by(party, lfdn = paste(wknr, partei)) %>%
  summarise(score = mean(z, na.rm=TRUE), .groups="drop") %>%
  group_by(party) %>%
  summarise(n=n(), mean_econ = round(mean(score, na.rm=TRUE),3), .groups="drop"))
