## ============================================================
## 2_compute_cand_scores_irt.R
##
## Estimates anti-elite and economic L-R candidate positions
## using a unidimensional graded-response IRT model per dimension,
## fit on items pooled across 2013/2017/2021. Items that only
## appear in some years are NA for other years; mirt handles
## missingness natively.
##
## Direction convention is preserved from the pooled script
## (higher = more right / more anti-elite) because we reuse its
## `long_all` object (already rev-coded).
##
## Output files mirror the pooled script:
##   data/cand{13,17,21}_scored_irt.Rdata
##   variable inside: cand{YY}_scored
##
## Implementation: sources `1_compute_cand_scores_pooled.R` into a
## sandbox env to reuse the data-loading + item catalog (single
## source of truth), then replaces STEP 2–4 with IRT.
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
  if (!requireNamespace("mirt", quietly = TRUE)) {
    install.packages("mirt", repos = "https://cloud.r-project.org")
  }
  library(mirt)
})

## ---- Reuse pooled script's data + item catalog -----------------
pooled_env <- new.env()
source("1_compute_cand_scores_pooled.R", local = pooled_env, echo = FALSE)

long_all        <- pooled_env$long_all
party_lut_codes <- pooled_env$party_lut_codes
party_labels    <- pooled_env$party_labels
cand13_raw      <- pooled_env$cand13_raw
cand17_raw      <- pooled_env$cand17_raw
cand21_raw      <- pooled_env$cand21_raw

## ============================================================
## IRT FIT per dimension
## ============================================================

fit_irt_dim <- function(ldf, dim_code) {
  d <- ldf %>%
    filter(dim == dim_code, !is.na(value)) %>%
    mutate(value = as.integer(round(value)))    # mirt needs integer categories

  wide <- d %>%
    distinct(year, wknr, partei, concept, .keep_all = TRUE) %>%
    select(year, wknr, partei, concept, value) %>%
    pivot_wider(names_from = concept, values_from = value)

  ids   <- wide %>% select(year, wknr, partei)
  items <- wide %>% select(-year, -wknr, -partei)

  message(sprintf("[IRT %s] candidates: %d   concepts: %d",
                  dim_code, nrow(items), ncol(items)))

  keep <- rowSums(!is.na(items)) > 0
  ids_k   <- ids[keep, ]
  items_k <- items[keep, , drop = FALSE]

  mod <- mirt(items_k, model = 1, itemtype = "graded",
              SE = FALSE, verbose = FALSE,
              technical = list(NCYCLES = 500))

  theta <- as.numeric(fscores(mod, method = "EAP",
                              full.scores = TRUE, full.scores.SE = FALSE))
  ids_k %>% mutate(theta = theta)
}

message("\nFitting IRT for ec dimension...")
ec_theta <- fit_irt_dim(long_all, "ec") %>% rename(econ_lr_score = theta)
message("Fitting IRT for ae dimension...")
ae_theta <- fit_irt_dim(long_all, "ae") %>% rename(anti_elite_score = theta)

## ============================================================
## STANDARDIZE θ to z-scale
##
## IRT identifies latent scale up to mean 0, sd 1 by default. We
## re-z-score across all candidates × years so the output magnitude
## is comparable to the pooled mean-of-z scores. (Per-year means
## then reflect real cross-year movement, since calibration was
## pooled.)
## ============================================================

ec_theta <- ec_theta %>%
  mutate(econ_lr_score = as.numeric(scale(econ_lr_score)))
ae_theta <- ae_theta %>%
  mutate(anti_elite_score = as.numeric(scale(anti_elite_score)))

## ============================================================
## STATE for state, year for state-fallback imputation:
## fold ec_theta + ae_theta into one (year, wknr, partei) table
## with state attached from long_all.
## ============================================================

state_map <- long_all %>%
  distinct(year, wknr, partei, state)

scores_imputed <- state_map %>%
  left_join(ec_theta, by = c("year","wknr","partei")) %>%
  left_join(ae_theta, by = c("year","wknr","partei")) %>%
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

scores_with_party <- scores_imputed %>%
  left_join(party_lut_codes, by = c("partei" = "orig")) %>%
  filter(!is.na(party_id)) %>%
  mutate(party = factor(party_id,
                        levels = names(party_labels),
                        labels = party_labels))

## ============================================================
## Build per-year scored tables (same shape as pooled)
## ============================================================

make_scored <- function(raw_dat, yr, scores_df) {
  sc <- scores_df %>%
    filter(year == yr) %>%
    select(wknr, partei, econ_lr_score, anti_elite_score, party_id, party)
  raw_dat %>%
    left_join(sc, by = c("wknr", "partei"))
}

cand13_scored <- make_scored(cand13_raw, 2013L, scores_with_party) %>% filter(!is.na(party))
cand17_scored <- make_scored(cand17_raw, 2017L, scores_with_party) %>% filter(!is.na(party))
cand21_scored <- make_scored(cand21_raw, 2021L, scores_with_party) %>% filter(!is.na(party))

message(sprintf(
  "\nCandidates scored (IRT): 2013 = %d, 2017 = %d, 2021 = %d",
  nrow(cand13_scored), nrow(cand17_scored), nrow(cand21_scored)
))

for (yr in c(2013, 2017, 2021)) {
  d <- get(sprintf("cand%s_scored", substr(yr, 3, 4)))
  message(sprintf(
    "%d  econ θ: mean=%.3f sd=%.3f | anti-elite θ: mean=%.3f sd=%.3f",
    yr,
    mean(d$econ_lr_score,    na.rm = TRUE), sd(d$econ_lr_score,    na.rm = TRUE),
    mean(d$anti_elite_score, na.rm = TRUE), sd(d$anti_elite_score, na.rm = TRUE)
  ))
}

## ============================================================
## SAVE
## ============================================================

if (!dir.exists("data")) dir.create("data", showWarnings = FALSE)
save(cand13_scored, file = "data/cand13_scored_irt.Rdata")
save(cand17_scored, file = "data/cand17_scored_irt.Rdata")
save(cand21_scored, file = "data/cand21_scored_irt.Rdata")
message("\nSaved: data/cand{13,17,21}_scored_irt.Rdata")
