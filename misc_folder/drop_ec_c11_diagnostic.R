## ============================================================
## drop_ec_c11_diagnostic.R
##
## QUESTION: how much of the AfD x extremeness in 2021 is driven by
## the single 2021-only item ec_c11 (Taxes vs. social spending,
## 1–11 anchored scale)? All other ec_* items are 1–5 Likert-like.
##
## METHOD:
##   1. Source the pooled GESIS scoring script into a local env so we
##      can grab `long_normed` (one row per (candidate × item) with z).
##   2. Recompute the per-candidate ec score TWO ways:
##        (a) baseline: average all ec_* items   (= what the pipeline does)
##        (b) drop_c11: average ec_* items EXCEPT ec_c11
##   3. Compare per-party means in 2021. If AfD drops a lot relative
##      to other parties, ec_c11 is the lever; if everyone moves
##      together, the item is fine.
##   4. Also show, for AfD specifically, the mean z on ec_c11 vs the
##      mean z on the other ec items, so we can see whether ec_c11 is
##      pushing AfD to an extreme that the rest of the data does not
##      support.
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(haven)
})

set.seed(42)

# ---------- run the pooled scoring in a sandbox env to capture long_normed ----
scoring_dir <- "/Users/ninanikiforova/Desktop/jmp/0_data_construction/1_cand_scores_GESIS"
env <- new.env()
old_wd <- getwd()
setwd(scoring_dir)
on.exit(setwd(old_wd), add = TRUE)
source("1_compute_cand_scores_pooled.R", local = env, echo = FALSE)
setwd(old_wd)

long_normed <- env$long_normed     # one row per (cand × item) with z
party_lut    <- env$party_lut_codes
party_labels <- env$party_labels

cat("\nlong_normed rows:", nrow(long_normed), "\n")
cat("ec items present (concept x year):\n")
print(long_normed %>%
        filter(dim == "ec") %>%
        distinct(year, concept) %>%
        arrange(year, concept), n = Inf)

# ---------- helper: aggregate z to candidate-level econ score ----------
aggregate_ec <- function(ldf) {
  ldf %>%
    filter(dim == "ec") %>%
    group_by(year, wknr, partei) %>%
    summarise(econ_lr_score = mean(z, na.rm = TRUE),
              n_items       = sum(!is.na(z)),
              .groups = "drop") %>%
    mutate(econ_lr_score = ifelse(is.nan(econ_lr_score), NA_real_, econ_lr_score))
}

base_scores <- aggregate_ec(long_normed) %>% mutate(variant = "baseline")
drop_scores <- aggregate_ec(long_normed %>% filter(concept != "ec_c11")) %>%
  mutate(variant = "drop_ec_c11")

both <- bind_rows(base_scores, drop_scores) %>%
  inner_join(party_lut, by = c("partei" = "orig")) %>%
  mutate(party = factor(party_id,
                        levels = names(party_labels),
                        labels = party_labels))

# ---------- 2021 per-party comparison ----------
party_summary <- both %>%
  filter(year == 2021) %>%
  group_by(party, variant) %>%
  summarise(n          = dplyr::n(),
            mean_econ  = mean(econ_lr_score, na.rm = TRUE),
            median_econ = median(econ_lr_score, na.rm = TRUE),
            sd_econ    = sd(econ_lr_score, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = variant,
              values_from = c(mean_econ, median_econ, sd_econ)) %>%
  mutate(shift_mean   = mean_econ_drop_ec_c11   - mean_econ_baseline,
         shift_median = median_econ_drop_ec_c11 - median_econ_baseline) %>%
  arrange(desc(mean_econ_baseline))

cat("\n=== 2021: per-party econ score, baseline vs drop_ec_c11 ===\n")
print(party_summary, width = Inf)

# ---------- AfD: ec_c11 contribution vs the rest ----------
afd_partei_codes <- party_lut %>% filter(party_id == 6) %>% pull(orig)
afd_long_2021 <- long_normed %>%
  filter(year == 2021, dim == "ec", partei %in% afd_partei_codes)

cat("\n=== AfD 2021 — per-item z summary (mean, sd, n) ===\n")
print(afd_long_2021 %>%
        group_by(concept) %>%
        summarise(n     = sum(!is.na(z)),
                  mean_z = mean(z, na.rm = TRUE),
                  sd_z   = sd(z,   na.rm = TRUE),
                  .groups = "drop") %>%
        arrange(desc(abs(mean_z))), n = Inf)

cat("\n=== AfD 2021 — share of mean econ score coming from ec_c11 vs rest ===\n")
afd_share <- afd_long_2021 %>%
  mutate(group = ifelse(concept == "ec_c11", "ec_c11", "other_ec")) %>%
  group_by(group) %>%
  summarise(mean_z = mean(z, na.rm = TRUE),
            n_obs  = sum(!is.na(z)),
            .groups = "drop")
print(afd_share)

cat(sprintf(
  "\nAfD baseline mean econ (avg of all ec z's per cand, then avg over cands): %.3f\n",
  mean(base_scores$econ_lr_score[base_scores$year == 2021 &
                                 base_scores$partei %in% afd_partei_codes],
       na.rm = TRUE)))
cat(sprintf(
  "AfD drop_ec_c11 mean econ:                                                %.3f\n",
  mean(drop_scores$econ_lr_score[drop_scores$year == 2021 &
                                 drop_scores$partei %in% afd_partei_codes],
       na.rm = TRUE)))

# ---------- save for later inspection ----------
out <- list(
  party_summary_2021 = party_summary,
  afd_per_item_2021  = afd_long_2021 %>%
    group_by(concept) %>%
    summarise(mean_z = mean(z, na.rm = TRUE),
              sd_z   = sd(z,   na.rm = TRUE),
              n      = sum(!is.na(z)),
              .groups = "drop")
)
save(out, file = "/Users/ninanikiforova/Desktop/jmp/misc_folder/drop_ec_c11_diagnostic.Rdata")
cat("\n[saved] /Users/ninanikiforova/Desktop/jmp/misc_folder/drop_ec_c11_diagnostic.Rdata\n")
