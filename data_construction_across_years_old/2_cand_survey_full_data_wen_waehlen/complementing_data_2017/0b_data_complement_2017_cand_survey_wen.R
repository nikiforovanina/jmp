library(dplyr)
library(stringr)
library(haven)
library(tidyr)
library(tibble)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
rm(list = ls())
set.seed(42)


#----------------------------
# 1) Read cand17 + LUT
#----------------------------
cand17 <- haven::read_dta("candidates_2017.dta")

party_lut_codes <- tibble::tribble(
  ~orig, ~party_id,
  2   ,   1,  # CDU
  3   ,   1,  # CSU -> CDU bucket
  4   ,   2,  # SPD
  6   ,   3,  # GRUENE
  5   ,   4,  # FDP
  7   ,   5,  # LINKE
  322 ,   6   # AfD
)

cand17$partei <- cand17$a1
# Map to party_id, keep only 6 main, presence per (wknr, party_id)
cand17_mapped <- cand17 %>%
  mutate(wknr = as.numeric(wknr)) %>%
  left_join(party_lut_codes, by = c("partei" = "orig")) %>%
  filter(!is.na(party_id), !is.na(wknr), wknr > 0) %>%
  distinct(wknr, party_id) %>%
  mutate(exist = 1L)

# Complete grid for cand17
wknr_all <- cand17 %>% mutate(wknr = as.numeric(wknr)) %>%
  filter(!is.na(wknr), wknr > 0) %>% pull(wknr) %>% unique() %>% sort()

df_exist <- tidyr::expand_grid(
  wknr = wknr_all,
  party_id = 1:6
) %>%
  left_join(cand17_mapped, by = c("wknr","party_id")) %>%
  mutate(exist = if_else(is.na(exist), 0L, exist)) %>%
  arrange(wknr, party_id)

#----------------------------
# 2) Read wen_waehlen + party LUT by name
#----------------------------
wen_waehlen <- read.csv("wen_waehlen_btw17_candidates.csv")
wen_waehlen <- wen_waehlen[wen_waehlen$answered == TRUE, ]

party_lut_names <- tibble::tribble(
  ~party_std,  ~party_id,
  "CDU",           1,
  "CSU",           1,
  "SPD",           2,
  "GRÜNE",         3,   # exact umlaut
  "GRUENE",        3,   # ASCII fallback
  "FDP",           4,
  "DIE LINKE",     5,
  "LINKE",         5,
  "AFD",           6
)

# Normalize & map; keep candidate IDs; one row per (wknr, party_id, candidate_id)
wen_core <- wen_waehlen %>%
  transmute(
    wknr = as.numeric(district_no),
    party_std = toupper(trimws(party)),
    candidate_id = candidate_id
  ) %>%
  left_join(party_lut_names, by = "party_std") %>%
  filter(!is.na(party_id), !is.na(wknr), wknr > 0) %>%
  distinct(wknr, party_id, candidate_id)

# Summarize to existence + list of candidate IDs per (wknr, party_id)
wen_by_group <- wen_core %>%
  group_by(wknr, party_id) %>%
  summarize(
    exist_wen = 1L,
    wen_candidate_ids = list(sort(unique(candidate_id))),
    n_wen_candidates = dplyr::n_distinct(candidate_id),
    .groups = "drop"
  )

# Complete grid for wen (district_no × 6 parties), fill missing as 0 / empty list
wknr_wen <- wen_waehlen$district_no %>% as.numeric() %>% unique() %>% sort()
wen_exist_grid <- expand_grid(
  wknr = wknr_wen,
  party_id = 1:6
) %>%
  left_join(wen_by_group, by = c("wknr","party_id")) %>%
  mutate(
    exist_wen = if_else(is.na(exist_wen), 0L, exist_wen),
    n_wen_candidates = if_else(is.na(n_wen_candidates), 0L, n_wen_candidates)
  )

# Fill empty list for list-column where missing
wen_exist_grid$wen_candidate_ids[is.na(wen_exist_grid$exist_wen)] <- list(character(0))

#----------------------------
# 3) Add exist_wen + candidate IDs to df_exist
#    (keeps the cand17 universe of wknr; switch to full_join if you want union)
#----------------------------
df_exist <- df_exist %>%
  left_join(
    wen_exist_grid %>% select(wknr, party_id, exist_wen, wen_candidate_ids, n_wen_candidates),
    by = c("wknr","party_id")
  ) %>%
  mutate(
    exist_wen = if_else(is.na(exist_wen), 0L, exist_wen),
    n_wen_candidates = if_else(is.na(n_wen_candidates), 0L, n_wen_candidates)
  )

df_exist$add_wen <- df_exist$exist_wen - df_exist$exist

#----------------------------
# 4) Load scores and merge onto wen_exist_grid
#----------------------------
load("../../1_cand_scores_GESIS/data/cand17_scored_LR.Rdata")
# cand17_scored columns assumed: partei, wknr, econ_lr_score, anti_elite_score

cand17_scored_collapsed <- cand17_scored %>%
  mutate(
    partei_num = as.numeric(partei),
    wknr_num   = as.numeric(wknr)
  ) %>%
  #left_join(party_lut_codes, by = c("partei_num" = "orig")) %>%
  filter(!is.na(party_id), !is.na(wknr_num), wknr_num > 0) %>%
  group_by(wknr = wknr_num, party_id) %>%
  summarize(
    econ_lr_score_mean     = mean(econ_lr_score, na.rm = TRUE),
    anti_elite_score_mean  = mean(anti_elite_score, na.rm = TRUE),
    n_candidates_scored    = dplyr::n(),
    .groups = "drop"
  )

# Merge onto wen_exist_grid (which already has exist_wen + candidate IDs)
wen_merged <- wen_exist_grid %>%
  left_join(cand17_scored_collapsed, by = c("wknr","party_id"))

# Optional labels
party_labels <- tibble::tribble(
  ~party_id, ~partei_label,
  1, "CDU/CSU",
  2, "SPD",
  3, "GRUENE",
  4, "FDP",
  5, "LINKE",
  6, "AfD"
)

wen_merged <- wen_merged %>%
  left_join(party_labels, by = "party_id") %>%
  relocate(partei_label, .after = party_id)

wen_merged <- wen_merged %>% filter(exist_wen == 1)


