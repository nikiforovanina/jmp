# =========================================================
# Build results_std / pos_std for ALL states (1..16) in 2013
# Uses education-split census shares (census_2013_edu.Rdata)
# =========================================================
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(haven)
  library(stringr)
  library(psych)
  library(purrr)
  library(rlang)
  library(readr)
})

dir.create("data", showWarnings = FALSE)

YEAR   <- 2013
STATES <- 1:16
SMALL_MUNI_MAX_ELIGIBLE <- 10000

# ---------- helpers ----------
wk_file_for_year   <- function(Y) "btw13_wkr_gemeinden.csv"
cand_file_for_year <- function(Y) sprintf("candidates_%d.dta", Y)

nz_na0 <- function(x) ifelse(is.na(x), 0, x)
zstd   <- function(x) as.numeric(scale(x))
have   <- function(df, v) v[v %in% names(df)]

normalize_cand_shares <- function(df) {
  cand_cols <- intersect(paste0("cand", 1:6, "_share"), names(df))
  if (!length(cand_cols)) return(df)
  row_sum <- rowSums(df[, cand_cols, drop = FALSE], na.rm = TRUE)
  for (cc in cand_cols) {
    df[[cc]] <- ifelse(row_sum > 0, df[[cc]] / row_sum, df[[cc]])
  }
  df
}

# ---------- party code LUT (map CSU -> CDU bucket 1) ----------
party_lut_codes <- tibble::tribble(
  ~orig, ~party_id,
  2  ,   1,  # CDU
  3  ,   1,  # CSU -> CDU bucket
  4  ,   2,  # SPD
  6  ,   3,  # GRUENE
  5  ,   4,  # FDP
  7  ,   5,  # LINKE
  322,   6   # AfD
)

# =========================================================
# Census -> education-split demographic shares
# =========================================================
load("../census_2013_edu.Rdata")   # → census_2013_edu
census <- census_2013_edu

# Derive 8-digit AGS from the census code (same logic as original scripts)
normalize_code <- function(code) {
  code <- as.character(code)
  n    <- nchar(code)
  ifelse(
    n == 12, paste0(substr(code, 1, 5), substr(code, 10, 12)),
    ifelse(
      n == 11, paste0(substr(code, 1, 4), substr(code, 9, n)),
      ifelse(n == 8, code, NA_character_)
    )
  )
}

census$ags <- vapply(census$code, normalize_code, character(1))
census$ags <- str_pad(census$ags, 8, pad = "0")

# =========================================================
# Elections
# =========================================================
elections_all <- read.csv("federal_muni_harm_25.txt", stringsAsFactors = FALSE)

# =========================================================
# NATIONAL candidate medians (fallback level 3)
# =========================================================
load("../output_data/cand13_scored_full_shrunk.Rdata")
dat_nat          <- cand13_scored_full_shrunk
party_code_nat   <- suppressWarnings(as.numeric(dat_nat$partei))

nat_pos <- dat_nat %>%
  mutate(a1_num = party_code_nat) %>%
  inner_join(party_lut_codes, by = c("a1_num" = "orig")) %>%
  select(wknr, party_id, econ_lr_score, anti_elite_score)

nat_party_medians <- nat_pos %>%
  group_by(party_id) %>%
  summarise(
    econ_nat  = median(econ_lr_score,    na.rm = TRUE),
    elite_nat = median(anti_elite_score, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# One-state builder
# =========================================================
build_state_2013 <- function(STATE_ID, out_dir = "data") {
  message(sprintf("\n=== YEAR %d | STATE %02d ===", YEAR, STATE_ID))

  wk_file <- wk_file_for_year(YEAR)
  if (!file.exists(wk_file)) {
    message(sprintf("  [SKIP] Missing WK->AGS file: %s", wk_file))
    return(invisible(NULL))
  }

  # ---- slice & normalize elections ----
  party_cols <- c("cdu","csu","spd","gruene","fdp","linke_pds","afd")

  e_year <- elections_all %>%
    filter(election_year == YEAR) %>%
    mutate(across(all_of(party_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(across(all_of(party_cols), ~ coalesce(.x, 0))) %>%
    mutate(party_sum = rowSums(across(all_of(party_cols)), na.rm = TRUE)) %>%
    mutate(across(all_of(party_cols),
                  ~ if_else(party_sum > 0, .x / party_sum, NA_real_),
                  .names = "{.col}_norm")) %>%
    select(-party_sum)

  if (nrow(e_year) == 0) { message("  [SKIP] no municipalities"); return(invisible(NULL)) }

  e_year$ags <- str_pad(as.character(e_year$ags), 8, pad = "0")
  census$ags <- str_pad(as.character(census$ags), 8, pad = "0")

  # ---- keep overlap (Census ∩ Elections) ----
  muni_base <- census %>% inner_join(e_year, by = "ags")
  if (nrow(muni_base) == 0) { message("  [SKIP] no census/election overlap"); return(invisible(NULL)) }

  # ---- WK <-> AGS map ----
  wk_map <- read_csv2(
    wk_file,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) %>%
    select(any_of(c("Land","Regierungsbezirk","Kreis","Verbandsgemeinde","Gemeinde","Wahlkreis"))) %>%
    mutate(
      Land             = suppressWarnings(as.integer(Land)),
      Regierungsbezirk = suppressWarnings(as.integer(Regierungsbezirk)),
      Kreis            = suppressWarnings(as.integer(Kreis)),
      Gemeinde         = suppressWarnings(as.integer(Gemeinde))
    ) %>%
    mutate(
      land  = str_pad(Land,             2, pad = "0"),
      rb    = str_pad(Regierungsbezirk, 1, pad = "0"),
      kreis = str_pad(Kreis,            2, pad = "0"),
      gem   = str_pad(Gemeinde,         3, pad = "0"),
      ags   = paste0(land, rb, kreis, gem)
    ) %>%
    filter(Land == STATE_ID) %>%
    transmute(Wahlkreis = as.integer(Wahlkreis), ags = str_pad(ags, 8, pad = "0"))

  if (nrow(wk_map) == 0) { message("  [SKIP] WK map empty"); return(invisible(NULL)) }

  municipalities <- muni_base %>% inner_join(wk_map, by = "ags")

  if (!"Wahlkreis" %in% names(municipalities)) {
    message("  [SKIP] missing Wahlkreis after join"); return(invisible(NULL))
  }

  municipalities_small <- municipalities %>% filter(eligible_voters < SMALL_MUNI_MAX_ELIGIBLE)
  keep_wk <- unique(municipalities_small$Wahlkreis)
  if (length(keep_wk) == 0) { message("  [SKIP] no WK after size filter"); return(invisible(NULL)) }

  # =========================================================
  # Candidate medians: WK -> State -> National fallback
  # =========================================================
  dat_state        <- dat_nat %>% filter(wknr %in% keep_wk)
  if (nrow(dat_state) == 0) { message("  [SKIP] no survey rows in these WK"); return(invisible(NULL)) }

  party_code_state <- suppressWarnings(as.numeric(dat_state$partei))
  pos_long0 <- dat_state %>%
    mutate(a1_num = party_code_state) %>%
    inner_join(party_lut_codes, by = c("a1_num" = "orig")) %>%
    select(wknr, party_id, econ_lr_score, anti_elite_score)

  wk_party_medians <- pos_long0 %>%
    group_by(wknr, party_id) %>%
    summarise(
      econ_wk  = median(econ_lr_score,    na.rm = TRUE),
      elite_wk = median(anti_elite_score, na.rm = TRUE),
      .groups = "drop"
    )

  state_party_medians <- pos_long0 %>%
    group_by(party_id) %>%
    summarise(
      econ_state  = median(econ_lr_score,    na.rm = TRUE),
      elite_state = median(anti_elite_score, na.rm = TRUE),
      .groups = "drop"
    )

  grid <- tidyr::expand_grid(wknr = keep_wk, party_id = 1:6) %>%
    left_join(wk_party_medians,    by = c("wknr","party_id")) %>%
    left_join(state_party_medians, by = "party_id") %>%
    left_join(nat_party_medians,   by = "party_id") %>%
    mutate(
      econ_lr_score    = coalesce(econ_wk,  econ_state,  econ_nat),
      anti_elite_score = coalesce(elite_wk, elite_state, elite_nat)
    ) %>%
    select(wknr, party_id, econ_lr_score, anti_elite_score)

  if (anyNA(grid$econ_lr_score) || anyNA(grid$anti_elite_score)) {
    econ_backup  <- median(grid$econ_lr_score,    na.rm = TRUE)
    elite_backup <- median(grid$anti_elite_score, na.rm = TRUE)
    grid <- grid %>%
      mutate(
        econ_lr_score    = ifelse(is.na(econ_lr_score),    econ_backup,  econ_lr_score),
        anti_elite_score = ifelse(is.na(anti_elite_score), elite_backup, anti_elite_score)
      )
  }

  # =========================================================
  # results_std  (education-split demographic shares + vote shares)
  # =========================================================
  brks <- c("18_24","25_34","35_44","45_59","60_69","70plus")

  municipalities_small <- municipalities_small %>%
    mutate(
      CDU_share    = nz_na0(cdu_norm) + nz_na0(csu_norm),
      SPD_share    = nz_na0(spd_norm),
      GRUENE_share = nz_na0(gruene_norm),
      FDP_share    = nz_na0(fdp_norm),
      LINKE_share  = nz_na0(linke_pds_norm),
      AFD_share    = nz_na0(afd_norm)
    )

  make_results_std <- function(df) {
    party_map <- c(
      CDU_share    = "cand1_share",
      SPD_share    = "cand2_share",
      GRUENE_share = "cand3_share",
      FDP_share    = "cand4_share",
      LINKE_share  = "cand5_share",
      AFD_share    = "cand6_share"
    )
    # 24 education-split demographic columns
    dem_cols <- c(
      paste0("male_",   brks, "_edu_share"),
      paste0("female_", brks, "_edu_share"),
      paste0("male_",   brks, "_noedu_share"),
      paste0("female_", brks, "_noedu_share")
    )

    df %>%
      mutate(
        code_cleaned = coalesce(!!!syms(intersect(c("code_cleaned","ags","code"), names(.)))),
        name         = coalesce(!!!syms(intersect(c("name","region_name"),         names(.))))
      ) %>%
      select(any_of(c(
        "code_cleaned","name","eligible_voters","turnout","Wahlkreis",
        dem_cols, names(party_map)
      ))) %>%
      rename_with(~ unname(party_map[.x]),
                  .cols = intersect(names(party_map), names(.))) %>%
      rename_with(~ "election_id", any_of("Wahlkreis"))
  }

  results_std <- make_results_std(municipalities_small)
  results_std <- normalize_cand_shares(results_std)

  pos_std <- grid %>%
    transmute(
      election_id = wknr,
      cand        = party_id,
      x           = econ_lr_score,
      y           = anti_elite_score
    ) %>%
    distinct()

  out_file <- file.path(out_dir, sprintf("data_state%02d_year%d_edu.RData", STATE_ID, YEAR))
  save(results_std, pos_std, file = out_file)
  message(sprintf("  [OK] Saved: %s  (n_muni=%d, n_pos=%d)",
                  out_file, nrow(results_std), nrow(pos_std)))
}

# ---------- run for all states ----------
invisible(lapply(STATES, function(st) try(build_state_2013(st), silent = FALSE)))
message("\nDone 2013 edu.")

save(dat_nat, file = "data/full_cand_position_13.RData")
