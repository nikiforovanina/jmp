## ============================================================
## compare_2021_new_old.R
##
## Compares candidate (x, y) positions for YEAR 2021 between
## misc_folder/data_new and misc_folder/data_old, broken down by:
##   - region   (West = state 1..10, East = state 11..16)
##   - party    (cand 1..6 → CDU/CSU, SPD, GRUENE, FDP, LINKE, AfD)
##   - region × party
##
## Background: the two folders contain the same row sets and
## have 100% coverage of x and y in both versions; the values
## have been re-scaled/re-centered between versions. This script
## tells us WHERE the shift concentrates.
## ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

new_dir <- "/Users/ninanikiforova/Desktop/jmp/misc_folder/data_new"
old_dir <- "/Users/ninanikiforova/Desktop/jmp/misc_folder/data_old"
year    <- 2021

party_labels <- c(`1` = "CDU/CSU", `2` = "SPD", `3` = "GRUENE",
                  `4` = "FDP",     `5` = "LINKE", `6` = "AfD")

load_obj <- function(p) {
  e <- new.env(); load(p, envir = e); nm <- ls(e); e[[nm[1]]]
}

read_state_year <- function(dir, st, yr) {
  fn <- file.path(dir, sprintf("data_state%02d_year%d_edu.RData", st, yr))
  d  <- as_tibble(load_obj(fn))
  d$state <- st
  d
}

## ---- Stack both folders, year 2021 -------------------------
new_all <- bind_rows(lapply(1:16, read_state_year, dir = new_dir, yr = year))
old_all <- bind_rows(lapply(1:16, read_state_year, dir = old_dir, yr = year))

stopifnot(nrow(new_all) == nrow(old_all))

## ---- Merge so each row carries x_new, x_old, y_new, y_old --
combined <- new_all %>%
  rename(x_new = x, y_new = y) %>%
  left_join(
    old_all %>% rename(x_old = x, y_old = y),
    by = c("state", "election_id", "cand")
  ) %>%
  mutate(
    region = ifelse(state <= 10, "West", "East"),
    party  = factor(party_labels[as.character(cand)],
                    levels = party_labels),
    dx     = x_new - x_old,
    dy     = y_new - y_old
  )

stopifnot(sum(is.na(combined$x_old)) == 0,
          sum(is.na(combined$y_old)) == 0)

cat(sprintf("Year %d: %d candidate-rows total (%d East, %d West)\n\n",
            year, nrow(combined),
            sum(combined$region == "East"),
            sum(combined$region == "West")))

## ---- Helper: compact summary -------------------------------
summ <- function(df, group_cols) {
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      n           = dplyr::n(),
      mean_x_new  = mean(x_new), mean_x_old = mean(x_old),
      d_mean_x    = mean_x_new - mean_x_old,
      mean_abs_dx = mean(abs(dx)),
      sd_x_new    = sd(x_new),   sd_x_old   = sd(x_old),
      mean_y_new  = mean(y_new), mean_y_old = mean(y_old),
      d_mean_y    = mean_y_new - mean_y_old,
      mean_abs_dy = mean(abs(dy)),
      sd_y_new    = sd(y_new),   sd_y_old   = sd(y_old),
      cor_x       = cor(x_new, x_old),
      cor_y       = cor(y_new, y_old),
      .groups     = "drop"
    )
}

## ---- 1) Overall (sanity) -----------------------------------
cat("==== 1) Overall 2021 ====\n")
print(summ(combined %>% mutate(all = "all"), "all") %>%
        select(-all), width = Inf)

## ---- 2) By region ------------------------------------------
cat("\n==== 2) By region (East vs West) ====\n")
by_region <- summ(combined, "region")
print(by_region %>%
        select(region, n, d_mean_x, mean_abs_dx, sd_x_new, sd_x_old, cor_x,
               d_mean_y, mean_abs_dy, sd_y_new, sd_y_old, cor_y),
      width = Inf)

## ---- 3) By party -------------------------------------------
cat("\n==== 3) By party ====\n")
by_party <- summ(combined, "party")
print(by_party %>%
        select(party, n, d_mean_x, mean_abs_dx, cor_x,
               d_mean_y, mean_abs_dy, cor_y),
      n = Inf, width = Inf)

## ---- 4) By region x party ----------------------------------
cat("\n==== 4) By region x party ====\n")
by_rp <- summ(combined, c("region", "party"))
print(by_rp %>%
        select(region, party, n, d_mean_x, mean_abs_dx, cor_x,
               d_mean_y, mean_abs_dy, cor_y) %>%
        arrange(region, party),
      n = Inf, width = Inf)

## ---- 5) Where is the shift biggest? ------------------------
cat("\n==== 5) Top 10 region x party cells by mean |dx| ====\n")
print(by_rp %>%
        arrange(desc(mean_abs_dx)) %>%
        select(region, party, n, mean_abs_dx, d_mean_x, cor_x) %>%
        head(10),
      width = Inf)

cat("\n==== 6) Top 10 region x party cells by mean |dy| ====\n")
print(by_rp %>%
        arrange(desc(mean_abs_dy)) %>%
        select(region, party, n, mean_abs_dy, d_mean_y, cor_y) %>%
        head(10),
      width = Inf)

## ---- 7) Save the merged dataset for later use --------------
save(combined, by_region, by_party, by_rp,
     file = file.path(dirname(new_dir), "compare_2021_new_old.Rdata"))
cat("\nSaved: misc_folder/compare_2021_new_old.Rdata\n")
