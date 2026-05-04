# =========================================================
# Build NATIONAL results_std / pos_std by stacking states
# =========================================================
rm(list = ls())

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(stringr)
})

YEAR   <- 2017
STATES <- c(1,3,5,6:10,12:16)

# where your per-state .RData files were saved in your script
in_dir  <- "data"
out_dir <- "data"

state_files <- file.path(in_dir, sprintf("data_state%02d_year%d.RData", STATES, YEAR))

missing <- state_files[!file.exists(state_files)]
if (length(missing) > 0) {
  stop("Missing these state files:\n", paste(missing, collapse = "\n"))
}

# helper to load and return a list(results_std=..., pos_std=...)
load_state_bundle <- function(f) {
  e <- new.env(parent = emptyenv())
  load(f, envir = e)
  if (!exists("results_std", envir = e) || !exists("pos_std", envir = e)) {
    stop("File does not contain results_std/pos_std: ", f)
  }
  list(
    results_std = get("results_std", envir = e),
    pos_std     = get("pos_std",     envir = e)
  )
}

bundles <- map(state_files, load_state_bundle)

# stack results_std (municipality rows)
results_nat <- bind_rows(map(bundles, "results_std")) %>%
  mutate(
    election_id = as.integer(election_id),
    eligible_voters = as.numeric(eligible_voters)
  )

# stack pos_std (WK-level party positions) and deduplicate
pos_nat <- bind_rows(map(bundles, "pos_std")) %>%
  mutate(
    election_id = as.integer(election_id),
    cand = as.integer(cand),
    x = as.numeric(x),
    y = as.numeric(y)
  ) %>%
  distinct(election_id, cand, .keep_all = TRUE)

# -----------------------------
# sanity checks
# -----------------------------
cand_cols <- intersect(paste0("cand", 1:6, "_share"), names(results_nat))
if (length(cand_cols) > 0) {
  rs <- rowSums(results_nat[, cand_cols], na.rm = TRUE)
  message("Cand share row-sum summary (should be ~1 where party_sum>0):")
  print(summary(rs))
}

message("National results_std rows: ", nrow(results_nat))
message("National pos_std rows:     ", nrow(pos_nat))
message("Unique election_id (WK):   ", n_distinct(pos_nat$election_id))

# -----------------------------
# save
# -----------------------------
results_std <- results_nat
pos_std     <- pos_nat


sum(results_std$cand6_share*results_std$eligible_voters*results_std$turnout, na.rm = TRUE)/sum(results_std$eligible_voters*results_std$turnout, na.rm = TRUE)


out_file <- file.path(out_dir, sprintf("data_national_year%d.RData", YEAR))
save(results_std, pos_std, file = out_file)
message("Saved national file: ", out_file)