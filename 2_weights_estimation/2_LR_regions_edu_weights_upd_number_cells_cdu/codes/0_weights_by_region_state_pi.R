# ===============================================================
# 0_weights_by_region_state_pi.R — unified runner (CDU version)
# w2 grid search, 24-group edu model, state-specific Pi
#
# Usage (sourced by 0_region_state_pi_cdu_<region>_<YEAR>.sh):
#   Rscript 0_weights_by_region_state_pi.R <YEAR> --region east|west
#
# YEAR may also be supplied via the YEAR env var (e.g. YEAR=2017 ...).
# ===============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(tibble); library(purrr)
  library(CVXR); library(rlang); library(readr)
})
suppressMessages(sf::sf_use_s2(FALSE))
set.seed(1509)

# -------------------- parse CLI: YEAR + --region --------------------
args <- commandArgs(trailingOnly = TRUE)

# YEAR: first positional arg, or env var
year_pos_args <- args[!grepl("^--", args)]
year_pos_args <- year_pos_args[!(year_pos_args %in% c("east","west"))]
.year_raw <- if (length(year_pos_args) >= 1 && nzchar(year_pos_args[[1]])) {
  year_pos_args[[1]]
} else {
  Sys.getenv("YEAR", "")
}
if (!nzchar(.year_raw)) {
  stop("YEAR not provided. Pass as first CLI arg ",
       "(Rscript 0_weights_by_region_state_pi.R 2017 --region east) ",
       "or set env var YEAR=2017.")
}
YEAR <- as.integer(.year_raw)
if (is.na(YEAR)) stop("YEAR must be an integer, got: ", .year_raw)

# --region east|west
region_arg_pos <- which(args == "--region")
if (!length(region_arg_pos) || length(args) < region_arg_pos + 1) {
  stop("Usage: Rscript 0_weights_by_region_state_pi.R <YEAR> --region east|west")
}
REGION_RUN <- tolower(trimws(args[region_arg_pos + 1]))
if (!REGION_RUN %in% c("east", "west")) {
  stop("--region must be 'east' or 'west'")
}

message(sprintf(">>> Running 0_weights_by_region_state_pi.R (CDU) | YEAR=%d | REGION=%s",
                YEAR, REGION_RUN))

# -------------------- config + model --------------------
source("0_configuration.R")
source("0_model.R")
stopifnot(exists("SPEC_TAG"), nzchar(SPEC_TAG))

# Output dirs
dir.create("fits_cdu_only", showWarnings = FALSE, recursive = TRUE)
dir.create("diagnostics",   showWarnings = FALSE, recursive = TRUE)

DIAG_CSV <- sprintf(
  "../../weights_output/w2_grid_diagnostics_by_region_state_pi_%s.csv",
  SPEC_TAG
)

# -------------------- region -> states map --------------------
REGIONS <- list(
  east = c(12, 14, 15, 16),
  west = 1:11
)

# -------------------- year-specific targets --------------------
# CDU solver only needs turnout (no group-tolerance / SC constraints).
source(sprintf("../../targets/RW_%d_6groups_turnout.R", YEAR))  # get_age_gender_turnout()

# -------------------- run for this (year, region) --------------------
for (seed_idx in seq_along(SEED_LIST)) {
  seed_base <- SEED_LIST[seed_idx]
  set.seed(seed_base)

  message(sprintf(
    "\n########## YEAR %d | REGION %s | SEED %d (seed_idx=%d of %d) ##########\n",
    YEAR, toupper(REGION_RUN), seed_base, seed_idx, length(SEED_LIST)
  ))

  region_label <- REGION_RUN
  STATES       <- REGIONS[[region_label]]

  res <- tryCatch(
    run_one_region_w2(region_label, STATES,
                      seed_base = seed_base, seed_idx = seed_idx),
    error = function(e) {
      message(sprintf("[ERROR] Region %s: %s", region_label, conditionMessage(e)))
      NULL
    }
  )

  if (is.list(res)) {
    if (!is.null(res$grid_df) && nrow(res$grid_df) > 0) {
      year_csv <- sprintf("diagnostics/w2_grid_region_state_pi_year%d_%s_seed%02d_%s.csv",
                          YEAR, region_label, seed_idx, SPEC_TAG)
      write.table(res$grid_df, file = year_csv, sep = ",",
                  row.names = FALSE,
                  col.names = !file.exists(year_csv),
                  append    = file.exists(year_csv))

      dir.create(dirname(DIAG_CSV), showWarnings = FALSE, recursive = TRUE)
      write.table(res$grid_df, file = DIAG_CSV, sep = ",",
                  row.names = FALSE,
                  col.names = !file.exists(DIAG_CSV),
                  append    = file.exists(DIAG_CSV))
    }
  }

  out_rds <- sprintf("fits_cdu_only/cdu_only_region_state_pi_year%d_%s_seed%02d_%s.rds",
                     YEAR, region_label, seed_idx, SPEC_TAG)
  saveRDS(list(
    year         = YEAR,
    geo_region   = region_label,
    seed_idx     = seed_idx,
    seed_base    = seed_base,
    w1           = W1_FIXED,
    w2_grid      = W2_GRID,
    result       = res,
    EDU_DIFF_TOL = EDU_DIFF_TOL,
    SUBSAMPLE_N  = SUBSAMPLE_N
  ), file = out_rds)
  message(sprintf("[SAVED] Year %d | %s | seed_idx %d -> %s",
                  YEAR, toupper(region_label), seed_idx, out_rds))
}
