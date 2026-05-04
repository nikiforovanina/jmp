# ===============================================================
# Year-parametrized 2-parts estimation.
#
# Usage (sourced by 0_2parts_<YEAR>.sh):
#   Rscript 0_2parts_edu.R <YEAR>
# Or set the YEAR environment variable:
#   YEAR=2017 Rscript 0_2parts_edu.R
# ===============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(tibble); library(purrr)
  library(CVXR); library(rlang);  library(readr)
})
suppressMessages(sf::sf_use_s2(FALSE))

# ---- YEAR from CLI arg or env var ----
.args <- commandArgs(trailingOnly = TRUE)
.year_raw <- if (length(.args) >= 1 && nzchar(.args[[1]])) .args[[1]] else Sys.getenv("YEAR", "")
if (!nzchar(.year_raw)) {
  stop("YEAR not provided. Pass as CLI arg (Rscript 0_2parts_edu.R 2017) ",
       "or set env var YEAR=2017.")
}
YEAR <- as.integer(.year_raw)
if (is.na(YEAR)) stop("YEAR must be an integer, got: ", .year_raw)
message(sprintf(">>> Running 0_2parts_edu.R for YEAR = %d", YEAR))

source("0_configuration.R")
source("0_model.R")

# Output dirs
dir.create("bounds",         showWarnings = FALSE, recursive = TRUE)
dir.create("diagnostics",    showWarnings = FALSE, recursive = TRUE)
dir.create("data",           showWarnings = FALSE, recursive = TRUE)
dir.create("fits_afd_only",  showWarnings = FALSE, recursive = TRUE)
dir.create("debug_sc_geom",  showWarnings = FALSE, recursive = TRUE)

SC_GEO_DIR       <- "debug_sc_geom"
WRITE_SC_GEOJSON <- FALSE

REGIONS <- list(
  east = 12:16,   # states 12–16
  west = 1:8     # states 1–11
)

# Year-specific targets providers
source(sprintf("../../targets/RW_%d_6groups.R",         YEAR))  # get_age_gender_targets()
source(sprintf("../../targets/RW_%d_6groups_turnout.R", YEAR))  # get_age_gender_turnout()

for (region in names(REGIONS)) {
  STATES <- REGIONS[[region]]

  for (seed_idx in seq_along(SEED_LIST)) {
    seed_base <- SEED_LIST[seed_idx]
    set.seed(seed_base)

    message(sprintf(
      "\n########## YEAR %d | REGION %s | SEED %d (seed_idx=%d of %d) ##########\n",
      YEAR, toupper(region), seed_base, seed_idx, length(SEED_LIST)
    ))

    for (S in STATES) {
      try(run_one(S, seed_base = seed_base, seed_idx = seed_idx), silent = FALSE)
    }
  }
}
