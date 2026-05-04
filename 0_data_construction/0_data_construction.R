##########################################################################
### Run the full data-construction pipeline                            ###
###                                                                    ###
### From the repo root:                                                ###
###     R> source("0_data_construction/0_data_construction.R")         ###
###     $ Rscript 0_data_construction/0_data_construction.R            ###
###                                                                    ###
### Each script is sourced with chdir = TRUE so it executes inside     ###
### its own directory, regardless of how this entry point is launched. ###
###                                                                    ###
### Web-scraping scripts (0a_scraping_*.R) are intentionally skipped — ###
### their CSV outputs are committed and re-running them would re-hit   ###
### the wen-waehlen.de server.                                         ###
##########################################################################

if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

##########################################################################
### CONFIG                                                             ###
##########################################################################
# Candidate-score mode used by downstream scripts (folders 2 and 4):
#   "pooled"        – cross-year z-scaled scores (default; comparable across waves)
#   "per_year"      – within-year z-scaled scores (independent per wave)
#   "irt"           – pooled graded-response IRT θ per dimension
#   "legacy"        – pooled scores + reproduce pre-fix downstream behaviour
#                     (no partei_label predictor, mixed-data shrink anchor) so
#                     outputs approximate data_construction_across_years_old.
#   "z_capped_1"    – pooled scores with per-item |z| capped at 1.0 before
#                     averaging across items. Compresses extreme parties
#                     (esp. AfD) more than mainstream parties. The cap is
#                     applied uniformly to all items, all years, all parties.
#   "z_capped_1_5"  – pooled scores with per-item |z| capped at 1.5.
#
# Each mode writes its final per-state-per-year edu files to a *separate*
# folder named  4_data_with_education/4_full_data_education/data_<MODE>/
# so re-running with a different mode does not overwrite the previous one.
CAND_SCORE_MODE <- "pooled"

.allowed_modes <- c("pooled", "per_year", "irt", "legacy",
                    "z_capped_1", "z_capped_1_5")
stopifnot(CAND_SCORE_MODE %in% .allowed_modes)
Sys.setenv(CAND_SCORE_MODE = CAND_SCORE_MODE)

# z-cap modes propagate the numeric cap to the scoring script via
# CAND_Z_CAP. Empty string = no cap (matches pooled).
.cap_for_mode <- function(m) switch(m,
  z_capped_1   = "1",
  z_capped_1_5 = "1.5",
  ""
)
Sys.setenv(CAND_Z_CAP = .cap_for_mode(CAND_SCORE_MODE))
message(">>> Candidate-score mode: ", CAND_SCORE_MODE,
        if (nzchar(Sys.getenv("CAND_Z_CAP")))
          sprintf("  (|z| cap = %s)", Sys.getenv("CAND_Z_CAP")) else "")

# Resume from a given step. Set to NULL to run everything.
# Pass a substring that uniquely identifies the script path, e.g.:
#   START_FROM <- "3_census_data"
#   START_FROM <- "0_compute_data_2017_edu"
START_FROM <- NULL

.started <- is.null(START_FROM)
run_step <- function(path) {
  if (!.started) {
    if (grepl(START_FROM, path, fixed = TRUE)) {
      .started <<- TRUE
    } else {
      message("--- SKIP (before START_FROM): ", path)
      return(invisible(NULL))
    }
  }
  message("\n>>> SOURCING: ", path)
  t0 <- Sys.time()
  source(path, chdir = TRUE, echo = FALSE)
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  message("    done in ", dt, "s")
}

base <- "."   # script lives inside 0_data_construction/, so cwd == base

# ---------- 1. GESIS candidate scores ----------
# In "pooled" mode only the pooled script is needed (it reads raw .dta directly
# and writes cand{YY}_scored_pooled.Rdata). In "per_year" mode each year is
# z-scored independently and the pooled script is skipped.
if (CAND_SCORE_MODE == "per_year") {
  run_step(file.path(base, "1_cand_scores_GESIS/0_compute_cand_score_2013.R"))
  run_step(file.path(base, "1_cand_scores_GESIS/0_compute_cand_score_2017.R"))
  run_step(file.path(base, "1_cand_scores_GESIS/0_compute_cand_score_2021.R"))
} else if (CAND_SCORE_MODE == "irt") {
  run_step(file.path(base, "1_cand_scores_GESIS/2_compute_cand_scores_irt.R"))
} else {
  # "pooled", "legacy", "z_capped_1", and "z_capped_1_5" all consume
  # cand{YY}_scored_pooled.Rdata. The capped modes additionally apply
  # a per-item |z| cap inside the pooled-scoring script (driven by
  # CAND_Z_CAP set above).
  run_step(file.path(base, "1_cand_scores_GESIS/1_compute_cand_scores_pooled.R"))
}

# ---------- 2. wen-waehlen merge + elastic-net imputation ----------
for (y in c(2013, 2017, 2021)) {
  dir_y <- file.path(base, "2_cand_survey_full_data_wen_waehlen",
                     sprintf("complementing_data_%d", y))
  run_step(file.path(dir_y, sprintf("0b_data_complement_%d_cand_survey_wen.R", y)))
  run_step(file.path(dir_y, sprintf("1_imputing_positions_%d.R", y)))
  run_step(file.path(dir_y, sprintf("2_merge_full_dataset_%d.R", y)))
}

# ---------- 3. Census shares by municipality ----------
run_step(file.path(base, "3_census_data/0_census_separate_years.R"))

# ---------- 4. Education split + final per-year/state datasets ----------
run_step(file.path(base, "4_data_with_education/1_split_shares_by_education.R"))
for (y in c(2013, 2017, 2021)) {
  run_step(file.path(base, "4_data_with_education/4_full_data_education",
                     sprintf("0_compute_data_%d_edu.R", y)))
}

message("\n=== Pipeline finished ===")
