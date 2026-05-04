# Data construction

End-to-end pipeline that builds the municipality-level analysis datasets used in
the paper. Run it with one click from the repo root:

```r
source("0_data_construction/0_data_construction.R")
```

Or from the terminal:

```sh
Rscript 0_data_construction/0_data_construction.R
```

The entry script sources every step in order, with `chdir = TRUE`, so each
script runs inside its own folder regardless of where you launch it from.

## Configuration (top of `0_data_construction.R`)

- `CAND_SCORE_MODE` — `"pooled"` (default) or `"per_year"`.
  - `"pooled"`: candidate positions are z-scored across all three waves
    (2013/2017/2021). Comparable across years.
  - `"per_year"`: each wave is z-scored independently.
  - The choice is exported via `Sys.setenv()` and read by downstream scripts in
    folders 2 and 4 to pick the matching `cand{YY}_scored_*.Rdata` file.
- `START_FROM` — `NULL` to run the full pipeline, or a substring of a script
  path (e.g. `"3_census_data"`) to resume from that step. Earlier steps print
  `--- SKIP` and are not executed.

## Pipeline steps

1. **`1_cand_scores_GESIS/`** — GESIS candidate-survey item scoring.
   - `pooled` mode runs only `1_compute_cand_scores_pooled.R`.
   - `per_year` mode runs the three `0_compute_cand_score_{YYYY}.R` scripts.
   - Output: `data/cand{YY}_scored_{pooled|LR}.Rdata`.

2. **`2_cand_survey_full_data_wen_waehlen/`** — merge GESIS scores with
   wen-waehlen.de profiles and impute missing positions via elastic net
   (tidymodels/glmnet, ~13 min/year). For each wave:
   - `0b_data_complement_{YYYY}_cand_survey_wen.R`
   - `1_imputing_positions_{YYYY}.R`
   - `2_merge_full_dataset_{YYYY}.R`
   - The `0a_scraping_*.R` scripts are **not** run by the pipeline (their CSV
     outputs are committed; re-running would re-hit the wen-waehlen server).

3. **`3_census_data/`** — `0_census_separate_years.R` parses Destatis
   age/sex tables into per-year municipality demographic shares.

4. **`4_data_with_education/`** — splits demographic shares by education using
   Destatis tables, then assembles the final per-state/per-year datasets.
   - `1_split_shares_by_education.R`
   - `4_full_data_education/0_compute_data_{YYYY}_edu.R` for each wave.
   - Output: `4_full_data_education/data/data_state{SS}_year{YYYY}_edu.RData`,
     containing `results_std` (municipality-level shares) and `pos_std`
     (Wahlkreis × party position medians with WK → state → national fallback).

## Notes

- Every script has a guarded `setwd(...)` block that only fires in interactive
  RStudio runs and is bypassed when sourced from the orchestrator, so paths
  resolve correctly in both modes.
- Inside `norm_age()` the census script uses `stringr::fixed(...)` explicitly to
  avoid masking by `recipes::fixed()` (loaded earlier via tidymodels).
