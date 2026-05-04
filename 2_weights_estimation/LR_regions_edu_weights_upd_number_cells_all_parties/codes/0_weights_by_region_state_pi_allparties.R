# ===============================================================
# 0_weights_by_region_state_pi_allparties.R
# PER-REGION (EAST / WEST) w2 GRID SEARCH, 24-GROUP EDU MODEL
# WITH STATE-SPECIFIC Pi DISTRIBUTIONS — ALL-PARTIES VERSION
#
# All-parties model:
#   - Full Voronoi partition: all 6 winner regions kept
#     (no OUTSIDE collapsing). KEEP_TOP_N_CELLS = 1000.
#   - ONE shared Pi[state][group] (nc-vector) per state and
#     demographic group — the same preference distribution
#     explains all parties simultaneously.
#   - Capacity matrices B_list[[p]][[gk]] built for every
#     party p = 1..6.
#   - Objective: min sum_{r, p} |pred_{p,r} - share_{p,r}|
#     where pred_{p,r} = sum_gk B_list[[p]][[gk]][r,] %*% Pi[[s_r]][[gk]]
#   - No second-choice or representative-statistics-moments
#     constraints; only Pi >= 0, sum(Pi) = 1, and
#     edu/non-edu similarity.
#   - Year is passed as --year argument (2013 | 2017 | 2021).
#
# Usage:
#   Rscript 0_weights_by_region_state_pi_allparties.R --region east --year 2013
#   Rscript 0_weights_by_region_state_pi_allparties.R --region west --year 2017
# ===============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(tibble); library(purrr)
  library(CVXR); library(rlang); library(readr)
})
suppressMessages(sf::sf_use_s2(FALSE))
set.seed(1509)

# -------------------- command-line arguments --------------------
args <- commandArgs(trailingOnly = TRUE)

region_arg_pos <- which(args == "--region")
if (!length(region_arg_pos) || length(args) < region_arg_pos + 1)
  stop("Usage: Rscript 0_weights_by_region_state_pi_allparties.R --region east|west --year 2013|2017|2021")
REGION_RUN <- tolower(trimws(args[region_arg_pos + 1]))
if (!REGION_RUN %in% c("east", "west"))
  stop("--region must be 'east' or 'west'")

year_arg_pos <- which(args == "--year")
if (!length(year_arg_pos) || length(args) < year_arg_pos + 1)
  stop("Usage: Rscript 0_weights_by_region_state_pi_allparties.R --region east|west --year 2013|2017|2021")
YEAR <- as.integer(trimws(args[year_arg_pos + 1]))
if (!YEAR %in% c(2013L, 2017L, 2021L))
  stop("--year must be 2013, 2017, or 2021")

# -------------------- knobs --------------------
SUBSAMPLE_N               <- 50
MAX_ELECTIONS_PER_STATE   <- 10
RESULTS_MIN               <- 1e-5

EDU_DIFF_TOL              <- 0.20
ABS_EPS                   <- 1e-5
REL_EPS                   <- 1e-5

SMALL_MUNI_MAX_ELIGIBLE   <- 10000
MIN_ROWS_PER_ELECTION     <- 5

SEED_LIST                 <- c(1509)

VOR_PAD                   <- 5

# Keep this many largest cells across all parties
KEEP_TOP_N_CELLS          <- 1000

W1_FIXED <- 1
W2_GRID  <- c(0.2, 0.25, 0.4, 0.5, 0.75, 1, 1.25, 2, 2.5, 4, 5)

LAMBDA_CELLS <- 0.0

N_PARTIES <- 6L

dir.create("fits_allparties", showWarnings = FALSE, recursive = TRUE)
dir.create("diagnostics",     showWarnings = FALSE, recursive = TRUE)

DIAG_CSV <- "../../weights_output/w2_grid_diagnostics_by_region_state_pi_allparties.csv"

# -------------------- helpers --------------------
safe_int <- function(x) {
  if (inherits(x, c("haven_labelled","labelled","labelled_spss"))) {
    y <- try(haven::zap_labels(x), silent = TRUE)
    if (!inherits(y, "try-error")) return(suppressWarnings(as.integer(y)))
  }
  if (is.factor(x))    return(suppressWarnings(as.integer(as.character(x))))
  if (is.character(x)) return(suppressWarnings(as.integer(x)))
  suppressWarnings(as.integer(x))
}

safe_first_non_na <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) return(rep(NA_character_, nrow(df)))
  vals <- lapply(cols, function(nm) df[[nm]])
  do.call(coalesce, vals)
}

# -------------------- 24-group specification (edu) --------------------
group_spec <- tibble::tribble(
  ~key,                  ~pretty,               ~col,                           ~tgt_name,
  "female_1825",         "Female 18-25",        "female_18_24_noedu_share",     "female_1825",
  "female_1825_edu",     "Female 18-25 (edu)",  "female_18_24_edu_share",       "female_1825_edu",
  "female_2535",         "Female 25-35",        "female_25_34_noedu_share",     "female_2535",
  "female_2535_edu",     "Female 25-35 (edu)",  "female_25_34_edu_share",       "female_2535_edu",
  "female_3545",         "Female 35-45",        "female_35_44_noedu_share",     "female_3545",
  "female_3545_edu",     "Female 35-45 (edu)",  "female_35_44_edu_share",       "female_3545_edu",
  "female_4560",         "Female 45-60",        "female_45_59_noedu_share",     "female_4560",
  "female_4560_edu",     "Female 45-60 (edu)",  "female_45_59_edu_share",       "female_4560_edu",
  "female_6070",         "Female 60-70",        "female_60_69_noedu_share",     "female_6070",
  "female_6070_edu",     "Female 60-70 (edu)",  "female_60_69_edu_share",       "female_6070_edu",
  "female_70plus",       "Female 70+",          "female_70plus_noedu_share",    "female_70plus",
  "female_70plus_edu",   "Female 70+ (edu)",    "female_70plus_edu_share",      "female_70plus_edu",
  "male_1825",           "Male 18-25",          "male_18_24_noedu_share",       "male_1825",
  "male_1825_edu",       "Male 18-25 (edu)",    "male_18_24_edu_share",         "male_1825_edu",
  "male_2535",           "Male 25-35",          "male_25_34_noedu_share",       "male_2535",
  "male_2535_edu",       "Male 25-35 (edu)",    "male_25_34_edu_share",         "male_2535_edu",
  "male_3545",           "Male 35-45",          "male_35_44_noedu_share",       "male_3545",
  "male_3545_edu",       "Male 35-45 (edu)",    "male_35_44_edu_share",         "male_3545_edu",
  "male_4560",           "Male 45-60",          "male_45_59_noedu_share",       "male_4560",
  "male_4560_edu",       "Male 45-60 (edu)",    "male_45_59_edu_share",         "male_4560_edu",
  "male_6070",           "Male 60-70",          "male_60_69_noedu_share",       "male_6070",
  "male_6070_edu",       "Male 60-70 (edu)",    "male_60_69_edu_share",         "male_6070_edu",
  "male_70plus",         "Male 70+",            "male_70plus_noedu_share",      "male_70plus",
  "male_70plus_edu",     "Male 70+ (edu)",      "male_70plus_edu_share",        "male_70plus_edu"
)
name_map    <- setNames(as.character(group_spec$pretty), as.character(group_spec$key))
pretty_name <- function(gkey) {
  nm <- name_map[[as.character(gkey)]]
  if (is.null(nm)) as.character(gkey) else nm
}

# -------------------- turnout adjustment --------------------
aggregate_turnout_to_6 <- function(turnout10) {
  if (is.null(turnout10) || !length(turnout10) || is.null(names(turnout10)))
    return(rep(NA_real_, 6))
  map <- list(
    "18_24"  = c("18_21","21_25"),
    "25_34"  = c("25_30","30_35"),
    "35_44"  = c("35_40","40_45"),
    "45_59"  = c("45_50","50_60"),
    "60_69"  = c("60_70"),
    "70plus" = c("70plus")
  )
  out <- sapply(names(map), function(k) mean(turnout10[map[[k]]], na.rm = TRUE))
  names(out) <- names(map)
  out
}

apply_turnout_adjustment <- function(df, STATE, group_cols) {
  tr      <- get_age_gender_turnout(STATE)
  male6   <- aggregate_turnout_to_6(tr$male_turnout)
  female6 <- aggregate_turnout_to_6(tr$female_turnout)
  if (max(male6,   na.rm = TRUE) > 1.5) male6   <- male6/100
  if (max(female6, na.rm = TRUE) > 1.5) female6 <- female6/100
  w <- setNames(rep(1, length(group_cols)), group_cols)
  for (cc in group_cols) {
    age_key <- gsub("^female_|^male_", "", cc)
    age_key <- gsub("_(noedu|edu)_share$", "", age_key)
    if (grepl("^female_", cc)) w[cc] <- female6[age_key]
    if (grepl("^male_",   cc)) w[cc] <- male6[age_key]
  }
  w[!is.finite(w) | is.na(w) | w <= 0] <- 1
  df %>%
    mutate(across(all_of(group_cols), ~ as.numeric(.x) * w[cur_column()])) %>%
    mutate(.den = rowSums(across(all_of(group_cols)), na.rm = TRUE)) %>%
    mutate(across(all_of(group_cols), ~ ifelse(.den > 0, .x / .den, .x))) %>%
    select(-.den)
}

# -------------------- geometry helpers --------------------
as_sf <- function(g) {
  if (is.null(g)) return(NULL)
  if (inherits(g, "sf"))  return(g)
  if (inherits(g, "sfc")) return(st_sf(geometry = g))
  g
}
n_geoms <- function(g) {
  if (is.null(g)) return(0L)
  if (inherits(g, "sf"))  return(nrow(g))
  if (inherits(g, "sfc")) return(length(g))
  length(g)
}

drop_tiny_empty <- function(g, eps = 1e-12) {
  g <- as_sf(g)
  if (n_geoms(g) == 0) return(NULL)
  g <- suppressWarnings(st_make_valid(g))
  g_cast1 <- try(suppressWarnings(st_cast(g, "MULTIPOLYGON")), silent = TRUE)
  if (!inherits(g_cast1, "try-error")) g <- g_cast1
  g_cast2 <- try(suppressWarnings(st_cast(g, "POLYGON")), silent = TRUE)
  if (!inherits(g_cast2, "try-error")) g <- g_cast2
  g <- as_sf(g)
  if (n_geoms(g) == 0) return(NULL)
  a  <- suppressWarnings(as.numeric(st_area(g)))
  ok <- is.finite(a) & a > eps & !st_is_empty(g)
  if (!any(ok)) return(NULL)
  g[ok, , drop = FALSE]
}

merge_tiny_to_neighbor <- function(inter_sf2, top_n = NULL, frac_eps = 1e-8,
                                   allow_merge_into_outside = TRUE) {
  stopifnot(inherits(inter_sf2, "sf"))
  if (!("key" %in% names(inter_sf2))) stop("inter_sf2 must have a 'key' column.")
  message("[MERGE-TINY] Starting geometry repair (buffer(0) + make_valid)...")
  inter_sf2 <- tryCatch(
    suppressWarnings(sf::st_buffer(inter_sf2, 0)),
    error = function(e) {
      message("[MERGE-TINY] bulk buffer(0) failed, repairing per-geometry: ", conditionMessage(e))
      for (ii in seq_len(nrow(inter_sf2))) {
        inter_sf2$geometry[ii] <- tryCatch(
          suppressWarnings(sf::st_buffer(sf::st_make_valid(inter_sf2$geometry[ii]), 0)),
          error = function(e2) {
            tryCatch(
              suppressWarnings(sf::st_make_valid(inter_sf2$geometry[ii])),
              error = function(e3) inter_sf2$geometry[ii]
            )
          }
        )
      }
      inter_sf2
    }
  )
  inter_sf2 <- suppressWarnings(sf::st_make_valid(inter_sf2))
  a <- suppressWarnings(as.numeric(sf::st_area(inter_sf2)))

  if (!is.null(top_n)) {
    non_out_idx <- which(inter_sf2$key != "OUTSIDE")
    a_non_out   <- a[non_out_idx]
    degenerate  <- !is.finite(a_non_out) | a_non_out <= 0
    rnks        <- rank(-pmax(a_non_out, 0), ties.method = "first")
    tiny_local  <- which(rnks > top_n | degenerate)
    tiny        <- non_out_idx[tiny_local]
    message(sprintf("[MERGE-TINY] top_n=%d | non-outside cells=%d | to merge=%d",
                    top_n, length(non_out_idx), length(tiny)))
  } else {
    bbox      <- sf::st_bbox(inter_sf2)
    bbox_area <- as.numeric((bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"]))
    if (!is.finite(bbox_area) || bbox_area <= 0) return(inter_sf2)
    afrac <- a / bbox_area
    tiny  <- which(
      inter_sf2$key != "OUTSIDE" &
        (!is.finite(afrac) | afrac <= frac_eps | !is.finite(a) | a <= 0)
    )
  }

  if (!length(tiny)) return(inter_sf2)
  message(sprintf("[MERGE-TINY] tiny=%d | area_tiny=%.3e | share_nonoutside=%.3e",
                  length(tiny),
                  sum(a[tiny], na.rm=TRUE),
                  sum(a[tiny], na.rm=TRUE) / sum(a[inter_sf2$key!="OUTSIDE"], na.rm=TRUE)))
  keep  <- setdiff(seq_len(nrow(inter_sf2)), tiny)
  message("[MERGE-TINY] Computing st_touches...")
  touch <- tryCatch(
    suppressWarnings(sf::st_touches(inter_sf2, sparse = TRUE)),
    error = function(e) {
      message("[MERGE-TINY] st_touches failed: ", conditionMessage(e))
      lapply(seq_len(nrow(inter_sf2)), function(x) integer(0))
    }
  )
  cent <- NULL
  for (i in tiny) {
    nbrs <- intersect(touch[[i]], keep)
    if (!allow_merge_into_outside && length(nbrs))
      nbrs <- nbrs[inter_sf2$key[nbrs] != "OUTSIDE"]
    jstar <- NA_integer_
    if (length(nbrs)) {
      if (is.null(cent)) {
        message("[MERGE-TINY] Computing centroids (fallback triggered)...")
        cent <- tryCatch(
          suppressWarnings(sf::st_coordinates(sf::st_centroid(inter_sf2))),
          error = function(e) {
            message("[MERGE-TINY] st_centroid failed: ", conditionMessage(e))
            matrix(0, nrow(inter_sf2), 2)
          }
        )
      }
      a_nbrs <- a[nbrs]
      jstar  <- nbrs[which.max(a_nbrs)]
    }
    if (is.na(jstar)) {
      if (is.null(cent)) {
        cent <- tryCatch(
          suppressWarnings(sf::st_coordinates(sf::st_centroid(inter_sf2))),
          error = function(e) matrix(0, nrow(inter_sf2), 2)
        )
      }
      dx    <- cent[keep, 1] - cent[i, 1]
      dy    <- cent[keep, 2] - cent[i, 2]
      jstar <- keep[which.min(dx * dx + dy * dy)]
    }
    merged_geom <- tryCatch({
      gi <- suppressWarnings(sf::st_make_valid(inter_sf2$geometry[i]))
      gj <- suppressWarnings(sf::st_make_valid(inter_sf2$geometry[jstar]))
      suppressWarnings(sf::st_make_valid(sf::st_union(gj, gi)))
    }, error = function(e) {
      tryCatch({
        gi <- suppressWarnings(sf::st_buffer(sf::st_make_valid(inter_sf2$geometry[i]), 0))
        gj <- suppressWarnings(sf::st_buffer(sf::st_make_valid(inter_sf2$geometry[jstar]), 0))
        suppressWarnings(sf::st_make_valid(sf::st_union(gj, gi)))
      }, error = function(e2) inter_sf2$geometry[jstar])
    })
    inter_sf2$geometry[jstar] <- merged_geom
  }
  inter_sf2 <- inter_sf2[-tiny, , drop = FALSE]
  inter_sf2 <- suppressWarnings(sf::st_make_valid(inter_sf2))
  inter_sf2$cell_id <- seq_len(nrow(inter_sf2))
  inter_sf2
}

safe_intersection_sf <- function(a, b) {
  a <- drop_tiny_empty(a); b <- drop_tiny_empty(b)
  if (is.null(a) || is.null(b)) return(NULL)
  out <- try(suppressWarnings(st_intersection(a, b)), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) return(NULL)
  drop_tiny_empty(out)
}
safe_union_sf <- function(a, b) {
  a <- drop_tiny_empty(a); b <- drop_tiny_empty(b)
  if (is.null(a) || n_geoms(a) == 0) return(b)
  if (is.null(b) || n_geoms(b) == 0) return(a)
  out <- try(suppressWarnings(st_union(a, b)), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) return(a)
  drop_tiny_empty(out)
}
safe_difference_sf <- function(a, b) {
  a <- drop_tiny_empty(a); b <- drop_tiny_empty(b)
  if (is.null(a) || n_geoms(a) == 0) return(NULL)
  if (is.null(b) || n_geoms(b) == 0) return(a)
  out <- try(suppressWarnings(st_difference(a, b)), silent = TRUE)
  if (inherits(out, "try-error") || is.null(out)) return(a)
  drop_tiny_empty(out)
}

clip_rect_by_halfplane <- function(rect_xy, n, b, eps = 1e-12) {
  if (any(!is.finite(n)) || !is.finite(b)) return(NULL)
  if (sum(abs(n)) < eps) {
    out <- rect_xy
    if (!all(out[1, ] == out[nrow(out), ])) out <- rbind(out, out[1, , drop = FALSE])
    return(out)
  }
  gval <- function(p) sum(p * n) - b
  V <- rect_xy; out <- list()
  for (i in seq_len(nrow(V))) {
    S  <- V[i, ]
    E  <- V[ifelse(i == nrow(V), 1L, i + 1L), ]
    gS <- gval(S); if (!is.finite(gS)) gS <- Inf
    gE <- gval(E); if (!is.finite(gE)) gE <- Inf
    S_in <- (gS <= eps); E_in <- (gE <= eps)
    if (S_in && E_in) {
      out[[length(out) + 1L]] <- E
    } else if (xor(S_in, E_in)) {
      den <- gS - gE
      if (is.finite(den) && abs(den) > eps && is.finite(gS)) {
        t <- gS / den
        if (is.finite(t) && t >= -1e-9 && t <= 1 + 1e-9) {
          I <- S + t * (E - S)
          if (all(is.finite(I))) {
            if (S_in && !E_in)  out[[length(out) + 1L]] <- I
            if (!S_in && E_in) { out[[length(out) + 1L]] <- I; out[[length(out) + 1L]] <- E }
          }
        }
      }
    }
  }
  if (!length(out)) return(NULL)
  res <- do.call(rbind, out)
  res <- unique(res)
  if (!all(is.finite(res))) return(NULL)
  if (nrow(res) < 3L) return(NULL)
  if (!all(res[1, ] == res[nrow(res), ])) res <- rbind(res, res[1, , drop = FALSE])
  res
}

halfplane_poly_sf <- function(n, b, bounds) {
  if (any(!is.finite(n)) || !is.finite(b)) return(NULL)
  rect_xy <- matrix(
    c(bounds["xmin"], bounds["ymin"],
      bounds["xmax"], bounds["ymin"],
      bounds["xmax"], bounds["ymax"],
      bounds["xmin"], bounds["ymax"],
      bounds["xmin"], bounds["ymin"]),
    ncol = 2, byrow = TRUE
  )
  clipped <- clip_rect_by_halfplane(rect_xy, n, b)
  if (is.null(clipped) || !is.matrix(clipped) || nrow(clipped) < 4L) return(NULL)
  if (any(clipped[1, ] != clipped[nrow(clipped), ]))
    clipped <- rbind(clipped, clipped[1, , drop = FALSE])
  distinct <- unique(clipped[-nrow(clipped), , drop = FALSE])
  if (nrow(distinct) < 3L) return(NULL)
  poly <- try(st_sfc(st_polygon(list(clipped)), crs = sf::NA_crs_), silent = TRUE)
  if (inherits(poly, "try-error")) return(NULL)
  as_sf(poly)
}

hp_j_beats_k <- function(Cj, Ck, bounds, w2 = 1) {
  w2 <- as.numeric(w2)[1]
  if (!is.finite(w2) || w2 <= 0) w2 <- 1
  n <- c(Ck[1] - Cj[1], w2 * (Ck[2] - Cj[2]))
  b <- ((Ck[1]^2 + w2 * Ck[2]^2) - (Cj[1]^2 + w2 * Cj[2]^2)) / 2
  halfplane_poly_sf(n, b, bounds)
}

winner_region_sf <- function(C, j, bounds, w2 = 1) {
  K <- nrow(C)
  P <- as_sf(st_sfc(st_polygon(list(matrix(
    c(bounds["xmin"], bounds["ymin"],
      bounds["xmax"], bounds["ymin"],
      bounds["xmax"], bounds["ymax"],
      bounds["xmin"], bounds["ymax"],
      bounds["xmin"], bounds["ymin"]),
    ncol = 2, byrow = TRUE)))))
  for (k in setdiff(1:K, j)) {
    HP <- hp_j_beats_k(C[j, ], C[k, ], bounds, w2 = w2)
    P  <- safe_intersection_sf(P, HP)
    if (is.null(P) || n_geoms(P) == 0) return(NULL)
  }
  drop_tiny_empty(as_sf(st_union(P)))
}

# All-parties partition: winner regions for all 6 parties
build_partition_one_election <- function(C, bounds, w2 = 1) {
  K      <- nrow(C)
  W_list <- vector("list", K)
  for (j in 1:K) W_list[[j]] <- winner_region_sf(C, j, bounds, w2 = w2)
  parts  <- list()
  for (j in 1:K) {
    Wj <- W_list[[j]]
    if (!is.null(Wj) && n_geoms(Wj) > 0)
      parts[[length(parts) + 1L]] <- Wj %>% mutate(part = as.integer(j))
  }
  if (!length(parts)) return(NULL)
  v_e <- do.call(rbind, lapply(parts, as_sf)) %>%
    group_by(part) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    as_sf()
  drop_tiny_empty(v_e)
}

# ============================================================
# Build pooled mesh inputs for a region (independent of w2)
# ============================================================
build_region_mesh_inputs_edu <- function(region_label, STATES, YEAR) {

  pos_pool      <- tibble()
  eids_by_state <- list()

  for (S in STATES) {
    f <- sprintf("../data/data_state%02d_year%d_edu.RData", S, YEAR)
    if (!file.exists(f)) {
      message(sprintf("[MESH-INPUT] No edu file for state %02d, year %d — skipping.", S, YEAR))
      next
    }
    load(f)

    if (!exists("results_std") || !nrow(results_std) ||
        !exists("pos_std")     || !nrow(pos_std)) {
      message(sprintf("[MESH-INPUT] Empty data for state %02d — skipping.", S, YEAR))
      next
    }

    results_std <- results_std %>%
      filter(eligible_voters < SMALL_MUNI_MAX_ELIGIBLE) %>%
      mutate(election_id = safe_int(election_id))
    pos_std     <- pos_std %>%
      mutate(election_id = safe_int(election_id), cand = safe_int(cand))

    keep_wk <- results_std %>%
      group_by(election_id) %>%
      filter(n() > MIN_ROWS_PER_ELECTION) %>%
      ungroup() %>% pull(election_id) %>% unique()
    results_std <- results_std %>% filter(election_id %in% keep_wk)
    pos_std     <- pos_std     %>% filter(election_id %in% keep_wk)

    eids <- sort(intersect(unique(results_std$election_id), unique(pos_std$election_id)))
    if (!length(eids)) next

    eids_by_state[[as.character(S)]] <- eids
    pos_pool <- bind_rows(
      pos_pool,
      pos_std %>%
        filter(election_id %in% eids) %>%
        transmute(state_id = S, election_id, cand,
                  x = as.numeric(x), y = as.numeric(y))
    )
  }

  if (!nrow(pos_pool)) {
    message(sprintf("[SKIP] No positions found for year %d, region %s", YEAR, region_label))
    return(NULL)
  }

  selected_by_state <- setNames(vector("list", length(eids_by_state)), names(eids_by_state))
  for (s in names(eids_by_state)) {
    eids_s <- eids_by_state[[s]]
    if (length(eids_s) > MAX_ELECTIONS_PER_STATE)
      eids_s <- sort(sample(eids_s, MAX_ELECTIONS_PER_STATE))
    selected_by_state[[s]] <- eids_s
    message(sprintf("[MESH-INPUT] State %s: %d elections selected (cap=%d)",
                    s, length(eids_s), MAX_ELECTIONS_PER_STATE))
  }
  elections_use <- sort(unique(unlist(selected_by_state, use.names = FALSE)))

  id_map <- tibble(election_id = elections_use, e_idx = seq_along(elections_use))
  E_data <- nrow(id_map)

  pos_rep <- pos_pool %>%
    filter(election_id %in% elections_use) %>%
    left_join(id_map, by = "election_id") %>%
    group_by(election_id, e_idx, cand) %>%
    summarise(x = median(x, na.rm = TRUE),
              y = median(y, na.rm = TRUE), .groups = "drop")

  cand_meds <- pos_rep %>%
    group_by(cand) %>%
    summarise(mx = median(x, na.rm = TRUE), my = median(y, na.rm = TRUE), .groups = "drop")

  fallback_x <- median(pos_pool$x, na.rm = TRUE); if (!is.finite(fallback_x)) fallback_x <- 0
  fallback_y <- median(pos_pool$y, na.rm = TRUE); if (!is.finite(fallback_y)) fallback_y <- 0

  pos_rep_full <- pos_rep %>%
    group_by(election_id, e_idx) %>%
    tidyr::complete(cand = 1:6) %>%
    ungroup() %>%
    left_join(cand_meds, by = "cand") %>%
    mutate(
      x = ifelse(is.na(x), mx, x),
      y = ifelse(is.na(y), my, y),
      x = ifelse(!is.finite(x) | is.na(x), fallback_x, x),
      y = ifelse(!is.finite(y) | is.na(y), fallback_y, y)
    ) %>%
    select(election_id, e_idx, cand, x, y)

  ysplit <- split(pos_rep_full, pos_rep_full$e_idx)
  if (!all(vapply(ysplit, nrow, 1L) == 6L))
    stop("Some elections missing candidates after position completion.")

  C_list <- lapply(seq_along(ysplit), function(e) {
    df <- ysplit[[e]][order(ysplit[[e]]$cand), ]
    as.matrix(df[, c("x", "y")])
  })
  xy_obs <- do.call(rbind, C_list)

  list(
    region_label      = region_label,
    STATES            = STATES,
    YEAR              = YEAR,
    elections_use     = elections_use,
    selected_by_state = selected_by_state,
    id_map            = id_map,
    E_data            = E_data,
    C_list            = C_list,
    xy_obs            = xy_obs
  )
}

# ============================================================
# Collect pooled municipality data — returns all 6 party shares
# ============================================================
collect_region_muni_edu <- function(STATES, YEAR, id_map, elections_by_state) {

  cand_cols  <- paste0("cand", 1:6, "_share")
  group_cols <- group_spec$col
  out        <- list()

  for (S in STATES) {
    f <- sprintf("../data/data_state%02d_year%d_edu.RData", S, YEAR)
    if (!file.exists(f)) next
    load(f)

    if (!exists("results_std") || !nrow(results_std)) next

    if (!all(cand_cols %in% names(results_std))) {
      src_cols <- c("cdu_norm","csu_norm","spd_norm","gruene_norm",
                    "fdp_norm","linke_pds_norm","afd_norm")
      if (!all(src_cols %in% names(results_std))) next
      results_std <- results_std %>%
        mutate(
          cand1_share = pmax(0, coalesce(cdu_norm,0) + coalesce(csu_norm,0)),
          cand2_share = pmax(0, coalesce(spd_norm,0)),
          cand3_share = pmax(0, coalesce(gruene_norm,0)),
          cand4_share = pmax(0, coalesce(fdp_norm,0)),
          cand5_share = pmax(0, coalesce(linke_pds_norm,0)),
          cand6_share = pmax(0, coalesce(afd_norm,0))
        )
    }

    results_std <- results_std %>%
      filter(eligible_voters < SMALL_MUNI_MAX_ELIGIBLE) %>%
      filter(if_all(all_of(cand_cols), ~ is.finite(.x) & .x >= RESULTS_MIN))
    if (!nrow(results_std)) next

    rs <- rowSums(dplyr::select(results_std, all_of(cand_cols)), na.rm = TRUE)
    results_std <- results_std %>%
      mutate(across(all_of(cand_cols), ~ .x / ifelse(rs > 0, rs, NA_real_)))

    state_eids <- elections_by_state[[as.character(S)]]
    if (!is.null(state_eids)) {
      state_id_map <- id_map %>% filter(election_id %in% state_eids)
    } else {
      state_id_map <- id_map
    }

    results_std <- results_std %>%
      mutate(election_id = safe_int(election_id)) %>%
      inner_join(state_id_map, by = "election_id") %>%
      mutate(election = e_idx) %>%
      select(-e_idx)
    if (!nrow(results_std)) next

    set.seed(10200 + S * 101 + YEAR)
    results_std <- results_std %>%
      group_by(election) %>%
      group_modify(~ {
        n <- nrow(.x)
        if (n <= SUBSAMPLE_N) .x else .x[sample.int(n, SUBSAMPLE_N), , drop = FALSE]
      }) %>%
      ungroup()

    miss <- setdiff(group_cols, names(results_std))
    if (length(miss)) results_std[miss] <- 0

    results_std <- apply_turnout_adjustment(results_std, S, group_cols)

    df <- results_std %>%
      transmute(
        state_id        = as.integer(S),
        election,
        eligible_voters = coalesce(eligible_voters, 1),
        across(all_of(group_cols)),
        across(all_of(cand_cols))   # all 6 party shares
      )

    out[[length(out) + 1L]] <- df
  }

  if (!length(out)) return(tibble())
  bind_rows(out)
}

# ============================================================
# Build capacity matrices for all 6 parties (24 edu groups)
#
# B_list[[p]][[gk]]: R×nc matrix
#   B_list[[p]][[gk]][r, ] = group_share_gk[r] * A_list[[e_r]][p, ]
#
# share_all: R×6 matrix of observed party vote shares
# ============================================================
build_region_capacity_edu <- function(inter_sf, muni_all, group_spec, E_data) {

  stopifnot(nrow(muni_all) > 0)
  K     <- N_PARTIES
  nc    <- nrow(inter_sf)
  R     <- nrow(muni_all)
  gkeys <- group_spec$key
  gcols <- group_spec$col

  A_list <- vector("list", E_data)
  for (e in seq_len(E_data)) {
    p_e   <- safe_int(inter_sf[[paste0("p_e", e)]])
    A     <- matrix(0L, K, nc)
    valid <- !is.na(p_e) & p_e >= 1L & p_e <= K
    if (any(valid)) A[cbind(p_e[valid], which(valid))] <- 1L
    A_list[[e]] <- A
  }

  # B_list[[p]][[gk]]: capacity for party p, group gk
  B_list <- setNames(vector("list", K), as.character(1:K))
  for (p in seq_len(K)) {
    B_list[[p]] <- setNames(vector("list", length(gkeys)), gkeys)
    for (gi in seq_along(gkeys)) {
      gcol <- gcols[gi]; gkey <- gkeys[gi]
      B    <- matrix(0, R, nc)
      for (r in seq_len(R)) {
        e       <- muni_all$election[r]
        B[r, ] <- as.numeric(muni_all[[gcol]][r]) * A_list[[e]][p, ]
      }
      B_list[[p]][[gkey]] <- B
    }
  }

  # share_all: R×6 matrix
  cand_cols <- paste0("cand", 1:K, "_share")
  share_all <- as.matrix(muni_all[, cand_cols])
  storage.mode(share_all) <- "double"

  states_present <- sort(unique(muni_all$state_id))
  states_chr     <- as.character(states_present)

  state_row_idx <- setNames(
    lapply(states_chr, function(sid) which(as.character(muni_all$state_id) == sid)),
    states_chr
  )

  list(
    B_list         = B_list,
    share_all      = share_all,
    weights_pop    = as.numeric(muni_all$eligible_voters),
    states_present = states_present,
    state_row_idx  = state_row_idx
  )
}

# ============================================================
# Region-level solver: 24 edu groups, STATE-SPECIFIC Pi
# ALL-PARTIES version.
#
# ONE shared Pi[sid][gk] (nc-vector) per state per group.
# For party p, row r from state sid:
#   pred_{p,r} = sum_gk B_list[[p]][[gk]][r,] %*% Pi[[sid]][[gk]]
#
# Objective: min sum_{p=1..6} sum_r |pred_{p,r} - share_{p,r}|
# Constraints: Pi >= 0, sum(Pi) = 1, edu/non-edu similarity.
# ============================================================
solve_allparties_region_state_pi <- function(cap,
                                             EDU_DIFF_TOL,
                                             prev_Pi_vals = NULL,
                                             nc_prev      = NULL) {

  B_list         <- cap$B_list
  share_all      <- cap$share_all      # R × 6
  weights_pop    <- cap$weights_pop
  gkeys          <- names(B_list[[1]])
  states_present <- cap$states_present
  states_chr     <- as.character(states_present)
  state_row_idx  <- cap$state_row_idx

  R  <- nrow(share_all)
  nc <- ncol(B_list[[1]][[1]])

  base_keys <- unique(gsub("_edu$", "", gkeys))

  # ----- CVXR variables: one Pi[sid][gk] shared across all parties -----
  Pi <- setNames(lapply(states_chr, function(sid) {
    setNames(lapply(gkeys, function(gk) CVXR::Variable(nc)), gkeys)
  }), states_chr)

  # Warm start
  if (!is.null(prev_Pi_vals)) {
    nc_match <- !is.null(nc_prev) && (nc_prev == nc)
    for (sid in states_chr) {
      pv_s <- prev_Pi_vals[[sid]]
      if (is.null(pv_s)) next
      for (gk in gkeys) {
        pv_gk <- pv_s[[gk]]
        if (is.null(pv_gk) || !is.numeric(pv_gk)) next
        if (nc_match && length(pv_gk) == nc) {
          init_val <- pmax(pv_gk, 0)
          s        <- sum(init_val)
          if (is.finite(s) && s > 0) init_val <- init_val / s
          CVXR::value(Pi[[sid]][[gk]]) <- init_val
        } else {
          CVXR::value(Pi[[sid]][[gk]]) <- rep(1/nc, nc)
        }
      }
    }
  }

  # Row reorder: stack by state
  row_reorder     <- unlist(state_row_idx[states_chr], use.names = FALSE)
  share_all_ord   <- share_all[row_reorder, , drop = FALSE]   # R × 6
  weights_pop_ord <- weights_pop[row_reorder]

  # ----- Build prediction for each party -----
  # pred_p: R×1 CVXR expression for party p, stacked across states
  pred_list <- vector("list", N_PARTIES)
  for (p in seq_len(N_PARTIES)) {
    pred_parts_p <- vector("list", length(states_chr))
    names(pred_parts_p) <- states_chr
    for (sid in states_chr) {
      idx_s <- state_row_idx[[sid]]
      if (!length(idx_s)) next
      pred_s <- Reduce(`+`, lapply(gkeys, function(gk) {
        B_s <- B_list[[p]][[gk]][idx_s, , drop = FALSE]
        B_s %*% Pi[[sid]][[gk]]
      }))
      pred_parts_p[[sid]] <- pred_s
    }
    pred_parts_p <- pred_parts_p[!sapply(pred_parts_p, is.null)]
    if (!length(pred_parts_p)) return(list(status = "no_states"))
    pred_list[[p]] <- Reduce(CVXR::vstack, pred_parts_p)   # R × 1
  }

  # ----- Objective: sum over all parties of sum |pred_p - share_p| -----
  u_list <- lapply(seq_len(N_PARTIES), function(p) CVXR::Variable(R))

  cons <- list()
  for (p in seq_len(N_PARTIES)) {
    share_p <- share_all_ord[, p]
    cons <- c(cons, list(
      u_list[[p]] >= 0,
      pred_list[[p]] - share_p <= u_list[[p]],
      -(pred_list[[p]] - share_p) <= u_list[[p]]
    ))
  }

  # Positivity + normalisation per state per group
  for (sid in states_chr) {
    for (gk in gkeys) {
      cons <- c(cons, list(
        Pi[[sid]][[gk]] >= 0,
        CVXR::sum_entries(Pi[[sid]][[gk]]) == 1
      ))
    }
  }

  # Edu/non-edu similarity per state
  for (sid in states_chr) {
    for (bg in base_keys) {
      edu_key <- paste0(bg, "_edu")
      if (!(edu_key %in% gkeys)) next
      cons <- c(cons, list(
        Pi[[sid]][[bg]]      <= (1 + EDU_DIFF_TOL) * Pi[[sid]][[edu_key]],
        Pi[[sid]][[edu_key]] <= (1 + EDU_DIFF_TOL) * Pi[[sid]][[bg]]
      ))
    }
  }

  obj  <- Reduce(`+`, lapply(u_list, CVXR::sum_entries))
  prob <- CVXR::Problem(CVXR::Minimize(obj), cons)

  use_warm <- !is.null(prev_Pi_vals)

  res <- try(CVXR::solve(prob, solver = "ECOS", verbose = FALSE,
                         warm_start = use_warm,
                         num_iter   = 500L,
                         feastol    = 1e-7,
                         reltol     = 1e-7,
                         abstol     = 1e-7),
             silent = TRUE)

  if (inherits(res, "try-error") || is.null(res$status) || res$status == "solver_error") {
    message("[SOLVE] ECOS solver_error — retrying with SCS ...")
    res <- try(CVXR::solve(prob, solver = "SCS", verbose = FALSE,
                           warm_start = use_warm,
                           max_iters  = 10000L,
                           eps        = 1e-5),
               silent = TRUE)
  }

  if (inherits(res, "try-error") || is.null(res$status))
    return(list(status = "solver_error"))
  if (!(res$status %in% c("optimal","optimal_inaccurate")))
    return(list(status = as.character(res$status)[1]))

  Pi_vals <- setNames(lapply(states_chr, function(sid) {
    setNames(lapply(gkeys, function(gk) as.numeric(res$getValue(Pi[[sid]][[gk]]))), gkeys)
  }), states_chr)

  # Residuals: R × N_PARTIES matrix
  resid_mat <- do.call(cbind, lapply(seq_len(N_PARTIES), function(p) {
    as.numeric(res$getValue(pred_list[[p]] - share_all_ord[, p]))
  }))
  colnames(resid_mat) <- paste0("party", 1:N_PARTIES)

  total_u <- as.numeric(res$getValue(obj))
  mae     <- total_u / (R * N_PARTIES)

  list(
    status         = as.character(res$status)[1],
    Pi_vals        = Pi_vals,
    mae            = mae,
    residuals      = resid_mat,
    weights_pop    = weights_pop_ord,
    row_reorder    = row_reorder
  )
}

# -------------------- fit metrics --------------------
compute_fit_metrics <- function(fit, n_cells, lambda_cells = 0.0) {
  # residuals is R × N_PARTIES; flatten for aggregate metrics
  resid <- as.vector(fit$residuals)
  w     <- rep(as.numeric(fit$weights_pop), times = N_PARTIES)
  w[!is.finite(w) | is.na(w) | w <= 0] <- 0

  mae  <- mean(abs(resid), na.rm = TRUE)
  wmae <- NA_real_
  if (length(w) == length(resid) && sum(w) > 0)
    wmae <- sum(w * abs(resid), na.rm = TRUE) / sum(w, na.rm = TRUE)

  mae_per_cell  <- if (is.finite(mae)  && n_cells > 0) mae  / n_cells else NA_real_
  wmae_per_cell <- if (is.finite(wmae) && n_cells > 0) wmae / n_cells else NA_real_
  mae_plus_lambda_cells  <- if (is.finite(mae)  && n_cells > 0)
    mae  + lambda_cells * log(n_cells) else NA_real_
  wmae_plus_lambda_cells <- if (is.finite(wmae) && n_cells > 0)
    wmae + lambda_cells * log(n_cells) else NA_real_

  # Per-party MAE
  mae_by_party <- apply(abs(fit$residuals), 2, mean, na.rm = TRUE)

  list(mae = mae, wmae = wmae,
       mae_per_cell = mae_per_cell, wmae_per_cell = wmae_per_cell,
       mae_plus_lambda_cells = mae_plus_lambda_cells,
       wmae_plus_lambda_cells = wmae_plus_lambda_cells,
       mae_by_party = mae_by_party)
}

# ============================================================
# Main per-region function: w2 grid search with warm start
# ============================================================
run_one_region_w2 <- function(region_label, STATES, seed_base = 0, seed_idx = 1) {

  message(sprintf(
    "\n=======================\nRegion %s | Year %d | w2 grid search (all-parties, state-Pi, 24-group edu)\n=======================\n",
    toupper(region_label), YEAR
  ))

  set.seed(120 + YEAR + seed_base)
  mesh_in <- tryCatch(
    build_region_mesh_inputs_edu(region_label, STATES, YEAR),
    error = function(e) {
      message(sprintf("[ERROR] build_region_mesh_inputs_edu: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(mesh_in)) {
    message(sprintf("[SKIP] Region %s, year %d: could not build mesh inputs.", region_label, YEAR))
    return(FALSE)
  }

  E_data            <- mesh_in$E_data
  C_list            <- mesh_in$C_list
  xy_obs            <- mesh_in$xy_obs
  id_map            <- mesh_in$id_map
  selected_by_state <- mesh_in$selected_by_state

  muni_all <- collect_region_muni_edu(STATES, YEAR, id_map, selected_by_state)
  if (!nrow(muni_all)) {
    message(sprintf("[SKIP] Region %s, year %d: no municipality rows.", region_label, YEAR))
    return(FALSE)
  }

  R          <- nrow(muni_all)
  share_all  <- as.matrix(muni_all[, paste0("cand", 1:N_PARTIES, "_share")])
  w_muni     <- as.numeric(muni_all$eligible_voters)

  message(sprintf(
    "\n[W2 GRID SEARCH] YEAR %d | %s | grid=%s | n_states=%d | max_elections/state=%d | subsample_n=%d\n",
    YEAR, toupper(region_label),
    paste(W2_GRID, collapse=","),
    length(STATES), MAX_ELECTIONS_PER_STATE, SUBSAMPLE_N
  ))

  grid_rows    <- list()
  best         <- list(w2 = NA_real_, score = Inf, status = NA_character_,
                       row = NULL, fit = NULL, inter_sf = NULL)
  prev_Pi_vals <- NULL
  prev_nc      <- NULL

  for (w2 in W2_GRID) {
    message(sprintf("[W2 TRY] YEAR %d | %s | w2=%.3f", YEAR, toupper(region_label), w2))

    out <- tryCatch({
      bounds <- c(xmin = min(xy_obs[, 1]) - VOR_PAD,
                  ymin = min(xy_obs[, 2]) - VOR_PAD,
                  xmax = max(xy_obs[, 1]) + VOR_PAD,
                  ymax = max(xy_obs[, 2]) + VOR_PAD)

      partitions <- vector("list", E_data)
      for (e in seq_len(E_data)) {
        part_e <- build_partition_one_election(C_list[[e]], bounds, w2 = w2)
        if (is.null(part_e) || n_geoms(part_e) == 0)
          stop(sprintf("Empty partition for election %d (w2=%.3f)", e, w2))
        partitions[[e]] <- part_e
      }

      inter_sf <- NULL
      for (e in seq_len(E_data)) {
        part_e <- partitions[[e]] %>%
          mutate(!!sym(paste0("p_e", e)) := as.integer(part)) %>%
          dplyr::select(paste0("p_e", e), geometry)
        if (is.null(inter_sf)) {
          inter_sf <- part_e
        } else {
          inter_sf <- safe_intersection_sf(inter_sf, part_e)
          if (is.null(inter_sf) || n_geoms(inter_sf) == 0)
            stop(sprintf("Voronoi intersection collapsed at e=%d (w2=%.3f)", e, w2))
        }
      }

      p_cols   <- paste0("p_e", 1:E_data)
      geom_col <- attr(inter_sf, "sf_column")

      inter_sf <- inter_sf %>%
        mutate(across(all_of(p_cols), safe_int))

      # Consolidate cells with identical winner pattern across elections
      inter_sf2 <- inter_sf %>%
        mutate(key = do.call(paste, c(dplyr::pick(all_of(p_cols)), sep = "-"))) %>%
        group_by(key, across(all_of(p_cols))) %>%
        summarise(geometry = st_union(!!sym(geom_col)), .groups = "drop") %>%
        mutate(cell_id = dplyr::row_number()) %>%
        as_sf()

      inter_sf2 <- inter_sf2 %>%
        mutate(key = ifelse(
          if_any(all_of(p_cols), ~ !is.na(.x)),
          key, "OUTSIDE"))

      inter_sf2 <- drop_tiny_empty(inter_sf2)
      if (is.null(inter_sf2) || n_geoms(inter_sf2) == 0)
        stop("All cells dropped after consolidation.")

      inter_sf2 <- merge_tiny_to_neighbor(inter_sf2, top_n = KEEP_TOP_N_CELLS)
      if (is.null(inter_sf2) || n_geoms(inter_sf2) == 0)
        stop("All cells dropped after top-N merge.")

      nc <- nrow(inter_sf2)
      message(sprintf("[MESH] Region %s, year %d, w2=%.3f: %d cells, %d elections",
                      toupper(region_label), YEAR, w2, nc, E_data))

      cap <- build_region_capacity_edu(inter_sf2, muni_all, group_spec, E_data)

      fit <- solve_allparties_region_state_pi(
        cap          = cap,
        EDU_DIFF_TOL = EDU_DIFF_TOL,
        prev_Pi_vals = prev_Pi_vals,
        nc_prev      = prev_nc
      )

      list(inter_sf = inter_sf2, cap = cap, fit = fit, nc = nc)
    }, error = function(e) list(error = conditionMessage(e)))

    if (!is.null(out$error)) {
      grid_rows[[length(grid_rows) + 1L]] <- data.frame(
        year = YEAR, geo_region = region_label, w1 = W1_FIXED, w2 = w2,
        status = "error",
        n_cells = NA_integer_, E_data = E_data, n_obs = R,
        mae = NA_real_, wmae = NA_real_,
        mae_per_cell = NA_real_, wmae_per_cell = NA_real_,
        mae_plus_lambda_cells = NA_real_, wmae_plus_lambda_cells = NA_real_,
        error = out$error, stringsAsFactors = FALSE
      )
      message("  -> ERROR: ", out$error)
      next
    }

    status <- out$fit$status
    if (is.null(status) || length(status) == 0) status <- "NA"
    status  <- as.character(status)[1]
    n_cells <- out$nc

    if (!(status %in% c("optimal","optimal_inaccurate"))) {
      grid_rows[[length(grid_rows) + 1L]] <- data.frame(
        year = YEAR, geo_region = region_label, w1 = W1_FIXED, w2 = w2,
        status = status,
        n_cells = as.integer(n_cells), E_data = E_data, n_obs = R,
        mae = NA_real_, wmae = NA_real_,
        mae_per_cell = NA_real_, wmae_per_cell = NA_real_,
        mae_plus_lambda_cells = NA_real_, wmae_plus_lambda_cells = NA_real_,
        error = NA_character_, stringsAsFactors = FALSE
      )
      message("  -> status=", status, " (skip metrics)")
      next
    }

    mets <- compute_fit_metrics(out$fit, n_cells, lambda_cells = LAMBDA_CELLS)

    mae_party_str <- paste(
      sprintf("p%d=%.4f", 1:N_PARTIES, mets$mae_by_party),
      collapse = " | "
    )

    row <- data.frame(
      year = YEAR, geo_region = region_label, w1 = W1_FIXED, w2 = w2,
      status = status,
      n_cells = as.integer(n_cells), E_data = E_data, n_obs = R,
      mae  = mets$mae,
      wmae = mets$wmae,
      mae_per_cell  = mets$mae_per_cell,
      wmae_per_cell = mets$wmae_per_cell,
      mae_plus_lambda_cells  = mets$mae_plus_lambda_cells,
      wmae_plus_lambda_cells = mets$wmae_plus_lambda_cells,
      error = NA_character_, stringsAsFactors = FALSE
    )
    grid_rows[[length(grid_rows) + 1L]] <- row

    message(sprintf("  -> status=%s | MAE=%.6f | WMAE=%s | cells=%d",
                    status,
                    mets$mae,
                    if (is.finite(mets$wmae)) sprintf("%.6f", mets$wmae) else "NA",
                    n_cells))
    message(sprintf("     per-party MAE: %s", mae_party_str))

    prev_Pi_vals <- out$fit$Pi_vals
    prev_nc      <- n_cells

    score <- if (is.finite(mets$wmae)) mets$wmae else mets$mae
    if (is.finite(score) && score < best$score) {
      best <- list(w2 = w2, score = score, status = status,
                   row = row, fit = out$fit, inter_sf = out$inter_sf,
                   cap = out$cap)
    }
  }  # end w2 loop

  grid_df <- bind_rows(grid_rows)

  message("\n[W2 GRID SUMMARY] (sorted by WMAE then MAE)")
  if (nrow(grid_df) > 0) {
    print(grid_df %>% arrange(ifelse(is.finite(wmae), wmae, Inf),
                              ifelse(is.finite(mae),  mae,  Inf)))
  }

  if (!is.finite(best$score) || is.na(best$w2)) {
    message(sprintf("[FAIL] No feasible w2 found for region %s, year %d",
                    region_label, YEAR))
    return(list(
      geo_region = region_label,
      grid_df    = grid_df,
      feasible   = FALSE
    ))
  }

  message(sprintf("\n[W2 BEST] YEAR %d | %s | best w2=%.3f | score=%s\n",
                  YEAR, toupper(region_label), best$w2,
                  if (is.finite(best$score)) sprintf("%.6f", best$score) else "NA"))

  list(
    geo_region   = region_label,
    feasible     = TRUE,
    grid_df      = grid_df,
    w2_best      = best$w2,
    best_score   = best$score,
    best_row     = best$row,
    E_data       = E_data,
    n_cells      = nrow(best$inter_sf),
    Pi_vals      = best$fit$Pi_vals,
    residuals    = best$fit$residuals,
    mae          = best$fit$mae,
    inter_sf     = best$inter_sf,
    muni_all     = muni_all
  )
}

# ============================================================
# Outer loop: single region + year (from command-line args)
# ============================================================
REGIONS <- list(
  east = c(12, 14, 15, 16),
  west = 1:11
)

source(sprintf("../targets/RW_%d_6groups_turnout.R", YEAR))

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
      year_csv <- sprintf("diagnostics/w2_grid_region_state_pi_allparties_year%d_%s_seed%02d.csv",
                          YEAR, region_label, seed_idx)
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

  out_rds <- sprintf("fits_allparties/allparties_region_state_pi_year%d_%s_seed%02d.rds",
                     YEAR, region_label, seed_idx)
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
  message(sprintf("[SAVED] Year %d | %s | seed_idx %d → %s",
                  YEAR, toupper(region_label), seed_idx, out_rds))
}
