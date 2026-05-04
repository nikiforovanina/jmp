# ===============================================================
# 0_2013_weights_by_region_state_pi.R
# PER-REGION (EAST / WEST) w2 GRID SEARCH, 24-GROUP EDU MODEL
# WITH STATE-SPECIFIC Pi DISTRIBUTIONS
#
# Key differences from 0_2013_weights_by_region.R:
#   - Pi^d (preference distribution) is state-specific, not
#     shared across the whole region. One Pi[state][group]
#     per federal state and demographic group.
#   - Voronoi geometry (candidate positions) remains pooled
#     at the region level (median across states per election).
#   - MAX_ELECTIONS_PER_STATE = 10 (electoral districts sampled per state).
#   - W2 grid: c(0.5, 0.75, 1, 1.25, 1.5, 2).
#   - Warm start: each w2 trial initialises from the previous
#     w2's optimal Pi values (exact if nc matches, uniform otherwise).
#   - AFD_GROUP_TOL = 0.2, KEEP_TOP_N_CELLS = 500.
#   - Accepts --region east|west command-line argument; runs
#     exactly one region per invocation.
#
# Usage:
#   Rscript 0_2013_weights_by_region_state_pi.R --region east
#   Rscript 0_2013_weights_by_region_state_pi.R --region west
# ===============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(tibble); library(purrr)
  library(CVXR); library(rlang); library(readr)
})
suppressMessages(sf::sf_use_s2(FALSE))
set.seed(1509)

# -------------------- command-line region argument --------------------
args <- commandArgs(trailingOnly = TRUE)
region_arg_pos <- which(args == "--region")
if (!length(region_arg_pos) || length(args) < region_arg_pos + 1) {
  stop("Usage: Rscript 0_2013_weights_by_region_state_pi.R --region east|west")
}
REGION_RUN <- tolower(trimws(args[region_arg_pos + 1]))
if (!REGION_RUN %in% c("east", "west")) {
  stop("--region must be 'east' or 'west'")
}

# -------------------- knobs --------------------
SUBSAMPLE_N               <- 50
MAX_ELECTIONS_PER_STATE   <- 10     # electoral districts sampled per state
RESULTS_MIN             <- 1e-5

# Tolerances
AFD_GROUP_TOL           <- 0.4    # relaxed (was 0.005)
SC_TOL                  <- 0.05
EDU_DIFF_TOL            <- 0.20
ABS_EPS                 <- 1e-5
REL_EPS                 <- 1e-5

SMALL_MUNI_MAX_ELIGIBLE <- 10000
MIN_ROWS_PER_ELECTION   <- 5

SEED_LIST               <- c(1509)

# Voronoi padding
VOR_PAD                 <- 5

# Keep this many largest AfD cells; merge all smaller ones into their neighbours
KEEP_TOP_N_CELLS        <- 500

# AfD second-choice: track LINKE (5) only
SC_PARTIES              <- c(5L)

# w2 grid  (w1 fixed at 1)
W1_FIXED <- 1
#W2_GRID  <- c(0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 2, 2.25, 2.5, 4)
W2_GRID <- c(0.2, 5)
#W2_GRID <- c(0.4, 10)
#W2_GRID  <- c(0.0001, 0.001, 0.4, 0.6, 0.9, 1.1, 2.5, 4)

# Optional cell-count complexity penalty (0 = off)
LAMBDA_CELLS <- 0.0

# Output dirs
dir.create("fits_afd_only", showWarnings = FALSE, recursive = TRUE)
dir.create("diagnostics",   showWarnings = FALSE, recursive = TRUE)

# Global appended CSV
DIAG_CSV <- "../../weights_output/w2_grid_diagnostics_by_region_state_pi_SC.csv"

# YEAR set in outer loop
YEAR <- NA_integer_

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

PARTY_INDEX <- c(
  "CDU"=1,"CSU"=1,"SPD"=2,"GRUENE"=3,"GRÜNE"=3,"GRUEN"=3,"GREEN"=3,
  "FDP"=4,"LINKE"=5,"PDS"=5,"DIE LINKE"=5,"LEFT"=5,
  "AFD"=6,"AfD"=6
)

normalize_target_vec <- function(v) {
  v <- as.numeric(v); if (length(v) != 6) stop("Each target vector must have length 6.")
  nm <- names(v); if (is.null(nm)) nm <- as.character(seq_along(v))
  if (all(!is.na(suppressWarnings(as.integer(nm))))) {
    ord <- match(as.character(1:6), nm)
    if (any(is.na(ord))) stop("Target names must include '1'..'6'.")
    v <- v[ord]; names(v) <- as.character(1:6)
  } else {
    key <- toupper(trimws(nm)); idx <- PARTY_INDEX[key]
    vv  <- numeric(6); names(vv) <- as.character(1:6)
    for (i in seq_along(v)) if (!is.na(idx[i])) vv[idx[i]] <- vv[idx[i]] + v[i]
    v <- vv
  }
  v[!is.finite(v)] <- 0; s <- sum(v); if (s > 0) v <- v/s; v
}

# Returns a named list (24 keys) of length-6 normalised target vectors.
# Edu and non-edu groups share the same age-gender target vector.
get_targets_24 <- function(STATE) {
  if (!exists("get_age_gender_targets"))
    stop("get_age_gender_targets() not found; source() the targets file.")
  tg        <- get_age_gender_targets(STATE)
  base_keys <- group_spec$key[!grepl("_edu$", group_spec$key)]
  missing   <- setdiff(base_keys, names(tg))
  if (length(missing)) stop("Targets missing: ", paste(missing, collapse=", "))
  out <- setNames(vector("list", nrow(group_spec)), group_spec$key)
  for (i in seq_len(nrow(group_spec))) {
    gk       <- group_spec$key[i]
    base_key <- gsub("_edu$", "", gk)
    out[[gk]] <- normalize_target_vec(tg[[base_key]])
  }
  out
}

# -------------------- second-choice targets --------------------
get_P_SC_all <- function(csv_path, states, year) {
  parties_keep <- c(1,2,3,4,5)
  x <- readr::read_csv(csv_path, show_col_types = FALSE) %>%
    dplyr::mutate(
      bundesland    = as.integer(bundesland),
      second_choice = as.character(second_choice),
      share_party   = as.numeric(share_party)
    )
  if ("year" %in% names(x)) x <- dplyr::filter(x, year == !!year)
  x <- dplyr::filter(x, bundesland %in% states)
  if (max(x$share_party, na.rm = TRUE) > 1.5)
    x <- dplyr::mutate(x, share_party = share_party/100)
  x <- x %>%
    dplyr::mutate(idx = PARTY_INDEX[toupper(trimws(second_choice))]) %>%
    dplyr::filter(!is.na(idx), idx %in% parties_keep) %>%
    dplyr::group_by(idx) %>%
    dplyr::summarise(v = mean(share_party, na.rm = TRUE), .groups = "drop")
  out <- numeric(5); names(out) <- as.character(1:5)
  out[as.character(x$idx)] <- x$v
  out
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

# Edu-aware turnout adjustment: strips _(noedu|edu)_share suffix before lookup
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

# merge_tiny_to_neighbor: merges cells that should be absorbed into the mesh.
#
# Two modes (mutually exclusive):
#   top_n   (integer) – keep the top_n largest non-OUTSIDE cells; all smaller
#                       cells (plus any degenerate ones) are merged into their
#                       largest touching neighbour.  This is the active mode.
#   frac_eps (double) – legacy area-fraction threshold; used only when top_n
#                       is NULL (kept for reference / fallback).
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
    # ---- Rank-based mode: keep top_n largest non-OUTSIDE cells ----
    non_out_idx <- which(inter_sf2$key != "OUTSIDE")
    a_non_out   <- a[non_out_idx]
    degenerate  <- !is.finite(a_non_out) | a_non_out <= 0
    # rank() with ties.method="first": rank 1 = largest
    rnks        <- rank(-pmax(a_non_out, 0), ties.method = "first")
    tiny_local  <- which(rnks > top_n | degenerate)
    tiny        <- non_out_idx[tiny_local]
    message(sprintf("[MERGE-TINY] top_n=%d | non-outside cells=%d | to merge=%d",
                    top_n, length(non_out_idx), length(tiny)))
  } else {
    # ---- Legacy area-fraction mode ----
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

afd_second_choice_regions <- function(C, bounds, afd_idx = 6L, sc_parties = c(5L), w2 = 1) {
  K    <- nrow(C)
  P_A  <- winner_region_sf(C, afd_idx, bounds, w2 = w2)
  out  <- setNames(vector("list", length(sc_parties)), as.character(sc_parties))
  if (is.null(P_A)) return(out)
  for (j in sc_parties) {
    Pj <- P_A
    for (k in setdiff(1:K, c(j, afd_idx))) {
      HP <- hp_j_beats_k(C[j, ], C[k, ], bounds, w2 = w2)
      Pj <- safe_intersection_sf(Pj, HP)
      if (is.null(Pj) || n_geoms(Pj) == 0) { Pj <- NULL; break }
    }
    out[[as.character(j)]] <- if (is.null(Pj)) NULL else drop_tiny_empty(as_sf(st_union(Pj)))
  }
  out
}

build_partition_one_election <- function(C, bounds, sc_parties = c(5L), w2 = 1) {
  K       <- nrow(C)
  W_list  <- vector("list", K)
  for (j in 1:K) W_list[[j]] <- winner_region_sf(C, j, bounds, w2 = w2)
  sc_regs <- afd_second_choice_regions(C, bounds, afd_idx = 6L, sc_parties = sc_parties, w2 = w2)
  parts   <- list()
  for (j in 1:5) {
    Wj <- W_list[[j]]
    if (!is.null(Wj) && n_geoms(Wj) > 0)
      parts[[length(parts) + 1L]] <- Wj %>% mutate(part = as.integer(j), sc = 0L)
  }
  A_reg    <- W_list[[6]]
  union_sc <- NULL
  for (j in sc_parties) {
    Pj <- sc_regs[[as.character(j)]]
    if (!is.null(Pj) && n_geoms(Pj) > 0) {
      if (!is.null(A_reg) && n_geoms(A_reg) > 0) Pj <- safe_intersection_sf(A_reg, Pj)
      if (!is.null(Pj) && n_geoms(Pj) > 0) {
        parts[[length(parts) + 1L]] <- Pj %>% mutate(part = 6L, sc = as.integer(j))
        union_sc <- safe_union_sf(union_sc, Pj)
      }
    }
  }
  if (!is.null(A_reg) && n_geoms(A_reg) > 0) {
    rest <- safe_difference_sf(A_reg, union_sc)
    if (!is.null(rest) && n_geoms(rest) > 0)
      parts[[length(parts) + 1L]] <- rest %>% mutate(part = 6L, sc = 0L)
  }
  if (!length(parts)) return(NULL)
  v_e <- do.call(rbind, lapply(parts, as_sf)) %>%
    group_by(part, sc) %>%
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
    load(f)  # results_std, pos_std

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

  # Sample up to MAX_ELECTIONS_PER_STATE elections per state, then take the union
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
# Collect pooled municipality data for a region (edu version)
# elections_by_state: named list (character state IDs) of integer
#   election_id vectors — each state is restricted to its own
#   MAX_ELECTIONS_PER_STATE elections.
# Subsamples SUBSAMPLE_N rows per state per election.
# ============================================================
collect_region_muni_edu <- function(STATES, YEAR, id_map, elections_by_state) {

  cand_cols  <- paste0("cand", 1:6, "_share")
  group_cols <- group_spec$col
  out        <- list()

  for (S in STATES) {
    f <- sprintf("../data/data_state%02d_year%d_edu.RData", S, YEAR)
    if (!file.exists(f)) next
    load(f)  # results_std, pos_std

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

    # Restrict this state to its own sampled elections (per-state cap)
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

    # Subsample SUBSAMPLE_N rows per state per election
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
        cand6_share
      )

    out[[length(out) + 1L]] <- df
  }

  if (!length(out)) return(tibble())
  bind_rows(out)
}

# ============================================================
# Build capacity matrices for the region solver (24 edu groups)
# Extended: also returns state_row_idx for per-state Pi solver.
# ============================================================
build_region_capacity_edu <- function(inter_sf, muni_all, group_spec, E_data, SC_PARTIES) {

  stopifnot(nrow(muni_all) > 0)
  K     <- 6L
  nc    <- nrow(inter_sf)
  R     <- nrow(muni_all)
  gkeys <- group_spec$key
  gcols <- group_spec$col

  A_list      <- vector("list", E_data)
  S_indicator <- vector("list", E_data)
  for (e in seq_len(E_data)) {
    p_e  <- safe_int(inter_sf[[paste0("p_e", e)]])
    sc_e <- safe_int(inter_sf[[paste0("sc_e", e)]])
    A    <- matrix(0L, K, nc)
    valid <- !is.na(p_e) & p_e >= 1L & p_e <= K
    if (any(valid)) A[cbind(p_e[valid], which(valid))] <- 1L
    A_list[[e]] <- A
    S_e <- setNames(vector("list", length(SC_PARTIES)), as.character(SC_PARTIES))
    for (j in SC_PARTIES) S_e[[as.character(j)]] <- as.numeric(p_e == 6L & sc_e == j)
    S_indicator[[e]] <- S_e
  }

  # B_AFD_list: R×nc per group key (all rows across all states)
  B_AFD_list <- setNames(vector("list", length(gkeys)), gkeys)
  for (gi in seq_along(gkeys)) {
    gcol <- gcols[gi]; gkey <- gkeys[gi]
    B    <- matrix(0, R, nc)
    for (r in seq_len(R)) {
      e       <- muni_all$election[r]
      B[r, ] <- as.numeric(muni_all[[gcol]][r]) * A_list[[e]][6, ]
    }
    B_AFD_list[[gkey]] <- B
  }

  states_present <- sort(unique(muni_all$state_id))
  states_chr     <- as.character(states_present)

  # Per-state row indices (needed by the state-Pi solver)
  state_row_idx <- setNames(
    lapply(states_chr, function(sid) which(as.character(muni_all$state_id) == sid)),
    states_chr
  )

  # Per-state group masses
  gm_by_state <- setNames(vector("list", length(states_chr)), states_chr)
  for (sid in states_chr) {
    rows_s <- muni_all[muni_all$state_id == as.integer(sid), ]
    w_s    <- as.numeric(rows_s$eligible_voters)
    gm_s   <- setNames(numeric(length(gkeys)), gkeys)
    for (gi in seq_along(gkeys)) {
      gcol          <- gcols[gi]
      gm_s[gkeys[gi]] <- sum(w_s * as.numeric(rows_s[[gcol]]), na.rm = TRUE)
    }
    gm_by_state[[sid]] <- gm_s
  }

  # Per-state rows_afd and rows_sc (accumulated weighted nc-vectors)
  rows_afd_state <- setNames(vector("list", length(states_chr)), states_chr)
  rows_sc_state  <- setNames(vector("list", length(states_chr)), states_chr)
  for (sid in states_chr) {
    rows_afd_state[[sid]] <- setNames(lapply(gkeys, function(gk) numeric(nc)), gkeys)
    rows_sc_state[[sid]]  <- setNames(lapply(gkeys, function(gk) {
      setNames(lapply(as.character(SC_PARTIES), function(j) numeric(nc)), as.character(SC_PARTIES))
    }), gkeys)
  }

  for (r in seq_len(R)) {
    e      <- muni_all$election[r]
    sid    <- as.character(muni_all$state_id[r])
    w_r    <- as.numeric(muni_all$eligible_voters[r])
    afd_v  <- A_list[[e]][6, ]
    for (gi in seq_along(gkeys)) {
      gcol <- gcols[gi]; gkey <- gkeys[gi]
      m    <- w_r * as.numeric(muni_all[[gcol]][r])
      if (!is.finite(m) || m == 0) next
      rows_afd_state[[sid]][[gkey]] <- rows_afd_state[[sid]][[gkey]] + m * afd_v
      for (j in SC_PARTIES) {
        sc_v <- S_indicator[[e]][[as.character(j)]]
        rows_sc_state[[sid]][[gkey]][[as.character(j)]] <-
          rows_sc_state[[sid]][[gkey]][[as.character(j)]] + m * (afd_v * sc_v)
      }
    }
  }

  list(
    B_AFD_list     = B_AFD_list,
    share_afd      = as.numeric(muni_all$cand6_share),
    weights_pop    = as.numeric(muni_all$eligible_voters),
    states_present = states_present,
    state_row_idx  = state_row_idx,
    gm_by_state    = gm_by_state,
    rows_afd_state = rows_afd_state,
    rows_sc_state  = rows_sc_state
  )
}

# ============================================================
# Region-level solver: 24 edu groups, STATE-SPECIFIC Pi
#
# Each state s has its own Pi[s][gk] (nc-vector, sums to 1).
# Objective: min sum_r |pred_r - share_afd_r|
#   where pred_r = sum_gk B_AFD_list[[gk]][r,] %*% Pi[[s_r]][[gk]]
#   and s_r is the state that row r belongs to.
#
# Constraints per state s, per paired base group bg:
#   |rows_afd_state[s][bg] . Pi[s][bg]
#    + rows_afd_state[s][bg_edu] . Pi[s][bg_edu]
#    - X_bg * T_total_s| <= AFD_GROUP_TOL * T_total_s
#   (analogous SC constraint)
#
# Edu similarity per state:
#   Pi[s][bg] <= (1 + EDU_DIFF_TOL) * Pi[s][bg_edu]  (and vice versa)
#
# Warm start: if prev_Pi_vals is supplied (nested list [sid][gk]),
#   initialise the new Variables from those values.
#   If nc matches the previous nc, use exact values;
#   otherwise fall back to uniform 1/nc initialisation.
# ============================================================
solve_afd_only_region_state_pi <- function(cap,
                                           tg_by_state,
                                           sc_by_state,
                                           AFD_GROUP_TOL,
                                           SC_TOL,
                                           EDU_DIFF_TOL,
                                           prev_Pi_vals = NULL,
                                           nc_prev      = NULL) {

  B_AFD_list     <- cap$B_AFD_list
  share_afd      <- cap$share_afd
  weights_pop    <- cap$weights_pop
  gkeys          <- names(B_AFD_list)
  states_present <- cap$states_present
  states_chr     <- as.character(states_present)
  state_row_idx  <- cap$state_row_idx
  gm_by_state    <- cap$gm_by_state
  rows_afd_state <- cap$rows_afd_state
  rows_sc_state  <- cap$rows_sc_state

  R  <- length(share_afd)
  nc <- ncol(B_AFD_list[[1]])

  base_keys <- unique(gsub("_edu$", "", gkeys))

  # ----- CVXR variables: one Pi[sid][gk] per state per group -----
  Pi <- setNames(lapply(states_chr, function(sid) {
    setNames(lapply(gkeys, function(gk) CVXR::Variable(nc)), gkeys)
  }), states_chr)

  # Warm start: set initial values on the new Variables
  if (!is.null(prev_Pi_vals)) {
    nc_match <- !is.null(nc_prev) && (nc_prev == nc)
    for (sid in states_chr) {
      pv_s <- prev_Pi_vals[[sid]]
      if (is.null(pv_s)) next
      for (gk in gkeys) {
        pv_gk <- pv_s[[gk]]
        if (is.null(pv_gk) || !is.numeric(pv_gk)) next
        if (nc_match && length(pv_gk) == nc) {
          # Exact warm start: use previous optimal values
          init_val <- pmax(pv_gk, 0)
          s        <- sum(init_val)
          if (is.finite(s) && s > 0) init_val <- init_val / s
          CVXR::value(Pi[[sid]][[gk]]) <- init_val
        } else {
          # Dimension mismatch: uniform initialisation
          CVXR::value(Pi[[sid]][[gk]]) <- rep(1/nc, nc)
        }
      }
    }
  }

  # ----- Build prediction for all R rows -----
  # Rows in muni_all are grouped by state (from collect_region_muni_edu),
  # so state_row_idx gives consecutive blocks.  Stack predictions in
  # states_chr order and reorder share_afd / weights_pop accordingly.

  row_reorder     <- unlist(state_row_idx[states_chr], use.names = FALSE)
  share_afd_ord   <- share_afd[row_reorder]
  weights_pop_ord <- weights_pop[row_reorder]

  pred_parts <- vector("list", length(states_chr))
  names(pred_parts) <- states_chr

  for (sid in states_chr) {
    idx_s <- state_row_idx[[sid]]
    if (!length(idx_s)) next
    pred_s <- Reduce(`+`, lapply(gkeys, function(gk) {
      B_s <- B_AFD_list[[gk]][idx_s, , drop = FALSE]
      B_s %*% Pi[[sid]][[gk]]
    }))
    pred_parts[[sid]] <- pred_s
  }

  # Remove empty states
  pred_parts <- pred_parts[!sapply(pred_parts, is.null)]
  if (!length(pred_parts))
    return(list(status = "no_states"))

  pred <- Reduce(CVXR::vstack, pred_parts)   # R × 1
  u    <- CVXR::Variable(R)

  # ----- Constraints -----
  cons <- list(
    u >= 0,
    pred - share_afd_ord <= u,
    -(pred - share_afd_ord) <= u
  )

  # Positivity + normalisation per state per group
  for (sid in states_chr) {
    for (gk in gkeys) {
      cons <- c(cons, list(
        Pi[[sid]][[gk]] >= 0,
        CVXR::sum_entries(Pi[[sid]][[gk]]) == 1
      ))
    }
  }

  # Per-state, per-base-group paired AfD share + SC constraints
  for (sid in states_chr) {
    tg_state <- tg_by_state[[sid]]
    sc_state <- sc_by_state[[sid]]
    if (is.null(tg_state) || is.null(sc_state)) next

    gm_s <- gm_by_state[[sid]]

    for (bg in base_keys) {
      sub_keys <- intersect(c(bg, paste0(bg, "_edu")), gkeys)
      T_total  <- sum(gm_s[sub_keys], na.rm = TRUE)
      if (!is.finite(T_total) || T_total <= 0) next

      X_bg <- as.numeric(tg_state[[bg]][6])
      if (!is.finite(X_bg) || X_bg < 0) next

      tol_abs   <- AFD_GROUP_TOL * T_total
      lhs_parts <- lapply(sub_keys, function(gk)
        t(matrix(rows_afd_state[[sid]][[gk]], ncol = 1)) %*% Pi[[sid]][[gk]])
      lhs <- Reduce(`+`, lhs_parts)
      cons <- c(cons,
                list(lhs >= (X_bg * T_total - tol_abs),
                     lhs <= (X_bg * T_total + tol_abs)))

      sc_target <- as.numeric(sc_state["LEFT"])
      if (is.finite(sc_target) && sc_target >= 0 && X_bg > 0) {
        sc_parts <- lapply(sub_keys, function(gk)
          t(matrix(rows_sc_state[[sid]][[gk]][["5"]], ncol = 1)) %*% Pi[[sid]][[gk]])
        lhs_sc      <- Reduce(`+`, sc_parts)
        target_mass <- sc_target * X_bg * T_total
        tol_sc      <- SC_TOL    * X_bg * T_total
        cons <- c(cons,
                  list(lhs_sc >= (target_mass - tol_sc),
                       lhs_sc <= (target_mass + tol_sc)))
      }
    }

    # Edu/non-edu similarity per state
    for (bg in base_keys) {
      edu_key <- paste0(bg, "_edu")
      if (!(edu_key %in% gkeys)) next
      cons <- c(cons, list(
        Pi[[sid]][[bg]]      <= (1 + EDU_DIFF_TOL) * Pi[[sid]][[edu_key]],
        Pi[[sid]][[edu_key]] <= (1 + EDU_DIFF_TOL) * Pi[[sid]][[bg]]
      ))
    }
  }

  prob <- CVXR::Problem(CVXR::Minimize(CVXR::sum_entries(u)), cons)

  use_warm <- !is.null(prev_Pi_vals)

  # Attempt 1: ECOS
  res <- try(CVXR::solve(prob, solver = "ECOS", verbose = FALSE,
                         warm_start = use_warm,
                         num_iter   = 500L,
                         feastol    = 1e-7,
                         reltol     = 1e-7,
                         abstol     = 1e-7),
             silent = TRUE)

  # Attempt 2: SCS fallback
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

  resid_v <- as.numeric(res$getValue(pred - share_afd_ord))
  mae     <- as.numeric(res$getValue(CVXR::sum_entries(u))) / R

  list(
    status         = as.character(res$status)[1],
    Pi_vals        = Pi_vals,
    mae            = mae,
    residuals      = resid_v,
    weights_pop    = weights_pop_ord,   # reordered to match residuals
    row_reorder    = row_reorder
  )
}

# -------------------- fit metrics --------------------
compute_fit_metrics <- function(fit, n_cells, lambda_cells = 0.0) {
  resid <- fit$residuals
  w     <- as.numeric(fit$weights_pop)
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

  list(mae = mae, wmae = wmae,
       mae_per_cell = mae_per_cell, wmae_per_cell = wmae_per_cell,
       mae_plus_lambda_cells = mae_plus_lambda_cells,
       wmae_plus_lambda_cells = wmae_plus_lambda_cells)
}

# ============================================================
# Main per-region function: w2 grid search with warm start
# ============================================================
run_one_region_w2 <- function(region_label, STATES, seed_base = 0, seed_idx = 1) {

  message(sprintf(
    "\n=======================\nRegion %s | Year %d | w2 grid search (state-Pi, 24-group edu)\n=======================\n",
    toupper(region_label), YEAR
  ))

  # ----- Build pooled mesh inputs -----
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

  # ----- Collect pooled municipality data -----
  # Pass per-state election lists so each state contributes only its own
  # MAX_ELECTIONS_PER_STATE districts.
  muni_all <- collect_region_muni_edu(STATES, YEAR, id_map, selected_by_state)
  if (!nrow(muni_all)) {
    message(sprintf("[SKIP] Region %s, year %d: no municipality rows.", region_label, YEAR))
    return(FALSE)
  }

  R         <- nrow(muni_all)
  share_afd <- as.numeric(muni_all$cand6_share)
  w_muni    <- as.numeric(muni_all$eligible_voters)

  # ----- Per-state targets and second-choice -----
  tg_by_state <- list()
  sc_by_state <- list()
  for (S in STATES) {
    sid <- as.character(S)
    tg_by_state[[sid]] <- tryCatch(
      get_targets_24(S),
      error = function(e) {
        message(sprintf("[SKIP] targets for state %02d: %s", S, conditionMessage(e)))
        NULL
      }
    )
    P_SC_all <- tryCatch(
      get_P_SC_all(sprintf("../targets/second_choice_%d.csv", YEAR),
                   states = S, year = YEAR),
      error = function(e) {
        message(sprintf("[SKIP] SC targets for state %02d: %s", S, conditionMessage(e)))
        NULL
      }
    )
    sc_by_state[[sid]] <- if (!is.null(P_SC_all)) c(LEFT = P_SC_all["5"]) else NULL
  }

  # ----- w2 grid search with warm start -----
  message(sprintf(
    "\n[W2 GRID SEARCH] YEAR %d | %s | grid=%s | n_states=%d | max_elections/state=%d | subsample_n=%d\n",
    YEAR, toupper(region_label),
    paste(W2_GRID, collapse=","),
    length(STATES), MAX_ELECTIONS_PER_STATE, SUBSAMPLE_N
  ))

  grid_rows    <- list()
  best         <- list(w2 = NA_real_, score = Inf, status = NA_character_,
                       row = NULL, fit = NULL, inter_sf = NULL)
  prev_Pi_vals <- NULL  # warm start state
  prev_nc      <- NULL  # nc from previous w2

  for (w2 in W2_GRID) {
    message(sprintf("[W2 TRY] YEAR %d | %s | w2=%.3f", YEAR, toupper(region_label), w2))

    out <- tryCatch({
      bounds <- c(xmin = min(xy_obs[, 1]) - VOR_PAD,
                  ymin = min(xy_obs[, 2]) - VOR_PAD,
                  xmax = max(xy_obs[, 1]) + VOR_PAD,
                  ymax = max(xy_obs[, 2]) + VOR_PAD)

      partitions <- vector("list", E_data)
      for (e in seq_len(E_data)) {
        part_e <- build_partition_one_election(C_list[[e]], bounds,
                                               sc_parties = SC_PARTIES, w2 = w2)
        if (is.null(part_e) || n_geoms(part_e) == 0)
          stop(sprintf("Empty partition for election %d (w2=%.3f)", e, w2))
        partitions[[e]] <- part_e
      }

      inter_sf <- NULL
      for (e in seq_len(E_data)) {
        part_e <- partitions[[e]] %>%
          mutate(
            !!sym(paste0("p_e",  e)) := as.integer(part),
            !!sym(paste0("sc_e", e)) := as.integer(sc)
          ) %>%
          dplyr::select(all_of(c(paste0("p_e", e), paste0("sc_e", e))), geometry)
        if (is.null(inter_sf)) {
          inter_sf <- part_e
        } else {
          inter_sf <- safe_intersection_sf(inter_sf, part_e)
          if (is.null(inter_sf) || n_geoms(inter_sf) == 0)
            stop(sprintf("Voronoi intersection collapsed at e=%d (w2=%.3f)", e, w2))
        }
      }

      p_cols   <- paste0("p_e",  1:E_data)
      sc_cols  <- paste0("sc_e", 1:E_data)
      geom_col <- attr(inter_sf, "sf_column")

      inter_sf <- inter_sf %>%
        mutate(across(all_of(p_cols),  safe_int),
               across(all_of(sc_cols), safe_int))

      any_afd <- inter_sf %>%
        mutate(any_afd = if_any(all_of(p_cols), ~ .x == 6L)) %>%
        mutate(any_afd = ifelse(is.na(any_afd), FALSE, any_afd)) %>%
        pull(any_afd)

      afd_sf <- inter_sf[any_afd, ]
      out_sf <- inter_sf[!any_afd, ]

      if (nrow(out_sf) > 0) {
        out_geom <- suppressWarnings(st_union(out_sf$geometry))
        out_row  <- out_sf[1, , drop = FALSE]
        out_row[[geom_col]] <- out_geom
        for (pc in p_cols)  out_row[[pc]] <- NA_integer_
        for (sc in sc_cols) out_row[[sc]] <- 0L
        inter_sf2 <- bind_rows(afd_sf, out_row)
      } else {
        inter_sf2 <- afd_sf
      }

      inter_sf2 <- inter_sf2 %>%
        mutate(key = do.call(paste, c(dplyr::pick(all_of(c(p_cols, sc_cols))), sep = "-"))) %>%
        group_by(key, across(all_of(c(p_cols, sc_cols)))) %>%
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

      cap <- build_region_capacity_edu(inter_sf2, muni_all, group_spec, E_data, SC_PARTIES)

      fit <- solve_afd_only_region_state_pi(
        cap           = cap,
        tg_by_state   = tg_by_state,
        sc_by_state   = sc_by_state,
        AFD_GROUP_TOL = AFD_GROUP_TOL,
        SC_TOL        = SC_TOL,
        EDU_DIFF_TOL  = EDU_DIFF_TOL,
        prev_Pi_vals  = prev_Pi_vals,
        nc_prev       = prev_nc
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

    # Update warm start state for next w2
    prev_Pi_vals <- out$fit$Pi_vals
    prev_nc      <- n_cells

    # Track best (prefer WMAE if available, else MAE)
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
    muni_all     = muni_all,
    tg_by_state  = tg_by_state,
    sc_by_state  = sc_by_state
  )
}

# ============================================================
# Outer loop: single region (from --region arg), multiple years × seeds
# ============================================================
YEARS <- c(2013)

REGIONS <- list(
  east = c(12, 14, 15, 16),
  west = 1:11
)

for (YEAR in YEARS) {
  source(sprintf("../targets/RW_%d_6groups.R",         YEAR))
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
        year_csv <- sprintf("diagnostics/w2_grid_region_state_pi_year%d_%s_seed%02d.csv",
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

    out_rds <- sprintf("fits_afd_only/afd_only_region_state_pi_year%d_%s_seed%02d.rds",
                       YEAR, region_label, seed_idx)
    saveRDS(list(
      year          = YEAR,
      geo_region    = region_label,
      seed_idx      = seed_idx,
      seed_base     = seed_base,
      w1            = W1_FIXED,
      w2_grid       = W2_GRID,
      result        = res,
      AFD_GROUP_TOL = AFD_GROUP_TOL,
      SC_TOL        = SC_TOL,
      EDU_DIFF_TOL  = EDU_DIFF_TOL,
      SUBSAMPLE_N   = SUBSAMPLE_N
    ), file = out_rds)
    message(sprintf("[SAVED] Year %d | %s | seed_idx %d → %s",
                    YEAR, toupper(region_label), seed_idx, out_rds))
  }
}
