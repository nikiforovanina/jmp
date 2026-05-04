# ===============================================================
# Linke-only model
# Geometry: 6-party full-plane Voronoi winner regions.
#   - Keep ONLY cells where Linke (party 5) wins in at least one election.
#   - No second-choice subcarving; no representative-statistic constraints.
#   - Turnout adjustment is kept.
#
# Optimisation (baseline):
#   - Objective: min L1 discrepancy of municipal Linke shares.
#   - Constraints: none beyond non-negativity (no group targets, no SC).
#
# Counterfactual 2-party mapping (for Linke voters only, BY OVERLAP):
#   - Relative to the -45-degree line through the median Linke position.
#     Linke is located in the positive-y / negative-x area, so the relevant
#     separator is the -45-degree line  (slope = -1), not the 45-degree line.
#   - Two counterfactual parties symmetrically placed along the (1,1) direction
#     from the median Linke point (med_x, med_y):
#       * AboveNeg45 at (med_x + 1, med_y + 1)  → wins voters where x+y > med_x+med_y
#       * BelowNeg45 at (med_x - 1, med_y - 1)  → wins voters where x+y < med_x+med_y
#     The Voronoi boundary between these two points IS the -45-degree line
#     through (med_x, med_y).
#   - For each Linke mesh cell c and each counterfactual party P, define:
#         mask_P(c) = 1  if Voronoi region of P intersects cell c, else 0.
#
# Bounds:
#   - SINGLE META-GROUP: all 24 demographic groups summed.
#   - Bisection over b ∈ [0, 0.5]: share of Linke voter mass in each
#     counterfactual region, subject to MAE ≤ baseline MAE.
#
# Outputs:
#   - fits_linke_only/linke_only_state%02d_year%d_seed%02d.rds
#   - bounds/linke_bounds_year%d_seed%02d.csv
#   - diagnostics/linke_only_two_party_cell_counts_state%02d_year%d_seed%02d.csv
# ===============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr); library(tibble); library(purrr)
  library(CVXR); library(rlang);  library(readr)
})
suppressMessages(sf::sf_use_s2(FALSE))
set.seed(1509)

# ---- YEAR from CLI arg or env var ----
.args <- commandArgs(trailingOnly = TRUE)
.year_raw <- if (length(.args) >= 1 && nzchar(.args[[1]])) .args[[1]] else Sys.getenv("YEAR", "")
if (!nzchar(.year_raw)) {
  stop("YEAR not provided. Pass as CLI arg (Rscript 0_2parts_linke.R 2017) ",
       "or set env var YEAR=2017.")
}
YEAR <- as.integer(.year_raw)
if (is.na(YEAR)) stop("YEAR must be an integer, got: ", .year_raw)
message(sprintf(">>> Running 0_2parts_linke.R for YEAR = %d", YEAR))

source("0_configuration.R")  # SPEC_TAG (used by 1_graph_cluster_shares.R)

# -------------------- knobs (year/state handled below) --------------------
SUBSAMPLE_N        <- 50
MAX_ELECTIONS      <- 40
RESULTS_MIN        <- 1e-5

ABS_EPS            <- 1e-5   # tolerance for MAE benchmark
REL_EPS            <- 1e-5
MAX_BISECT_IT      <- 15
BISECT_TOL         <- 5e-3

SMALL_MUNI_MAX_ELIGIBLE <- 10000
MIN_ROWS_PER_ELECTION  <- 5    # election_ids with <= this many rows are dropped

SEED_LIST <- c(1509)

# Voronoi padding for geometry
VOR_PAD            <- 5

# Tiny-cell merging
TINY_CELL_FRAC_EPS <- 1e-5

# Output dirs
dir.create("bounds",           showWarnings = FALSE, recursive = TRUE)
dir.create("diagnostics",      showWarnings = FALSE, recursive = TRUE)
dir.create("data",             showWarnings = FALSE, recursive = TRUE)
dir.create("fits_linke_only",  showWarnings = FALSE, recursive = TRUE)

# ---------- helpers ----------
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

group_spec <- tibble::tribble(
  ~key,                  ~pretty,               ~col,                           ~tgt_name,
  "female_1825",         "Female 18–25",        "female_18_24_noedu_share",     "female_1825",
  "female_1825_edu",     "Female 18–25 (edu)",  "female_18_24_edu_share",       "female_1825_edu",
  "female_2535",         "Female 25–35",        "female_25_34_noedu_share",     "female_2535",
  "female_2535_edu",     "Female 25–35 (edu)",  "female_25_34_edu_share",       "female_2535_edu",
  "female_3545",         "Female 35–45",        "female_35_44_noedu_share",     "female_3545",
  "female_3545_edu",     "Female 35–45 (edu)",  "female_35_44_edu_share",       "female_3545_edu",
  "female_4560",         "Female 45–60",        "female_45_59_noedu_share",     "female_4560",
  "female_4560_edu",     "Female 45–60 (edu)",  "female_45_59_edu_share",       "female_4560_edu",
  "female_6070",         "Female 60–70",        "female_60_69_noedu_share",     "female_6070",
  "female_6070_edu",     "Female 60–70 (edu)",  "female_60_69_edu_share",       "female_6070_edu",
  "female_70plus",       "Female 70+",          "female_70plus_noedu_share",    "female_70plus",
  "female_70plus_edu",   "Female 70+ (edu)",    "female_70plus_edu_share",      "female_70plus_edu",
  "male_1825",           "Male 18–25",          "male_18_24_noedu_share",       "male_1825",
  "male_1825_edu",       "Male 18–25 (edu)",    "male_18_24_edu_share",         "male_1825_edu",
  "male_2535",           "Male 25–35",          "male_25_34_noedu_share",       "male_2535",
  "male_2535_edu",       "Male 25–35 (edu)",    "male_25_34_edu_share",         "male_2535_edu",
  "male_3545",           "Male 35–45",          "male_35_44_noedu_share",       "male_3545",
  "male_3545_edu",       "Male 35–45 (edu)",    "male_35_44_edu_share",         "male_3545_edu",
  "male_4560",           "Male 45–60",          "male_45_59_noedu_share",       "male_4560",
  "male_4560_edu",       "Male 45–60 (edu)",    "male_45_59_edu_share",         "male_4560_edu",
  "male_6070",           "Male 60–70",          "male_60_69_noedu_share",       "male_6070",
  "male_6070_edu",       "Male 60–70 (edu)",    "male_60_69_edu_share",         "male_6070_edu",
  "male_70plus",         "Male 70+",            "male_70plus_noedu_share",      "male_70plus",
  "male_70plus_edu",     "Male 70+ (edu)",      "male_70plus_edu_share",        "male_70plus_edu"
)
name_map <- setNames(as.character(group_spec$pretty), as.character(group_spec$key))
pretty_name <- function(gkey) {
  nm <- name_map[[as.character(gkey)]]
  if (is.null(nm)) as.character(gkey) else nm
}

PARTY_INDEX <- c("CDU"=1,"CSU"=1,"SPD"=2,"GRUENE"=3,"GRÜNE"=3,"GRUEN"=3,"GREEN"=3,
                 "FDP"=4,"LINKE"=5,"PDS"=5,"DIE LINKE"=5,"LEFT" = 5,
                 "AFD"=6,"AfD"=6)

normalize_target_vec <- function(v) {
  v <- as.numeric(v); if (length(v)!=6) stop("Each target vector must have length 6.")
  nm <- names(v); if (is.null(nm)) nm <- as.character(seq_along(v))
  if (all(!is.na(suppressWarnings(as.integer(nm))))) {
    ord <- match(as.character(1:6), nm); if (any(is.na(ord))) stop("Target names must include '1'..'6'.")
    v <- v[ord]; names(v) <- as.character(1:6)
  } else {
    key <- toupper(trimws(nm)); idx <- PARTY_INDEX[key]
    vv <- numeric(6); names(vv) <- as.character(1:6)
    for (i in seq_along(v)) if (!is.na(idx[i])) vv[idx[i]] <- vv[idx[i]] + v[i]
    v <- vv
  }
  v[!is.finite(v)] <- 0; s <- sum(v); if (s>0) v <- v/s; v
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

# ---------------- geometry helpers ----------------
as_sf <- function(g) {
  if (is.null(g)) return(NULL)
  if (inherits(g,"sf")) return(g)
  if (inherits(g,"sfc")) return(st_sf(geometry=g))
  g
}
n_geoms <- function(g) {
  if (is.null(g)) return(0L)
  if (inherits(g,"sf")) return(nrow(g))
  if (inherits(g,"sfc")) return(length(g))
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

merge_tiny_to_neighbor <- function(inter_sf2, frac_eps = 1e-8, allow_merge_into_outside = TRUE) {
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
  bbox <- sf::st_bbox(inter_sf2)
  bbox_area <- as.numeric((bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"]))
  if (!is.finite(bbox_area) || bbox_area <= 0) return(inter_sf2)
  a <- suppressWarnings(as.numeric(sf::st_area(inter_sf2)))
  afrac <- a / bbox_area
  tiny <- which(
    inter_sf2$key != "OUTSIDE" &
      (!is.finite(afrac) | afrac <= frac_eps | !is.finite(a) | a <= 0)
  )
  if (!length(tiny)) return(inter_sf2)
  message(sprintf("[MERGE-TINY] tiny=%d | area_tiny=%.3e | share_nonoutside=%.3e",
                  length(tiny),
                  sum(a[tiny], na.rm=TRUE),
                  sum(a[tiny], na.rm=TRUE) / sum(a[inter_sf2$key!="OUTSIDE"], na.rm=TRUE)))
  keep <- setdiff(seq_len(nrow(inter_sf2)), tiny)
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
    if (!allow_merge_into_outside && length(nbrs)) {
      nbrs <- nbrs[ inter_sf2$key[nbrs] != "OUTSIDE" ]
    }
    jstar <- NA_integer_
    if (length(nbrs)) {
      nbr_areas <- a[nbrs]
      nbr_areas[!is.finite(nbr_areas)] <- 0
      jstar <- nbrs[which.max(nbr_areas)]
    }
    if (is.na(jstar)) {
      if (is.null(cent)) {
        message("[MERGE-TINY] Computing centroids (fallback triggered for isolated tiny cell)...")
        cent <- tryCatch(
          suppressWarnings(sf::st_coordinates(sf::st_centroid(inter_sf2))),
          error = function(e) {
            message("[MERGE-TINY] st_centroid failed: ", conditionMessage(e))
            bb <- sf::st_bbox(inter_sf2)
            matrix(c(rep(mean(c(bb["xmin"], bb["xmax"])), nrow(inter_sf2)),
                     rep(mean(c(bb["ymin"], bb["ymax"])), nrow(inter_sf2))),
                   ncol = 2)
          }
        )
      }
      dx <- cent[keep, 1] - cent[i, 1]
      dy <- cent[keep, 2] - cent[i, 2]
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
  inter_sf2$cell_id <- seq_len(nrow(inter_sf2))
  inter_sf2
}

safe_intersection_sf <- function(a,b){
  a <- drop_tiny_empty(a); b <- drop_tiny_empty(b)
  if (is.null(a)||is.null(b)) return(NULL)
  out <- try(suppressWarnings(st_intersection(a,b)), silent=TRUE)
  if (inherits(out,"try-error")||is.null(out)) return(NULL)
  drop_tiny_empty(out)
}
safe_union_sf <- function(a,b) {
  a <- drop_tiny_empty(a); b <- drop_tiny_empty(b)
  if (is.null(a) || n_geoms(a)==0) return(b)
  if (is.null(b) || n_geoms(b)==0) return(a)
  out <- try(suppressWarnings(st_union(a, b)), silent=TRUE)
  if (inherits(out,"try-error")||is.null(out)) return(a)
  drop_tiny_empty(out)
}
safe_difference_sf <- function(a,b) {
  a <- drop_tiny_empty(a); b <- drop_tiny_empty(b)
  if (is.null(a) || n_geoms(a)==0) return(NULL)
  if (is.null(b) || n_geoms(b)==0) return(a)
  out <- try(suppressWarnings(st_difference(a, b)), silent=TRUE)
  if (inherits(out,"try-error")||is.null(out)) return(a)
  drop_tiny_empty(out)
}

clip_rect_by_halfplane <- function(rect_xy, n, b, eps = 1e-12) {
  if (any(!is.finite(n)) || !is.finite(b)) return(NULL)

  if (sum(abs(n)) < eps) {
    out <- rect_xy
    if (!all(out[1, ] == out[nrow(out), ])) {
      out <- rbind(out, out[1, , drop = FALSE])
    }
    return(out)
  }

  gval <- function(p) sum(p * n) - b

  V   <- rect_xy
  out <- list()

  for (i in seq_len(nrow(V))) {
    S <- V[i, ]
    E <- V[ifelse(i == nrow(V), 1L, i + 1L), ]

    gS <- gval(S); if (!is.finite(gS)) gS <- Inf
    gE <- gval(E); if (!is.finite(gE)) gE <- Inf

    S_in <- (gS <= eps)
    E_in <- (gE <= eps)

    if (S_in && E_in) {
      out[[length(out) + 1L]] <- E

    } else if (xor(S_in, E_in)) {
      den <- gS - gE
      if (is.finite(den) && abs(den) > eps && is.finite(gS)) {
        t <- gS / den
        if (is.finite(t) && t >= -1e-9 && t <= 1 + 1e-9) {
          I <- S + t * (E - S)
          if (all(is.finite(I))) {
            if (S_in && !E_in) {
              out[[length(out) + 1L]] <- I
            } else if (!S_in && E_in) {
              out[[length(out) + 1L]] <- I
              out[[length(out) + 1L]] <- E
            }
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

  if (!all(res[1, ] == res[nrow(res), ])) {
    res <- rbind(res, res[1, , drop = FALSE])
  }

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
  if (is.null(clipped)) return(NULL)
  if (!is.matrix(clipped)) return(NULL)
  if (nrow(clipped) < 4L) return(NULL)
  if (any(clipped[1, ] != clipped[nrow(clipped), ])) {
    clipped <- rbind(clipped, clipped[1, , drop = FALSE])
  }

  distinct <- unique(clipped[-nrow(clipped), , drop = FALSE])
  if (nrow(distinct) < 3L) return(NULL)

  poly <- try(
    st_sfc(st_polygon(list(clipped)), crs = sf::NA_crs_),
    silent = TRUE
  )
  if (inherits(poly, "try-error")) return(NULL)

  as_sf(poly)
}

# ------------------ Geometry: NO VALENCE ------------------
hp_j_beats_k <- function(Cj, Ck, bounds) {
  n <- Ck - Cj
  b <- (sum(Ck^2) - sum(Cj^2)) / 2
  halfplane_poly_sf(n, b, bounds)
}
winner_region_sf <- function(C, j, bounds) {
  K <- nrow(C)
  P <- as_sf(st_sfc(st_polygon(list(matrix(c(bounds["xmin"],bounds["ymin"],
                                             bounds["xmax"],bounds["ymin"],
                                             bounds["xmax"],bounds["ymax"],
                                             bounds["xmin"],bounds["ymax"],
                                             bounds["xmin"],bounds["ymin"]),
                                           ncol=2, byrow=TRUE)))))
  for (k in setdiff(1:K, j)) {
    HP <- hp_j_beats_k(C[j,], C[k,], bounds)
    P  <- safe_intersection_sf(P, HP)
    if (is.null(P) || n_geoms(P) == 0) return(NULL)
  }
  drop_tiny_empty(as_sf(st_union(P)))
}

# Build per-election partition: 6 winner regions, no SC subcarving.
build_partition_one_election <- function(C, bounds) {
  K <- nrow(C)
  parts <- list()
  for (j in 1:K) {
    Wj <- winner_region_sf(C, j, bounds)
    if (!is.null(Wj) && n_geoms(Wj) > 0) {
      df <- Wj %>% mutate(part = as.integer(j), sc = 0L)
      parts[[length(parts)+1L]] <- df
    }
  }
  if (!length(parts)) return(NULL)
  v_e <- do.call(rbind, lapply(parts, as_sf))
  v_e <- v_e %>%
    group_by(part, sc) %>%
    summarise(geometry = st_union(geometry), .groups="drop") %>%
    as_sf()
  drop_tiny_empty(v_e)
}

# ------------------ Capacity building (Linke-only) ------------------
rebuild_capacity_from_mesh_linke <- function(inter_sf, muni, group_spec, w_muni, E_data) {
  K  <- 6L
  nc <- nrow(inter_sf)
  R  <- nrow(muni)

  A_list <- vector("list", E_data)
  for (e in seq_len(E_data)) {
    p_col <- paste0("p_e", e)
    p_e   <- safe_int(inter_sf[[p_col]])
    A <- matrix(0L, K, nc)
    valid <- !is.na(p_e) & p_e >= 1L & p_e <= K
    if (any(valid)) {
      A[cbind(p_e[valid], which(valid))] <- 1L
    }
    A_list[[e]] <- A
  }

  # B_LINKE_list[gkey]: R × nc matrix; entry (r,c) = muni_share_g[r] * 1[Linke wins cell c in election r]
  B_LINKE_list <- vector("list", nrow(group_spec)); names(B_LINKE_list) <- group_spec$key
  for (gi in seq_len(nrow(group_spec))) {
    gcol <- group_spec$col[gi]
    gkey <- group_spec$key[gi]
    B <- matrix(0, R, nc)
    for (r in seq_len(R)) {
      e <- muni$election[r]
      B[r, ] <- muni[[gcol]][r] * A_list[[e]][5, ]  # party 5 = Linke
    }
    B_LINKE_list[[gkey]] <- B
  }

  # rows_linke[gkey]: nc-vector of weighted Linke mass per cell (for bisection constraints)
  rows_linke <- setNames(vector("list", nrow(group_spec)), group_spec$key)
  for (gi in seq_len(nrow(group_spec))) {
    gcol <- group_spec$col[gi]
    gkey <- group_spec$key[gi]
    acc <- numeric(nc)
    for (r in seq_len(R)) {
      e <- muni$election[r]
      acc <- acc + (w_muni[r] * muni[[gcol]][r]) * A_list[[e]][5, ]
    }
    rows_linke[[gkey]] <- acc
  }

  list(B_LINKE_list = B_LINKE_list,
       rows_linke   = rows_linke,
       A_list       = A_list)
}

# ---------- JOINT solver: Linke-only, no representative-stat or SC constraints ----------
solve_linke_only <- function(B_LINKE_list, share_linke,
                             add_constraints_fn = NULL) {

  gkeys <- names(B_LINKE_list)
  if (is.null(gkeys) || !length(gkeys)) stop("B_LINKE_list must be a named list by group key.")
  R  <- length(share_linke)
  nc <- ncol(B_LINKE_list[[1]])

  Pi <- setNames(vector("list", length(gkeys)), gkeys)
  for (gk in gkeys) Pi[[gk]] <- CVXR::Variable(nc)
  u  <- CVXR::Variable(R)

  cons <- list(u >= 0)

  pred <- 0
  for (gk in gkeys) {
    pred <- pred + B_LINKE_list[[gk]] %*% Pi[[gk]]
    cons <- c(cons, list(Pi[[gk]] >= 0))
  }
  residual <- pred - share_linke
  cons <- c(cons, list(residual <= u, -residual <= u))

  if (!is.null(add_constraints_fn)) {
    env <- list(Pi = Pi)
    extra <- add_constraints_fn(env)
    if (length(extra)) cons <- c(cons, extra)
  }

  prob <- CVXR::Problem(CVXR::Minimize(CVXR::sum_entries(u)), cons)

  res <- try(CVXR::solve(prob, solver = "ECOS", verbose = FALSE,
                         num_iter  = 500L,
                         feastol   = 1e-7,
                         reltol    = 1e-7,
                         abstol    = 1e-7),
             silent = TRUE)

  if (inherits(res, "try-error") || is.null(res$status) ||
      res$status == "solver_error") {
    message("[SOLVE] ECOS solver_error — retrying with SCS ...")
    res <- try(CVXR::solve(prob, solver = "SCS", verbose = FALSE,
                           max_iters = 10000L,
                           eps       = 1e-5),
               silent = TRUE)
  }

  if (inherits(res, "try-error") || is.null(res$status)) {
    return(list(status = "solver_error"))
  }
  if (!(res$status %in% c("optimal","optimal_inaccurate"))) {
    return(list(status = res$status))
  }

  Pi_vals <- lapply(Pi, function(v) as.numeric(res$getValue(v)))
  mae     <- as.numeric(res$getValue(CVXR::sum_entries(u)))/R

  list(status = res$status,
       Pi_vals = Pi_vals,
       mae = mae,
       residuals = as.numeric(res$getValue(residual)))
}

bench_ok <- function(mae, t_bench) {
  mae <= t_bench + max(ABS_EPS, REL_EPS * t_bench)
}

# ---------- 2-party masks (AboveNeg45 vs BelowNeg45) on Linke mesh, BY OVERLAP ----------
# C_cf[1,] = BelowNeg45  at (med_x - 1, med_y - 1)
# C_cf[2,] = AboveNeg45  at (med_x + 1, med_y + 1)
# Voronoi boundary between these two points is the -45-degree line through (med_x, med_y):
#   y = -x + (med_x + med_y),  i.e.  x + y = med_x + med_y.
# AboveNeg45 wins cells where x + y > med_x + med_y.
# BelowNeg45 wins cells where x + y < med_x + med_y.
two_party_masks <- function(inter_sf, bounds_cf, C_cf) {
  nc <- nrow(inter_sf)

  reg_BelowNeg45 <- winner_region_sf(C_cf, 1, bounds_cf)
  reg_AboveNeg45 <- winner_region_sf(C_cf, 2, bounds_cf)

  make_mask <- function(reg_sf) {
    if (is.null(reg_sf) || n_geoms(reg_sf) == 0L) {
      return(rep(0, nc))
    }
    rel <- suppressWarnings(st_intersects(inter_sf, reg_sf, sparse = TRUE))
    as.numeric(lengths(rel) > 0L)
  }

  mask_BelowNeg45 <- make_mask(reg_BelowNeg45)
  mask_AboveNeg45 <- make_mask(reg_AboveNeg45)

  list(BelowNeg45 = mask_BelowNeg45,
       AboveNeg45 = mask_AboveNeg45)
}

# ----------------- main per-state (Linke-only + two-party bounds) -----------------
run_one <- function(STATE, seed_base = 0, seed_idx = 1) {

  data_file <- sprintf("../data/data_state%02d_year%d_edu.RData", STATE, YEAR)
  if (!file.exists(data_file)) {
    message(sprintf("[SKIP] No data file for state %02d, year %d", STATE, YEAR))
    return(FALSE)
  }
  load(data_file)  # expects: results_std, pos_std

  if (!exists("results_std") || !nrow(results_std) ||
      !exists("pos_std")     || !nrow(pos_std)) {
    message(sprintf("[SKIP] Empty results/positions for state %02d, year %d", STATE, YEAR))
    return(FALSE)
  }

  results_std <- results_std %>% filter(eligible_voters < SMALL_MUNI_MAX_ELIGIBLE)
  keep_wk <- results_std %>%
    group_by(election_id) %>%
    filter(n() > MIN_ROWS_PER_ELECTION) %>%
    ungroup() %>%
    pull(election_id) %>%
    unique()
  results_std <- results_std %>% filter(election_id %in% keep_wk)
  pos_std     <- pos_std     %>% filter(election_id %in% keep_wk)

  message(sprintf(
    "\n=======================\nState %02d | Year %d | Linke-only model, 2-party bounds relative to -45-degree line through median Linke position\n=======================\n",
    STATE, YEAR
  ))

  # --- candidate shares (rebuild if needed) ---
  cand_cols <- paste0("cand", 1:6, "_share")
  if (!all(cand_cols %in% names(results_std))) {
    src_cols <- c("cdu_norm","csu_norm","spd_norm",
                  "gruene_norm","fdp_norm","linke_pds_norm","afd_norm")
    if (!all(src_cols %in% names(results_std))) {
      stop("Missing candX_share and no *_norm to rebuild.")
    }
    results_std <- results_std %>%
      mutate(
        cand1_share = pmax(0, coalesce(cdu_norm,0)+coalesce(csu_norm,0)),
        cand2_share = pmax(0, coalesce(spd_norm,0)),
        cand3_share = pmax(0, coalesce(gruene_norm,0)),
        cand4_share = pmax(0, coalesce(fdp_norm,0)),
        cand5_share = pmax(0, coalesce(linke_pds_norm,0)),
        cand6_share = pmax(0, coalesce(afd_norm,0))
      )
  }
  if (all(cand_cols %in% names(results_std))) {
    before_n <- nrow(results_std)
    results_std <- results_std %>%
      filter(if_all(all_of(cand_cols), ~ is.finite(.x) & .x >= RESULTS_MIN))
    removed_n <- before_n - nrow(results_std)
    message(sprintf("[CLEAN] Dropped %d rows with any result < %.5g",
                    removed_n, RESULTS_MIN))
    if (!nrow(results_std)) {
      message("[SKIP] No rows remain after cleaning.")
      return(FALSE)
    }
  }
  rs <- rowSums(dplyr::select(results_std, all_of(cand_cols)), na.rm = TRUE)
  results_std <- results_std %>%
    mutate(across(all_of(cand_cols), ~ .x / ifelse(rs>0, rs, NA_real_)))

  # Overlap & subsample elections
  results_std <- results_std %>%
    mutate(election_id = safe_int(election_id))
  pos_std <- pos_std %>%
    mutate(election_id = safe_int(election_id), cand = safe_int(cand))
  elections_both <- sort(intersect(unique(results_std$election_id),
                                   unique(pos_std$election_id)))
  if (!length(elections_both)) {
    message("[SKIP] No overlapping elections")
    return(FALSE)
  }
  set.seed(120 + STATE + YEAR + seed_base)
  if (length(elections_both) > MAX_ELECTIONS) {
    elections_both <- sort(sample(elections_both, MAX_ELECTIONS))
  }
  id_map <- tibble(election_id = elections_both, e_idx = seq_along(elections_both))
  results_std <- results_std %>%
    semi_join(id_map, by = "election_id") %>%
    inner_join(id_map, by = "election_id") %>%
    mutate(election = e_idx) %>% select(-e_idx)
  pos_std <- pos_std %>%
    semi_join(id_map, by = "election_id") %>%
    inner_join(id_map, by = "election_id") %>%
    mutate(election = e_idx) %>% select(-e_idx)
  E_data <- nrow(id_map)

  # Subsample municipalities per election
  set.seed(10200 + STATE*101 + YEAR + seed_base)
  results_std <- results_std %>%
    group_by(election) %>%
    group_modify(~ {
      n <- nrow(.x)
      if (n <= SUBSAMPLE_N) .x else .x[sample.int(n, SUBSAMPLE_N), , drop=FALSE]
    }) %>%
    ungroup()

  # Demography
  need_cols <- group_spec$col
  miss <- setdiff(need_cols, names(results_std))
  if (length(miss)) {
    warning(sprintf(
      "[STATE %02d] Missing %d/%d demographic columns — filling with 0.\n  Missing: %s\n  Available in data: %s",
      STATE, length(miss), length(need_cols),
      paste(miss, collapse=", "),
      paste(intersect(need_cols, names(results_std)), collapse=", ")
    ))
    results_std[miss] <- 0
  }
  demo_sums <- sapply(need_cols, function(cc) sum(abs(as.numeric(results_std[[cc]])), na.rm=TRUE))
  if (all(demo_sums == 0)) {
    stop(sprintf(
      "[STATE %02d] All %d demographic share columns are zero after fill.\n  Expected columns: %s\n  Data columns (first 30): %s",
      STATE, length(need_cols),
      paste(need_cols, collapse=", "),
      paste(head(names(results_std), 30), collapse=", ")
    ))
  } else {
    n_zero <- sum(demo_sums == 0)
    if (n_zero > 0)
      message(sprintf("[STATE %02d] %d demographic column(s) are all-zero (missing edu data?): %s",
                      STATE, n_zero, paste(need_cols[demo_sums == 0], collapse=", ")))
  }

  # Turnout adjustment (state-specific) — kept
  results_std <- apply_turnout_adjustment(results_std, STATE, need_cols)
  results_std$municipality <- safe_first_non_na(
    results_std,
    c("code_cleaned","ags","code")
  )
  muni <- results_std %>%
    transmute(election, municipality,
              across(all_of(group_spec$col)),
              across(all_of(cand_cols)),
              eligible_voters = coalesce(eligible_voters, 1))

  # Candidate positions → complete per election
  K <- 6L
  cand_means <- pos_std %>%
    group_by(cand) %>%
    summarise(mx=mean(x, na.rm=TRUE),
              my=mean(y, na.rm=TRUE), .groups="drop")
  fallback_x <- mean(pos_std$x, na.rm = TRUE); if (!is.finite(fallback_x)) fallback_x <- 0
  fallback_y <- mean(pos_std$y, na.rm = TRUE); if (!is.finite(fallback_y)) fallback_y <- 0

  pos_std_full <- pos_std %>%
    select(election_id, election, cand, x, y) %>%
    group_by(election_id, election) %>%
    tidyr::complete(cand = 1:K) %>%
    ungroup() %>%
    left_join(cand_means, by="cand") %>%
    mutate(
      x = ifelse(is.na(x), mx, x),
      y = ifelse(is.na(y), my, y),
      x = ifelse(is.na(x) | !is.finite(x), fallback_x, x),
      y = ifelse(is.na(y) | !is.finite(y), fallback_y, y)
    ) %>%
    select(election_id, e_idx=election, cand, x, y)

  ysplit <- split(pos_std_full, pos_std_full$e_idx)
  if (!all(vapply(ysplit, nrow, 1L) == K)) {
    message("[SKIP] some elections miss candidates after completion")
    return(FALSE)
  }

  # Bounds for policy space
  C_list <- lapply(seq_along(ysplit), function(e) {
    df <- ysplit[[e]][order(ysplit[[e]]$cand), ]
    as.matrix(df[, c("x","y")])
  })
  xy_obs <- do.call(rbind, C_list)
  bounds <- c(xmin=min(xy_obs[,1]) - VOR_PAD,
              ymin=min(xy_obs[,2]) - VOR_PAD,
              xmax=max(xy_obs[,1]) + VOR_PAD,
              ymax=max(xy_obs[,2]) + VOR_PAD)

  # Build per-election partitions (no SC subcarving), then superimpose
  partitions <- vector("list", E_data)
  for (e in seq_len(E_data)) {
    part_e <- build_partition_one_election(C_list[[e]], bounds)
    if (is.null(part_e) || n_geoms(part_e)==0) {
      message(sprintf("[SKIP] Election %d in state %02d produced empty partition", e, STATE))
      return(FALSE)
    }
    partitions[[e]] <- part_e
  }

  # Superimpose partitions across elections
  inter_sf <- NULL
  for (e in seq_len(E_data)) {
    part_e <- partitions[[e]] %>%
      mutate(
        !!sym(paste0("p_e", e))  := as.integer(part),
        !!sym(paste0("sc_e", e)) := as.integer(sc)
      ) %>%
      dplyr::select(all_of(c(paste0("p_e",e), paste0("sc_e",e))), geometry)
    if (is.null(inter_sf)) {
      inter_sf <- part_e
    } else {
      inter_sf <- safe_intersection_sf(inter_sf, part_e)
      if (is.null(inter_sf) || n_geoms(inter_sf) == 0) {
        message(sprintf("[SKIP] Voronoi intersection collapsed at election e=%d", e))
        return(FALSE)
      }
    }
  }

  # Keep only cells where Linke (party 5) wins in at least one election
  p_cols  <- paste0("p_e", 1:E_data)
  sc_cols <- paste0("sc_e", 1:E_data)
  geom_col <- attr(inter_sf, "sf_column")

  inter_sf <- inter_sf %>%
    mutate(across(all_of(p_cols), safe_int),
           across(all_of(sc_cols), safe_int),
           any_linke = if_any(all_of(p_cols), ~ .x == 5L)) %>%
    filter(any_linke) %>%
    select(-any_linke) %>%
    mutate(key = do.call(paste, c(dplyr::pick(all_of(c(p_cols, sc_cols))), sep="-"))) %>%
    group_by(key, across(all_of(c(p_cols, sc_cols)))) %>%
    summarise(geometry = st_union(!!sym(geom_col)), .groups="drop") %>%
    mutate(cell_id = dplyr::row_number()) %>%
    as_sf()

  inter_sf <- drop_tiny_empty(inter_sf)
  if (is.null(inter_sf) || n_geoms(inter_sf) == 0) {
    message("[SKIP] All Linke cells dropped after consolidation")
    return(FALSE)
  }
  inter_sf <- merge_tiny_to_neighbor(inter_sf, frac_eps = TINY_CELL_FRAC_EPS)
  if (is.null(inter_sf) || n_geoms(inter_sf) == 0) {
    message("[SKIP] All Linke cells dropped after tiny-cell merge")
    return(FALSE)
  }
  nc <- nrow(inter_sf)
  message(sprintf("[MESH] State %02d, year %d: %d Linke-relevant cells after superimposition + tiny-cell merge", STATE, YEAR, nc))

  # ---------- Build targets ----------
  R  <- nrow(muni)
  w_muni <- if ("eligible_voters" %in% names(muni)) muni$eligible_voters else rep(1, R)

  share_linke <- as.numeric(muni$cand5_share)

  group_masses <- setNames(numeric(nrow(group_spec)), group_spec$key)
  for (gi in seq_len(nrow(group_spec))) {
    gcol <- group_spec$col[gi]
    group_masses[ group_spec$key[gi] ] <- sum(w_muni * muni[[gcol]], na.rm=TRUE)
  }

  geom_linke <- rebuild_capacity_from_mesh_linke(inter_sf, muni, group_spec, w_muni, E_data)

  # ---------- Baseline Linke-only problem ----------
  fit_star <- solve_linke_only(geom_linke$B_LINKE_list,
                               share_linke,
                               add_constraints_fn = NULL)

  if (!(fit_star$status %in% c("optimal","optimal_inaccurate"))) {
    message(sprintf("[STATE %02d, year %d] Solver status (baseline): %s", STATE, YEAR, fit_star$status))
    return(FALSE)
  }

  t_bench <- fit_star$mae
  message(sprintf("[STATE %02d, year %d] Baseline optimal; municipal Linke MAE = %.5f", STATE, YEAR, t_bench))

  # ---------- Save baseline fit ----------
  state_names <- c("1"="Schleswig-Holstein","2"="Hamburg","3"="Niedersachsen","4"="Bremen",
                   "5"="Nordrhein-Westfalen","6"="Hessen","7"="Rheinland-Pfalz","8"="Baden-Württemberg",
                   "9"="Bayern","10"="Saarland","11"="Berlin","12"="Brandenburg",
                   "13"="Mecklenburg-Vorpommern","14"="Sachsen","15"="Sachsen-Anhalt","16"="Thüringen")
  state_str <- state_names[as.character(STATE)]
  if (is.na(state_str)) state_str <- sprintf("State %s", STATE)

  out_rds <- sprintf("fits_linke_only/linke_only_state%02d_year%d_seed%02d.rds", STATE, YEAR, seed_idx)
  saveRDS(list(
    state_id      = STATE,
    state_name    = state_str,
    year          = YEAR,
    Pi_vals       = fit_star$Pi_vals,
    residuals     = fit_star$residuals,
    mae           = fit_star$mae,
    inter_sf      = inter_sf,
    muni          = muni,
    group_masses  = group_masses
  ), file = out_rds)
  message(sprintf("[BASELINE SAVED] State %02d, year %d → %s", STATE, YEAR, out_rds))

  # ---------- Counterfactual Linke-median-based positions ----------
  # Collect Linke positions (candidate 5) across all elections
  linke_xy <- do.call(rbind, lapply(C_list, function(M) M[5, , drop = FALSE]))
  med_x    <- stats::median(linke_xy[, 1])
  med_y    <- stats::median(linke_xy[, 2])

  # Two counterfactual parties placed along the (1,1) direction from the median:
  #   BelowNeg45 at (med_x - 1, med_y - 1)  → wins voters where x + y < med_x + med_y
  #   AboveNeg45 at (med_x + 1, med_y + 1)  → wins voters where x + y > med_x + med_y
  # The Voronoi boundary is the -45-degree line: x + y = med_x + med_y.
  C_cf <- rbind(
    c(med_x - 1, med_y - 1),  # BelowNeg45
    c(med_x + 1, med_y + 1)   # AboveNeg45
  )

  # Bounding box for counterfactual Voronoi
  xy_cf     <- rbind(xy_obs, C_cf)
  bounds_cf <- c(xmin=min(xy_cf[,1]) - VOR_PAD,
                 ymin=min(xy_cf[,2]) - VOR_PAD,
                 xmax=max(xy_cf[,1]) + VOR_PAD,
                 ymax=max(xy_cf[,2]) + VOR_PAD)

  # ---------- two-party masks ----------
  masks <- two_party_masks(inter_sf, bounds_cf, C_cf)
  diag_counts <- data.frame(
    region  = names(masks),
    n_cells = vapply(masks, sum, numeric(1))
  )
  diag_file <- sprintf("diagnostics/linke_only_two_party_cell_counts_state%02d_year%d_seed%02d.csv", STATE, YEAR, seed_idx)
  write.table(diag_counts, file=diag_file,
              sep=",", row.names=FALSE, col.names=TRUE)
  message(sprintf("[DIAG] two-party (BelowNeg45/AboveNeg45, Linke-median-based) cell counts written to %s", diag_file))

  # ---------- Single META-GROUP for bounds: ALL groups ----------
  meta_groups <- list(
    all = group_spec$key
  )
  meta_pretty <- c(
    all = "All groups (combined)"
  )

  make_region_constraint_meta <- function(rows_linke, meta_keys, region_mask,
                                          bound_type=c("upper","lower"), b=0) {
    bound_type <- match.arg(bound_type)
    function(env) {
      mass <- sum(group_masses[meta_keys])
      if (!is.finite(mass) || mass <= 0) return(list())

      lhs_list <- lapply(meta_keys, function(gk) {
        row_vec <- rows_linke[[gk]] * as.numeric(region_mask)
        t(matrix(row_vec, ncol=1)) %*% env$Pi[[gk]]
      })
      lhs <- Reduce(`+`, lhs_list)

      if (bound_type=="upper") {
        list(lhs >= b * mass)
      } else {
        list(lhs <= b * mass)
      }
    }
  }

  print_check_meta <- function(meta_name, region_label, tag, b, fit, prefix="") {
    delta <- fit$mae - t_bench
    ok_solver <- fit$status %in% c("optimal","optimal_inaccurate")
    ok_bench  <- !is.na(fit$mae) && bench_ok(fit$mae, t_bench)
    cat(sprintf("%s[%s | %s | %s] b=%.4f → status=%s, MAE=%.5f, Δ=%.3g → %s\n",
                prefix, meta_pretty[meta_name], region_label, tag, b,
                fit$status, fit$mae, delta,
                if (ok_solver && ok_bench) "FEASIBLE" else "infeasible"))
    ok_solver && ok_bench
  }

  bisect_meta <- function(meta_name, meta_keys, region_mask, region_label,
                          bound_type = c("upper","lower"),
                          lo_init = 0, hi_init = 0.5,
                          tol = BISECT_TOL, max_iter = MAX_BISECT_IT) {
    bound_type <- match.arg(bound_type)
    lo <- as.numeric(lo_init); hi <- as.numeric(hi_init)
    cat(sprintf("\n>>> %s | ABS Linke mass choosing %s (within meta-group) — %s bound\n",
                meta_pretty[meta_name], region_label, bound_type))
    cat(sprintf("  START bracket: lo=%.4f, hi=%.4f\n", lo, hi))

    cons_lo <- make_region_constraint_meta(geom_linke$rows_linke, meta_keys, region_mask, bound_type, lo)
    cons_hi <- make_region_constraint_meta(geom_linke$rows_linke, meta_keys, region_mask, bound_type, hi)

    fit_lo <- solve_linke_only(geom_linke$B_LINKE_list, share_linke,
                               add_constraints_fn = cons_lo)
    feas_lo <- print_check_meta(meta_name, region_label, paste0(bound_type,"/lo"), lo, fit_lo, prefix="  ")

    fit_hi <- solve_linke_only(geom_linke$B_LINKE_list, share_linke,
                               add_constraints_fn = cons_hi)
    feas_hi <- print_check_meta(meta_name, region_label, paste0(bound_type,"/hi"), hi, fit_hi, prefix="  ")

    if (!feas_lo && !feas_hi) {
      cat("  infeasible at both endpoints → NA\n")
      return(NA_real_)
    }
    if (bound_type == "upper" && feas_hi) {
      cat("  b=0.5 feasible → upper_abs=0.5000\n")
      return(0.5)
    }
    if (bound_type == "lower" && feas_lo) {
      cat("  b=0 feasible → lower_abs=0.0000\n")
      return(0.0)
    }

    iters <- 0L
    while ((hi - lo) > tol && iters < max_iter) {
      mid <- 0.5 * (lo + hi)
      add_mid <- make_region_constraint_meta(geom_linke$rows_linke, meta_keys, region_mask, bound_type, mid)
      fit_mid <- solve_linke_only(geom_linke$B_LINKE_list, share_linke,
                                  add_constraints_fn = add_mid)
      feasible <- fit_mid$status %in% c("optimal","optimal_inaccurate") &&
        bench_ok(fit_mid$mae, t_bench)
      cat(sprintf("  iter=%02d | lo=%.4f mid=%.4f hi=%.4f | %s\n",
                  iters, lo, mid, hi, if (feasible) "FEAS" else "INF"))
      if (bound_type == "upper") {
        if (feasible) lo <- mid else hi <- mid
      } else {
        if (feasible) hi <- mid else lo <- mid
      }
      iters <- iters + 1L
    }

    if (bound_type == "upper") {
      cat(sprintf("  result upper_abs=%.4f (iters=%d, bracket=[%.4f, %.4f])\n",
                  lo, iters, lo, hi))
      return(lo)
    } else {
      cat(sprintf("  result lower_abs=%.4f (iters=%d, bracket=[%.4f, %.4f])\n",
                  hi, iters, lo, hi))
      return(hi)
    }
  }

  # ---------- run BelowNeg45 / AboveNeg45 bounds for meta-group "all" ----------
  out_list <- list()
  for (meta_name in names(meta_groups)) {
    meta_keys <- meta_groups[[meta_name]]
    mass_meta <- sum(group_masses[meta_keys])
    for (reg in names(masks)) {
      region_mask <- masks[[reg]]
      if (!is.finite(mass_meta) || mass_meta <= 0) {
        ub <- NA_real_; lb <- NA_real_
      } else {
        ub <- bisect_meta(meta_name, meta_keys, region_mask, reg, "upper",
                          lo_init = 0, hi_init = 0.5)
        hi_lb <- if (is.na(ub)) 0.5 else min(0.5, max(0, ub + 0.05))
        if (hi_lb <= 0) hi_lb <- BISECT_TOL
        lb <- bisect_meta(meta_name, meta_keys, region_mask, reg, "lower",
                          lo_init = 0, hi_init = hi_lb)
      }
      out_list[[length(out_list)+1L]] <- data.frame(
        state_id    = STATE,
        state_name  = state_str,
        year        = YEAR,
        east_west   = ifelse(STATE %in% 12:16, "east", "west"),
        group_key   = meta_name,
        group_pretty= meta_pretty[meta_name],
        region      = reg,       # "BelowNeg45" or "AboveNeg45"
        party       = 5,         # Linke voters
        abs_lower   = lb,
        abs_upper   = ub,
        baseline_mae= t_bench,
        stringsAsFactors = FALSE
      )
    }
  }
  summary_bounds <- dplyr::bind_rows(out_list)

  # ---------- Save bounds ----------
  out_csv <- sprintf("bounds/linke_bounds_year%d_seed%02d.csv", YEAR, seed_idx)
  write.table(summary_bounds,
              file = out_csv, sep = ",",
              row.names = FALSE,
              col.names = !file.exists(out_csv),
              append = file.exists(out_csv))
  message(sprintf("[BOUNDS SAVED] State %02d, year %d, seed_idx %d → appended to %s", STATE, YEAR, seed_idx, out_csv))

  TRUE
}

# ----------------- Outer loop: east/west regions for the chosen YEAR -----------------

REGIONS <- list(
  east = 12:16,
  west = 1:11
)

source(sprintf("../../targets/RW_%d_6groups.R",         YEAR))
source(sprintf("../../targets/RW_%d_6groups_turnout.R", YEAR))

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
