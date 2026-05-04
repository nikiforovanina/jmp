# ===============================================================
# 2_region_bounds_afd_absolute_12groups_SCREDIRECT_SOFT.R
# - Observed-only Voronoi intersection (no alpha/valences)
# - Baseline L1 LP with age×gender targets → baseline MAE (t_bench)
# - Regions N, NE, E, SE (3x3 grid over cell centroids)
# - Compute ABSOLUTE bounds for AfD (party 6) share by region & group
# - NEW: Second-choice "redirect" for AfD voters → parties 4 & 5
#   * AfD totals per group are HARD (AFD_TOL=0): no mass disappears
#   * Shares to parties 4/5 among AfD-won&second-closest cells are
#     enforced SOFTLY: outside [p±tol] incurs a hinge penalty
#     added to the L1 objective (guarantees feasibility).
# ===============================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tidyr)
  library(CVXR); library(rlang); library(ggplot2)
})

set.seed(122223)

# -------------------- knobs --------------------
YEAR               <- 2013
STATES             <- 1:16
SUBSAMPLE_N        <- 20
MAX_ELECTIONS      <- 5
TARGET_TOL         <- 0.01   # ±1% of group mass for parties 1..5
AFD_TOL            <- 0.01   # AfD (party 6) tolerance as fraction of group mass (0 = exact)
ABS_EPS            <- 5e-6   # feasibility vs baseline
REL_EPS            <- 5e-6
MAX_BISECT_IT      <- 30
BISECT_TOL         <- 1e-3
RESULTS_MIN        <- 1e-5

# Second-choice redirection for AfD → parties 4 & 5 (soft)
APPLY_SECOND_CHOICE <- TRUE
P_SC45              <- c(0.08, 0.2)   # target fractions (party 4, party 5)
SC_TOL              <- 0.025            # ±5% band around those fractions
SC_PENALTY          <- 1e3             # weight for soft-constraint violations

dir.create("bounds", showWarnings = FALSE, recursive = TRUE)
dir.create("diagnostics", showWarnings = FALSE, recursive = TRUE)
dir.create("data", showWarnings = FALSE, recursive = TRUE)

# Targets provider: must define get_age_gender_targets(state_id)
source("RW_2013_6groups.R")

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
  ~key,              ~pretty,          ~col,                    ~tgt_name,
  "female_1825",     "Female 18–25",   "female_18_24_share",    "female_1825",
  "female_2535",     "Female 25–35",   "female_25_34_share",    "female_2535",
  "female_3545",     "Female 35–45",   "female_35_44_share",    "female_3545",
  "female_4560",     "Female 45–60",   "female_45_59_share",    "female_4560",
  "female_6070",     "Female 60–70",   "female_60_69_share",    "female_6070",
  "female_70plus",   "Female 70+",     "female_70plus_share",   "female_70plus",
  "male_1825",       "Male 18–25",     "male_18_24_share",      "male_1825",
  "male_2535",       "Male 25–35",     "male_25_34_share",      "male_2535",
  "male_3545",       "Male 35–45",     "male_35_44_share",      "male_3545",
  "male_4560",       "Male 45–60",     "male_45_59_share",      "male_4560",
  "male_6070",       "Male 60–70",     "male_60_69_share",      "male_6070",
  "male_70plus",     "Male 70+",       "male_70plus_share",     "male_70plus"
)
name_map <- setNames(as.character(group_spec$pretty), as.character(group_spec$key))
pretty_name <- function(gkey) { nm <- name_map[[as.character(gkey)]]; if (is.null(nm)) as.character(gkey) else nm }

PARTY_INDEX <- c("CDU"=1,"CSU"=1,"SPD"=2,"GRUENE"=3,"GRÜNE"=3,"GRUEN"=3,
                 "FDP"=4,"LINKE"=5,"PDS"=5,"DIE LINKE"=5,"AFD"=6,"AfD"=6)

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

get_targets_12 <- function(STATE) {
  if (!exists("get_age_gender_targets")) stop("get_age_gender_targets() not found; source() the targets file.")
  tg <- get_age_gender_targets(STATE)
  missing <- setdiff(group_spec$key, names(tg))
  if (length(missing)) stop("Targets missing: ", paste(missing, collapse=", "))
  out <- setNames(vector("list", nrow(group_spec)), group_spec$key)
  for (i in seq_len(nrow(group_spec))) out[[ group_spec$key[i] ]] <- normalize_target_vec(tg[[ group_spec$key[i] ]])
  out
}

# ------ geometry helpers ------
dist_alpha <- function(cand_xy, centers, alpha=1) {
  Dx <- outer(cand_xy[,1], centers[,1], `-`)
  Dy <- outer(cand_xy[,2], centers[,2], `-`)
  sqrt(Dx*Dx + (alpha*Dy)*(alpha*Dy))
}
A_from_hard <- function(D) {
  K <- nrow(D); nc <- ncol(D); win <- max.col(t(-D))
  A <- matrix(0L, nrow=K, ncol=nc); A[cbind(win, seq_len(nc))] <- 1L; A
}
second_closest_index <- function(D) {
  K <- nrow(D); nc <- ncol(D)
  win <- max.col(t(-D))
  D2  <- D; D2[cbind(win, seq_len(nc))] <- Inf
  sec <- max.col(t(-D2))
  list(winner = win, second = sec)
}

# ---------- main ----------
run_one <- function(STATE) {
  data_file <- sprintf("data/data_state%02d_year%d.RData", STATE, YEAR)
  if (!file.exists(data_file)) { message(sprintf("[SKIP] No data file for state %02d", STATE)); return(FALSE) }
  load(data_file)  # expects: results_std, pos_std
  if (!exists("results_std") || !nrow(results_std) || !exists("pos_std") || !nrow(pos_std)) {
    message(sprintf("[SKIP] Empty results/positions for state %02d", STATE)); return(FALSE)
  }
  
  message(sprintf("\n=======================\nState %02d | ABS region AfD bounds + SOFT SC redirect\n=======================\n", STATE))
  
  # --- candidate shares (rebuild if needed) ---
  cand_cols <- paste0("cand", 1:6, "_share")
  if (!all(cand_cols %in% names(results_std))) {
    src_cols <- c("cdu_norm","csu_norm","spd_norm","gruene_norm","fdp_norm","linke_pds_norm","afd_norm")
    if (!all(src_cols %in% names(results_std))) stop("Missing candX_share and no *_norm to rebuild.")
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
  
  # drop tiny rows, renormalize
  if (all(cand_cols %in% names(results_std))) {
    before_n <- nrow(results_std)
    results_std <- results_std %>% filter(if_all(all_of(cand_cols), ~ is.finite(.x) & .x >= RESULTS_MIN))
    removed_n <- before_n - nrow(results_std)
    message(sprintf("[CLEAN] Dropped %d rows with any result < %.5g", removed_n, RESULTS_MIN))
    if (!nrow(results_std)) { message("[SKIP] No rows remain after cleaning."); return(FALSE) }
  }
  rs <- rowSums(dplyr::select(results_std, all_of(cand_cols)), na.rm=TRUE)
  results_std <- results_std %>% mutate(across(all_of(cand_cols), ~ .x / ifelse(rs>0, rs, NA_real_)))
  
  # overlap & subsample
  results_std <- results_std %>% mutate(election_id = safe_int(election_id))
  pos_std     <- pos_std     %>% mutate(election_id = safe_int(election_id), cand=safe_int(cand))
  elections_both <- sort(intersect(unique(results_std$election_id), unique(pos_std$election_id)))
  if (!length(elections_both)) { message("[SKIP] No overlapping elections"); return(FALSE) }
  set.seed(120 + STATE)
  if (length(elections_both) > MAX_ELECTIONS) elections_both <- sort(sample(elections_both, MAX_ELECTIONS))
  id_map <- tibble(election_id=elections_both, e_idx=seq_along(elections_both))
  results_std <- results_std %>% semi_join(id_map, by="election_id") %>%
    inner_join(id_map, by="election_id") %>% mutate(election=e_idx) %>% select(-e_idx)
  pos_std     <- pos_std     %>% semi_join(id_map, by="election_id") %>%
    inner_join(id_map, by="election_id") %>% mutate(election=e_idx) %>% select(-e_idx)
  E_data <- nrow(id_map)
  
  set.seed(10200 + STATE*101)
  results_std <- results_std %>%
    group_by(election) %>%
    group_modify(~ { n <- nrow(.x); if (n<=SUBSAMPLE_N) .x else .x[sample.int(n, SUBSAMPLE_N), , drop=FALSE] }) %>%
    ungroup()
  
  # Demography
  need_cols <- group_spec$col
  miss <- setdiff(need_cols, names(results_std))
  if (length(miss)) { warning("Missing demo cols: ", paste(miss, collapse=", ")); results_std[miss] <- 0 }
  results_std$municipality <- safe_first_non_na(results_std, c("code_cleaned","ags","code"))
  muni <- results_std %>%
    transmute(election, municipality, across(all_of(group_spec$col)), across(all_of(cand_cols)),
              eligible_voters = coalesce(eligible_voters, 1))
  
  # Candidate positions (fill missing)
  K <- 6
  cand_means <- pos_std %>% group_by(cand) %>% summarise(mx=mean(x, na.rm=TRUE), my=mean(y, na.rm=TRUE), .groups="drop")
  pos_std_full <- pos_std %>% select(election_id, election, cand, x, y) %>%
    group_by(election_id, election) %>% tidyr::complete(cand=1:K) %>%
    ungroup() %>% left_join(cand_means, by="cand") %>%
    mutate(x=ifelse(is.na(x), mx, x), y=ifelse(is.na(y), my, y)) %>%
    select(election_id, e_idx=election, cand, x, y)
  ysplit <- split(pos_std_full, pos_std_full$e_idx)
  if (!all(vapply(ysplit, nrow, 1L) == K)) { message("[SKIP] some elections miss candidates"); return(FALSE) }
  
  # Voronoi intersection (observed elections)
  Y_list_obs <- lapply(seq_along(ysplit), function(i) {
    df <- ysplit[[i]][order(ysplit[[i]]$cand), ]
    data.frame(election = i, cand=factor(df$cand, levels=1:K), x=df$x, y=df$y)
  })
  xy_obs <- do.call(rbind, lapply(Y_list_obs, function(d) cbind(d$x, d$y)))
  pad  <- 0.05 * max(diff(range(xy_obs[,1])), diff(range(xy_obs[,2])))
  bounds <- c(xmin=min(xy_obs[,1])-pad, ymin=min(xy_obs[,2])-pad,
              xmax=max(xy_obs[,1])+pad, ymax=max(xy_obs[,2])+pad)
  mk_env <- function(b) {
    coords <- matrix(c(b["xmin"],b["ymin"], b["xmax"],b["ymin"], b["xmax"],b["ymax"],
                       b["xmin"],b["ymax"], b["xmin"],b["ymin"]), ncol=2, byrow=TRUE)
    sf::st_sfc(sf::st_polygon(list(coords)), crs = sf::NA_crs_)
  }
  sq <- mk_env(bounds)
  vor_wrap <- function(pts_sf, lab) {
    v <- st_voronoi(st_union(pts_sf), envelope=sq) |> st_collection_extract("POLYGON") |> st_sf()
    cen <- st_centroid(v); idx <- st_nearest_feature(cen, pts_sf); v[[lab]] <- as.character(idx); v[, lab, drop=FALSE]
  }
  vor_list_obs <- vector("list", E_data)
  for (e in seq_along(Y_list_obs)) {
    pts <- st_as_sf(Y_list_obs[[e]], coords=c("x","y"), crs=sf::NA_crs_)
    vor_list_obs[[e]] <- vor_wrap(pts, paste0("cand_e", e))
  }
  inter_sf <- vor_list_obs[[1]] %>% st_make_valid()
  for (e in 2:E_data) {
    nxt <- vor_list_obs[[e]] %>% st_make_valid()
    tmp <- suppressWarnings(st_intersection(inter_sf, nxt))
    tmp <- st_collection_extract(tmp, "POLYGON")
    tmp <- suppressWarnings(st_cast(tmp, "POLYGON", warn=FALSE))
    tmp <- tmp[!st_is_empty(tmp), ]
    if (nrow(tmp)>0) {
      a <- as.numeric(st_area(tmp)); tmp <- tmp[is.finite(a) & a>1e-12, ]
    }
    if (nrow(tmp)==0) { message(sprintf("[SKIP] Voronoi intersection collapsed at step e=%d", e)); return(FALSE) }
    inter_sf <- tmp
  }
  inter_sf <- st_sf(inter_sf) %>% mutate(cell_id = dplyr::row_number())
  geom_col    <- attr(inter_sf, "sf_column")
  cand_cols_e <- paste0("cand_e", 1:E_data)
  inter_sf <- inter_sf %>%
    dplyr::select(all_of(c("cell_id", cand_cols_e, geom_col))) %>%
    mutate(across(all_of(cand_cols_e), as.character),
           key = do.call(paste, c(dplyr::pick(all_of(cand_cols_e)), sep="-"))) %>%
    group_by(key, across(all_of(cand_cols_e))) %>%
    summarise(geometry = st_union(!!sym(geom_col)), .groups="drop")
  
  centers <- st_coordinates(st_centroid(inter_sf$geometry))
  nc      <- nrow(inter_sf)
  C_list  <- lapply(seq_len(E_data), function(e) as.matrix(ysplit[[e]][order(ysplit[[e]]$cand), c("x","y")]))
  dist_mats <- lapply(seq_len(E_data), function(e) dist_alpha(C_list[[e]], centers, 1))
  A_hard    <- lapply(seq_len(E_data), function(e) A_from_hard(dist_mats[[e]]))
  
  # ---------- Build linear system ----------
  R <- nrow(muni); KM <- K*R
  
  # B_g matrices (KM x nc)
  B_list <- vector("list", nrow(group_spec)); names(B_list) <- group_spec$key
  for (gi in seq_len(nrow(group_spec))) {
    B <- matrix(0, KM, nc); gcol <- group_spec$col[gi]
    for (r in seq_len(R)) {
      e <- muni$election[r]; Ae <- A_hard[[e]]; rows <- ((r-1L)*K + 1L):(r*K)
      B[rows, ] <- muni[[gcol]][r] * Ae
    }
    B_list[[ group_spec$key[gi] ]] <- B
  }
  # observed stack s
  s <- numeric(KM)
  for (r in seq_len(R)) { rows <- ((r-1L)*K + 1L):(r*K); s[rows] <- as.numeric(muni[r, paste0("cand",1:6,"_share")]) }
  
  # rows_precomp: per (group, party) row vectors length nc
  w_muni <- if ("eligible_voters" %in% names(muni)) muni$eligible_voters else rep(1, R)
  rows_precomp <- vector("list", nrow(group_spec)); names(rows_precomp) <- group_spec$key
  for (gi in seq_len(nrow(group_spec))) {
    gcol <- group_spec$col[gi]
    Mgj <- vector("list", K)
    for (j in seq_len(K)) {
      acc <- numeric(nc)
      for (r in seq_len(R)) {
        e  <- muni$election[r]; Ae <- A_hard[[e]]
        acc <- acc + (w_muni[r] * muni[[gcol]][r]) * Ae[j, ]
      }
      Mgj[[j]] <- acc
    }
    rows_precomp[[ group_spec$key[gi] ]] <- Mgj
  }
  
  # ---------- Second-closest masks (per election) ----------
  sc_masks <- vector("list", E_data)
  for (e in seq_len(E_data)) {
    De <- dist_mats[[e]]
    ids <- second_closest_index(De)
    win <- ids$winner
    sec <- ids$second
    mask_afd <- as.integer(win == 6L)
    sc_masks[[e]] <- lapply(1:5, function(j) as.numeric(mask_afd * as.integer(sec == j)))
    names(sc_masks[[e]]) <- as.character(1:5)
  }
  # rows_sc[g][[j]]: length-nc vector accumulating AfD-won cells with second=j
  rows_sc <- vector("list", nrow(group_spec)); names(rows_sc) <- group_spec$key
  for (gi in seq_len(nrow(group_spec))) {
    gcol <- group_spec$col[gi]
    SC <- vector("list", 5)
    for (j in 1:5) {
      acc <- numeric(nc)
      for (r in seq_len(R)) {
        e <- muni$election[r]
        acc <- acc + (w_muni[r] * muni[[gcol]][r]) * sc_masks[[e]][[as.character(j)]]
      }
      SC[[j]] <- acc
    }
    names(SC) <- as.character(1:5)
    rows_sc[[ group_spec$key[gi] ]] <- SC
  }
  
  # targets & group masses
  tg_list <- get_targets_12(STATE)
  group_masses <- setNames(numeric(nrow(group_spec)), group_spec$key)
  for (gi in seq_len(nrow(group_spec))) {
    gcol <- group_spec$col[gi]
    group_masses[ group_spec$key[gi] ] <- sum(w_muni * muni[[gcol]], na.rm=TRUE)
  }
  
  # ---------- LP solver (targets built-in + SOFT SC redirect) ----------
  solve_L1 <- function(add_constraints_fn=NULL) {
    Pi <- lapply(seq_len(nrow(group_spec)), function(i) CVXR::Variable(nc)); names(Pi) <- group_spec$key
    u  <- CVXR::Variable(KM)
    
    residual <- 0
    for (gk in names(B_list)) residual <- residual + B_list[[gk]] %*% Pi[[gk]]
    residual <- residual - s
    
    cons <- list()
    for (gk in names(B_list)) cons <- c(cons, list(Pi[[gk]] >= 0, sum_entries(Pi[[gk]]) == 1))
    cons <- c(cons, list(residual <= u, -residual <= u, u >= 0))
    
    # party-moment targets; AfD tightened via AFD_TOL (0 => equality)
    for (gk in names(rows_precomp)) {
      Mgj <- rows_precomp[[gk]]
      tgt <- tg_list[[gk]]
      mass <- group_masses[[gk]]
      for (j in seq_len(K)) {
        row <- matrix(Mgj[[j]], ncol = 1)
        lhs <- t(row) %*% Pi[[gk]]
        tol_abs <- (if (j==6) AFD_TOL else TARGET_TOL) * mass
        cons <- c(cons, list(lhs <= tgt[j]*mass + tol_abs),
                  list(lhs >= tgt[j]*mass - tol_abs))
      }
    }
    
    # --- SOFT second-choice constraints (hinge loss outside [lo, hi]) ---
    sc_penalty_terms <- list()
    if (APPLY_SECOND_CHOICE) {
      for (gk in names(rows_sc)) {
        # AfD mass (statewide) for this group
        M6 <- matrix(rows_precomp[[gk]][[6]], ncol=1)
        Tg <- t(M6) %*% Pi[[gk]]
        # If AfD mass is (numerically) zero for the group, skip (avoids 0*band)
        cons <- c(cons, list(Tg >= 0))
        
        # party 4 soft band
        R4  <- t(matrix(rows_sc[[gk]][[4]], ncol=1)) %*% Pi[[gk]]
        lo4 <- max(0, P_SC45[1] - SC_TOL); hi4 <- min(1, P_SC45[1] + SC_TOL)
        s4_lo <- CVXR::Variable(1)  # shortfall below lo4*Tg
        s4_hi <- CVXR::Variable(1)  # excess above hi4*Tg
        cons <- c(cons,
                  list(R4 + s4_lo >= lo4 * Tg,
                       R4 - s4_hi <= hi4 * Tg,
                       s4_lo >= 0, s4_hi >= 0))
        sc_penalty_terms[[length(sc_penalty_terms)+1]] <- (s4_lo + s4_hi)
        
        # party 5 soft band
        R5  <- t(matrix(rows_sc[[gk]][[5]], ncol=1)) %*% Pi[[gk]]
        lo5 <- max(0, P_SC45[2] - SC_TOL); hi5 <- min(1, P_SC45[2] + SC_TOL)
        s5_lo <- CVXR::Variable(1)
        s5_hi <- CVXR::Variable(1)
        cons <- c(cons,
                  list(R5 + s5_lo >= lo5 * Tg,
                       R5 - s5_hi <= hi5 * Tg,
                       s5_lo >= 0, s5_hi >= 0))
        sc_penalty_terms[[length(sc_penalty_terms)+1]] <- (s5_lo + s5_hi)
      }
    }
    
    # extra user constraints (e.g., region bound)
    if (!is.null(add_constraints_fn)) {
      env <- list(Pi=Pi, rows_precomp=rows_precomp, rows_sc=rows_sc,
                  tg_list=tg_list, masses=group_masses, target_tol=TARGET_TOL)
      cons <- c(cons, add_constraints_fn(env))
    }
    
    # Objective: L1 fit + soft SC penalties
    obj <- sum_entries(u)
    if (length(sc_penalty_terms) > 0) {
      obj <- obj + SC_PENALTY * Reduce(`+`, sc_penalty_terms)
    }
    prob <- Problem(Minimize(obj), cons)
    
    res  <- try(CVXR::solve(prob, solver="ECOS", verbose=FALSE,
                            feastol=1e-8, reltol=1e-7, abstol=1e-7, maxit=20000),
                silent=TRUE)
    if (inherits(res, "try-error") || is.null(res$status)) return(list(status="solver_error"))
    if (!(res$status %in% c("optimal","optimal_inaccurate"))) return(list(status=res$status))
    residual_vals <- as.numeric(res$getValue(residual))
    Pi_vals <- lapply(Pi, function(v) as.numeric(res$getValue(v)))
    fitted  <- Reduce(`+`, Map(function(Bg, key) Bg %*% matrix(Pi_vals[[key]], ncol=1), B_list, names(B_list)))
    list(status=res$status,
         ratio = mean(abs(residual_vals)),
         Pi_vals=Pi_vals,
         residual_vals=residual_vals,
         fitted_stack=as.numeric(fitted))
  }
  
  # ---------- baseline fit ----------
  fit_base <- solve_L1()
  if (!(fit_base$status %in% c("optimal","optimal_inaccurate"))) {
    message(sprintf("[SKIP] Baseline LP not optimal for state %02d (status=%s)", STATE, fit_base$status)); return(FALSE)
  }
  t_bench <- fit_base$ratio
  R <- nrow(muni)
  message(sprintf("State %02d: Baseline MAE = %.5f (TARGET_TOL=%.3f; AFD_TOL=%.3f; R=%d, nc=%d, E=%d)",
                  STATE, t_bench, TARGET_TOL, AFD_TOL, R, nc, E_data))
  
  # ---------- regions (N, NE, E, SE) ----------
  ctr <- st_coordinates(st_centroid(inter_sf$geometry))
  xs <- ctr[,1]; ys <- ctr[,2]
  x_min <- min(xs); x_max <- max(xs); dx <- x_max - x_min
  y_min <- min(ys); y_max <- max(ys); dy <- y_max - y_min
  x1 <- x_min + dx/3; x2 <- x_min + 2*dx/3
  y1 <- y_min + dy/3; y2 <- y_min + 2*dy/3
  
  xbin_W <- xs <= x1; xbin_C <- xs > x1 & xs <= x2; xbin_E <- xs > x2
  ybin_S <- ys <= y1; ybin_M <- ys > y1 & ys <= y2; ybin_N <- ys > y2
  
  mask_N  <- as.numeric(ybin_N & xbin_C)
  mask_NE <- as.numeric(ybin_N & xbin_E)
  mask_E  <- as.numeric(ybin_M & xbin_E)
  mask_SE <- as.numeric(ybin_S & xbin_E)
  
  masks <- list(N=mask_N, NE=mask_NE, E=mask_E, SE=mask_SE)
  
  # ---------- region constraints ----------
  afd <- 6L
  # Upper bound: s_{g,R} >= b   (push b up)   → use ">="
  # Lower bound: s_{g,R} <= b   (push b down) → use "<="
  make_region_constraint <- function(gkey, region_mask, bound_type=c("upper","lower"), b=0) {
    bound_type <- match.arg(bound_type)
    function(env) {
      mass <- env$masses[[gkey]]
      if (!is.finite(mass) || mass <= 0) return(list())
      row_vec <- env$rows_precomp[[gkey]][[afd]] * as.numeric(region_mask)
      lhs <- t(matrix(row_vec, ncol=1)) %*% env$Pi[[gkey]]
      if (bound_type=="upper") list(lhs >= b * mass) else list(lhs <= b * mass)
    }
  }
  
  bench_ok <- function(mae, t_bench) mae <= t_bench + max(ABS_EPS, REL_EPS * t_bench)
  print_check <- function(gkey, region_label, tag, b, fit, prefix="") {
    delta <- fit$ratio - t_bench
    ok_solver <- fit$status %in% c("optimal","optimal_inaccurate")
    ok_bench  <- !is.na(fit$ratio) && bench_ok(fit$ratio, t_bench)
    cat(sprintf("%s[%s | %s | %s] b=%.4f → status=%s, MAE=%.5f, Δ=%.3g → %s\n",
                prefix, pretty_name(gkey), region_label, tag, b, fit$status, fit$ratio, delta,
                if (ok_solver && ok_bench) "FEASIBLE" else "infeasible"))
    ok_solver && ok_bench
  }
  
  bisect_abs <- function(gkey, region_mask, region_label,
                         bound_type=c("upper","lower"),
                         lo_init=0, hi_init=1,
                         tol=BISECT_TOL, max_iter=MAX_BISECT_IT) {
    bound_type <- match.arg(bound_type)
    lo <- as.numeric(lo_init); hi <- as.numeric(hi_init)
    cat(sprintf("\n>>> %s | ABS AfD share in %s (%s)\n",
                pretty_name(gkey), region_label, bound_type))
    cat(sprintf("  START bracket (requested): lo=%.4f, hi=%.4f\n", lo, hi))
    if (!(is.finite(lo) && is.finite(hi) && lo == 0 && hi == 1)) {
      stop(sprintf("[BUG GUARD] bisect_abs starting bracket mutated: got [%.6f, %.6f], expected [0, 1].",
                   lo, hi))
    }
    
    fit_lo <- solve_L1(add_constraints_fn = make_region_constraint(gkey, region_mask, bound_type, lo))
    feas_lo <- print_check(gkey, region_label, paste0(bound_type,"/check"), lo, fit_lo, prefix="  ")
    fit_hi <- solve_L1(add_constraints_fn = make_region_constraint(gkey, region_mask, bound_type, hi))
    feas_hi <- print_check(gkey, region_label, paste0(bound_type,"/check"), hi, fit_hi, prefix="  ")
    
    if (bound_type == "upper") {
      if (!feas_lo && !feas_hi) { cat("  infeasible at both endpoints → NA\n"); return(NA_real_) }
      if (feas_hi) { cat("  b=1 feasible → result upper_abs=1.0000 (snap)\n"); return(1) }
    } else {
      if (!feas_lo && !feas_hi) { cat("  infeasible at both endpoints → NA\n"); return(NA_real_) }
      if (feas_lo) { cat("  b=0 feasible → result lower_abs=0.0000 (snap)\n"); return(0) }
    }
    
    iters <- 0L
    while ((hi - lo) > tol && iters < max_iter) {
      mid <- 0.5 * (lo + hi)
      cat(sprintf("  iter=%02d | lo=%.4f mid=%.4f hi=%.4f\n", iters, lo, mid, hi))
      fit_mid <- solve_L1(add_constraints_fn = make_region_constraint(gkey, region_mask, bound_type, mid))
      feasible <- print_check(gkey, region_label, bound_type, mid, fit_mid, prefix="    ")
      if (bound_type == "upper") { if (feasible) lo <- mid else hi <- mid }
      else                        { if (feasible) hi <- mid else lo <- mid }
      iters <- iters + 1L
    }
    
    if (bound_type == "upper") {
      cat(sprintf("  result upper_abs=%.4f (iters=%d, bracket=[%.4f, %.4f])\n", lo, iters, lo, hi))
      return(lo)
    } else {
      cat(sprintf("  result lower_abs=%.4f (iters=%d, bracket=[%.4f, %.4f])\n", hi, iters, lo, hi))
      return(hi)
    }
  }
  
  # ---------- run bounds ----------
  out_list <- list()
  for (gkey in group_spec$key) {
    mass_g <- group_masses[[as.character(gkey)]]
    for (reg in names(masks)) {
      region_mask <- masks[[reg]]
      if (!is.finite(mass_g) || mass_g <= 0) {
        ub <- NA_real_; lb <- NA_real_
      } else {
        ub <- bisect_abs(gkey, region_mask, reg, "upper")  # s >= b
        lb <- bisect_abs(gkey, region_mask, reg, "lower")  # s <= b
      }
      out_list[[length(out_list)+1]] <- data.frame(
        state_id = STATE,
        group_key = gkey,
        group_pretty = pretty_name(gkey),
        region = reg,
        party = 6,
        abs_lower = lb,
        abs_upper = ub,
        stringsAsFactors = FALSE
      )
    }
  }
  summary_bounds <- bind_rows(out_list)
  
  # ---------- Save ----------
  state_names <- c("1"="Schleswig-Holstein","2"="Hamburg","3"="Niedersachsen","4"="Bremen",
                   "5"="Nordrhein-Westfalen","6"="Hessen","7"="Rheinland-Pfalz","8"="Baden-Württemberg",
                   "9"="Bayern","10"="Saarland","11"="Berlin","12"="Brandenburg",
                   "13"="Mecklenburg-Vorpommern","14"="Sachsen","15"="Sachsen-Anhalt","16"="Thüringen")
  state_str <- state_names[as.character(STATE)]; if (is.na(state_str)) state_str <- sprintf("State %s", STATE)
  
  out_csv <- sprintf("bounds/bounds_state%02d_year%d_mode6.csv", STATE, YEAR)
  write.table(
    summary_bounds %>%
      mutate(state_name = state_str, year = YEAR,
             target_tol = TARGET_TOL, afd_tol = AFD_TOL,
             sc_apply = APPLY_SECOND_CHOICE, sc_p4 = P_SC45[1], sc_p5 = P_SC45[2],
             sc_tol = SC_TOL, sc_penalty = SC_PENALTY) %>%
      select(state_id, state_name, year, target_tol, afd_tol,
             group_key, group_pretty, region, party, abs_lower, abs_upper,
             sc_apply, sc_p4, sc_p5, sc_tol, sc_penalty),
    file = out_csv, sep=",", row.names=FALSE, col.names=!file.exists(out_csv), append=TRUE
  )
  
  message(sprintf("[DONE] State %02d → wrote %s", STATE, out_csv))
  TRUE
}

# -----------------
# Outer loop
# -----------------
for (S in STATES) {
  try(run_one(S), silent = FALSE)
}