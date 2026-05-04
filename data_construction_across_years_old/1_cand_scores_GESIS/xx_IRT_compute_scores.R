## ============================================================
##  IRT-based candidate ideal points (drop-in replacement)
##  - Uses graded response IRT (mirt) separately for:
##      (1) Economic Left–Right  (econ_lr_score)
##      (2) Anti-elite           (anti_elite_score)
##  - Keeps your item direction/reverse-coding exactly as before
##  - Keeps your imputation-by-state + party-shrinker pipeline
## ============================================================

## --- Working directory boilerplate (safe in/out of RStudio) ---
#if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
#  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#}
getwd()

rm(list = ls())
set.seed(42)

## ======= Setup (reuse if already loaded) =======
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

## --- IRT package (install if missing) ---
if (!requireNamespace("mirt", quietly = TRUE)) install.packages("mirt")
suppressPackageStartupMessages(library(mirt))

## ======= Helpers (same as before) =======
to_num <- function(x) {
  if (inherits(x, c("haven_labelled","labelled","labelled_spss"))) {
    x <- haven::as_factor(x, levels = "values")
  }
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}
zstd <- function(x) as.numeric(scale(x))
rev_scale <- function(x, lo, hi) { x <- to_num(x); ifelse(is.finite(x), (lo + hi) - x, NA_real_) }
validate_range <- function(x, lo, hi) { x <- to_num(x); x[!(x >= lo & x <= hi)] <- NA_real_; x }

## Make sure processed items are integers (mirt likes 1..K integers for ordinal items)
as_irt_int <- function(x) {
  x <- to_num(x)
  out <- ifelse(is.na(x), NA_integer_, as.integer(round(x)))
  out
}

party_lut_codes <- tibble::tribble(
  ~orig, ~party_id,
  2  ,   1,  # CDU
  3  ,   1,  # CSU -> CDU bucket
  4  ,   2,  # SPD
  6  ,   3,  # GRUENE
  5  ,   4,  # FDP
  7  ,   5,  # LINKE
  322,  6   # AfD
)

## ======= Party shrinker (unchanged) =======
shrink_to_party <- function(df_scored,
                            lut,
                            max_dev     = 0.15,
                            lambda_with = 0.75,
                            clamp_min   = -1.5,
                            clamp_max   =  1.5) {
  stopifnot(all(c("wknr","a1_or_partei") %in% names(df_scored)))
  out <- df_scored %>%
    inner_join(lut, by = c("a1_or_partei" = "orig")) %>%
    filter(!is.na(party_id))
  
  meds <- out %>%
    group_by(party_id) %>%
    summarise(
      econ_med  = median(econ_lr_score,    na.rm = TRUE),
      elite_med = median(anti_elite_score, na.rm = TRUE),
      .groups = "drop"
    )
  out <- out %>% left_join(meds, by = "party_id")
  
  shrink_axis <- function(val, med) {
    dev <- val - med
    adj <- ifelse(abs(dev) <= max_dev,
                  med + lambda_with * dev,
                  med + sign(dev) * max_dev)
    pmin(pmax(adj, clamp_min), clamp_max)
  }
  
  out$econ_lr_score    <- shrink_axis(out$econ_lr_score,    out$econ_med)
  out$anti_elite_score <- shrink_axis(out$anti_elite_score, out$elite_med)
  
  out %>% select(-econ_med, -elite_med)
}

## ============================================================
##                 IRT scoring helpers
## ============================================================

## Fit 1D graded response IRT and return person scores (theta)
## - Drops items that have <2 observed categories (cannot be estimated)
## - Fits on respondents with >=1 response, but scores ALL rows via response.pattern
## - Ensures sign/orientation: theta should correlate positively with average raw response
irt_theta_1d <- function(items_df,
                         itemtype = "graded",
                         fs_method = "EAP",
                         min_items_answered = 2,
                         technical = list(NCYCLES = 10000),
                         verbose = FALSE) {
  
  items_df <- as.data.frame(items_df)
  
  ## ensure integer categories
  items_df <- as.data.frame(lapply(items_df, as_irt_int))
  
  ## drop items with too little variation
  keep_item <- vapply(items_df, function(x) {
    ux <- unique(x[!is.na(x)])
    length(ux) >= 2
  }, logical(1))
  items_use <- items_df[, keep_item, drop = FALSE]
  
  ## if nothing usable -> all NA
  if (ncol(items_use) == 0) {
    return(list(theta = rep(NA_real_, nrow(items_df)),
                model = NULL,
                items = character(0)))
  }
  
  ## if only 1 item usable -> fallback to z-standardized item (still a "position")
  if (ncol(items_use) == 1) {
    x <- items_use[[1]]
    theta <- zstd(x)
    answered <- rowSums(!is.na(items_use))
    theta[answered < min_items_answered] <- NA_real_
    return(list(theta = as.numeric(theta),
                model = NULL,
                items = names(items_use)))
  }
  
  ## fit on rows with any observed response
  fit_rows <- rowSums(!is.na(items_use)) > 0
  
  ## if too few rows to fit, fallback to z-averaged rowMeans (like before)
  if (sum(fit_rows) < 25) {
    theta <- rowMeans(as.data.frame(lapply(items_use, zstd)), na.rm = TRUE)
    answered <- rowSums(!is.na(items_use))
    theta[answered < min_items_answered] <- NA_real_
    return(list(theta = as.numeric(theta),
                model = NULL,
                items = names(items_use)))
  }
  
  mod <- tryCatch(
    mirt::mirt(items_use[fit_rows, , drop = FALSE],
               model    = 1,
               itemtype = itemtype,
               technical = technical,
               verbose  = verbose),
    error = function(e) NULL
  )
  
  ## if mirt fails, fallback to z-averaged rowMeans
  if (is.null(mod)) {
    theta <- rowMeans(as.data.frame(lapply(items_use, zstd)), na.rm = TRUE)
    answered <- rowSums(!is.na(items_use))
    theta[answered < min_items_answered] <- NA_real_
    return(list(theta = as.numeric(theta),
                model = NULL,
                items = names(items_use)))
  }
  
  ## score ALL rows (including those not used in fit) via response.pattern
  fs <- mirt::fscores(mod,
                      method = fs_method,
                      response.pattern = as.matrix(items_use),
                      full.scores.SE = FALSE)
  theta <- as.numeric(fs[, 1])
  
  ## enforce orientation: theta should increase with higher average item response
  proxy <- rowMeans(items_use, na.rm = TRUE)
  r <- suppressWarnings(cor(theta, proxy, use = "pairwise.complete.obs"))
  if (is.finite(r) && r < 0) theta <- -theta
  
  ## optionally blank out thetas with too few answered items
  answered <- rowSums(!is.na(items_use))
  theta[answered < min_items_answered] <- NA_real_
  
  list(theta = theta, model = mod, items = names(items_use))
}

## ============================================================
##        Scoring (2021) with IRT instead of z-averages
## ============================================================
compute_scores_2021_irt <- function(dat,
                                    itemtype = "graded",
                                    fs_method = "EAP",
                                    min_items_answered = 1,
                                    technical = list(NCYCLES = 2000),
                                    verbose = FALSE) {
  
  names(dat) <- tolower(names(dat))
  if (!"wknr" %in% names(dat) && !"wkname" %in% names(dat)) stop("2021: 'wknr' or 'wkname' missing")
  if (!"partei" %in% names(dat)) stop("2021: 'partei' missing")
  
  out <- dat
  
  ## ----- Elite items (same direction handling as before) -----
  elite_specs <- list(
    list(var="d7a", rev=TRUE,  lo=1, hi=5),
    list(var="d7b", rev=TRUE,  lo=1, hi=5),
    list(var="d7c", rev=TRUE,  lo=1, hi=5),
    list(var="d7g", rev=TRUE,  lo=1, hi=5),
    list(var="d7h", rev=FALSE, lo=1, hi=5),
    list(var="d7f", rev=FALSE, lo=1, hi=5),
    list(var="c23", rev=FALSE, lo=1, hi=4)
  )
  
  elite_use <- character(0)
  for (sp in elite_specs) {
    v <- sp$var
    if (v %in% names(out)) {
      tmp <- validate_range(out[[v]], sp$lo, sp$hi)
      tmp <- if (sp$rev) rev_scale(tmp, sp$lo, sp$hi) else to_num(tmp)
      tmp <- as_irt_int(tmp)
      newv <- paste0(v, "_ae")
      out[[newv]] <- tmp
      elite_use <- c(elite_use, newv)
    }
  }
  
  ## ----- Econ items (same direction handling as before) -----
  econ_specs <- list(
    list(var="c2b", rev=FALSE, lo=1, hi=5),
    list(var="c2h", rev=TRUE,  lo=1, hi=5),
    list(var="c2g", rev=TRUE,  lo=1, hi=5),
    list(var="c5",  rev=FALSE, lo=1, hi=11),
    list(var="c11", rev=TRUE,  lo=1, hi=11),
    
    ## extras you included
    list(var="c2d", rev=FALSE, lo=1, hi=5),
    list(var="c2i", rev=TRUE,  lo=1, hi=5),
    list(var="c2j", rev=TRUE,  lo=1, hi=5),
    list(var="c2a", rev=FALSE, lo=1, hi=5),
    list(var="c8",  rev=FALSE, lo=1, hi=11)
  )
  
  econ_use <- character(0)
  for (sp in econ_specs) {
    v <- sp$var
    if (v %in% names(out)) {
      tmp <- validate_range(out[[v]], sp$lo, sp$hi)
      tmp <- if (sp$rev) rev_scale(tmp, sp$lo, sp$hi) else to_num(tmp)
      tmp <- as_irt_int(tmp)
      newv <- paste0(v, "_ec")
      out[[newv]] <- tmp
      econ_use <- c(econ_use, newv)
    }
  }
  
  ## ----- Fit IRT and get theta scores -----
  elite_fit <- irt_theta_1d(out[elite_use],
                            itemtype = itemtype,
                            fs_method = fs_method,
                            min_items_answered = min_items_answered,
                            technical = technical,
                            verbose = verbose)
  
  econ_fit  <- irt_theta_1d(out[econ_use],
                            itemtype = itemtype,
                            fs_method = fs_method,
                            min_items_answered = min_items_answered,
                            technical = technical,
                            verbose = verbose)
  
  wknr_vec <- if ("wknr" %in% names(out)) out$wknr else out$wkname
  
  scores <- tibble(
    wknr             = to_num(wknr_vec),
    a1_or_partei     = to_num(out$partei),
    econ_lr_score    = as.numeric(econ_fit$theta),
    anti_elite_score = as.numeric(elite_fit$theta),
    bula             = to_num(out$bula)   # keep bula for your state-median imputation
  ) %>%
    filter(!is.na(wknr))
  
  ## return scores + models (so you can inspect item discrimination/thresholds)
  list(
    scores = scores,
    models = list(econ = econ_fit$model, elite = elite_fit$model),
    items  = list(econ = econ_fit$items, elite = elite_fit$items)
  )
}

## ============================================================
##                       Usage
## ============================================================

dir.create("data",   showWarnings = FALSE, recursive = TRUE)
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)

cand21 <- read_dta("candidates_2021.dta")
names(cand21) <- tolower(names(cand21))
cand21 <- cand21 %>%
  mutate(wknr = if ("wkname" %in% names(.)) wkname else wknr) %>%
  filter(!is.na(wknr) & to_num(wknr) > 0)

## ---- IRT scores (raw) ----
irt21 <- compute_scores_2021_irt(
  cand21,
  itemtype = "graded",      # graded response model for ordinal items
  fs_method = "EAP",        # EAP person scores (stable with missingness)
  min_items_answered = 2,   # set to 2 if you want to require >=2 answers
  technical = list(NCYCLES = 2000),
  verbose = FALSE
)

scores_2021_raw <- irt21$scores

## ---- Same NA-fix strategy you used before (per state, then overall) ----
scores_2021_not_shrinked <- scores_2021_raw %>%
  group_by(bula) %>%
  mutate(
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score, na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  ungroup() %>%
  mutate(
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score, na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  select(-bula)

## attach party_id (as you did)
scores_2021_not_shrinked <- scores_2021_not_shrinked %>%
  left_join(party_lut_codes, by = c("a1_or_partei" = "orig")) %>%
  filter(!is.na(party_id))

## ---- Shrinked version (unchanged method) ----
scores_2021 <- scores_2021_raw %>%
  group_by(bula) %>%
  mutate(
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score, na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  ungroup() %>%
  mutate(
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score, na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  select(-bula) %>%
  shrink_to_party(
    lut = party_lut_codes,
    max_dev = 0.5,
    lambda_with = 0.75,
    clamp_min = -1.5,
    clamp_max = 1.5
  )

## ---- Join back & plot (same as before) ----
party_labels <- c("1"="CDU/CSU","2"="SPD","3"="GRUENE","4"="FDP","5"="LINKE","6"="AfD")
party_colors <- c("CDU/CSU"="#000000","SPD"="#E3000F","GRUENE"="#46962B","FDP"="#FFED00","LINKE"="#BE3075","AfD"="#009EE0")

cand21_scored <- cand21 %>%
  mutate(a1_or_partei = to_num(partei)) %>%
  left_join(
    scores_2021_not_shrinked %>% select(wknr, a1_or_partei, econ_lr_score, anti_elite_score, party_id),
    by = c("wknr","a1_or_partei")
  ) %>%
  mutate(party = factor(party_id, levels = names(party_labels), labels = party_labels))

save(cand21_scored, file = "data/cand21_scored_LR_IRT.Rdata")

cand21_scored_plot <- ggplot(cand21_scored, aes(econ_lr_score, anti_elite_score, color = party)) +
  geom_point(size = 2, alpha = 0.8, na.rm = TRUE) +
  scale_color_manual(values = party_colors, name = "Party") +
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
  labs(x = "Economic Left–Right (IRT theta)", y = "Anti-Elite (IRT theta)") +
  theme_minimal(base_size = 14)

ggsave("graphs/plot_can_2021_LR_IRT.png", cand21_scored_plot, width = 7, height = 5, dpi = 300)
cand21_scored_plot
## ============================================================
## OPTIONAL: Inspect which questions have more “consensus / info”
## (High discrimination a-parameters => more informative items)
## ============================================================
if (!is.null(irt21$models$econ)) {
  econ_item_pars  <- mirt::coef(irt21$models$econ,  IRTpars = TRUE, simplify = TRUE)$items
  elite_item_pars <- mirt::coef(irt21$models$elite, IRTpars = TRUE, simplify = TRUE)$items
  
  print("=== ECON item parameters (a = discrimination) ===")
  print(econ_item_pars)
  
  print("=== ELITE item parameters (a = discrimination) ===")
  print(elite_item_pars)
  
  ## If you want item information curves:
  ## plot(irt21$models$econ,  type = "infotrace")
  ## plot(irt21$models$elite, type = "infotrace")
}


###### 
rm(list = ls())
load('data/cand21_scored_LR_IRT.Rdata')
cand21_scoredIRT <- cand21_scored
cand21_scored <- NULL
load('data/cand21_scored_LR.Rdata')

cor(cand21_scored$econ_lr_score, cand21_scoredIRT$econ_lr_score) #0.9606896
cor(cand21_scored$anti_elite_score, cand21_scoredIRT$anti_elite_score) #0.9613508



