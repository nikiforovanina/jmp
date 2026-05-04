## ======= Setup (reuse if already loaded) =======
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

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

## ======= Party shrinker (same as before) =======
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

## ======= Scoring (2021) =======
# ELITE
# d7a,d7b,d7c,d7g: REVERSE, 1–5
# d7h,d7f: NOT reverse, 1–5
# c23: NOT reverse, 1–4
#
# ECON
# c2b:  NOT reverse, 1–5
# c2h:  REVERSE,     1–5
# c2g:  REVERSE,     1–5
# c5:   NOT reverse, 1–11
# c11:  REVERSE,     1–11  (Steuern vs. Sozialleistungen)

compute_scores_2021 <- function(dat) {
  names(dat) <- tolower(names(dat))
  if (!"wknr" %in% names(dat) && !"wkname" %in% names(dat)) stop("2021: 'wknr' or 'wkname' missing")
  if (!"partei" %in% names(dat)) stop("2021: 'partei' missing")
  
  out <- dat
  n <- nrow(out)
  
  # --- Elite items ---
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
      out[[paste0(v,"_ae")]] <- tmp
      elite_use <- c(elite_use, paste0(v,"_ae"))
    }
  }
  
  anti_elite_score <- if (length(elite_use)) {
    Z <- as.data.frame(lapply(out[elite_use], zstd)); rowMeans(Z, na.rm = TRUE)
  } else rep(NA_real_, n)
  
  # --- Econ items ---
  econ_specs <- list(
    list(var="c2b", rev=FALSE, lo=1, hi=5),
    list(var="c2h", rev=TRUE,  lo=1, hi=5),
    list(var="c2g", rev=TRUE,  lo=1, hi=5),
    list(var="c5",  rev=FALSE, lo=1, hi=11),
    list(var="c11", rev=TRUE,  lo=1, hi=11),
    
    list(var="c2d", rev=TRUE, lo=1, hi=5),
    list(var="c2i", rev=TRUE, lo=1, hi=5),
    list(var="c2j", rev=TRUE, lo=1, hi=5),
    list(var="c2a", rev=FALSE, lo=1, hi=5),
    list(var="c8", rev=FALSE, lo=1, hi=11)
  )
  
  econ_use <- character(0)
  for (sp in econ_specs) {
    v <- sp$var
    if (v %in% names(out)) {
      tmp <- validate_range(out[[v]], sp$lo, sp$hi)
      tmp <- if (sp$rev) rev_scale(tmp, sp$lo, sp$hi) else to_num(tmp)
      out[[paste0(v,"_ec")]] <- tmp
      econ_use <- c(econ_use, paste0(v,"_ec"))
    }
  }
  
  econ_lr_score <- if (length(econ_use)) {
    Z <- as.data.frame(lapply(out[econ_use], zstd)); rowMeans(Z, na.rm = TRUE)
  } else rep(NA_real_, n)
  
  tibble(
    wknr             = to_num(if ("wknr" %in% names(out)) out$wknr else out$wkname),
    a1_or_partei     = to_num(out$partei),
    econ_lr_score    = as.numeric(econ_lr_score),
    anti_elite_score = as.numeric(anti_elite_score),
    bula             = to_num(out$bula)   # << keep bula so we can impute by state
  ) %>% filter(!is.na(wknr))
}

## ======= Usage =======
cand21 <- read_dta('candidates_2021.dta')
names(cand21) <- tolower(names(cand21))
cand21 <- cand21 %>%
  mutate(wknr = if ("wkname" %in% names(.)) wkname else wknr) %>%
  filter(!is.na(wknr) & to_num(wknr) > 0)

# Compute scores, then fix NAs by per-state median (bula), then overall median as fallback,
# then shrink to party (unchanged).
scores_2021 <- compute_scores_2021(cand21) %>%
  group_by(bula) %>%
  mutate(
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score, na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  ungroup() %>%
  mutate(
    # global fallback in case an entire state was NA for a dimension
    econ_lr_score    = ifelse(is.na(econ_lr_score),
                              median(econ_lr_score, na.rm = TRUE), econ_lr_score),
    anti_elite_score = ifelse(is.na(anti_elite_score),
                              median(anti_elite_score, na.rm = TRUE), anti_elite_score)
  ) %>%
  select(-bula) %>%
  shrink_to_party(lut = party_lut_codes,   # same LUT as other years
                  max_dev = 0.5,
                  lambda_with = 0.75,
                  clamp_min = -1.5,
                  clamp_max = 1.5)

# Join back & (optional) quick plot
party_labels <- c("1"="CDU/CSU","2"="SPD","3"="GRUENE","4"="FDP","5"="LINKE","6"="AfD")
party_colors <- c("CDU/CSU"="#000000","SPD"="#E3000F","GRUENE"="#46962B","FDP"="#FFED00","LINKE"="#BE3075","AfD"="#009EE0")

cand21_scored <- cand21 %>%
  mutate(a1_or_partei = to_num(partei)) %>%
  left_join(scores_2021 %>% select(wknr, a1_or_partei, econ_lr_score, anti_elite_score, party_id),
            by = c("wknr","a1_or_partei")) %>%
  mutate(party = factor(party_id, levels = names(party_labels), labels = party_labels))

cand21_scored_plot <- ggplot(cand21_scored, aes(econ_lr_score, anti_elite_score, color = party)) +
  geom_point(size = 2, alpha = 0.8, na.rm = TRUE) +
  scale_color_manual(values = party_colors, name = "Party") +
  coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5)) +
  labs(x = "Economic Left–Right", y = "Anti-Elite (↑)") +
  theme_minimal(base_size = 14)

ggsave("graphs_data/plot_can_2021_LR.png", cand21_scored_plot, width = 7, height = 5, dpi = 300)

cand21_scored <- cand21_scored[c('partei', 'wknr', 'econ_lr_score', 'anti_elite_score')]
save(cand21_scored, file = 'data/cand21_scored_LR.Rdata')