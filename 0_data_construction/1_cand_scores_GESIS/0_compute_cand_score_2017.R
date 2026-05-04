.in_source <- any(vapply(sys.calls(), function(x) {
  fn <- tryCatch(as.character(x[[1]])[1], error = function(e) "")
  identical(fn, "source") || identical(fn, "sys.source")
}, logical(1)))
if (interactive() && !.in_source && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
rm(.in_source)
set.seed(42)

## ======= Setup (reuse if already loaded) =======
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

party_lut_codes <- tibble::tribble(
  ~orig, ~party_id,
  2  ,   1,   # CDU
  3  ,   1,   # CSU -> CDU bucket
  4  ,   2,   # SPD
  6  ,   3,   # GRUENE
  5  ,   4,   # FDP
  7  ,   5,   # LINKE
  322,   6    # AfD
)

to_num <- function(x) {
  if (inherits(x, c("haven_labelled","labelled","labelled_spss"))) {
    x <- haven::as_factor(x, levels = "values")
  }
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}
have <- function(d, v) intersect(v, names(d))
zstd <- function(x) as.numeric(scale(x))
rev_scale <- function(x, lo, hi) { x <- to_num(x); ifelse(is.finite(x), (lo + hi) - x, NA_real_) }
validate_range <- function(x, lo, hi) {
  x <- to_num(x); x[!(x >= lo & x <= hi)] <- NA_real_; x
}

## ======= Party shrinker (same as 2013) =======
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

## ======= Scoring (2017) =======
# ELITE
# d6a,d6b,d6c,d6g: NOT reverse, 1–5
# d6h,d6f: REVERSE, 1–5
# c20: NOT reverse, 1–4
#
# ECON
# c2b: REVERSE, 1–5
# c2h,c2g: NOT reverse, 1–5
# c3: NOT reverse, 1–11
# c18g: REVERSE, 1–5  (now “social benefits”)
# c18h: NOT reverse, 1–5

compute_scores_2017 <- function(dat) {
  names(dat) <- tolower(names(dat))
  if (!"wknr" %in% names(dat))   stop("2017: 'wknr' missing")
  if (!"partei" %in% names(dat)) stop("2017: 'partei' missing")
  
  out <- dat
  n <- nrow(out)
  
  ## --- Elite items ---
  elite_specs <- list(
    list(var="d6a", rev=FALSE, lo=1, hi=5),
    list(var="d6b", rev=FALSE, lo=1, hi=5),
    list(var="d6c", rev=FALSE, lo=1, hi=5),
    list(var="d6g", rev=FALSE, lo=1, hi=5),
    list(var="d6h", rev=TRUE,  lo=1, hi=5),
    list(var="d6f", rev=TRUE,  lo=1, hi=5),
    list(var="c20", rev=FALSE, lo=1, hi=4)
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
  
  ## --- Econ items ---
  econ_specs <- list(
    list(var="c2b",  rev=TRUE,  lo=1, hi=5),
    list(var="c2h",  rev=FALSE, lo=1, hi=5),
    list(var="c2g",  rev=FALSE, lo=1, hi=5),
    list(var="c3",   rev=FALSE, lo=1, hi=11),
    list(var="c18c", rev=TRUE,  lo=1, hi=5),
    list(var="c18h", rev=FALSE, lo=1, hi=5),
    
    list(var="c2d", rev=TRUE, lo=1, hi=5),
    list(var="c2i", rev=FALSE, lo=1, hi=5),
    list(var="c2j", rev=FALSE, lo=1, hi=5),
    list(var="c2a", rev=TRUE, lo=1, hi=5),
    list(var="c6", rev=FALSE, lo=1, hi=11)
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
    wknr             = to_num(out$wknr),
    a1_or_partei     = to_num(out$partei),
    econ_lr_score    = as.numeric(econ_lr_score),
    anti_elite_score = as.numeric(anti_elite_score)
  ) %>% filter(!is.na(wknr))
}

## ======= Usage =======
# Read your file (or use your existing cand17 object)
cand17 <- read_dta('candidates_2017.dta')
names(cand17) <- tolower(names(cand17))
cand17 <- cand17 %>%
  mutate(wknr  = wknr,
         partei = if ("a1" %in% names(.)) a1 else partei) %>%
  filter(!is.na(wknr) & to_num(wknr) > 0)

scores_2017_not_shrunked <- compute_scores_2017(cand17)

scores_2017 <- compute_scores_2017(cand17) %>%
  shrink_to_party(lut = party_lut_codes,   # same LUT used for 2013
                  max_dev = 0.5,
                  lambda_with = 0.75,
                  clamp_min = -1.5,
                  clamp_max = 1.5)


scores_2017_not_shrunked <- scores_2017_not_shrunked %>% 
  left_join(party_lut_codes, by = c("a1_or_partei" = "orig"))
scores_2017_not_shrunked <- scores_2017_not_shrunked %>% filter(!is.na(party_id))

party_labels <- c("1"="CDU/CSU","2"="SPD","3"="GRUENE","4"="FDP","5"="LINKE","6"="AfD")
party_colors <- c("CDU/CSU"="#000000","SPD"="#E3000F","GRUENE"="#46962B","FDP"="#FFED00","LINKE"="#BE3075","AfD"="#009EE0")

cand17_scored <- cand17 %>%
  mutate(a1_or_partei = to_num(partei)) %>%
  left_join(scores_2017_not_shrunked %>% select(wknr, a1_or_partei, econ_lr_score, anti_elite_score, party_id),
            by = c("wknr","a1_or_partei")) %>%
  mutate(party = factor(party_id, levels = names(party_labels), labels = party_labels))

save(cand17_scored, file = 'data/cand17_scored_LR.Rdata')




# Join back & (optional) quick plot
party_labels <- c("1"="CDU/CSU","2"="SPD","3"="GRUENE","4"="FDP","5"="LINKE","6"="AfD")
party_colors <- c("CDU/CSU"="#000000","SPD"="#E3000F","GRUENE"="#46962B","FDP"="#FFED00","LINKE"="#BE3075","AfD"="#009EE0")

cand17_scored_plot <- ggplot(cand17_scored, aes(econ_lr_score, anti_elite_score, color = party)) +
  geom_point(size = 2, alpha = 0.8, na.rm = TRUE) +
  scale_color_manual(values = party_colors, name = "Party") +
  coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) +
  labs(x = "Economic Left–Right", y = "Anti-Elite (↑)") +
  theme_minimal(base_size = 14)

ggsave("graphs/plot_can_2017_LR.png", cand17_scored_plot, width = 7, height = 5, dpi = 300)


