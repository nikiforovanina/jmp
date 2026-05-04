rm(list = ls())
set.seed(42)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library(dplyr)
library(purrr)
library(broom)
library(stringr)
library(knitr)
library(kableExtra)
library(tidyr)
library(readr)
library(lfe)


load('../2_cand_survey_full_data_wen_waehlen/data/cand21_scored_full_shrunk.Rdata')
cand21_scored <- cand21_scored_full_shrunk
colnames(cand21_scored) <- c( 'wknr',  "econ_lr_score", "anti_elite_score", 'party')
s <- read_csv2('btw2021_strukturdaten.csv')

new_cols <- c(
  "state","constituency_id","constituency_name","municipalities_n","area_km2",
  "pop_total_1000","pop_german_1000","pop_foreign_pct","pop_density_per_km2",
  "birth_balance_per_1000","net_migration_per_1000",
  "age_u18_pct","age_18_24_pct","age_25_34_pct","age_35_59_pct","age_60_74_pct","age_75plus_pct",
  "land_use_settlement_traffic_pct","land_use_vegetation_water_pct",
  "dwellings_completed_per_1000","dwellings_stock_per_1000",
  "living_space_per_dwelling_m2","living_space_per_capita_m2",
  "cars_per_1000","cars_electric_hybrid_pct",
  "firms_per_1000","craft_firms_per_1000",
  "voc_school_leavers_2019","gen_school_leavers_per_1000",
  "gen_school_no_cert_pct","gen_school_lower_cert_pct","gen_school_middle_cert_pct","gen_school_uni_entrance_pct",
  "childcare_u3_rate","childcare_3_6_rate",
  "disp_income_pc_eur","gdp_pc_eur",
  "sv_employed_per_1000","sv_agri_pct","sv_manufact_pct","sv_trade_hospitality_transport_pct",
  "sv_public_private_services_pct","sv_other_unknown_pct",
  "sgb2_recipients_per_1000","sgb2_non_employable_pct","sgb2_foreign_pct",
  "unemp_rate_total","unemp_rate_male","unemp_rate_female","unemp_rate_15_24","unemp_rate_55_64",
  "footnotes"
)

stopifnot(ncol(df) == length(new_cols))
colnames(s) <- new_cols
s <- s[-which(s$constituency_id > 300),]


s_joined <- s %>%
  mutate(constituency_id = as.character(constituency_id)) %>%
  left_join(
    cand21_scored %>% mutate(wknr = as.character(wknr)),
    by = c("constituency_id" = "wknr")
  )



# --- pretty labels you supplied (edit as needed) ---
pretty_map <- c(
  state                               = "State (Land)",
  constituency_id                     = "Constituency ID",
  constituency_name                   = "Constituency",
  municipalities_n                    = "Municipalities (count)",
  area_km2                            = "Area (km²)",
  pop_total_1000                      = "Population (×1k)",
  pop_german_1000                     = "Population, German (×1k)",
  pop_foreign_pct                     = "Foreigners (%)",
  pop_density_per_km2                 = "Population density (per km²)",
  birth_balance_per_1000              = "Birth balance (‰)",
  net_migration_per_1000              = "Net migration (‰)",
  age_u18_pct                         = "Share population <18 (%)",
  age_18_24_pct                       = "Share population 18–24 (%)",
  age_25_34_pct                       = "Share population 25–34 (%)",
  age_35_59_pct                       = "Share population 35–59 (%)",
  age_60_74_pct                       = "Share population 60–74 (%)",
  age_75plus_pct                      = "Share population 75+ (%)",
  land_use_settlement_traffic_pct     = "Land use: settlement/traffic (%)",
  land_use_vegetation_water_pct       = "Land use: vegetation/water (%)",
  dwellings_completed_per_1000        = "Dwellings completed (per 1k)",
  dwellings_stock_per_1000            = "Dwellings stock (per 1k)",
  living_space_per_dwelling_m2        = "Living space per dwelling (m²)",
  living_space_per_capita_m2          = "Living space per capita (m²)",
  cars_per_1000                       = "Cars (per 1k)",
  cars_electric_hybrid_pct            = "EV/Hybrid cars (%)",
  firms_per_1000                      = "Firms (per 1k)",
  craft_firms_per_1000                = "Craft firms (per 1k)",
  voc_school_leavers_2019             = "Vocational school leavers (2019)",
  gen_school_leavers_per_1000         = "General school leavers (per 1k)",
  gen_school_no_cert_pct              = "No school certificate (%)",
  gen_school_lower_cert_pct           = "Lower-secondary certificate (%)",
  gen_school_middle_cert_pct          = "Middle-secondary certificate (%)",
  gen_school_uni_entrance_pct         = "Upper-sec / university entrance (%)",
  childcare_u3_rate                   = "Childcare <3y (rate)",
  childcare_3_6_rate                  = "Childcare 3–6y (rate)",
  disp_income_pc_eur                  = "Disposable income (EUR pc)",
  gdp_pc_eur                          = "GDP (EUR pc)",
  sv_employed_per_1000                = "SV-employed (per 1k)",
  sv_agri_pct                         = "Employed: agriculture (%)",
  sv_manufact_pct                     = "Employed: manufacturing (%)",
  sv_trade_hospitality_transport_pct  = "Employed: trade/hospitality/transport (%)",
  sv_public_private_services_pct      = "Employed: services (%)",
  sv_other_unknown_pct                = "Employed: other/unknown (%)",
  sgb2_recipients_per_1000            = "SGB II recipients (per 1k)",
  sgb2_non_employable_pct             = "SGB II: not employable (%)",
  sgb2_foreign_pct                    = "SGB II: foreign (%)",
  unemp_rate_total                    = "Unemployment total (%)",
  unemp_rate_male                     = "Unemployment men (%)",
  unemp_rate_female                   = "Unemployment women (%)",
  unemp_rate_15_24                    = "Unemployment 15–24 (%)",
  unemp_rate_55_64                    = "Unemployment 55–64 (%)",
  footnotes                           = "Footnotes"
)

###########################################
# ---- inputs assumed: s_joined data frame ----
# Last two columns are the outcomes:
stopifnot(ncol(s_joined) >= 2)
outcomes <- tail(names(s_joined), 3)[1:2]

# Label those two outcome columns exactly as requested
outcome_label_map <- setNames(outcomes, c("LR Score","Elite Score"))

# Predictors = all columns after pop_total_1000 (inclusive) up to before the outcomes
start_idx <- match("pop_total_1000", names(s_joined))
end_idx   <- ncol(s_joined) - 2
stopifnot(!is.na(start_idx), end_idx >= start_idx)
predictors <- names(s_joined)[start_idx:end_idx]

# helper: significance stars
stars3 <- function(p) ifelse(p < 0.001, "***",
                             ifelse(p < 0.010, "**",
                                    ifelse(p < 0.050, "*",  "")))

# run all regressions: outcome ~ predictor + factor(state) + factor(party)
fit_one <- function(y, x) {
  fml <- as.formula(paste(y, "~", x, "+ factor(state) + factor(party)"))
  m   <- lm(fml, data = s_joined)
  broom::tidy(m) %>%
    filter(term == x) %>%
    transmute(
      outcome   = y,
      predictor = x,
      estimate  = estimate,
      std.error = std.error,
      p.value   = p.value
    )
}

res_long <- map_dfr(outcomes, \(y) map_dfr(predictors, \(x) fit_one(y, x)))

# pretty formatting per outcome
res_fmt <- res_long %>%
  mutate(stars = stars3(p.value),
         cell  = sprintf("%.3f%s (%.3f) [p=%.3f]", estimate, stars, std.error, p.value))

# pivot to two columns (LR Score, Elite Score)
res_wide <- res_fmt %>%
  mutate(outcome_pretty = names(outcome_label_map)[match(outcome, outcome_label_map)]) %>%
  select(predictor, outcome_pretty, cell) %>%
  tidyr::pivot_wider(names_from = outcome_pretty, values_from = cell)

# apply pretty predictor names (fallback to raw if missing)
res_wide <- res_wide %>%
  mutate(Predictor = coalesce(pretty_map[predictor], predictor)) %>%
  select(Predictor, `LR Score`, `Elite Score`) %>%
  arrange(Predictor)

# --- escape % for LaTeX: turn "%" into "\%" everywhere ---
escape_pct <- function(x) str_replace_all(x, "%", "\\\\%")
res_wide_escaped <- res_wide %>% mutate(across(everything(), escape_pct))

# --- build LaTeX table with the exact header you want ---
tab_tex <- knitr::kable(
  res_wide_escaped,
  format   = "latex",
  booktabs = TRUE,
  align    = c("l","c","c"),
  col.names = c("Predictor", "LR Score", "Elite Score"),
  escape   = FALSE # we already manually escaped %
) %>%
  kableExtra::kable_styling(latex_options = c("hold_position","striped"))

# write to file
dir.create("tables", showWarnings = FALSE)
writeLines(tab_tex, "tables/2021_regressions_predictors_lr_elite.tex")



###########################################
## ---------- sharpened q-values (BKY two-stage) helper ----------
# Benjamini–Krieger–Yekutieli (2006) adaptive step-up.
# Returns per-hypothesis adjusted q-values for the given p-vector.
compute_bky_q <- function(p, level = 0.10) {
  p <- as.numeric(p)
  m <- length(p)
  if (m == 0) return(p)
  qprime <- level / (1 + level)  # stage-1 BH level
  
  ord <- order(p)
  p_ord <- p[ord]
  ranks <- seq_len(m)
  
  # Stage 1: BH at qprime
  thresh1 <- ranks / m * qprime
  R <- max(c(0, which(p_ord <= thresh1)))
  m_hat <- max(m - R, 1)
  
  # Stage 2: adjusted p (like BH) but with m_hat instead of m
  adj_ord <- rev(cummin(rev((m_hat / ranks) * p_ord)))
  adj_ord <- pmin(adj_ord, 1)
  
  out <- numeric(m); out[ord] <- adj_ord
  out
}

## ---------- assumptions from previous step ----------
# s_joined exists; last two columns are outcomes
stopifnot(ncol(s_joined) >= 2)
outcomes <- tail(names(s_joined), 3)[1:2]
names(outcomes) <- c("LR Score", "Elite Score")  # pretty labels for columns

start_idx <- match("pop_total_1000", names(s_joined))
end_idx   <- ncol(s_joined) - 2
stopifnot(!is.na(start_idx), end_idx >= start_idx)
predictors <- names(s_joined)[start_idx:end_idx]

# significance stars (as before)
stars3 <- function(p) ifelse(p < 0.001, "***",
                             ifelse(p < 0.010, "**",
                                    ifelse(p < 0.050, "*",  "")))

# single regression and pull the predictor row
fit_one <- function(y, x) {
  fml <- as.formula(paste(y, "~", x, "+ factor(state) + factor(party)"))
  m   <- lm(fml, data = s_joined)
  broom::tidy(m) %>%
    filter(term == x) %>%
    transmute(outcome   = y,
              predictor = x,
              estimate  = estimate,
              std.error = std.error,
              p.value   = p.value)
}

# run all regressions
res_long <- map_dfr(names(outcomes), \(lab)
                    map_dfr(predictors, \(x) fit_one(outcomes[[lab]], x)) %>%
                      mutate(outcome_pretty = lab)
)

# add BH q and BKY sharpened q within each outcome family
fdr_level <- 0.10  # change if you want another FDR target
res_long <- res_long %>%
  group_by(outcome_pretty) %>%
  mutate(
    q_bh    = p.adjust(p.value, method = "BH"),
    q_sharp = compute_bky_q(p.value, level = fdr_level)
  ) %>%
  ungroup()

# format cell with estimate, SE, p, and sharpened q
res_fmt <- res_long %>%
  mutate(stars = stars3(p.value),
         cell  = sprintf(
           "%.3f%s (%.3f) [p=%.3f; q^S=%.3f]",
           estimate, stars, std.error, p.value, q_sharp
         ))

# wide by outcome (LR Score / Elite Score)
res_wide <- res_fmt %>%
  select(predictor, outcome_pretty, cell) %>%
  tidyr::pivot_wider(names_from = outcome_pretty, values_from = cell)

# pretty predictor names + order
res_wide <- res_wide %>%
  mutate(Predictor = coalesce(pretty_map[predictor], predictor)) %>%
  select(Predictor, `LR Score`, `Elite Score`) %>%
  arrange(Predictor)

# escape % for LaTeX
escape_pct <- function(x) stringr::str_replace_all(x, "%", "\\\\%")
res_wide_escaped <- res_wide %>% mutate(across(everything(), escape_pct))

# LaTeX table with exact header
tab_tex <- knitr::kable(
  res_wide_escaped,
  format   = "latex",
  booktabs = TRUE,
  align    = c("l","c","c"),
  col.names = c("Predictor", "LR Score", "Elite Score"),
  escape   = FALSE
) %>% kableExtra::kable_styling(latex_options = c("hold_position","striped"))

dir.create("tables", showWarnings = FALSE)
writeLines(tab_tex, "tables/2021_regressions_predictors_lr_elite_with_sharpened_q.tex")


############### plots based on this
###########################################
## ---- coefficient plots (descending) using sharpened q-values ----
## paste AFTER your table code (assumes: res_long exists with q_sharp, estimate, std.error, predictor, outcome_pretty)

library(ggplot2)
library(dplyr)
library(stringr)

dir.create("graphs", showWarnings = FALSE)

# --- helper: build a nice coef plot for one outcome ---
plot_coefs_desc <- function(df, outcome_name,
                            q_thresh = 0.05,
                            top_n = NULL,
                            file_out = NULL) {
  d <- df %>%
    filter(outcome_pretty == outcome_name) %>%
    mutate(
      Predictor = coalesce(pretty_map[predictor], predictor),
      ci_low  = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error,
      sig = q_sharp <= q_thresh
    ) %>%
    # order by coefficient value (descending)
    arrange(desc(estimate)) %>%
    # optional: keep only top_n by |estimate| or by rank in descending order
    { if (!is.null(top_n)) slice(., 1:top_n) else . } %>%
    mutate(Predictor = factor(Predictor, levels = rev(Predictor)))  # so largest at top
  
  p <- ggplot(d, aes(x = estimate, y = Predictor)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35, alpha = 0.8) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.18, linewidth = 0.4, alpha = 0.9) +
    geom_point(aes(shape = sig), size = 2.2, alpha = 0.95) +
    scale_shape_manual(values = c(`TRUE` = 19, `FALSE` = 1)) +
    labs(
      x = "Coefficient (95% CI)",
      y = NULL,
      title = paste0(outcome_name, ": predictor-by-predictor regressions")
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 9),
      plot.title.position = "plot"
    )
  
  if (!is.null(file_out)) {
    ggsave(file_out, p, width = 8.5, height = max(4.5, 0.18 * nrow(d) + 1.8), dpi = 300)
  }
  p
}

res_long <-  res_long %>% filter(!predictor == 'econ_lr_score')
# --- make the two plots you want ---
p_lr  <- plot_coefs_desc(res_long, outcome_name = "LR Score",
                         q_thresh = 0.05,
                         top_n = NULL,
                         file_out = "graphs/coefplot_econ_lr_desc_qsharp.png")

p_ae  <- plot_coefs_desc(res_long, outcome_name = "Elite Score",
                         q_thresh = 0.05,
                         top_n = NULL,
                         file_out = "graphs/coefplot_anti_elite_desc_qsharp.png")

p_lr
p_ae

# --- optional: also save a compact "top 15" version (nice for slides) ---
plot_coefs_desc(res_long, "LR Score",    q_thresh = 0.05, top_n = 15,
                file_out = "graphs/coefplot_econ_lr_top15_desc_qsharp.png")
plot_coefs_desc(res_long, "Elite Score", q_thresh = 0.10, top_n = 15,
                file_out = "graphs/coefplot_anti_elite_top15_desc_qsharp.png")


unique(res_long$predictor)


# ---- selective-label coefficient plot (keep all points, label only important vars) ----
must_label_vars <- c(
  "unemp_rate_total",
  "age_u18_pct",
  "net_migration_per_1000",
  "pop_foreign_pct",
  "gdp_pc_eur",
  "gen_school_uni_entrance_pct"
)


plot_coefs_desc_selective_labels <- function(df, outcome_name,
                                             q_thresh = 0.10,
                                             label_sig = TRUE,
                                             must_label = must_label_vars,
                                             file_out = NULL) {
  
  d <- df %>%
    filter(outcome_pretty == outcome_name) %>%
    mutate(
      Predictor_pretty = coalesce(pretty_map[predictor], predictor),
      ci_low  = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error,
      sig = q_sharp <= q_thresh,
      keep_label = predictor %in% must_label | (label_sig & sig)
    ) %>%
    arrange(-desc(estimate)) %>%   # <- fixed (you had desc(-estimate))
    mutate(
      row_id = row_number(),
      ylab = ifelse(keep_label, Predictor_pretty, "")
    )
  
  p <- ggplot(d, aes(x = estimate, y = factor(row_id))) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35, alpha = 0.8) +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high),
                   height = 0.22, linewidth = 0.4, alpha = 0.9) +  # <- slightly taller
    geom_point(aes(shape = sig), size = 2.2, alpha = 0.95) +
    scale_shape_manual(
      name = "Significance",                 # <- legend title
      values = c(`TRUE` = 19, `FALSE` = 1),
      labels = c(`FALSE` = paste0("q\u02e2 > ", q_thresh),
                 `TRUE`  = paste0("q\u02e2 \u2264 ", q_thresh))
    ) +
    scale_y_discrete(labels = d$ylab) +
    labs(
      x = "Coefficient (95% CI)",
      y = NULL,
      title = paste0(outcome_name, ": predictor-by-predictor regressions")
    ) +
    theme_bw(base_size = 15) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 13, margin = margin(r = 6)), # <- more spacing from axis
      plot.title.position = "plot",
      legend.position = "right",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      plot.margin = margin(8, 18, 8, 8)  # <- extra right margin (helps if labels feel cramped)
    )
  
  if (!is.null(file_out)) {
    ggsave(file_out, p, width = 9, height = 6.5, dpi = 300)
  }
  p
}

res_long  <- res_long  %>% filter(!predictor == "sgb2_non_employable_pct")
p_lr_sel <- plot_coefs_desc_selective_labels(
  res_long, "LR Score",
  q_thresh = 0.05,
  label_sig = TRUE,
  file_out = "graphs/coefplot_econ_lr_desc_qsharp_selectivelabels.png"
)
p_lr_sel

p_el_sel <- plot_coefs_desc_selective_labels(
  res_long, "Elite Score",
  q_thresh = 0.05,
  label_sig = TRUE,
  file_out = "graphs/coefplot_anti_elite_desc_qsharp_selectivelabels.png"
)

p_lr_sel
p_el_sel








