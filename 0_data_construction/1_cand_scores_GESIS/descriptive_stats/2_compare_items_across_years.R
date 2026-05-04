## ============================================================
## 2_compare_items_across_years.R
##
## Compares questionnaire items used across the 2013, 2017,
## and 2021 candidate surveys (GESIS Candidate Study).
##
## Outputs:
##   misc/item_catalog_across_years.csv
##     Mapping table: concept → variable name, reversal flag,
##     scale, availability per year.
##
##   misc/item_stats_across_years.csv
##     Descriptive statistics (N, mean, SD, % missing) for
##     each item × year, on both raw and transformed scale.
##
##   graphs/plot_item_availability.png
##     Heatmap: N valid responses per item per year.
##
##   graphs/plot_item_means_comparison.png
##     Dot-plot: transformed means per concept per year –
##     useful to spot systematic level shifts.
##
##   graphs/plot_item_density_cross_year.png
##     Density overlay for cross-year items (≥2 waves).
##
## ============================================================

.in_source <- any(vapply(sys.calls(), function(x) {
  fn <- tryCatch(as.character(x[[1]])[1], error = function(e) "")
  identical(fn, "source") || identical(fn, "sys.source")
}, logical(1)))
if (interactive() && !.in_source && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
rm(.in_source)
setwd("..")  # operate from folder 1 root (reads candidates_*.dta, writes misc/, graphs/)
set.seed(42)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(readr)
})


## ---- Helpers -----------------------------------------------

to_num <- function(x) {
  if (inherits(x, c("haven_labelled", "labelled", "labelled_spss")))
    x <- haven::as_factor(x, levels = "values")
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}
rev_scale     <- function(x, lo, hi) {
  x <- to_num(x); ifelse(is.finite(x), (lo + hi) - x, NA_real_)
}
validate_range <- function(x, lo, hi) {
  x <- to_num(x); x[!(x >= lo & x <= hi)] <- NA_real_; x
}


## ============================================================
## ITEM CATALOG  (identical to 1_compute_cand_scores_pooled.R)
## ============================================================

item_catalog <- list(

  ## -- Anti-Elite --
  list(concept = "ae_a",   dim = "ae",
       y2013 = list(var = "d5a", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6a", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7a", rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ae_b",   dim = "ae",
       y2013 = list(var = "d5b", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6b", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7b", rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ae_c",   dim = "ae",
       y2013 = list(var = "d5c", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6c", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7c", rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ae_g",   dim = "ae",
       y2013 = list(var = "d5g", rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "d6g", rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "d7g", rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ae_h",   dim = "ae",
       y2013 = list(var = "d5h", rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "d6h", rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "d7h", rev = FALSE, lo = 1, hi = 5)),
  list(concept = "ae_f",   dim = "ae",
       y2013 = list(var = "d5f", rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "d6f", rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "d7f", rev = FALSE, lo = 1, hi = 5)),
  list(concept = "ae_dem", dim = "ae",
       y2013 = list(var = "c20", rev = FALSE, lo = 1, hi = 4),
       y2017 = list(var = "c20", rev = FALSE, lo = 1, hi = 4),
       y2021 = list(var = "c23", rev = FALSE, lo = 1, hi = 4)),

  ## -- Economic L-R (cross-year) --
  list(concept = "ec_c2b",         dim = "ec",
       y2013 = list(var = "c2b",  rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "c2b",  rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "c2b",  rev = FALSE, lo = 1, hi = 5)),
  list(concept = "ec_c2h",         dim = "ec",
       y2013 = list(var = "c2h",  rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2h",  rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2h",  rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ec_c2g",         dim = "ec",
       y2013 = list(var = "c2g",  rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2g",  rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2g",  rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ec_c2d",         dim = "ec",
       y2013 = list(var = "c2d",  rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "c2d",  rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "c2d",  rev = FALSE, lo = 1, hi = 5)),
  list(concept = "ec_c2i",         dim = "ec",
       y2013 = list(var = "c2i",  rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2i",  rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2i",  rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ec_c2j",         dim = "ec",
       y2013 = list(var = "c2j",  rev = FALSE, lo = 1, hi = 5),
       y2017 = list(var = "c2j",  rev = FALSE, lo = 1, hi = 5),
       y2021 = list(var = "c2j",  rev = TRUE,  lo = 1, hi = 5)),
  list(concept = "ec_c2a",         dim = "ec",
       y2013 = list(var = "c2a",  rev = TRUE,  lo = 1, hi = 5),
       y2017 = list(var = "c2a",  rev = TRUE,  lo = 1, hi = 5),
       y2021 = list(var = "c2a",  rev = FALSE, lo = 1, hi = 5)),
  list(concept = "ec_lr_self",     dim = "ec",
       y2013 = list(var = "c3",   rev = FALSE, lo = 1, hi = 11),
       y2017 = list(var = "c3",   rev = FALSE, lo = 1, hi = 11),
       y2021 = list(var = "c5",   rev = FALSE, lo = 1, hi = 11)),
  list(concept = "ec_immigration", dim = "ec",
       y2013 = list(var = "c9",   rev = FALSE, lo = 1, hi = 11),
       y2017 = list(var = "c6",   rev = FALSE, lo = 1, hi = 11),
       y2021 = list(var = "c8",   rev = FALSE, lo = 1, hi = 11)),

  ## -- Economic L-R (year-specific) --
  list(concept = "ec_c19e",  dim = "ec",
       y2013 = list(var = "c19e", rev = FALSE, lo = 1, hi = 5),
       y2017 = NULL, y2021 = NULL),
  list(concept = "ec_c19f",  dim = "ec",
       y2013 = list(var = "c19f", rev = FALSE, lo = 1, hi = 5),
       y2017 = NULL, y2021 = NULL),
  list(concept = "ec_c18c",  dim = "ec",
       y2013 = NULL,
       y2017 = list(var = "c18c", rev = TRUE,  lo = 1, hi = 5),
       y2021 = NULL),
  list(concept = "ec_c18h",  dim = "ec",
       y2013 = NULL,
       y2017 = list(var = "c18h", rev = FALSE, lo = 1, hi = 5),
       y2021 = NULL),
  list(concept = "ec_c11",   dim = "ec",
       y2013 = NULL, y2017 = NULL,
       y2021 = list(var = "c11",  rev = TRUE,  lo = 1, hi = 11))
)


## ============================================================
## ITEM DESCRIPTIONS
## ============================================================

item_desc <- tibble::tribble(
  ~concept,          ~description,
  "ae_a",            "Sufficient participation opportunities (Partizipation)",
  "ae_b",            "Legislation reflects public interests",
  "ae_c",            "Parties as central political intermediary",
  "ae_g",            "Referenda lead to bad legislation",
  "ae_h",            "Citizens' trust in government declining",
  "ae_f",            "Support for federal referenda (Volksentscheid)",
  "ae_dem",          "Satisfaction with democracy (c20/c20/c23)",
  "ec_c2b",          "Keep government out of economy",
  "ec_c2h",          "Government should reduce income inequality",
  "ec_c2g",          "Social security as government goal",
  "ec_c2d",          "Ban same-sex marriage",
  "ec_c2i",          "Immigration is good for the economy",
  "ec_c2j",          "Self-determination on abortion",
  "ec_c2a",          "Immigrants should adapt culturally",
  "ec_lr_self",      "Left-Right self-placement [1=L, 11=R] (c3/c3/c5*)",
  "ec_immigration",  "Immigration restriction scale [1=ease, 11=restrict] (c9/c6/c8*)",
  "ec_c19e",         "2013 ONLY: Economic stimulus funds (eurozone crisis)",
  "ec_c19f",         "2013 ONLY: Financial support from Germany (eurozone crisis)",
  "ec_c18c",         "2017 ONLY: Social benefits item A (REV)",
  "ec_c18h",         "2017 ONLY: Social benefits item B",
  "ec_c11",          "2021 ONLY: Taxes vs. social spending [1=taxes, 11=benefits]"
)

# Note on assumed cross-year mappings
cat(
  "\n*** ASSUMPTION NOTE ***\n",
  "ec_lr_self   : c3(2013) = c3(2017) = c5(2021) assumed equivalent.\n",
  "ec_immigration: c9(2013) = c6(2017) = c8(2021) assumed equivalent.\n",
  "Verify these mappings against the survey codebook before use.\n\n"
)


## ============================================================
## TABLE 1: Item catalog – mapping across years
## ============================================================

catalog_rows <- lapply(item_catalog, function(item) {
  yr_label <- function(spec) {
    if (is.null(spec)) return(c(NA_character_, NA, NA_character_))
    c(spec$var,
      spec$rev,
      sprintf("[%d\u2013%d]", spec$lo, spec$hi))
  }
  l13 <- yr_label(item$y2013)
  l17 <- yr_label(item$y2017)
  l21 <- yr_label(item$y2021)
  n_yrs <- sum(!is.na(c(if (!is.null(item$y2013)) 1 else NA,
                         if (!is.null(item$y2017)) 1 else NA,
                         if (!is.null(item$y2021)) 1 else NA)))
  tibble(
    concept    = item$concept,
    dimension  = item$dim,
    var_2013   = l13[1],  rev_2013 = as.logical(l13[2]),  scale_2013 = l13[3],
    var_2017   = l17[1],  rev_2017 = as.logical(l17[2]),  scale_2017 = l17[3],
    var_2021   = l21[1],  rev_2021 = as.logical(l21[2]),  scale_2021 = l21[3],
    n_years    = n_yrs,
    availability = dplyr::case_when(
      n_yrs == 3 ~ "all 3 years",
      n_yrs == 2 ~ "2 years",
      TRUE       ~ "year-specific"
    )
  )
})

catalog_table <- bind_rows(catalog_rows) %>%
  left_join(item_desc, by = "concept")

write_csv(catalog_table, "misc/item_catalog_across_years.csv")
cat("Written: misc/item_catalog_across_years.csv\n\n")

cat("=== ITEM CATALOG ===\n")
catalog_table %>%
  select(concept, dimension, availability,
         var_2013, rev_2013,
         var_2017, rev_2017,
         var_2021, rev_2021,
         description) %>%
  print(n = Inf, width = 120)


## ============================================================
## LOAD DATA
## ============================================================

message("\nLoading data...")
cand13 <- read_dta("candidates_2013.dta"); names(cand13) <- tolower(names(cand13))
cand17 <- read_dta("candidates_2017.dta"); names(cand17) <- tolower(names(cand17))
cand21 <- read_dta("candidates_2021.dta"); names(cand21) <- tolower(names(cand21))

raw_list  <- list(cand13, cand17, cand21)
year_ints <- c(2013L, 2017L, 2021L)
year_keys <- c("y2013", "y2017", "y2021")


## ============================================================
## TABLE 2: Descriptive statistics per item × year
##   Computed on both the raw scale and the transformed
##   (direction-harmonised) scale.
## ============================================================

get_item_stats <- function(dat, year_int, year_key, catalog) {
  out <- list()
  for (item in catalog) {
    spec <- item[[year_key]]
    if (is.null(spec)) next
    v <- spec$var
    if (!v %in% names(dat)) next

    raw_num   <- to_num(dat[[v]])
    raw_valid <- validate_range(dat[[v]], spec$lo, spec$hi)
    trans     <- if (spec$rev) rev_scale(raw_valid, spec$lo, spec$hi) else to_num(raw_valid)

    out[[length(out) + 1]] <- tibble(
      year         = year_int,
      concept      = item$concept,
      dimension    = item$dim,
      var_name     = v,
      scale        = sprintf("[%d\u2013%d]", spec$lo, spec$hi),
      reversed     = spec$rev,
      n_respondents= nrow(dat),
      n_raw_nonNA  = sum(!is.na(raw_num)),
      n_valid      = sum(!is.na(raw_valid)),
      pct_missing  = round(100 * mean(is.na(raw_valid)), 1),
      mean_raw     = round(mean(raw_num,  na.rm = TRUE), 3),
      sd_raw       = round(sd(raw_num,    na.rm = TRUE), 3),
      min_raw      = min(raw_num,          na.rm = TRUE),
      max_raw      = max(raw_num,          na.rm = TRUE),
      mean_trans   = round(mean(trans,    na.rm = TRUE), 3),
      sd_trans     = round(sd(trans,      na.rm = TRUE), 3)
    )
  }
  bind_rows(out)
}

stats_all <- bind_rows(
  mapply(get_item_stats,
         dat      = raw_list,
         year_int = year_ints,
         year_key = year_keys,
         MoreArgs = list(catalog = item_catalog),
         SIMPLIFY = FALSE)
) %>%
  left_join(item_desc, by = "concept") %>%
  arrange(dimension, concept, year)

write_csv(stats_all, "misc/item_stats_across_years.csv")
cat("\nWritten: misc/item_stats_across_years.csv\n\n")

cat("=== ITEM STATISTICS (transformed scale) ===\n")
stats_all %>%
  select(dimension, concept, year, var_name, reversed, n_valid,
         pct_missing, mean_trans, sd_trans, description) %>%
  print(n = Inf, width = 130)


## ============================================================
## TABLE 3: Coverage summary across years
## ============================================================

coverage <- catalog_table %>%
  select(concept, dimension, availability,
         var_2013, var_2017, var_2021, description) %>%
  mutate(
    in_2013 = !is.na(var_2013),
    in_2017 = !is.na(var_2017),
    in_2021 = !is.na(var_2021)
  )

cat("\n=== COVERAGE SUMMARY ===\n")
cat(sprintf("  Items in ALL 3 years : %d\n",
            sum(coverage$in_2013 & coverage$in_2017 & coverage$in_2021)))
cat(sprintf("  Items in 2013 & 2017 only : %d\n",
            sum(coverage$in_2013 & coverage$in_2017 & !coverage$in_2021)))
cat(sprintf("  Items in 2013 & 2021 only : %d\n",
            sum(coverage$in_2013 & !coverage$in_2017 & coverage$in_2021)))
cat(sprintf("  Items in 2017 & 2021 only : %d\n",
            sum(!coverage$in_2013 & coverage$in_2017 & coverage$in_2021)))
cat(sprintf("  2013-specific items  : %d\n",
            sum(coverage$in_2013 & !coverage$in_2017 & !coverage$in_2021)))
cat(sprintf("  2017-specific items  : %d\n",
            sum(!coverage$in_2013 & coverage$in_2017 & !coverage$in_2021)))
cat(sprintf("  2021-specific items  : %d\n",
            sum(!coverage$in_2013 & !coverage$in_2017 & coverage$in_2021)))

cat("\n=== DIRECTION-CODING CHANGES ACROSS YEARS ===\n")
# Highlight items where the rev flag differs between years
cat("Items with reversed coding polarity in 2021 vs. 2013/2017:\n")
catalog_table %>%
  filter(!is.na(var_2013) & !is.na(var_2021)) %>%
  filter(rev_2013 != rev_2021) %>%
  select(concept, var_2013, rev_2013, var_2021, rev_2021, description) %>%
  print(n = Inf)


## ============================================================
## PLOT 1: Item availability heatmap
## ============================================================

avail_long <- stats_all %>%
  select(concept, dimension, year, n_valid) %>%
  mutate(year = factor(year),
         concept_label = paste0(concept, "\n(dim: ", dimension, ")"))

p_avail <- ggplot(avail_long,
                  aes(x = year,
                      y = reorder(concept, desc(concept)),
                      fill = n_valid)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(is.na(n_valid), "—", as.character(n_valid))),
            size = 3, color = "black") +
  scale_fill_gradient(low  = "#fee8c8", high = "#d7301f",
                      na.value = "grey90", name = "N valid") +
  facet_wrap(~dimension, scales = "free_y",
             labeller = labeller(dimension = c(ae = "Anti-Elite",
                                               ec = "Economic L-R"))) +
  labs(
    title    = "Item availability: N valid responses per year",
    subtitle = "Grey cell = item not in that year's questionnaire",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.y = element_text(size = 9),
        panel.grid  = element_blank())

ggsave("graphs/plot_item_availability.png",
       p_avail, width = 12, height = 9, dpi = 300)
cat("Written: graphs/plot_item_availability.png\n")


## ============================================================
## PLOT 2: Transformed means per concept per year
##         Spot systematic level shifts after harmonisation
## ============================================================

p_means <- stats_all %>%
  filter(!is.na(mean_trans)) %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(x = mean_trans,
             y = reorder(concept, mean_trans),
             color = year, shape = year)) +
  geom_point(size = 3.5, alpha = 0.9) +
  geom_line(aes(group = concept), color = "grey70", linewidth = 0.4) +
  scale_color_manual(
    values = c("2013" = "#1b7837", "2017" = "#762a83", "2021" = "#e66101"),
    name   = "Year"
  ) +
  scale_shape_manual(values = c("2013" = 15, "2017" = 17, "2021" = 19),
                     name   = "Year") +
  facet_wrap(~dimension, scales = "free",
             labeller = labeller(dimension = c(ae = "Anti-Elite",
                                               ec = "Economic L-R"))) +
  labs(
    title    = "Transformed item means across election years",
    subtitle = "Direction-harmonised; higher = more anti-elite / more right-wing.\nLines connect the same concept across years.",
    x        = "Mean (transformed scale)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9),
        legend.position = "top")

ggsave("graphs/plot_item_means_comparison.png",
       p_means, width = 13, height = 8, dpi = 300)
cat("Written: graphs/plot_item_means_comparison.png\n")


## ============================================================
## PLOT 3: Density of transformed values for cross-year items
##         (presence in ≥2 waves)
## ============================================================

# Extract long data for density plots
extract_vals <- function(dat, year_int, year_key, catalog) {
  out <- list()
  for (item in catalog) {
    spec <- item[[year_key]]
    if (is.null(spec)) next
    v <- spec$var
    if (!v %in% names(dat)) next
    vals <- validate_range(dat[[v]], spec$lo, spec$hi)
    vals <- if (spec$rev) rev_scale(vals, spec$lo, spec$hi) else to_num(vals)
    out[[length(out) + 1]] <- tibble(
      year    = year_int,
      concept = item$concept,
      dim     = item$dim,
      value   = vals
    )
  }
  bind_rows(out)
}

vals_long <- bind_rows(
  mapply(extract_vals,
         dat      = raw_list,
         year_int = year_ints,
         year_key = year_keys,
         MoreArgs = list(catalog = item_catalog),
         SIMPLIFY = FALSE)
) %>%
  filter(!is.na(value))

# Keep only concepts present in ≥2 years
cross_year_concepts <- vals_long %>%
  distinct(concept, year) %>%
  count(concept) %>%
  filter(n >= 2) %>%
  pull(concept)

p_density <- vals_long %>%
  filter(concept %in% cross_year_concepts) %>%
  left_join(item_desc %>% select(concept, description), by = "concept") %>%
  mutate(
    year      = factor(year),
    item_label = paste0(concept, "\n", str_wrap(description, 28))
  ) %>%
  ggplot(aes(x = value, color = year, fill = year)) +
  geom_density(alpha = 0.18, adjust = 1.2, linewidth = 0.7) +
  scale_color_manual(
    values = c("2013" = "#1b7837", "2017" = "#762a83", "2021" = "#e66101"),
    name   = "Year"
  ) +
  scale_fill_manual(
    values = c("2013" = "#1b7837", "2017" = "#762a83", "2021" = "#e66101"),
    name   = "Year"
  ) +
  facet_wrap(~concept, scales = "free", ncol = 4) +
  labs(
    title    = "Distributions of cross-year items (transformed, pre-z-score)",
    subtitle = paste0("Concepts present in ≥2 waves; direction harmonised.\n",
                      "Distributions should overlap well for comparability."),
    x = "Transformed value", y = "Density"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text      = element_text(size = 8),
    legend.position = "top"
  )

ggsave("graphs/plot_item_density_cross_year.png",
       p_density, width = 16, height = 14, dpi = 300)
cat("Written: graphs/plot_item_density_cross_year.png\n")


## ============================================================
## PLOT 4: Mean ± SD per concept × year (forest-plot style)
##         Focuses on the direction and dispersion comparison
## ============================================================

p_forest <- stats_all %>%
  filter(!is.na(mean_trans)) %>%
  filter(concept %in% cross_year_concepts) %>%
  mutate(year = factor(year),
         lo   = mean_trans - sd_trans,
         hi   = mean_trans + sd_trans) %>%
  ggplot(aes(y = reorder(concept, mean_trans),
             x = mean_trans, xmin = lo, xmax = hi,
             color = year, shape = year)) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.5) +
  scale_color_manual(
    values = c("2013" = "#1b7837", "2017" = "#762a83", "2021" = "#e66101"),
    name   = "Year"
  ) +
  scale_shape_manual(values = c("2013" = 15, "2017" = 17, "2021" = 19),
                     name   = "Year") +
  facet_wrap(~dimension, scales = "free_y",
             labeller = labeller(dimension = c(ae = "Anti-Elite",
                                               ec = "Economic L-R"))) +
  labs(
    title    = "Mean ± SD of cross-year items (transformed scale)",
    subtitle = "Overlapping error bars indicate compatible distributions across years",
    x        = "Mean ± SD (transformed scale)", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 9),
        legend.position = "top")

ggsave("graphs/plot_item_sd_comparison.png",
       p_forest, width = 13, height = 7, dpi = 300)
cat("Written: graphs/plot_item_sd_comparison.png\n")


## ============================================================
## SUMMARY
## ============================================================

cat("\n=== SUMMARY ===\n")
cat("Files written:\n")
cat("  misc/item_catalog_across_years.csv  – concept → variable mapping\n")
cat("  misc/item_stats_across_years.csv    – descriptive stats per item × year\n")
cat("  graphs/plot_item_availability.png   – availability heatmap\n")
cat("  graphs/plot_item_means_comparison.png – means across years\n")
cat("  graphs/plot_item_density_cross_year.png – density comparison\n")
cat("  graphs/plot_item_sd_comparison.png  – mean ± SD forest plot\n")
cat("\nSee misc/item_catalog_across_years.csv for the full item mapping.\n")
cat("Run 1_compute_cand_scores_pooled.R to produce the pooled scores.\n")
