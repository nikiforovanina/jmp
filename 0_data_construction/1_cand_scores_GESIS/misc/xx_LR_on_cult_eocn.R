rm(list = ls())
set.seed(42)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(haven)
})

# -----------------------------
# Helpers
# -----------------------------
to_num <- function(x) {
  if (inherits(x, c("haven_labelled", "labelled", "labelled_spss"))) {
    x <- haven::as_factor(x, levels = "values")
  }
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}
rev_scale <- function(x, lo, hi) { x <- to_num(x); ifelse(is.finite(x), (lo + hi) - x, NA_real_) }
validate_range <- function(x, lo, hi) {
  x <- to_num(x)
  x[!(x >= lo & x <= hi)] <- NA_real_
  x
}
zstd <- function(x) as.numeric(scale(x))

# -----------------------------
# Read candidate files
# -----------------------------
cand13 <- read_dta("../candidates_2013.dta")
names(cand13) <- tolower(names(cand13))
cand13 <- cand13 %>% mutate(partei = if ("a1" %in% names(.)) a1 else partei)

cand17 <- read_dta("../candidates_2017.dta")
names(cand17) <- tolower(names(cand17))
cand17 <- cand17 %>% mutate(partei = if ("a1" %in% names(.)) a1 else partei)

cand21 <- read_dta("../candidates_2021.dta")
names(cand21) <- tolower(names(cand21))


# ============================================================
# 1) Define item lists (econ vs culture) and self-placement var
#    IMPORTANT: adjust dep_var if needed.
# ============================================================

specs_2013 <- list(
  year = 2013,
  dep_var = "c3",  # <-- self-placement (change if needed)
  econ = list(
    list(var="c2b", rev=TRUE,  lo=1, hi=5),
    list(var="c2h", rev=FALSE, lo=1, hi=5),
    list(var="c2g", rev=FALSE, lo=1, hi=5)
  ),
  cult = list(
    list(var="c2d", rev=TRUE,  lo=1, hi=5),
    list(var="c2i", rev=FALSE, lo=1, hi=5),
    list(var="c2j", rev=FALSE, lo=1, hi=5),
    list(var="c2a", rev=TRUE,  lo=1, hi=5)
  )
)

specs_2017 <- list(
  year = 2017,
  dep_var = "c3",  # <-- self-placement (change if needed)
  econ = list(
    list(var="c2b", rev=TRUE,  lo=1, hi=5),
    list(var="c2h", rev=FALSE, lo=1, hi=5),
    list(var="c2g", rev=FALSE, lo=1, hi=5)
  ),
  cult = list(
    list(var="c2d", rev=TRUE,  lo=1, hi=5),
    list(var="c2i", rev=FALSE, lo=1, hi=5),
    list(var="c2j", rev=FALSE, lo=1, hi=5),
    list(var="c2a", rev=TRUE,  lo=1, hi=5)
  )
)

specs_2021 <- list(
  year = 2021,
  dep_var = "c5", 
  econ = list(
    list(var="c2b", rev=FALSE, lo=1, hi=5),
    list(var="c2h", rev=TRUE,  lo=1, hi=5),
    list(var="c2g", rev=TRUE,  lo=1, hi=5)
  ),
  cult = list(
    list(var="c2d", rev=FALSE,  lo=1, hi=5),
    list(var="c2i", rev=TRUE,  lo=1, hi=5),
    list(var="c2j", rev=TRUE,  lo=1, hi=5),
    list(var="c2a", rev=FALSE, lo=1, hi=5)
  )
)

# ============================================================
# 2) Build a clean regression dataset for each year:
#    - dep = self placement, validated to [1,11]
#    - X = econ/cult items, validated + reverse-coded + standardized
# ============================================================

make_reg_df <- function(dat, specs) {
  year <- specs$year
  dep_var <- specs$dep_var
  
  # dependent variable: self placement (assume 1..11)
  if (!dep_var %in% names(dat)) stop(sprintf("Year %d: dep_var '%s' not found", year, dep_var))
  dep <- validate_range(dat[[dep_var]], 1, 11)
  
  # helper for a block
  build_block <- function(spec_list, prefix) {
    out <- list()
    for (sp in spec_list) {
      v <- sp$var
      if (v %in% names(dat)) {
        x <- validate_range(dat[[v]], sp$lo, sp$hi)
        x <- if (sp$rev) rev_scale(x, sp$lo, sp$hi) else to_num(x)
        out[[paste0(prefix, v)]] <- x
      } else {
        out[[paste0(prefix, v)]] <- NA_real_
      }
    }
    as_tibble(out)
  }
  
  X_econ_raw <- build_block(specs$econ, "econ_")
  X_cult_raw <- build_block(specs$cult, "cult_")
  
  df <- tibble(
    year = year,
    LR_self = dep,
    partei  = to_num(dat$partei),
  ) %>%
    bind_cols(X_econ_raw, X_cult_raw)
  
  # standardize predictors (NOT the dependent variable)
  pred_cols <- setdiff(names(df), c("year", "LR_self", "partei"))
  df <- df %>% mutate(across(all_of(pred_cols), zstd))
  
  df
}

df13 <- make_reg_df(cand13, specs_2013)
df17 <- make_reg_df(cand17, specs_2017)
df21 <- make_reg_df(cand21, specs_2021)

# ============================================================
# 3) Run regressions:
#    (1) separate: LR_self ~ x_k  (store coef over time)
#    (2) group: LR_self ~ econ_*  and LR_self ~ cult_*
#    (3) all:   LR_self ~ econ_* + cult_*
# ============================================================

run_models_one_year <- function(df_year) {
  df_year <- df_year %>% filter(partei %in% c(2,3,4,5,6,7,322))
  stopifnot(all(c("year", "LR_self") %in% names(df_year)))
  y <- df_year$year[1]
  
  pred_cols <- setdiff(names(df_year), c("year", "LR_self"))
  econ_cols <- pred_cols[grepl("^econ_", pred_cols)]
  cult_cols <- pred_cols[grepl("^cult_", pred_cols)]
  
  # --------- (1) separate regressions for each item ----------
  coef_rows <- lapply(pred_cols, function(xcol) {
    d <- df_year %>%
      select(LR_self, all_of(xcol)) %>%
      filter(is.finite(LR_self) & is.finite(.data[[xcol]]))
    
    if (nrow(d) < 10) {
      return(tibble(
        year = y,
        var = xcol,
        group = ifelse(startsWith(xcol, "econ_"), "Econ", "Culture"),
        beta = NA_real_,
        se = NA_real_,
        r2 = NA_real_,
        n = nrow(d)
      ))
    }
    
    fit <- lm(LR_self ~ ., data = d)
    sm  <- summary(fit)
    
    tibble(
      year = y,
      var = xcol,
      group = ifelse(startsWith(xcol, "econ_"), "Econ", "Culture"),
      beta = sm$coefficients[2, 1],
      se   = sm$coefficients[2, 2],
      r2   = sm$r.squared,
      n    = nrow(d)
    )
  })
  coef_df <- bind_rows(coef_rows)
  
  # --------- (2) group regressions ----------
  fit_group <- function(cols, model_name) {
    d <- df_year %>% select(LR_self, all_of(cols)) %>% filter(is.finite(LR_self))
    # drop rows with any missing X
    d <- d %>% filter(if_all(all_of(cols), is.finite))
    if (nrow(d) < (length(cols) + 10)) {
      return(tibble(year = y, model = model_name, r2 = NA_real_, n = nrow(d)))
    }
    fml <- as.formula(paste("LR_self ~", paste(cols, collapse = " + ")))
    fit <- lm(fml, data = d)
    sm <- summary(fit)
    tibble(year = y, model = model_name, r2 = sm$r.squared, n = nrow(d))
  }
  
  r2_df <- bind_rows(
    fit_group(econ_cols, "Econ only"),
    fit_group(cult_cols, "Culture only"),
    fit_group(c(c(econ_cols, cult_cols)), "Econ + Culture")
  )
  
  list(coefs = coef_df, r2 = r2_df)
}

res13 <- run_models_one_year(df13)
res17 <- run_models_one_year(df17)
res21 <- run_models_one_year(df21)

coef_all <- bind_rows(res13$coefs, res17$coefs, res21$coefs) %>%
  mutate(var_short = gsub("^econ_|^cult_", "", var))

r2_all <- bind_rows(res13$r2, res17$r2, res21$r2)

# ============================================================
# 4) PLOTS
#    (A) Coefficients over time for each variable (facet by var)
#        Econ = blue, Culture = green
# ============================================================
# ============================================================
# Replace everything from item_labels ... through both ggsave()
# Adds automatic 2-line (or multi-line) facet labels via str_wrap
# ============================================================

library(dplyr)
library(ggplot2)
library(stringr)
library(tibble)

item_labels <- tibble::tribble(
  ~item,  ~label_raw,
  # ---- ECON items ----
  "c2b",  "Govt should stay out of economy",
  "c2h",  "Govt should reduce income inequality.",
  "c2g",  "Social security as a Govt goal",
  
  # ---- CULTURE items ----
  "c2d",  "Same-sex marriage should be banned",
  "c2i",  "Immigration benefits German economy",
  "c2j",  "Abortion: women should decide freely",
  "c2a",  "Immigrants should adapt to German culture",
)

# ---- 2) Auto-wrap labels for facet strips ----
# width controls where line breaks occur (try 22–30; 26 is a good start)
wrap_width <- 26

item_labels <- item_labels %>%
  mutate(label = stringr::str_wrap(label_raw, width = wrap_width))

# ---- 3) Join labels onto coef_all, keep facet order consistent ----
coef_all_lab <- coef_all %>%
  left_join(item_labels %>% select(item, label, label_raw),
            by = c("var_short" = "item")) %>%
  mutate(
    # fallback: if not in mapping, keep the code as label (unwrapped)
    label = ifelse(is.na(label), var_short, label),
    # facet order: use your mapping order first; any extra codes afterwards
    label = factor(label, levels = unique(c(item_labels$label, label)))
  )

# ---- colors: Culture green, Econ blue ----
cols_gc <- c("Culture" = "#3CB371", "Econ" = "#1F4EAD")

# ============================================================
# 4) Coefficient plot
# ============================================================
p_coef <- ggplot(
  coef_all_lab %>% filter(var_short != "partei"),
  aes(x = year, y = beta, group = var, color = group)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
    width = 0.15, alpha = 0.8
  ) +
  facet_wrap(~ label, scales = "free_y") +
  scale_color_manual(values = cols_gc, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  labs(
    x = NULL,
    y = "Coefficient (standardized X)",
    title = "Self-placement on item: coefficient change"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_coef
ggsave(
  "graphs_misc/lr_self_item_coefs_over_time.png",
  p_coef, width = 12, height = 7.5, dpi = 300
)

# ============================================================
# 5) R^2 plot for the separate (single-item) regressions
# ============================================================
p_coef_r2 <- ggplot(
  coef_all_lab %>% filter(var_short != "partei"),
  aes(x = year, y = r2, group = var, color = group)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ label, scales = "free_y") +
  scale_color_manual(values = cols_gc, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = NULL,
    y = expression(R^2),
    title = expression(R^2 ~ "for separate regressions")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_coef_r2
ggsave(
  "graphs_misc/lr_self_item_r2_over_time.png",
  p_coef_r2, width = 12, height = 7.5, dpi = 300
)
# ============================================================
# (B) R^2 evolution for the three models
#     Econ only = blue, Culture only = green, All = black
# ============================================================

r2_cols <- c("Econ only" = "#1F4EAD", "Culture only" = "#3CB371", "Econ + Culture" = "black")

p_r2 <- ggplot(r2_all, aes(x = year, y = r2, color = model, group = model)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.6) +
  scale_color_manual(values = r2_cols, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = NULL, y = expression(R^2), title = expression(R^2~"over time")) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_r2
ggsave("graphs_misc/lr_self_r2_over_time.png", p_r2, width = 8.5, height = 4.8, dpi = 300)


cand13$partei
cand17$partei
cand21


# ============================================================
# R^2 with party fixed effects (controls for partei)
# ============================================================

unique(df13$partei)
unique(df17$partei)
unique(df21$partei)

fit_group_fe <- function(df_year, cols, model_name) {
  df_year <- df_year %>% filter(partei %in% c(2,3,4,5,6,7,322))
  
  y <- df_year$year[1]
  d <- df_year %>%
    select(LR_self, partei, all_of(cols)) %>%
    filter(is.finite(LR_self) & is.finite(partei)) %>%
    filter(if_all(all_of(cols), is.finite))
  
  if (nrow(d) < (length(cols) + 10)) {
    return(tibble(year = y, model = model_name, r2 = NA_real_, n = nrow(d)))
  }
  
  fml <- as.formula(paste("LR_self ~", paste(cols, collapse = " + "), "+ factor(partei)"))
  fit <- lm(fml, data = d)
  sm  <- summary(fit)
  
  tibble(year = y, model = model_name, r2 = sm$r.squared, n = nrow(d))
}

run_r2_fe_one_year <- function(df_year) {
  pred_cols <- setdiff(names(df_year), c("year", "LR_self", "partei"))
  econ_cols <- pred_cols[grepl("^econ_", pred_cols)]
  cult_cols <- pred_cols[grepl("^cult_", pred_cols)]
  
  bind_rows(
    fit_group_fe(df_year, econ_cols, "Econ only (FE)"),
    fit_group_fe(df_year, cult_cols, "Culture only (FE)"),
    fit_group_fe(df_year, c(econ_cols, cult_cols), "Econ + Culture (FE)")
  )
}

r2_fe_all <- bind_rows(
  run_r2_fe_one_year(df13),
  run_r2_fe_one_year(df17),
  run_r2_fe_one_year(df21)
)

# plot (same style as your last R^2 figure)
r2_fe_cols <- c(
  "Econ only (FE)" = "#1F4EAD",
  "Culture only (FE)" = "#3CB371",
  "Econ + Culture (FE)" = "black"
)

p_r2_fe <- ggplot(r2_fe_all, aes(x = year, y = r2, color = model, group = model)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.6) +
  scale_color_manual(values = r2_fe_cols, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = NULL, y = expression(R^2), title = expression(R^2~"over time (controlling for party FE)")) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_r2_fe
ggsave("graphs_misc/lr_self_r2_over_time_partyFE.png", p_r2_fe, width = 8.5, height = 4.8, dpi = 300)


#################################
# coef with party fe
#################################

# ============================================================
# Single-item regressions WITH party fixed effects:
#   LR_self ~ x_k + factor(partei)
# Save: beta, se, r2, n for each item and year
# Then plot beta+CI and R^2 over time (faceted by item label)
# ============================================================

library(dplyr)
library(ggplot2)
library(stringr)

# --- helper: run FE single-item regressions for one year ---
run_item_fe_one_year <- function(df_year) {
  df_year <- df_year %>% filter(partei %in% c(2,3,4,5,6,7,322))
  y <- df_year$year[1]
  
  pred_cols <- setdiff(names(df_year), c("year", "LR_self", "partei"))
  
  rows <- lapply(pred_cols, function(xcol) {
    d <- df_year %>%
      select(LR_self, partei, all_of(xcol)) %>%
      filter(is.finite(LR_self) & is.finite(partei) & is.finite(.data[[xcol]]))
    
    if (nrow(d) < 20) {
      return(tibble(
        year = y,
        var = xcol,
        group = ifelse(startsWith(xcol, "econ_"), "Econ", "Culture"),
        beta = NA_real_, se = NA_real_, r2 = NA_real_, n = nrow(d)
      ))
    }
    
    fml <- as.formula(paste0("LR_self ~ ", xcol, " + factor(partei)"))
    fit <- lm(fml, data = d)
    sm  <- summary(fit)
    
    # coefficient row name is xcol (safe)
    b  <- sm$coefficients[xcol, 1]
    se <- sm$coefficients[xcol, 2]
    
    tibble(
      year = y,
      var = xcol,
      group = ifelse(startsWith(xcol, "econ_"), "Econ", "Culture"),
      beta = b,
      se   = se,
      r2   = sm$r.squared,
      n    = nrow(d)
    )
  })
  
  bind_rows(rows)
}

# --- build FE coef table across years ---
coef_fe_all <- bind_rows(
  run_item_fe_one_year(df13),
  run_item_fe_one_year(df17),
  run_item_fe_one_year(df21)
) %>%
  mutate(
    var_short = gsub("^econ_|^cult_", "", var)
  )

# --- attach nice wrapped labels (re-uses your item_labels) ---
# item_labels must contain columns: item, label (already wrapped), label_raw (optional)
coef_fe_all_lab <- coef_fe_all %>%
  left_join(item_labels %>% select(item, label, label_raw),
            by = c("var_short" = "item")) %>%
  mutate(
    label = ifelse(is.na(label), var_short, label),
    label = factor(label, levels = unique(c(item_labels$label, label)))
  )

# ============================================================
# Plot 1: FE coefficients over time (with 95% CI)
# ============================================================
p_coef_fe <- ggplot(
  coef_fe_all_lab %>% filter(var_short != "partei"),
  aes(x = year, y = beta, group = var, color = group)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_errorbar(
    aes(ymin = beta - 1.96 * se, ymax = beta + 1.96 * se),
    width = 0.15, alpha = 0.8
  ) +
  facet_wrap(~ label, scales = "free_y") +
  scale_color_manual(values = cols_gc, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  labs(
    x = NULL,
    y = "Coefficient (standardized X)",
    title = "Self-placement on item: coefficient change (party FE)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_coef_fe
ggsave(
  "graphs_misc/lr_self_item_coefs_over_time_partyFE.png",
  p_coef_fe, width = 12, height = 7.5, dpi = 300
)

# ============================================================
# Plot 2: FE R^2 over time for single-item regressions
# ============================================================
p_coef_r2_fe <- ggplot(
  coef_fe_all_lab %>% filter(var_short != "partei"),
  aes(x = year, y = r2, group = var, color = group)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ label, scales = "free_y") +
  scale_color_manual(values = cols_gc, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = NULL,
    y = expression(R^2),
    title = expression(R^2 ~ "for separate regressions (party FE)")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_coef_r2_fe
ggsave(
  "graphs_misc/lr_self_item_r2_over_time_partyFE.png",
  p_coef_r2_fe, width = 12, height = 7.5, dpi = 300
)



##### ============================================================
# R^2 of LR_self ~ party fixed effects only
# ============================================================

library(dplyr)
library(ggplot2)

r2_party_only_one_year <- function(df_year) {
  df_year <- df_year %>% filter(partei %in% c(2,3,4,5,6,7,322))
  y <- df_year$year[1]
  
  d <- df_year %>%
    select(LR_self, partei) %>%
    filter(is.finite(LR_self) & is.finite(partei))
  
  if (nrow(d) < 20) {
    return(tibble(year = y, model = "Party FE only", r2 = NA_real_, n = nrow(d)))
  }
  
  fit <- lm(LR_self ~ factor(partei), data = d)
  sm  <- summary(fit)
  
  tibble(year = y, model = "Party FE only", r2 = sm$r.squared, n = nrow(d))
}

r2_party_only <- bind_rows(
  r2_party_only_one_year(df13),
  r2_party_only_one_year(df17),
  r2_party_only_one_year(df21)
)

# Plot
p_r2_party_only <- ggplot(r2_party_only, aes(x = year, y = r2, group = 1)) +
  geom_line(linewidth = 1.0, color = "black") +
  geom_point(size = 2.6, color = "black") +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = NULL, y = expression(R^2),
       title = expression(R^2 ~ "explained by party FE only")) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

p_r2_party_only
ggsave("graphs_misc/lr_self_r2_partyFE_only_over_time.png",
       p_r2_party_only, width = 7.5, height = 4.6, dpi = 300)


# ============================================================
# Within-person VARIANCE across items:
#  - Econ variance (across econ_* items)
#  - Culture variance (across cult_* items)
#  - All variance (across econ_* + cult_* items)
# Then:
#  (1) Histograms by year (faceted by dimension)
#  (2) Mean variance over years (3 lines)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# --- your palette (same colors) ---
cols_gc <- c("Culture" = "#3CB371", "Econ" = "#1F4EAD")
cols_var <- c(cols_gc, "All" = "black")  # third line for joint econ+cult

# ---- helper: row-wise variance across selected columns ----
row_var <- function(df, cols) {
  m <- as.matrix(df[, cols, drop = FALSE])
  # var across items, per row; require at least 2 finite items
  apply(m, 1, function(v) {
    v <- v[is.finite(v)]
    if (length(v) < 2) NA_real_ else var(v)
  })
}

# ---- build variance df for one year ----
make_var_df_one_year <- function(df_year) {
  pred_cols <- setdiff(names(df_year), c("year", "LR_self", "partei"))
  econ_cols <- pred_cols[grepl("^econ_", pred_cols)]
  cult_cols <- pred_cols[grepl("^cult_", pred_cols)]
  all_cols  <- c(econ_cols, cult_cols)
  
  out <- df_year %>%
    mutate(
      var_econ = row_var(., econ_cols),
      var_cult = row_var(., cult_cols),
      var_all  = row_var(., all_cols)
    ) %>%
    select(year, partei, var_econ, var_cult, var_all)
  
  out
}

var13 <- make_var_df_one_year(df13)
var17 <- make_var_df_one_year(df17)
var21 <- make_var_df_one_year(df21)

var_all <- bind_rows(var13, var17, var21)

# ============================================================
# (1) HISTOGRAMS by year (facet by dimension)
# ============================================================

var_long <- var_all %>%
  pivot_longer(
    cols = c(var_econ, var_cult, var_all),
    names_to = "dimension",
    values_to = "variance"
  ) %>%
  mutate(
    dimension = recode(
      dimension,
      var_econ = "Econ",
      var_cult = "Culture",
      var_all  = "All"
    ),
    dimension = factor(dimension, levels = c("Econ", "Culture", "All"))
  )

# Make one histogram plot per year (saved separately)
make_hist_year <- function(y) {
  dfy <- var_long %>% filter(year == y)
  
  p <- ggplot(dfy, aes(x = variance, fill = dimension)) +
    geom_histogram(bins = 35, alpha = 0.55, color = "grey25") +
    facet_wrap(~ dimension, nrow = 1, scales = "free_x") +
    scale_fill_manual(values = cols_var, guide = "none") +
    labs(
      x = "Within-person variance across items",
      y = "Count",
      title = paste0("Within-person item variance (", y, ")")
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  
  p
}

p_hist_2013 <- make_hist_year(2013)
p_hist_2017 <- make_hist_year(2017)
p_hist_2021 <- make_hist_year(2021)

p_hist_2013
p_hist_2017
p_hist_2021

ggsave("graphs_misc/var_hist_2013.png", p_hist_2013, width = 12, height = 4.2, dpi = 300)
ggsave("graphs_misc/var_hist_2017.png", p_hist_2017, width = 12, height = 4.2, dpi = 300)
ggsave("graphs_misc/var_hist_2021.png", p_hist_2021, width = 12, height = 4.2, dpi = 300)

# ============================================================
# (2) MEAN variance over years: 3 lines (Econ/Culture/All)
# ============================================================

mean_var <- var_long %>%
  group_by(year, dimension) %>%
  summarise(mean_var = mean(variance, na.rm = TRUE), n = sum(is.finite(variance)), .groups = "drop")

p_mean_var <- ggplot(mean_var, aes(x = year, y = mean_var, color = dimension, group = dimension)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.6) +
  scale_color_manual(values = cols_var, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  labs(
    x = NULL,
    y = "Mean within-person variance",
    title = "Mean within-person variance across items over time"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_mean_var
ggsave("graphs_misc/mean_within_person_variance_over_time.png",
       p_mean_var, width = 8.5, height = 4.8, dpi = 300)


# ============================================================
# Mean within-person variance over time BY PARTY (faceted)
# Requires: var_long (year, partei, dimension, variance)
# ============================================================

library(dplyr)
library(ggplot2)

# If you have party_lut_codes + party_labels already, use them.
# party_lut_codes: columns orig, party_id
# party_labels: named vector mapping party_id -> label

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

party_labels <- c("1"="CDU/CSU", "2"="SPD","3"="GRUENE","4"="FDP","5"="LINKE","6"="AfD")


# 1) attach party labels
var_long_party <- var_long %>%
  left_join(party_lut_codes, by = c("partei" = "orig")) %>%
  mutate(
    party = factor(party_id, levels = names(party_labels), labels = party_labels)
  ) %>%
  filter(!is.na(party))

# 2) mean by year × party × dimension
mean_var_party <- var_long_party %>%
  group_by(year, party, dimension) %>%
  summarise(mean_var = mean(variance, na.rm = TRUE),
            n = sum(is.finite(variance)),
            .groups = "drop")

# colors (same palette)
cols_var <- c("Culture" = "#3CB371", "Econ" = "#1F4EAD", "All" = "black")

# 3) plot: facet by party
p_mean_var_party <- ggplot(mean_var_party,
                           aes(x = year, y = mean_var, color = dimension, group = dimension)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.4) +
  facet_wrap(~ party, ncol = 3, scales = "free_y") +
  scale_color_manual(values = cols_var, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021)) +
  labs(x = NULL, y = "Mean within-person variance",
       title = "Mean within-person variance across items over time (by party)") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_mean_var_party
ggsave("graphs_misc/mean_within_person_variance_over_time_by_party.png",
       p_mean_var_party, width = 12, height = 7.2, dpi = 300)



library(dplyr)
library(ggplot2)

# 1) mean + SE by year × party × dimension
mean_var_party <- var_long_party %>%
  group_by(year, party, dimension) %>%
  summarise(
    n        = sum(is.finite(variance)),
    mean_var = mean(variance, na.rm = TRUE),
    sd_var   = sd(variance, na.rm = TRUE),
    se_var   = sd_var / sqrt(n),
    .groups  = "drop"
  ) %>%
  mutate(
    x_plot = year + case_when(
      dimension == "Econ"    ~ -0.18,
      dimension == "Culture" ~  0.00,
      dimension == "All"     ~  0.18,
      TRUE ~ 0
    )
  )

cols_var <- c("Culture" = "#3CB371", "Econ" = "#1F4EAD", "All" = "black")

# 2) plot (shifted horizontally)
p_mean_var_party <- ggplot(
  mean_var_party,
  aes(x = x_plot, y = mean_var, color = dimension, group = dimension)
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.4) +
  geom_errorbar(
    aes(ymin = mean_var - 1.96 * se_var, ymax = mean_var + 1.96 * se_var),
    width = 0.08, linewidth = 0.5, alpha = 0.9
  ) +
  facet_wrap(~ party, ncol = 3, scales = "free_y") +
  scale_color_manual(values = cols_var, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021), labels = c(2013, 2017, 2021)) +
  labs(
    x = NULL, y = "Mean within-person variance",
    title = "Mean within-person variance across items over time (by party)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_mean_var_party
ggsave("graphs_misc/mean_within_person_variance_over_time_by_party_errorbar.png",
       p_mean_var_party, width = 12, height = 7.2, dpi = 300)




library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 1) Helper: row-wise variance across selected columns
# ------------------------------------------------------------
row_var <- function(df, cols) {
  m <- as.matrix(df[, cols, drop = FALSE])
  apply(m, 1, function(v) {
    v <- v[is.finite(v)]
    if (length(v) < 2) NA_real_ else var(v)
  })
}

# ------------------------------------------------------------
# 2) Standardize predictors within (year × party)
#    (we only standardize X's, not LR_self)
# ------------------------------------------------------------
within_party_z <- function(df_year) {
  pred_cols <- setdiff(names(df_year), c("year", "LR_self", "partei"))
  
  df_year %>%
    filter(partei %in% c(2,3,4,5,6,7,322)) %>%
    group_by(year, partei) %>%
    mutate(across(all_of(pred_cols), ~ as.numeric(scale(.x)))) %>%
    ungroup()
}

df13_zp <- within_party_z(df13)
df17_zp <- within_party_z(df17)
df21_zp <- within_party_z(df21)

df_all_zp <- bind_rows(df13_zp, df17_zp, df21_zp)

# ------------------------------------------------------------
# 3) Compute within-person variances (econ / culture / all)
# ------------------------------------------------------------
make_var_df <- function(df_year_zp) {
  pred_cols <- setdiff(names(df_year_zp), c("year", "LR_self", "partei"))
  econ_cols <- pred_cols[grepl("^econ_", pred_cols)]
  cult_cols <- pred_cols[grepl("^cult_", pred_cols)]
  all_cols  <- c(econ_cols, cult_cols)
  
  df_year_zp %>%
    mutate(
      var_econ = row_var(., econ_cols),
      var_cult = row_var(., cult_cols),
      var_all  = row_var(., all_cols)
    ) %>%
    select(year, partei, var_econ, var_cult, var_all)
}

var_all_partyZ <- bind_rows(
  make_var_df(df13_zp),
  make_var_df(df17_zp),
  make_var_df(df21_zp)
)

var_long_partyZ <- var_all_partyZ %>%
  pivot_longer(
    cols = c(var_econ, var_cult, var_all),
    names_to = "dimension",
    values_to = "variance"
  ) %>%
  mutate(
    dimension = recode(
      dimension,
      var_econ = "Econ",
      var_cult = "Culture",
      var_all  = "All"
    ),
    dimension = factor(dimension, levels = c("Econ", "Culture", "All"))
  )

# ------------------------------------------------------------
# 4) Attach party labels + compute mean & SE
# ------------------------------------------------------------
var_long_partyZ <- var_long_partyZ %>%
  left_join(party_lut_codes, by = c("partei" = "orig")) %>%
  mutate(party = factor(party_id, levels = names(party_labels), labels = party_labels)) %>%
  filter(!is.na(party))

mean_var_partyZ <- var_long_partyZ %>%
  group_by(year, party, dimension) %>%
  summarise(
    n        = sum(is.finite(variance)),
    mean_var = mean(variance, na.rm = TRUE),
    sd_var   = sd(variance, na.rm = TRUE),
    se_var   = sd_var / sqrt(n),
    .groups  = "drop"
  ) %>%
  mutate(
    # small horizontal shifts so Econ/Culture/All are visible
    x_plot = year + case_when(
      dimension == "Econ"    ~ -0.18,
      dimension == "Culture" ~  0.00,
      dimension == "All"     ~  0.18,
      TRUE ~ 0
    )
  )

# ------------------------------------------------------------
# 5) Plot: faceted by party (within-party standardized)
# ------------------------------------------------------------
cols_var <- c("Culture" = "#3CB371", "Econ" = "#1F4EAD", "All" = "black")

p_mean_var_partyZ <- ggplot(
  mean_var_partyZ,
  aes(x = x_plot, y = mean_var, color = dimension, group = dimension)
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.4) +
  geom_errorbar(
    aes(ymin = mean_var - 1.96 * se_var, ymax = mean_var + 1.96 * se_var),
    width = 0.08, linewidth = 0.5, alpha = 0.9
  ) +
  facet_wrap(~ party, ncol = 3, scales = "free_y") +
  scale_color_manual(values = cols_var, name = NULL) +
  scale_x_continuous(breaks = c(2013, 2017, 2021), labels = c(2013, 2017, 2021)) +
  labs(
    x = NULL,
    y = "Mean within-person variance (items z-scored within party-year)",
    title = "Within-person variance over time (standardized within party × year)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

p_mean_var_partyZ
ggsave("graphs_misc/mean_within_person_variance_partyZ_over_time_by_party.png",
       p_mean_var_partyZ, width = 12, height = 7.2, dpi = 300)
