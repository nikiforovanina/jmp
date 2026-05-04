# ============================================================
# FULL WORKING CODE: 2013 / 2017 / 2021
# - combined (all-years) plots WITH year facets
# - PLUS separate plots per year (3x: violin, ridges, 2D density)
# - saves all PNGs
# ============================================================

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(ggridges)

# ---- Paths ----
data_dir  <- "/Users/ninanikiforova/Desktop/жмп/data_construction/4_full_data/data"
out_dir   <- "/Users/ninanikiforova/Desktop/жмп/data_construction/4_full_data/graphs/g_descr"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

years <- c(13, 17, 21)

# ---- Party mapping ----
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

party_names <- c(
  `1`="CDU", `2`="SPD", `3`="Greens", `4`="FDP", `5`="Left", `6`="AfD"
)

party_cols <- c(
  "CDU"="#000000",
  "FDP"="#FFED07",
  "SPD"="#E3000F",
  "Greens"="#1AA037",
  "Left"="#BE3075",
  "AfD"="#009EE0"
)

# ---- Helper: load dat_nat from each RData reliably ----
load_dat_nat <- function(path) {
  e <- new.env(parent = emptyenv())
  nms <- load(path, envir = e)
  if (!("dat_nat" %in% nms)) stop("File does not contain object 'dat_nat': ", path)
  e$dat_nat
}

# ---- Load + combine years ----
all_dat <- lapply(years, function(yy) {
  f <- file.path(data_dir, sprintf("full_cand_position_%02d.RData", yy))
  if (!file.exists(f)) stop("Missing file: ", f)
  
  dat_nat <- load_dat_nat(f)
  
  req <- c("partei", "econ_lr_score", "anti_elite_score")
  if (!all(req %in% names(dat_nat))) {
    stop("Missing required columns in ", f, ": ",
         paste(setdiff(req, names(dat_nat)), collapse = ", "))
  }
  
  dat_nat %>%
    mutate(year = 2000 + yy) %>%
    inner_join(party_lut_codes, by = c("partei" = "orig")) %>%
    mutate(
      party_name = recode(as.character(party_id), !!!party_names),
      party_name = factor(party_name, levels = c("CDU","SPD","Greens","FDP","Left","AfD"))
    ) %>%
    select(year, party_id, party_name, econ_lr_score, anti_elite_score,
           any_of("wknr"))
}) %>%
  bind_rows()

# Long format for 1D distribution plots
df_long_all <- all_dat %>%
  pivot_longer(
    cols = c(econ_lr_score, anti_elite_score),
    names_to = "dimension",
    values_to = "score"
  ) %>%
  mutate(
    dimension = recode(dimension,
                       econ_lr_score = "Left-Right",
                       anti_elite_score = "Anti-elite"),
    dimension = factor(dimension, levels = c("Left-Right", "Anti-elite")),
    year = factor(year)
  )

party_order <- c("CDU", "FDP", "SPD", "Greens", "Left", "AfD")

df_long_all <- df_long_all %>%
  mutate(party_name = factor(party_name, levels = party_order))

# ============================================================
# A) COMBINED PLOTS (faceted by year)
# ============================================================

# ---- A1) Violin+box+mean: facet year x dimension ----
violin_plot_all <- ggplot(df_long_all, aes(x = party_name, y = score, fill = party_name)) +
  geom_violin(trim = FALSE, alpha = 0.35) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.25, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_grid(year ~ dimension, scales = "free_y") +
  scale_fill_manual(values = party_cols) +
  labs(x = NULL, y = NULL, title = "Candidate positions by party (distributions)") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

ggsave(file.path(out_dir, "violin_plot_dimensions_ALLYEARS.png"),
       violin_plot_all, width = 10.5, height = 8, dpi = 300)

# ---- A2) Ridgelines: facet year x dimension ----
ridge_plot_all <- ggplot(df_long_all, aes(x = score, y = party_name, fill = party_name)) +
  geom_density_ridges(alpha = 0.5, scale = 1.2, rel_min_height = 0.01) +
  facet_grid(year ~ dimension, scales = "free_x") +
  scale_fill_manual(values = party_cols) +
  labs(x = NULL, y = NULL, title = "Candidate positions by party (densities)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")

ggsave(file.path(out_dir, "ridge_plot_dimensions_ALLYEARS.png"),
       ridge_plot_all, width = 10.5, height = 8, dpi = 300)

# ---- A3) 2D density: facet by year ----
dens_positions_all <- ggplot(all_dat, aes(x = econ_lr_score, y = anti_elite_score)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.25, n = 80) +
  facet_wrap(~ year, nrow = 1) +
  labs(x = "Left-Right", y = "Anti-elite", title = "Joint distribution of positions") +
  theme_minimal(base_size = 14)

ggsave(file.path(out_dir, "dens_positions_ALLYEARS.png"),
       dens_positions_all, width = 10.5, height = 4, dpi = 300)

# ============================================================
# B) SEPARATE PLOTS PER YEAR (3 years × 3 plot types)
# ============================================================

for (yr in sort(unique(all_dat$year))) {
  
  dat_y  <- all_dat %>% filter(year == yr)
  long_y <- dat_y %>%
    pivot_longer(c(econ_lr_score, anti_elite_score),
                 names_to = "dimension", values_to = "score") %>%
    mutate(
      dimension = recode(dimension,
                         econ_lr_score = "Left-Right",
                         anti_elite_score = "Anti-elite"),
      dimension = factor(dimension, levels = c("Left-Right", "Anti-elite"))
    )
  
  party_order <- c("AfD", "CDU", "FDP", "SPD", "Greens", "Left")
  
  long_y <- long_y %>%
    mutate(party_name = factor(party_name, levels = party_order))
  
  # ---- B1) Violin plot for this year ----
  p_violin <- ggplot(long_y, aes(x = party_name, y = score, fill = party_name)) +
    geom_violin(trim = FALSE, alpha = 0.35) +
    geom_boxplot(width = 0.15, outlier.alpha = 0.25, alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", size = 2) +
    facet_wrap(~ dimension, scales = "free_y") +
    scale_fill_manual(values = party_cols) +
    labs(x = NULL, y = NULL, title = paste0("Candidate positions by party (", yr, ")")) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 25, hjust = 1)
    )
  
  ggsave(file.path(out_dir, paste0("violin_plot_dimensions_", yr, ".png")),
         p_violin, width = 8, height = 4, dpi = 300)
  
  # ---- B2) Ridgeline plot for this year ----
  p_ridge <- ggplot(long_y, aes(x = score, y = party_name, fill = party_name)) +
    geom_density_ridges(alpha = 0.5, scale = 1.2, rel_min_height = 0.01) +
    facet_wrap(~ dimension, scales = "free_x") +
    scale_fill_manual(values = party_cols) +
    labs(x = NULL, y = NULL, title = paste0("Candidate positions by party (", yr, ")")) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "none")
  
  ggsave(file.path(out_dir, paste0("ridge_plot_dimensions_", yr, ".png")),
         p_ridge, width = 8, height = 4, dpi = 300)
  
  # ---- B3) 2D density plot for this year ----
  p_2d <- ggplot(dat_y, aes(x = econ_lr_score, y = anti_elite_score)) +
    stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.25, n = 80) +
    labs(x = "Left-Right", y = "Anti-elite", title = paste0("Joint distribution of positions (", yr, ")")) +
    theme_minimal(base_size = 14)
  
  ggsave(file.path(out_dir, paste0("dens_positions_", yr, ".png")),
         p_2d, width = 8, height = 4, dpi = 300)
}

# ---- Optional: show combined plots in session ----
print(violin_plot_all)
print(ridge_plot_all)
print(dens_positions_all)

message("Saved all plots to: ", out_dir)

#############
# ============================================================
# ADD-ON: Party evolution over time (2013 -> 2017 -> 2021)
# - computes party-year means on both dimensions
# - draws trajectories in 2D with arrows and year labels
# - saves PNG
# NOTE: run AFTER you have created `all_dat`, `party_cols`, etc.
# ============================================================

library(dplyr)
library(ggplot2)

# 1) Party-year means
party_year_means <- all_dat %>%
  group_by(year, party_name) %>%
  summarise(
    econ_mean = mean(econ_lr_score, na.rm = TRUE),
    anti_mean = mean(anti_elite_score, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  arrange(party_name, year)

# 2) Build segments (start -> end) for arrows between consecutive elections
party_year_arrows <- party_year_means %>%
  group_by(party_name) %>%
  arrange(year) %>%
  mutate(
    econ_next = lead(econ_mean),
    anti_next = lead(anti_mean),
    year_next = lead(year)
  ) %>%
  filter(!is.na(year_next)) %>%
  ungroup()

# 3) Plot: points + arrows + year labels
evolution_plot <- ggplot() +
  # arrows between years for each party
  geom_segment(
    data = party_year_arrows,
    aes(x = econ_mean, y = anti_mean, xend = econ_next, yend = anti_next, color = party_name),
    arrow = arrow(length = unit(0.18, "cm")),
    linewidth = 1
  ) +
  # points at each year
  geom_point(
    data = party_year_means,
    aes(x = econ_mean, y = anti_mean, color = party_name),
    size = 3
  ) +
  # year labels at each point
  geom_text(
    data = party_year_means,
    aes(x = econ_mean, y = anti_mean, label = year, color = party_name),
    vjust = -1.0, size = 4, show.legend = FALSE
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.5) +
  scale_color_manual(values = party_cols) +
  labs(
    x = "Left-Right",
    y = "Anti-elite",
    color = "Party",
    title = "Evolution of party mean positions"
  ) +
  theme_minimal(base_size = 14)

# 4) Save
evolution_path <- file.path(out_dir, "party_evolution_arrows_2013_2017_2021.png")
ggsave(evolution_path, evolution_plot, width = 9, height = 6, dpi = 300)

# 5) Print for inspection
print(party_year_means)
print(evolution_plot)
message("Saved evolution plot to: ", evolution_path)

evolution_plot <-  ggplot() +
  # arrows between years for each party
  geom_segment(
    data = party_year_arrows,
    aes(x = econ_mean, y = anti_mean, xend = econ_next, yend = anti_next, color = party_name),
    arrow = arrow(length = unit(0.18, "cm")),
    linewidth = 1
  ) +
  # points at each year
  geom_point(
    data = party_year_means,
    aes(x = econ_mean, y = anti_mean, color = party_name),
    size = 3
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.5) +
  scale_color_manual(values = party_cols) +
  labs(
    x = "Left-Right",
    y = "Anti-elite",
    color = "Party",
    title = "Evolution of party mean positions"
  ) +
  theme_minimal(base_size = 14) +
  ggrepel::geom_text_repel(
    data = party_year_means,
    aes(x = econ_mean, y = anti_mean, label = year, color = party_name),
    size = 4, show.legend = FALSE
  )


evolution_plot
ggsave(evolution_path, evolution_plot, width = 9, height = 6, dpi = 300)


# ============================================================
# ADD-ON: Party evolution over time — MEDIANS
# Mirror of the means section above, using median instead of mean
# ============================================================

# 1) Party-year medians
party_year_medians <- all_dat %>%
  group_by(year, party_name) %>%
  summarise(
    econ_median = median(econ_lr_score, na.rm = TRUE),
    anti_median = median(anti_elite_score, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  arrange(party_name, year)

# 2) Segments for arrows between consecutive elections
party_year_arrows_med <- party_year_medians %>%
  group_by(party_name) %>%
  arrange(year) %>%
  mutate(
    econ_next = lead(econ_median),
    anti_next = lead(anti_median),
    year_next = lead(year)
  ) %>%
  filter(!is.na(year_next)) %>%
  ungroup()

# 3) Plot
evolution_plot_med <- ggplot() +
  geom_segment(
    data = party_year_arrows_med,
    aes(x = econ_median, y = anti_median,
        xend = econ_next, yend = anti_next, color = party_name),
    arrow = arrow(length = unit(0.18, "cm")),
    linewidth = 1
  ) +
  geom_point(
    data = party_year_medians,
    aes(x = econ_median, y = anti_median, color = party_name),
    size = 3
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.5) +
  scale_color_manual(values = party_cols) +
  labs(
    x = "Left-Right",
    y = "Anti-elite",
    color = "Party",
    title = "Evolution of party median positions"
  ) +
  theme_minimal(base_size = 14) +
  ggrepel::geom_text_repel(
    data = party_year_medians,
    aes(x = econ_median, y = anti_median, label = year, color = party_name),
    size = 4, show.legend = FALSE
  )

# 4) Save
evolution_med_path <- file.path(out_dir, "party_evolution_arrows_MEDIANS_2013_2017_2021.png")
ggsave(evolution_med_path, evolution_plot_med, width = 9, height = 6, dpi = 300)

print(party_year_medians)
print(evolution_plot_med)
message("Saved median evolution plot to: ", evolution_med_path)


# ============================================================
# ADD-ON: Federal state median positions and changes — barplots
# Uses wknr to derive Bundesland; computes median per state × year,
# then plots the change across election periods as bar charts.
# ============================================================

if ("wknr" %in% names(all_dat)) {

  # ── State metadata ─────────────────────────────────────────
  STATE_META <- tibble::tribble(
    ~wknr_min, ~wknr_max, ~state_name,              ~east_west,
    1L,   10L, "Schleswig-Holstein",    "West",
    11L,  13L, "Hamburg",               "West",
    14L,  35L, "Niedersachsen",         "West",
    36L,  37L, "Bremen",                "West",
    38L, 101L, "Nordrhein-Westfalen",   "West",
    102L,122L, "Hessen",                "West",
    123L,136L, "Rheinland-Pfalz",       "West",
    137L,174L, "Baden-Württemberg",     "West",
    175L,222L, "Bayern",                "West",
    223L,226L, "Saarland",              "West",
    227L,235L, "Berlin",                "East/West",
    236L,247L, "Brandenburg",           "East",
    248L,253L, "Mecklenburg-Vorpommern","East",
    254L,268L, "Sachsen",               "East",
    269L,276L, "Sachsen-Anhalt",        "East",
    277L,285L, "Thüringen",             "East"
  )

  wknr_to_state <- function(wknr) {
    dplyr::case_when(
      wknr <= 10  ~ "Schleswig-Holstein",
      wknr <= 13  ~ "Hamburg",
      wknr <= 35  ~ "Niedersachsen",
      wknr <= 37  ~ "Bremen",
      wknr <= 101 ~ "Nordrhein-Westfalen",
      wknr <= 122 ~ "Hessen",
      wknr <= 136 ~ "Rheinland-Pfalz",
      wknr <= 174 ~ "Baden-Württemberg",
      wknr <= 222 ~ "Bayern",
      wknr <= 226 ~ "Saarland",
      wknr <= 235 ~ "Berlin",
      wknr <= 247 ~ "Brandenburg",
      wknr <= 253 ~ "Mecklenburg-Vorpommern",
      wknr <= 268 ~ "Sachsen",
      wknr <= 276 ~ "Sachsen-Anhalt",
      wknr <= 285 ~ "Thüringen",
      TRUE        ~ NA_character_
    )
  }

  east_states <- c("Brandenburg", "Mecklenburg-Vorpommern",
                   "Sachsen", "Sachsen-Anhalt", "Thüringen")

  # ── State × year medians ────────────────────────────────────
  state_yr_med <- all_dat %>%
    filter(!is.na(wknr)) %>%
    mutate(state_name = wknr_to_state(wknr)) %>%
    filter(!is.na(state_name)) %>%
    group_by(year, state_name) %>%
    summarise(
      econ_median = median(econ_lr_score,    na.rm = TRUE),
      anti_median = median(anti_elite_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      east_west  = if_else(state_name %in% east_states, "East", "West"),
      state_name = factor(state_name,
                          levels = STATE_META$state_name)  # geographic order
    )

  # ── Median changes between election periods ─────────────────
  state_changes <- state_yr_med %>%
    arrange(state_name, year) %>%
    group_by(state_name) %>%
    mutate(
      d_econ_1317 = econ_median[year == 2017] - econ_median[year == 2013],
      d_anti_1317 = anti_median[year == 2017] - anti_median[year == 2013],
      d_econ_1721 = econ_median[year == 2021] - econ_median[year == 2017],
      d_anti_1721 = anti_median[year == 2021] - anti_median[year == 2017],
      d_econ_1321 = econ_median[year == 2021] - econ_median[year == 2013],
      d_anti_1321 = anti_median[year == 2021] - anti_median[year == 2013]
    ) %>%
    slice(1) %>%   # one row per state
    ungroup() %>%
    select(state_name, east_west, starts_with("d_"))

  # Long format for plotting
  state_changes_long <- state_changes %>%
    pivot_longer(
      cols = starts_with("d_"),
      names_to  = "variable",
      values_to = "change"
    ) %>%
    mutate(
      dimension = if_else(grepl("econ", variable), "Left-Right", "Anti-elite"),
      period    = dplyr::case_when(
        grepl("1317", variable) ~ "2013->2017",
        grepl("1721", variable) ~ "2017->2021",
        grepl("1321", variable) ~ "2013->2021"
      ),
      period    = factor(period, levels = c("2013->2017","2017->2021","2013->2021")),
      dimension = factor(dimension, levels = c("Left-Right","Anti-elite"))
    )

  east_west_cols <- c("East" = "#D55E00", "West" = "#0072B2")

  # ── Plot 1: Change in Left-Right median by state × period ──
  p_state_econ <- state_changes_long %>%
    filter(dimension == "Left-Right") %>%
    ggplot(aes(x = change, y = state_name, fill = east_west)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    facet_wrap(~ period, nrow = 1) +
    scale_fill_manual(values = east_west_cols) +
    labs(
      x = "Median change in Left-Right score",
      y = NULL,
      fill = NULL,
      title = "Change in median Left-Right position by federal state"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")

  state_econ_path <- file.path(out_dir, "state_median_change_econ_barplot.png")
  ggsave(state_econ_path, p_state_econ, width = 11, height = 7, dpi = 300)

  # ── Plot 2: Change in Anti-elite median by state × period ──
  p_state_anti <- state_changes_long %>%
    filter(dimension == "Anti-elite") %>%
    ggplot(aes(x = change, y = state_name, fill = east_west)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    facet_wrap(~ period, nrow = 1) +
    scale_fill_manual(values = east_west_cols) +
    labs(
      x = "Median change in Anti-elite score",
      y = NULL,
      fill = NULL,
      title = "Change in median Anti-elite position by federal state"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")

  state_anti_path <- file.path(out_dir, "state_median_change_anti_barplot.png")
  ggsave(state_anti_path, p_state_anti, width = 11, height = 7, dpi = 300)

  # ── Plot 3: Both dimensions combined (facet dim × period) ──
  p_state_both <- state_changes_long %>%
    filter(period != "2013->2021") %>%   # keep the two consecutive periods
    ggplot(aes(x = change, y = state_name, fill = east_west)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    facet_grid(dimension ~ period, scales = "free_x") +
    scale_fill_manual(values = east_west_cols) +
    labs(
      x = "Median change in score",
      y = NULL,
      fill = NULL,
      title = "Median position changes by federal state and dimension"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  state_both_path <- file.path(out_dir, "state_median_change_both_barplot.png")
  ggsave(state_both_path, p_state_both, width = 12, height = 9, dpi = 300)

  print(p_state_econ)
  print(p_state_anti)
  print(p_state_both)
  message("Saved state barplots to: ", out_dir)

} else {
  message("Column 'wknr' not found in all_dat — state barplots skipped.")
}
