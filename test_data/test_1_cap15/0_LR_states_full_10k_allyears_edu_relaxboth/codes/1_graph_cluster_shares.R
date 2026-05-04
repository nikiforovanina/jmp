library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)
library(ggplot2)
library(scales)

# SPEC_TAG (e.g. "5k_relaxboth") drives the graph filenames so different
# specifications don't overwrite each other.
source("0_configuration.R")
stopifnot(exists("SPEC_TAG"), nzchar(SPEC_TAG))

# -----------------------------
# COLORS: Anti-Elite = green, Right = pink
# -----------------------------
cols <- c("Anti-Elite" = "#3CB371", "Right" = "#FF69B4")

# -----------------------------
# Read bounds
# -----------------------------
df2013 <- read.csv("bounds/afd_bounds_year2013_seed01.csv")
df2017 <- read.csv("bounds/afd_bounds_year2017_seed01.csv")
df2021 <- read.csv("bounds/afd_bounds_year2021_seed01.csv")

df2013 <- df2013[13:36,]
df2017 <- df2017[13:36,]
#df2021 <- df2021[25:48,]


df2021$abs_lower <- as.numeric(df2021$abs_lower)
df2021$abs_upper  <- as.numeric(df2021$abs_upper)
df2021$baseline_mae  <- as.numeric(df2021$baseline_mae)
df2021$sc_tol  <- as.numeric(df2021$sc_tol)

bounds_all <- bind_rows(df2013, df2017, df2021)

# -----------------------------
# Drop state-year combinations flagged in missing_only.csv
# (years stored as 2-digit there → convert to 4-digit to match bounds)
# -----------------------------
missing_combos <- read.csv("bounds/missing_only.csv") %>%
  mutate(
    year     = 2000L + as.integer(year),
    state_id = as.integer(state)
  ) %>%
  dplyr::select(year, state_id)

bounds_all <- bounds_all %>%
  anti_join(missing_combos, by = c("year", "state_id"))

# -----------------------------
# Helper: normalize AGS codes
# -----------------------------
normalize_code <- function(x) paste0(substr(x, 1, 4), substr(x, 9, nchar(x)))

# map group keys to census columns
map_tbl <- tibble::tribble(
  ~group_key,      ~count_col,
  "male_1825",     "male_18_24_share_count",
  "male_2535",     "male_25_34_share_count",
  "male_3545",     "male_35_44_share_count",
  "male_4560",     "male_45_59_share_count",
  "male_6070",     "male_60_69_share_count",
  "male_70plus",   "male_70plus_share_count",
  "female_1825",   "female_18_24_share_count",
  "female_2535",   "female_25_34_share_count",
  "female_3545",   "female_35_44_share_count",
  "female_4560",   "female_45_59_share_count",
  "female_6070",   "female_60_69_share_count",
  "female_70plus", "female_70plus_share_count"
)

# -----------------------------
# Load year-specific census and aggregate to state level
# -----------------------------
load_census_by_land <- function(yr) {
  fname <- sprintf("../../data/census_%d.Rdata", yr)
  env   <- new.env(parent = emptyenv())
  load(fname, envir = env)
  census <- get(ls(env)[1], envir = env)

  census$ags  <- vapply(census$code, normalize_code, character(1))
  census$ags  <- stringr::str_pad(census$ags, 8, pad = "0")
  census      <- census[!is.na(census$total_all), ]
  census$land <- substr(census$ags, 1, 2)

  census %>%
    mutate(
      across(
        ends_with("_share"),
        ~ .x * total_all,
        .names = "{.col}_count"
      )
    ) %>%
    group_by(land) %>%
    summarise(
      total_all = sum(total_all, na.rm = TRUE),
      across(ends_with("_count"), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}

# -----------------------------
# Turnout helpers (same as 0_2parts_XX.R)
# -----------------------------
aggregate_turnout_to_6 <- function(turnout10) {
  if (is.null(turnout10) || !length(turnout10) || is.null(names(turnout10)))
    return(setNames(rep(NA_real_, 6), c("18_24","25_34","35_44","45_59","60_69","70plus")))
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

age_suffix_to_band <- c(
  "1825"   = "18_24",
  "2535"   = "25_34",
  "3545"   = "35_44",
  "4560"   = "45_59",
  "6070"   = "60_69",
  "70plus" = "70plus"
)

# -----------------------------
# Build group_sizes per year with turnout adjustment
# -----------------------------
years_all <- c(2013, 2017, 2021)

group_sizes <- purrr::map_dfr(years_all, function(yr) {

  cbl <- load_census_by_land(yr)

  gs_raw <- cbl %>%
    mutate(state_id = as.integer(land)) %>%
    dplyr::select(state_id, all_of(map_tbl$count_col)) %>%
    pivot_longer(
      cols      = -state_id,
      names_to  = "count_col",
      values_to = "group_size"
    ) %>%
    left_join(map_tbl, by = "count_col") %>%
    dplyr::select(state_id, group_key, group_size)

  turnout_path <- sprintf("../../targets/RW_%d_6groups_turnout.R", yr)
  if (!file.exists(turnout_path)) {
    message(sprintf("No turnout file for %d; skipping turnout adjustment.", yr))
    return(mutate(gs_raw, year = yr))
  }
  turnout_env <- new.env(parent = globalenv())
  sys.source(turnout_path, envir = turnout_env)

  gs_raw %>%
    group_by(state_id) %>%
    group_modify(function(df, key) {
      sid <- key$state_id
      tr  <- tryCatch(
        turnout_env$get_age_gender_turnout(sid),
        error = function(e) {
          message(sprintf("  Turnout lookup failed for state %d, year %d: %s",
                          sid, yr, conditionMessage(e)))
          NULL
        }
      )
      if (is.null(tr)) return(df)

      male6   <- aggregate_turnout_to_6(tr$male_turnout)
      female6 <- aggregate_turnout_to_6(tr$female_turnout)
      if (max(male6,   na.rm = TRUE) > 1.5) male6   <- male6 / 100
      if (max(female6, na.rm = TRUE) > 1.5) female6 <- female6 / 100

      for (j in seq_len(nrow(df))) {
        gk       <- df$group_key[j]
        age_raw  <- gsub("^(male|female)_", "", gk)
        age_band <- age_suffix_to_band[age_raw]
        if (is.na(age_band)) next

        w <- if (grepl("^male_", gk)) male6[age_band] else female6[age_band]
        if (is.finite(w) && !is.na(w) && w > 0)
          df$group_size[j] <- df$group_size[j] * w
      }
      df
    }) %>%
    ungroup() %>%
    mutate(year = yr)
})

# -----------------------------
# Read RW targets (AfD share party 6)
# -----------------------------
years <- c(2013, 2017, 2021)
paths <- file.path("../../targets", sprintf("RW_%d_6groups.R", years))
paths <- paths[file.exists(paths)]
stopifnot(length(paths) > 0)

expected_groups <- c(
  "male_1825","male_2535","male_3545","male_4560","male_6070","male_70plus",
  "female_1825","female_2535","female_3545","female_4560","female_6070","female_70plus"
)

pull_p6_from_file <- function(path) {
  yr <- as.integer(stringr::str_match(basename(path), "RW_(\\d{4})_")[, 2])

  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)

  if (!exists("get_age_gender_targets", envir = env, inherits = FALSE)) {
    stop("`get_age_gender_targets` not found in ", basename(path))
  }

  states <- 1:16
  purrr::map_dfr(states, function(sid) {
    targets <- tryCatch(
      env$get_age_gender_targets(sid),
      error = function(e) {
        message(sprintf("Skipping STATE = %d for %d: %s", sid, yr, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(targets)) return(tibble())

    groups_here <- intersect(names(targets), expected_groups)
    if (length(groups_here) == 0) return(tibble())

    tibble(
      year      = yr,
      state_id  = sid,
      group_key = groups_here,
      share_p6  = vapply(groups_here, function(g) as.numeric(targets[[g]][6]), numeric(1))
    )
  })
}

p6_shares <- purrr::map_dfr(paths, pull_p6_from_file)

# Merge p6_shares with turnout-adjusted, year-specific group_sizes
afd_state_year <- p6_shares %>%
  left_join(group_sizes, by = c("year", "state_id", "group_key")) %>%
  group_by(year, state_id) %>%
  summarise(
    pop_all       = sum(group_size, na.rm = TRUE),
    afd_voters    = sum(share_p6 * group_size, na.rm = TRUE),
    afd_share_all = afd_voters / pop_all,
    .groups       = "drop"
  )

# -----------------------------
# Merge bounds with counts
# -----------------------------
bounds_with_counts <- bounds_all %>%
  left_join(afd_state_year, by = c("year", "state_id")) %>%
  mutate(
    lower_count     = abs_lower * pop_all,
    upper_count     = abs_upper * pop_all,
    share_AfD_lower = lower_count / afd_voters,
    share_AfD_upper = upper_count / afd_voters
  )

# -----------------------------
# Turnout denominators — one row per (state × year), de-duplicated from
# bounds_with_counts so that only included state-years are counted
# (bounds_all was already filtered by missing_combos above)
# -----------------------------
turnout_by_state <- bounds_with_counts %>%
  distinct(year, state_id, pop_all) %>%
  mutate(east = as.integer(state_id > 11))

turnout_national <- turnout_by_state %>%
  group_by(year) %>%
  summarise(pop_turnout = sum(pop_all, na.rm = TRUE), .groups = "drop")

turnout_ew <- turnout_by_state %>%
  group_by(year, east) %>%
  summarise(pop_turnout = sum(pop_all, na.rm = TRUE), .groups = "drop")

# -----------------------------
# Aggregate across states → shares of total turnout (ALL Germany)
# -----------------------------
region_year_bounds <- bounds_with_counts %>%
  group_by(year, region) %>%
  summarise(
    lower_people = sum(lower_count, na.rm = TRUE),
    upper_people = sum(upper_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(turnout_national, by = "year") %>%
  mutate(
    lower_sh = lower_people / pop_turnout,
    upper_sh = upper_people / pop_turnout
  )

region_year_bounds$region_old <- region_year_bounds$region
region_year_bounds$region <- ifelse(region_year_bounds$region_old == "AntiEst", "Right", "Anti-Elite")
region_year_bounds$region <- factor(region_year_bounds$region, levels = c("Anti-Elite", "Right"))

########### PLOTS ###########
# ---- FACET plot (ALL) — shares of turnout
plot_total_facet <- ggplot(region_year_bounds, aes(x = year)) +
  geom_ribbon(aes(ymin = lower_sh, ymax = upper_sh, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_sh, color = region), linewidth = 0.6, linetype = "dotted") +
  geom_line(aes(y = upper_sh, color = region), linewidth = 0.6, linetype = "dotted") +
  facet_wrap(~ region, nrow = 1) +
  scale_fill_manual(values = cols, name = "") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds$year))) +
  scale_y_continuous(
    name   = "Share of total turnout",
    labels = label_percent(accuracy = 0.1)
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom"
  )

plot_total_facet
ggsave(sprintf("../graphs/plot_total_facet_shares_%s.png", SPEC_TAG),
       plot_total_facet, width = 6, height = 3.5)

# ---- NON-FACET plot (ALL)
plot_total <- ggplot(region_year_bounds, aes(x = year)) +
  geom_ribbon(aes(ymin = lower_sh, ymax = upper_sh, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_sh, color = region, group = region), linewidth = 0.6, linetype = "dotted") +
  geom_line(aes(y = upper_sh, color = region, group = region), linewidth = 0.6, linetype = "dotted") +
  scale_fill_manual(values = cols, name = "Region") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds$year))) +
  scale_y_continuous(
    name   = "Share of total turnout",
    labels = label_percent(accuracy = 0.1)
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom"
  )

ggsave(sprintf("../graphs/plot_total_shares_%s.png", SPEC_TAG),
       plot_total, width = 6, height = 3.5)

# -----------------------------
# EAST/WEST split
# -----------------------------
bounds_with_counts$east <- ifelse(bounds_with_counts$state_id > 11, 1, 0)

region_year_bounds_ew <- bounds_with_counts %>%
  group_by(year, region, east) %>%
  summarise(
    lower_people = sum(lower_count, na.rm = TRUE),
    upper_people = sum(upper_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(turnout_ew, by = c("year", "east")) %>%
  mutate(
    lower_sh = lower_people / pop_turnout,
    upper_sh = upper_people / pop_turnout
  )

region_year_bounds_ew$region_old <- region_year_bounds_ew$region
region_year_bounds_ew$region <- ifelse(region_year_bounds_ew$region_old == "AntiEst", "Right", "Anti-Elite")
region_year_bounds_ew$region <- factor(region_year_bounds_ew$region, levels = c("Anti-Elite", "Right"))

# ---- EAST facet
plot_east_facet <- ggplot(region_year_bounds_ew %>% filter(east == 1), aes(x = year)) +
  geom_ribbon(aes(ymin = lower_sh, ymax = upper_sh, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_sh, color = region), linewidth = 0.6, linetype = "dotted") +
  geom_line(aes(y = upper_sh, color = region), linewidth = 0.6, linetype = "dotted") +
  facet_wrap(~ region, nrow = 1) +
  scale_fill_manual(values = cols, name = "Region") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds_ew$year))) +
  scale_y_continuous(
    name   = "Share of East turnout",
    labels = label_percent(accuracy = 0.1)
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom"
  )

ggsave(sprintf("../graphs/plot_east_facet_shares_%s.png", SPEC_TAG),
       plot_east_facet, width = 6, height = 3.5)

# ---- EAST non-facet
plot_east <- ggplot(region_year_bounds_ew %>% filter(east == 1), aes(x = year)) +
  geom_ribbon(aes(ymin = lower_sh, ymax = upper_sh, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_sh, color = region, group = region), linewidth = 0.6, linetype = "dotted") +
  geom_line(aes(y = upper_sh, color = region, group = region), linewidth = 0.6, linetype = "dotted") +
  scale_fill_manual(values = cols, name = "Region") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds_ew$year))) +
  scale_y_continuous(
    name   = "Share of East turnout",
    labels = label_percent(accuracy = 0.1)
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom"
  )

ggsave(sprintf("../graphs/plot_east_shares_%s.png", SPEC_TAG),
       plot_east, width = 6, height = 3.5)

# ---- WEST facet
plot_west_facet <- ggplot(region_year_bounds_ew %>% filter(east == 0), aes(x = year)) +
  geom_ribbon(aes(ymin = lower_sh, ymax = upper_sh, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_sh, color = region), linewidth = 0.6, linetype = "dotted") +
  geom_line(aes(y = upper_sh, color = region), linewidth = 0.6, linetype = "dotted") +
  facet_wrap(~ region, nrow = 1) +
  scale_fill_manual(values = cols, name = "Region") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds_ew$year))) +
  scale_y_continuous(
    name   = "Share of West turnout",
    labels = label_percent(accuracy = 0.1)
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom"
  )

ggsave(sprintf("../graphs/plot_west_facet_shares_%s.png", SPEC_TAG),
       plot_west_facet, width = 6, height = 3.5)

# ---- WEST non-facet
plot_west <- ggplot(region_year_bounds_ew %>% filter(east == 0), aes(x = year)) +
  geom_ribbon(aes(ymin = lower_sh, ymax = upper_sh, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_sh, color = region, group = region), linewidth = 0.6, linetype = "dotted") +
  geom_line(aes(y = upper_sh, color = region, group = region), linewidth = 0.6, linetype = "dotted") +
  scale_fill_manual(values = cols, name = "Region") +
  scale_color_manual(values = cols, guide = "none") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds_ew$year))) +
  scale_y_continuous(
    name   = "Share of West turnout",
    labels = label_percent(accuracy = 0.1)
  ) +
  labs(x = NULL) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom"
  )

ggsave(sprintf("../graphs/plot_west_shares_%s.png", SPEC_TAG),
       plot_west, width = 6, height = 3.5)
