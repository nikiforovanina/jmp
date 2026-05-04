library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

df2013 <- read.csv('bounds/afd_bounds_year2013.csv')
df2017 <- read.csv('bounds/afd_bounds_year2017.csv')
df2021 <- read.csv('bounds/afd_bounds_year2021.csv')

bounds_all <- bind_rows(df2013, df2017, df2021)
df <- bounds_all

load("../data/census_2021.Rdata")  

census <- census_shares_2021

normalize_code <- function(x) paste0(substr(x, 1, 4), substr(x, 9, nchar(x)))
census$ags <- vapply(census$code, normalize_code, character(1))
census$ags <- stringr::str_pad(census$ags, 8, pad = "0")
census      <- census %>% filter(!is.na(total_all))
census$land <- substr(census$ags, 1, 2)

census_by_land <- census %>%
  mutate(
    across(
      ends_with("_share"),
      ~ .x * total_all,             # if shares are in [0,1]; if 0–100, use (.x/100)*total_all
      .names = "{.col}_count"
    )
  ) %>%
  group_by(land) %>%
  summarise(
    total_all = sum(total_all, na.rm = TRUE),
    across(ends_with("_count"), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

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

# group_sizes: state × group_key × group_size
group_sizes <- census_by_land %>%
  mutate(state_id = as.integer(land)) %>%
  select(state_id, all_of(map_tbl$count_col)) %>%
  pivot_longer(
    cols      = -state_id,
    names_to  = "count_col",
    values_to = "group_size"
  ) %>%
  left_join(map_tbl, by = "count_col") %>%
  select(state_id, group_key, group_size)

## -----------------------------------------------------------
## 2. RW files: AfD share (party 6) by state, year, group_key
## -----------------------------------------------------------

years <- c(2013, 2017, 2021)
paths <- file.path("../targets", sprintf("RW_%d_6groups.R", years))
paths <- paths[file.exists(paths)]
stopifnot(length(paths) > 0)

expected_groups <- c(
  "male_1825","male_2535","male_3545","male_4560","male_6070","male_70plus",
  "female_1825","female_2535","female_3545","female_4560","female_6070","female_70plus")

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
      # AfD is the 6th element of each vector
      share_p6  = vapply(groups_here, function(g) as.numeric(targets[[g]][6]), numeric(1))
    )
  })
}

p6_shares <- purrr::map_dfr(paths, pull_p6_from_file)

# Merge p6_shares with group_sizes and aggregate across groups
afd_state_year <- p6_shares %>%
  left_join(group_sizes, by = c("state_id", "group_key")) %>%
  group_by(year, state_id) %>%
  summarise(
    pop_all      = sum(group_size, na.rm = TRUE),           # total people
    afd_voters   = sum(share_p6 * group_size, na.rm = TRUE),# total AfD voters
    afd_share_all = afd_voters / pop_all,                   # overall AfD share
    .groups      = "drop"
  )


bounds_with_counts <- bounds_all %>%
  left_join(afd_state_year, by = c("year", "state_id")) %>%
  mutate(
    # if abs_lower / abs_upper are in [0,1]; if 0–100, divide by 100 first
    lower_count     = abs_lower * pop_all,
    upper_count     = abs_upper * pop_all,
    share_AfD_lower = lower_count / afd_voters,
    share_AfD_upper = upper_count / afd_voters
  )


# 1) Aggregate across states: total people in bounds per year × region
region_year_bounds <- bounds_with_counts %>%
  group_by(year, region) %>%
  summarise(
    lower_people = sum(lower_count, na.rm = TRUE),
    upper_people = sum(upper_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # optional: put in millions for readability
  mutate(
    lower_m = lower_people / 1e6,
    upper_m = upper_people / 1e6
  )


region_year_bounds$region_old <- region_year_bounds$region
region_year_bounds$region <- ifelse(region_year_bounds$region_old == 'AntiEst', 'Right', 'Anti-Elite')

# 2) Ribbon plot: lower vs upper over time, faceted by region
plot_total <- ggplot(region_year_bounds,
       aes(x = year)) +
  geom_ribbon(aes(ymin = lower_m, ymax = upper_m, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_m), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_line(aes(y = upper_m), color = "grey20", linewidth = 0.4,
            linetype = "dotted") +
  facet_wrap(~ region, nrow = 1) +
  scale_x_continuous(breaks = sort(unique(region_year_bounds$year))) +
  scale_y_continuous(
    name = "Number of people in bounds (millions)",
    labels = label_number(accuracy = 0.1)
  ) +
  labs(x = NULL, fill = "") +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" )

ggsave("../graphs/plot_total_facet.png", plot_total, width = 6, height = 3.5)


plot_total <- ggplot(region_year_bounds,
       aes(x = year)) +
  geom_ribbon(aes(ymin = lower_m, ymax = upper_m, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_m, group = region), color = "grey20", 
            linewidth = 0.4, linetype = "dotted") +
  geom_line(aes(y = upper_m, group = region), color = "grey20", linewidth = 0.4,
            linetype = "dotted") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds$year))) +
  scale_y_continuous(
    name = "Number of people in bounds (millions)",
    labels = label_number(accuracy = 0.1)
  ) +
  labs(x = NULL, fill = "Region") +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" )

ggsave("../graphs/plot_total.png", plot_total, width = 6, height = 3.5)

######### plot east
bounds_with_counts$east <- ifelse(bounds_with_counts$state_id > 11, 1, 0)
region_year_bounds <- bounds_with_counts %>%
  group_by(year, region, east) %>%
  summarise(
    lower_people = sum(lower_count, na.rm = TRUE),
    upper_people = sum(upper_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # optional: put in millions for readability
  mutate(
    lower_m = lower_people / 1e6,
    upper_m = upper_people / 1e6
  )

region_year_bounds$region_old <- region_year_bounds$region
region_year_bounds$region <- ifelse(region_year_bounds$region_old == 'AntiEst', 'Right', 'Anti-Elite')


# 2) Ribbon plot: lower vs upper over time, faceted by region
plot_east <- ggplot(region_year_bounds %>% filter(east == 1),
       aes(x = year)) +
  geom_ribbon(aes(ymin = lower_m, ymax = upper_m, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_m), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_line(aes(y = upper_m), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  facet_wrap(~ region, nrow = 1) +
  scale_x_continuous(breaks = sort(unique(region_year_bounds$year))) +
  scale_y_continuous(
    name = "Number of people in bounds (millions)",
    labels = label_number(accuracy = 0.1)
  ) +
  labs(x = NULL, fill = "Region") +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("../graphs/plot_east_facet.png", plot_east, width = 6, height = 3.5)

plot_east <- ggplot(region_year_bounds %>% filter(east == 1),
       aes(x = year)) +
  geom_ribbon(aes(ymin = lower_m, ymax = upper_m, fill = region),
              alpha = 0.25, color = NA) +
  geom_line(aes(y = lower_m, group = region), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  geom_line(aes(y = upper_m, group = region), color = "grey20", linewidth = 0.4, linetype = "dotted") +
  scale_x_continuous(breaks = sort(unique(region_year_bounds$year))) +
  scale_y_continuous(
    name = "Number of people in bounds (millions)",
    labels = label_number(accuracy = 0.1)
  ) +
  labs(x = NULL, fill = "Region") +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave("../graphs/plot_east_facet.png", plot_east, width = 6, height = 3.5)



