##########################################################################
### Diagnostic checks for census_XXXX_edu.Rdata outputs               ###
##########################################################################

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)

brks    <- c("18_24", "25_34", "35_44", "45_59", "60_69", "70plus")
genders <- c("male", "female")

edu_cols   <- paste0(rep(genders, each = length(brks)), "_", rep(brks, 2), "_edu_share")
noedu_cols <- paste0(rep(genders, each = length(brks)), "_", rep(brks, 2), "_noedu_share")


## ── 1. Load all three years ───────────────────────────────────────────

load("census_2021_edu.Rdata")  # → census_2021_edu
load("census_2017_edu.Rdata")  # → census_2017_edu
load("census_2013_edu.Rdata")  # → census_2013_edu

census_2021_edu$year <- 2021L
census_2017_edu$year <- 2017L
census_2013_edu$year <- 2013L

all_years <- bind_rows(census_2021_edu, census_2017_edu, census_2013_edu)


## ── 2. Check: edu + noedu shares sum to 1 per municipality ───────────
# Each pair (edu + noedu) reconstructs the original group share.
# All 24 split shares together should therefore still sum to ≈ 1.

row_sums <- all_years %>%
  filter(!is.na(total_all), total_all > 0) %>%          # drop agg. rows
  rowwise() %>%
  mutate(
    sum_edu   = sum(c_across(all_of(edu_cols)),   na.rm = TRUE),
    sum_noedu = sum(c_across(all_of(noedu_cols)), na.rm = TRUE),
    total_split = sum_edu + sum_noedu
  ) %>%
  ungroup()

cat("=== Share-sum check (should be ≈ 1.00) ===\n")
row_sums %>%
  group_by(year) %>%
  summarise(
    n_units     = n(),
    mean_sum    = round(mean(total_split, na.rm = TRUE), 4),
    min_sum     = round(min( total_split, na.rm = TRUE), 4),
    max_sum     = round(max( total_split, na.rm = TRUE), 4),
    n_exact_NA  = sum(is.na(total_split))
  ) %>%
  print()


## ── 3. Average / median Abitur rate per bracket × year ───────────────
# Abitur rate for a group = edu_share / (edu_share + noedu_share).
# Summarised across all municipalities (weighted by total_all).

edu_rates <- all_years %>%
  filter(!is.na(total_all), total_all > 0) %>%
  pivot_longer(
    cols      = all_of(edu_cols),
    names_to  = "group",
    values_to = "edu_share"
  ) %>%
  mutate(
    noedu_share = {
      noedu_col <- str_replace(group, "_edu_share$", "_noedu_share")
      # pull matching noedu value row-by-row
      map2_dbl(row_number(), noedu_col, function(i, col) {
        all_years[[col]][ceiling(i / length(edu_cols))]
      })
    }
  )

# Simpler approach: pivot both edu and noedu simultaneously
edu_long <- all_years %>%
  filter(!is.na(total_all), total_all > 0) %>%
  pivot_longer(
    cols      = all_of(c(edu_cols, noedu_cols)),
    names_to  = "col",
    values_to = "share_val"
  ) %>%
  mutate(
    edu_type  = if_else(str_detect(col, "_edu_share$"), "edu", "noedu"),
    gender    = str_extract(col, "^(male|female)"),
    age_group = str_remove(str_remove(col, "^(male|female)_"), "_(edu|noedu)_share$")
  ) %>%
  select(-col) %>%
  pivot_wider(names_from = edu_type, values_from = share_val) %>%
  mutate(
    group_total  = edu + noedu,                            # original group share
    abitur_rate  = ifelse(group_total > 0, edu / group_total, NA_real_)  # fraction with Abitur
  )

cat("\n=== Average & median Abitur rate by age group × year ===\n")
edu_long %>%
  group_by(year, gender, age_group) %>%
  summarise(
    mean_abitur   = round(mean(abitur_rate,             na.rm = TRUE), 3),
    median_abitur = round(median(abitur_rate,           na.rm = TRUE), 3),
    wmean_abitur  = round(weighted.mean(abitur_rate, w = total_all,
                                        na.rm = TRUE),               3),
    .groups = "drop"
  ) %>%
  arrange(year, gender, age_group) %>%
  print(n = Inf)


## ── 4. Plot: Abitur rate distribution by age bracket, gender, year ───

# Ordered factor so age groups appear left-to-right on the x axis
age_labels <- c(
  "18_24"  = "18–24",
  "25_34"  = "25–34",
  "35_44"  = "35–44",
  "45_59"  = "45–59",
  "60_69"  = "60–69",
  "70plus" = "70+"
)

plot_df <- edu_long %>%
  filter(!is.na(abitur_rate)) %>%
  mutate(
    age_group = factor(age_group, levels = names(age_labels), labels = age_labels),
    gender    = factor(gender, levels = c("male", "female"),
                       labels = c("Male", "Female")),
    year      = factor(year)
  )

p <- ggplot(plot_df, aes(x = age_group, y = abitur_rate, fill = gender)) +
  geom_boxplot(
    outlier.size  = 0.4,
    outlier.alpha = 0.3,
    linewidth     = 0.35,
    width         = 0.6,
    position      = position_dodge(width = 0.7)
  ) +
  # population-weighted mean as a diamond overlay
  stat_summary(
    aes(group = gender),
    fun          = function(x) weighted.mean(x, w = rep(1, length(x)), na.rm = TRUE),
    geom         = "point",
    shape        = 23,
    size         = 1.8,
    fill         = "white",
    position     = position_dodge(width = 0.7)
  ) +
  facet_wrap(~ year, ncol = 3) +
  scale_fill_manual(
    values = c("Male" = "#4E79A7", "Female" = "#F28E2B"),
    name   = NULL
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Abitur rate by age group, gender, and year",
    subtitle = "Distribution across municipalities · white diamond = mean",
    x        = "Age bracket",
    y        = "Share with Abitur"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "top",
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "grey92", colour = NA),
    strip.text       = element_text(face = "bold")
  )

print(p)
ggsave("abitur_rate_by_age_gender_year.pdf", p,
       width = 10, height = 4.5, device = cairo_pdf)
message("Plot saved to abitur_rate_by_age_gender_year.pdf")
