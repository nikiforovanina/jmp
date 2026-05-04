# install.packages("eurostat")
library(eurostat)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1) Pull Eurostat data (tertiary attainment) ----
# Dataset: edat_lfse_03 = Educational attainment level by sex, age and country
# We'll use ISCED 5-8 (tertiary), Germany, by sex and age groups
edu <- get_eurostat("edat_lfse_03", time_format = "num")

de_tertiary <- edu %>%
  dplyr::filter(
    geo == "DE",
    isced11 == "ED5-8",                 # tertiary attainment (Bachelor+)
    sex %in% c("F", "M", "T"),
    age %in% c("Y25-34","Y35-44","Y45-54","Y55-64"),
    TIME_PERIOD %in% c(2013, 2017, 2021, 2023)  # <- pick years you want
  ) %>%
  dplyr::mutate(
    sex = recode(sex, F = "Women", M = "Men", T = "Total"),
    age = recode(age,
                 "Y25-34" = "25–34",
                 "Y35-44" = "35–44",
                 "Y45-54" = "45–54",
                 "Y55-64" = "55–64"
    )
  ) %>%
  dplyr::select(TIME_PERIOD, sex, age, values)

# ---- 2) A “descriptives table” you can print/save ----
de_tertiary_wide <- de_tertiary %>%
  pivot_wider(names_from = sex, values_from = values) %>%
  arrange(TIME_PERIOD, age)

print(de_tertiary_wide)

# ---- 3) Plot: tertiary attainment over age groups (faceted by year) ----
p_tertiary <- ggplot(de_tertiary, aes(x = age, y = values, group = sex, color = sex)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.4) +
  facet_wrap(~ TIME_PERIOD, nrow = 1) +
  labs(x = NULL, y = "Tertiary attainment (%)", color = NULL,
       title = "Germany: tertiary educational attainment (ISCED 5–8) by age & gender") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

p_tertiary
ggsave("de_tertiary_attainment_age_sex.png", p_tertiary, width = 11, height = 4, dpi = 300)

# ---- OPTIONAL: Abitur-ish proxy (ISCED 3–4) ----
de_uppersec <- edu %>%
  filter(
    geo == "DE",
    isced11 == "ED3-4",                 # upper secondary + post-secondary non-tertiary
    sex %in% c("F", "M", "T"),
    age %in% c("Y25-34","Y35-44","Y45-54","Y55-64"),
    time %in% c(2013, 2017, 2021, 2023)
  ) %>%
  mutate(
    sex = recode(sex, F = "Women", M = "Men", T = "Total"),
    age = recode(age,
                 "Y25-34" = "25–34",
                 "Y35-44" = "35–44",
                 "Y45-54" = "45–54",
                 "Y55-64" = "55–64"
    )
  ) %>%
  select(time, sex, age, value)

# quick plot
p_uppersec <- ggplot(de_uppersec, aes(x = age, y = value, group = sex, color = sex)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.4) +
  facet_wrap(~ time, nrow = 1) +
  labs(x = NULL, y = "ISCED 3–4 attainment (%)", color = NULL,
       title = "Germany: upper secondary / post-secondary non-tertiary (proxy for Abitur-ish) by age & gender") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

p_uppersec
ggsave("de_isced34_attainment_age_sex.png", p_uppersec, width = 11, height = 4, dpi = 300)