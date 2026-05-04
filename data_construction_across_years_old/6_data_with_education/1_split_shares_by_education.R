##########################################################################
### Split census demographic shares by higher education status         ###
###                                                                    ###
### Territorial matching uses a three-tier fallback:                   ###
###   1. Gemeinde     (gem_2000S-3041_de_flat.csv,      12-digit ARS) ###
###   2. Gemeindeverband (gem_verb_2000S-3041_de_flat.csv, 9-digit)   ###
###   3. Kreise       (kresie_2000S-3041_de_flat.csv,    5-digit)     ###
###                                                                    ###
### "/" in source files = statistically suppressed small number;       ###
### treated as 1 (not 0), following the convention in                  ###
### 0_census_separate_years.R where "-" is also set to 1.             ###
##########################################################################

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(stringr)
library(readr)


##########################################################################
### SHARED CONSTANTS                                                   ###
##########################################################################

brks <- c("18_24", "25_34", "35_44", "45_59", "60_69", "70plus")

# Share columns that exist in every census_shares_XXXX object
share_cols <- c(
  paste0("male_",   brks, "_share"),
  paste0("female_", brks, "_share")
)

# Replace "/" (and any other non-numeric string) with 1.
# Rationale: suppressed cells are small but non-zero counts.
clean_n <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  x_num[is.na(x_num)] <- 1
  x_num
}


##########################################################################
### PIPELINE: raw source file → long education-fraction table          ###
###                                                                    ###
### Applied separately to the gem, gem_verb, and kreise files.         ###
### Each produces a long data frame with columns:                       ###
###   unit_code | gender | age_group | frac_edu                        ###
### where frac_edu = fraction of that group with Abitur.               ###
##########################################################################


## ── TIER 1: Gemeinde (gem_2000S-3041_de_flat.csv, 12-digit codes) ─────

gem_raw <- read_csv2(
  "gem_2000S-3041_de_flat.csv",
  col_types = cols(.default = "c")
)

gem <- gem_raw %>%
  rename(
    gem_code     = `1_variable_attribute_code`,
    age_label    = `2_variable_attribute_label`,
    gender_label = `3_variable_attribute_label`,
    edu_label    = `4_variable_attribute_label`,
    n            = value
  ) %>%
  filter(
    gender_label %in% c("M\u00e4nnlich", "Weiblich"),
    edu_label    %in% c(
      "Insgesamt",
      "Fachhochschul- oder Hochschulreife (Abitur)"
    ),
    age_label    != "Insgesamt"
  ) %>%
  mutate(
    gender = if_else(gender_label == "M\u00e4nnlich", "male", "female"),
    age_band = case_when(
      str_detect(age_label, "Unter 10")  ~ "au10",
      str_detect(age_label, "10 bis 19") ~ "a10_19",
      str_detect(age_label, "20 bis 29") ~ "a20_29",
      str_detect(age_label, "30 bis 39") ~ "a30_39",
      str_detect(age_label, "40 bis 49") ~ "a40_49",
      str_detect(age_label, "50 bis 59") ~ "a50_59",
      str_detect(age_label, "60 bis 69") ~ "a60_69",
      str_detect(age_label, "70 bis 79") ~ "a70_79",
      str_detect(age_label, "80")        ~ "a80plus",
      TRUE                               ~ NA_character_
    ),
    edu_cat = if_else(edu_label == "Insgesamt", "total", "abitur"),
    n       = clean_n(n)
  )

gem_wide <- gem %>%
  select(gem_code, gender, age_band, edu_cat, n) %>%
  pivot_wider(names_from = edu_cat, values_from = n)

gem_bands <- gem_wide %>%
  pivot_wider(
    id_cols     = c(gem_code, gender),
    names_from  = age_band,
    values_from = c(total, abitur),
    names_sep   = "_"
  )

gem_brackets <- gem_bands %>%
  mutate(
    total_18_24  = (2/5)*coalesce(total_a10_19,  0) + (1/2)*coalesce(total_a20_29,  0),
    abitur_18_24 = (2/5)*coalesce(abitur_a10_19, 0) + (1/2)*coalesce(abitur_a20_29, 0),

    total_25_34  = (1/2)*coalesce(total_a20_29,  0) + (1/2)*coalesce(total_a30_39,  0),
    abitur_25_34 = (1/2)*coalesce(abitur_a20_29, 0) + (1/2)*coalesce(abitur_a30_39, 0),

    total_35_44  = (1/2)*coalesce(total_a30_39,  0) + (1/2)*coalesce(total_a40_49,  0),
    abitur_35_44 = (1/2)*coalesce(abitur_a30_39, 0) + (1/2)*coalesce(abitur_a40_49, 0),

    total_45_59  = (1/2)*coalesce(total_a40_49,  0) + coalesce(total_a50_59,  0),
    abitur_45_59 = (1/2)*coalesce(abitur_a40_49, 0) + coalesce(abitur_a50_59, 0),

    total_60_69  = coalesce(total_a60_69,  0),
    abitur_60_69 = coalesce(abitur_a60_69, 0),

    total_70plus  = coalesce(total_a70_79,  0) + coalesce(total_a80plus,  0),
    abitur_70plus = coalesce(abitur_a70_79, 0) + coalesce(abitur_a80plus, 0)
  ) %>%
  select(gem_code, gender,
         total_18_24,  abitur_18_24,
         total_25_34,  abitur_25_34,
         total_35_44,  abitur_35_44,
         total_45_59,  abitur_45_59,
         total_60_69,  abitur_60_69,
         total_70plus, abitur_70plus)

gem_fracs <- gem_brackets %>%
  mutate(
    frac_18_24  = ifelse(total_18_24  > 0, abitur_18_24  / total_18_24,  NA_real_),
    frac_25_34  = ifelse(total_25_34  > 0, abitur_25_34  / total_25_34,  NA_real_),
    frac_35_44  = ifelse(total_35_44  > 0, abitur_35_44  / total_35_44,  NA_real_),
    frac_45_59  = ifelse(total_45_59  > 0, abitur_45_59  / total_45_59,  NA_real_),
    frac_60_69  = ifelse(total_60_69  > 0, abitur_60_69  / total_60_69,  NA_real_),
    frac_70plus = ifelse(total_70plus > 0, abitur_70plus / total_70plus, NA_real_)
  ) %>%
  select(gem_code, gender, starts_with("frac_"))

gem_fracs_long <- gem_fracs %>%
  pivot_longer(
    cols         = starts_with("frac_"),
    names_to     = "age_group",
    names_prefix = "frac_",
    values_to    = "frac_edu_gem"
  )
# Result: gem_code | gender | age_group | frac_edu_gem


## ── TIER 2: Gemeineverband (gem_verb_2000S-3041_de_flat.csv, 9-digit) ─

gv_raw <- read_csv2(
  "gem_verb_2000S-3041_de_flat.csv",
  col_types = cols(.default = "c")
)

gv <- gv_raw %>%
  rename(
    gv_code      = `1_variable_attribute_code`,
    age_label    = `2_variable_attribute_label`,
    gender_label = `3_variable_attribute_label`,
    edu_label    = `4_variable_attribute_label`,
    n            = value
  ) %>%
  filter(
    gender_label %in% c("M\u00e4nnlich", "Weiblich"),
    edu_label    %in% c(
      "Insgesamt",
      "Fachhochschul- oder Hochschulreife (Abitur)"
    ),
    age_label    != "Insgesamt"
  ) %>%
  mutate(
    gender = if_else(gender_label == "M\u00e4nnlich", "male", "female"),
    age_band = case_when(
      str_detect(age_label, "Unter 10")  ~ "au10",
      str_detect(age_label, "10 bis 19") ~ "a10_19",
      str_detect(age_label, "20 bis 29") ~ "a20_29",
      str_detect(age_label, "30 bis 39") ~ "a30_39",
      str_detect(age_label, "40 bis 49") ~ "a40_49",
      str_detect(age_label, "50 bis 59") ~ "a50_59",
      str_detect(age_label, "60 bis 69") ~ "a60_69",
      str_detect(age_label, "70 bis 79") ~ "a70_79",
      str_detect(age_label, "80")        ~ "a80plus",
      TRUE                               ~ NA_character_
    ),
    edu_cat = if_else(edu_label == "Insgesamt", "total", "abitur"),
    n       = clean_n(n)
  )

gv_wide <- gv %>%
  select(gv_code, gender, age_band, edu_cat, n) %>%
  pivot_wider(names_from = edu_cat, values_from = n)

gv_bands <- gv_wide %>%
  pivot_wider(
    id_cols     = c(gv_code, gender),
    names_from  = age_band,
    values_from = c(total, abitur),
    names_sep   = "_"
  )

gv_brackets <- gv_bands %>%
  mutate(
    total_18_24  = (2/5)*coalesce(total_a10_19,  0) + (1/2)*coalesce(total_a20_29,  0),
    abitur_18_24 = (2/5)*coalesce(abitur_a10_19, 0) + (1/2)*coalesce(abitur_a20_29, 0),

    total_25_34  = (1/2)*coalesce(total_a20_29,  0) + (1/2)*coalesce(total_a30_39,  0),
    abitur_25_34 = (1/2)*coalesce(abitur_a20_29, 0) + (1/2)*coalesce(abitur_a30_39, 0),

    total_35_44  = (1/2)*coalesce(total_a30_39,  0) + (1/2)*coalesce(total_a40_49,  0),
    abitur_35_44 = (1/2)*coalesce(abitur_a30_39, 0) + (1/2)*coalesce(abitur_a40_49, 0),

    total_45_59  = (1/2)*coalesce(total_a40_49,  0) + coalesce(total_a50_59,  0),
    abitur_45_59 = (1/2)*coalesce(abitur_a40_49, 0) + coalesce(abitur_a50_59, 0),

    total_60_69  = coalesce(total_a60_69,  0),
    abitur_60_69 = coalesce(abitur_a60_69, 0),

    total_70plus  = coalesce(total_a70_79,  0) + coalesce(total_a80plus,  0),
    abitur_70plus = coalesce(abitur_a70_79, 0) + coalesce(abitur_a80plus, 0)
  ) %>%
  select(gv_code, gender,
         total_18_24,  abitur_18_24,
         total_25_34,  abitur_25_34,
         total_35_44,  abitur_35_44,
         total_45_59,  abitur_45_59,
         total_60_69,  abitur_60_69,
         total_70plus, abitur_70plus)

gv_fracs <- gv_brackets %>%
  mutate(
    frac_18_24  = ifelse(total_18_24  > 0, abitur_18_24  / total_18_24,  NA_real_),
    frac_25_34  = ifelse(total_25_34  > 0, abitur_25_34  / total_25_34,  NA_real_),
    frac_35_44  = ifelse(total_35_44  > 0, abitur_35_44  / total_35_44,  NA_real_),
    frac_45_59  = ifelse(total_45_59  > 0, abitur_45_59  / total_45_59,  NA_real_),
    frac_60_69  = ifelse(total_60_69  > 0, abitur_60_69  / total_60_69,  NA_real_),
    frac_70plus = ifelse(total_70plus > 0, abitur_70plus / total_70plus, NA_real_)
  ) %>%
  select(gv_code, gender, starts_with("frac_"))

gv_fracs_long <- gv_fracs %>%
  pivot_longer(
    cols         = starts_with("frac_"),
    names_to     = "age_group",
    names_prefix = "frac_",
    values_to    = "frac_edu_gv"
  )
# Result: gv_code | gender | age_group | frac_edu_gv


## ── TIER 3: Kreise (kresie_2000S-3041_de_flat.csv, 5-digit codes) ─────

kr_raw <- read_csv2(
  "kresie_2000S-3041_de_flat.csv",
  col_types = cols(.default = "c")
)

kr <- kr_raw %>%
  rename(
    kr_code      = `1_variable_attribute_code`,
    age_label    = `2_variable_attribute_label`,
    gender_label = `3_variable_attribute_label`,
    edu_label    = `4_variable_attribute_label`,
    n            = value
  ) %>%
  filter(
    gender_label %in% c("M\u00e4nnlich", "Weiblich"),
    edu_label    %in% c(
      "Insgesamt",
      "Fachhochschul- oder Hochschulreife (Abitur)"
    ),
    age_label    != "Insgesamt"
  ) %>%
  mutate(
    gender = if_else(gender_label == "M\u00e4nnlich", "male", "female"),
    age_band = case_when(
      str_detect(age_label, "Unter 10")  ~ "au10",
      str_detect(age_label, "10 bis 19") ~ "a10_19",
      str_detect(age_label, "20 bis 29") ~ "a20_29",
      str_detect(age_label, "30 bis 39") ~ "a30_39",
      str_detect(age_label, "40 bis 49") ~ "a40_49",
      str_detect(age_label, "50 bis 59") ~ "a50_59",
      str_detect(age_label, "60 bis 69") ~ "a60_69",
      str_detect(age_label, "70 bis 79") ~ "a70_79",
      str_detect(age_label, "80")        ~ "a80plus",
      TRUE                               ~ NA_character_
    ),
    edu_cat = if_else(edu_label == "Insgesamt", "total", "abitur"),
    n       = clean_n(n)
  )

kr_wide <- kr %>%
  select(kr_code, gender, age_band, edu_cat, n) %>%
  pivot_wider(names_from = edu_cat, values_from = n)

kr_bands <- kr_wide %>%
  pivot_wider(
    id_cols     = c(kr_code, gender),
    names_from  = age_band,
    values_from = c(total, abitur),
    names_sep   = "_"
  )

kr_brackets <- kr_bands %>%
  mutate(
    total_18_24  = (2/5)*coalesce(total_a10_19,  0) + (1/2)*coalesce(total_a20_29,  0),
    abitur_18_24 = (2/5)*coalesce(abitur_a10_19, 0) + (1/2)*coalesce(abitur_a20_29, 0),

    total_25_34  = (1/2)*coalesce(total_a20_29,  0) + (1/2)*coalesce(total_a30_39,  0),
    abitur_25_34 = (1/2)*coalesce(abitur_a20_29, 0) + (1/2)*coalesce(abitur_a30_39, 0),

    total_35_44  = (1/2)*coalesce(total_a30_39,  0) + (1/2)*coalesce(total_a40_49,  0),
    abitur_35_44 = (1/2)*coalesce(abitur_a30_39, 0) + (1/2)*coalesce(abitur_a40_49, 0),

    total_45_59  = (1/2)*coalesce(total_a40_49,  0) + coalesce(total_a50_59,  0),
    abitur_45_59 = (1/2)*coalesce(abitur_a40_49, 0) + coalesce(abitur_a50_59, 0),

    total_60_69  = coalesce(total_a60_69,  0),
    abitur_60_69 = coalesce(abitur_a60_69, 0),

    total_70plus  = coalesce(total_a70_79,  0) + coalesce(total_a80plus,  0),
    abitur_70plus = coalesce(abitur_a70_79, 0) + coalesce(abitur_a80plus, 0)
  ) %>%
  select(kr_code, gender,
         total_18_24,  abitur_18_24,
         total_25_34,  abitur_25_34,
         total_35_44,  abitur_35_44,
         total_45_59,  abitur_45_59,
         total_60_69,  abitur_60_69,
         total_70plus, abitur_70plus)

kr_fracs <- kr_brackets %>%
  mutate(
    frac_18_24  = ifelse(total_18_24  > 0, abitur_18_24  / total_18_24,  NA_real_),
    frac_25_34  = ifelse(total_25_34  > 0, abitur_25_34  / total_25_34,  NA_real_),
    frac_35_44  = ifelse(total_35_44  > 0, abitur_35_44  / total_35_44,  NA_real_),
    frac_45_59  = ifelse(total_45_59  > 0, abitur_45_59  / total_45_59,  NA_real_),
    frac_60_69  = ifelse(total_60_69  > 0, abitur_60_69  / total_60_69,  NA_real_),
    frac_70plus = ifelse(total_70plus > 0, abitur_70plus / total_70plus, NA_real_)
  ) %>%
  select(kr_code, gender, starts_with("frac_"))

kr_fracs_long <- kr_fracs %>%
  pivot_longer(
    cols         = starts_with("frac_"),
    names_to     = "age_group",
    names_prefix = "frac_",
    values_to    = "frac_edu_kr"
  )
# Result: kr_code | gender | age_group | frac_edu_kr


##########################################################################
### Load census files and derive all three matching keys from each     ###
### census municipality code:                                           ###
###   gem_code: pad to 12 digits (full ARS)   → matches gem file       ###
###   gv_code:  first 9 of ARS-12             → matches gem_verb file  ###
###   kr_code:  first 5 of ARS-12             → matches kreise file    ###
##########################################################################

load("census_2013.Rdata")  # → census_shares_2013
load("census_2017.Rdata")  # → census_shares_2017
load("census_2021.Rdata")  # → census_shares_2021

census_shares_2013 <- census_shares_2013 %>%
  mutate(
    gem_code = str_pad(as.character(code), 12, "left", "0"),
    gv_code  = substr(gem_code, 1, 9),
    kr_code  = substr(gem_code, 1, 5)
  )

census_shares_2017 <- census_shares_2017 %>%
  mutate(
    gem_code = str_pad(as.character(code), 12, "left", "0"),
    gv_code  = substr(gem_code, 1, 9),
    kr_code  = substr(gem_code, 1, 5)
  )

census_shares_2021 <- census_shares_2021 %>%
  mutate(
    gem_code = str_pad(as.character(code), 12, "left", "0"),
    gv_code  = substr(gem_code, 1, 9),
    kr_code  = substr(gem_code, 1, 5)
  )


##########################################################################
### Split shares for census_2021                                       ###
##########################################################################

# Pivot 12 share columns to long (one row per municipality × group)
census_2021_long <- census_shares_2021 %>%
  pivot_longer(
    cols      = all_of(share_cols),
    names_to  = "col_name",
    values_to = "share"
  ) %>%
  mutate(
    gender    = str_extract(col_name, "^(male|female)"),
    age_group = str_remove(str_remove(col_name, "^(male|female)_"), "_share$")
  )

# Tier 1: join Gemeinde-level fractions
census_2021_long <- census_2021_long %>%
  left_join(gem_fracs_long, by = c("gem_code", "gender", "age_group"))

# Tier 2: join Gemeineverband-level fractions
census_2021_long <- census_2021_long %>%
  left_join(gv_fracs_long, by = c("gv_code", "gender", "age_group"))

# Tier 3: join Kreis-level fractions
census_2021_long <- census_2021_long %>%
  left_join(kr_fracs_long, by = c("kr_code", "gender", "age_group"))

# Apply fallback: Gemeinde → gem_verb → Kreise
census_2021_long <- census_2021_long %>%
  mutate(
    frac_edu = coalesce(frac_edu_gem, frac_edu_gv, frac_edu_kr),
    # record which tier was used (useful for diagnostics)
    frac_tier = case_when(
      !is.na(frac_edu_gem) ~ "gemeinde",
      !is.na(frac_edu_gv)  ~ "gem_verb",
      !is.na(frac_edu_kr)  ~ "kreise",
      TRUE                 ~ NA_character_
    )
  )

# Split shares
census_2021_long <- census_2021_long %>%
  mutate(
    share_edu   = share * frac_edu,
    share_noedu = share * (1 - frac_edu)
  )

# Pivot back to wide
census_2021_edu <- census_2021_long %>%
  select(code, region_name, gem_code, gv_code, kr_code, total_all,
         gender, age_group, share_edu, share_noedu) %>%
  pivot_wider(
    id_cols     = c(code, region_name, gem_code, gv_code, kr_code, total_all),
    names_from  = c(gender, age_group),
    values_from = c(share_edu, share_noedu),
    names_glue  = "{gender}_{age_group}_{.value}"
  ) %>%
  rename_with(
    ~ str_replace(., "_share_edu$",   "_edu_share") %>%
      str_replace(  "_share_noedu$", "_noedu_share"),
    matches("_share_(edu|noedu)$")
  )

save(census_2021_edu, file = "census_2021_edu.Rdata")
message("Saved census_2021_edu.Rdata  (",  nrow(census_2021_edu), " rows, ",
        ncol(census_2021_edu), " columns)")

# Tier coverage diagnostics for 2021
tier_summary_2021 <- census_2021_long %>%
  distinct(code, frac_tier) %>%
  count(frac_tier)
message("2021 tier coverage:"); print(tier_summary_2021)


##########################################################################
### Split shares for census_2017                                       ###
##########################################################################

census_2017_long <- census_shares_2017 %>%
  pivot_longer(
    cols      = all_of(share_cols),
    names_to  = "col_name",
    values_to = "share"
  ) %>%
  mutate(
    gender    = str_extract(col_name, "^(male|female)"),
    age_group = str_remove(str_remove(col_name, "^(male|female)_"), "_share$")
  )

census_2017_long <- census_2017_long %>%
  left_join(gem_fracs_long, by = c("gem_code", "gender", "age_group"))

census_2017_long <- census_2017_long %>%
  left_join(gv_fracs_long, by = c("gv_code", "gender", "age_group"))

census_2017_long <- census_2017_long %>%
  left_join(kr_fracs_long, by = c("kr_code", "gender", "age_group"))

census_2017_long <- census_2017_long %>%
  mutate(
    frac_edu = coalesce(frac_edu_gem, frac_edu_gv, frac_edu_kr),
    frac_tier = case_when(
      !is.na(frac_edu_gem) ~ "gemeinde",
      !is.na(frac_edu_gv)  ~ "gem_verb",
      !is.na(frac_edu_kr)  ~ "kreise",
      TRUE                 ~ NA_character_
    )
  )

census_2017_long <- census_2017_long %>%
  mutate(
    share_edu   = share * frac_edu,
    share_noedu = share * (1 - frac_edu)
  )

census_2017_edu <- census_2017_long %>%
  select(code, region_name, gem_code, gv_code, kr_code, total_all,
         gender, age_group, share_edu, share_noedu) %>%
  pivot_wider(
    id_cols     = c(code, region_name, gem_code, gv_code, kr_code, total_all),
    names_from  = c(gender, age_group),
    values_from = c(share_edu, share_noedu),
    names_glue  = "{gender}_{age_group}_{.value}"
  ) %>%
  rename_with(
    ~ str_replace(., "_share_edu$",   "_edu_share") %>%
      str_replace(  "_share_noedu$", "_noedu_share"),
    matches("_share_(edu|noedu)$")
  )

save(census_2017_edu, file = "census_2017_edu.Rdata")
message("Saved census_2017_edu.Rdata  (", nrow(census_2017_edu), " rows, ",
        ncol(census_2017_edu), " columns)")

tier_summary_2017 <- census_2017_long %>%
  distinct(code, frac_tier) %>%
  count(frac_tier)
message("2017 tier coverage:"); print(tier_summary_2017)


##########################################################################
### Split shares for census_2013                                       ###
##########################################################################

census_2013_long <- census_shares_2013 %>%
  pivot_longer(
    cols      = all_of(share_cols),
    names_to  = "col_name",
    values_to = "share"
  ) %>%
  mutate(
    gender    = str_extract(col_name, "^(male|female)"),
    age_group = str_remove(str_remove(col_name, "^(male|female)_"), "_share$")
  )

census_2013_long <- census_2013_long %>%
  left_join(gem_fracs_long, by = c("gem_code", "gender", "age_group"))

census_2013_long <- census_2013_long %>%
  left_join(gv_fracs_long, by = c("gv_code", "gender", "age_group"))

census_2013_long <- census_2013_long %>%
  left_join(kr_fracs_long, by = c("kr_code", "gender", "age_group"))

census_2013_long <- census_2013_long %>%
  mutate(
    frac_edu = coalesce(frac_edu_gem, frac_edu_gv, frac_edu_kr),
    frac_tier = case_when(
      !is.na(frac_edu_gem) ~ "gemeinde",
      !is.na(frac_edu_gv)  ~ "gem_verb",
      !is.na(frac_edu_kr)  ~ "kreise",
      TRUE                 ~ NA_character_
    )
  )

census_2013_long <- census_2013_long %>%
  mutate(
    share_edu   = share * frac_edu,
    share_noedu = share * (1 - frac_edu)
  )

census_2013_edu <- census_2013_long %>%
  select(code, region_name, gem_code, gv_code, kr_code, total_all,
         gender, age_group, share_edu, share_noedu) %>%
  pivot_wider(
    id_cols     = c(code, region_name, gem_code, gv_code, kr_code, total_all),
    names_from  = c(gender, age_group),
    values_from = c(share_edu, share_noedu),
    names_glue  = "{gender}_{age_group}_{.value}"
  ) %>%
  rename_with(
    ~ str_replace(., "_share_edu$",   "_edu_share") %>%
      str_replace(  "_share_noedu$", "_noedu_share"),
    matches("_share_(edu|noedu)$")
  )

save(census_2013_edu, file = "census_2013_edu.Rdata")
message("Saved census_2013_edu.Rdata  (", nrow(census_2013_edu), " rows, ",
        ncol(census_2013_edu), " columns)")

tier_summary_2013 <- census_2013_long %>%
  distinct(code, frac_tier) %>%
  count(frac_tier)
message("2013 tier coverage:"); print(tier_summary_2013)
