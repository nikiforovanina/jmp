#########################################################################
### Processing census data to create municipality - group shares data ###
#########################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(stringr)

# census was conducted in 2022, but was backdated to 2021 to comply with the EU regulations
# hence, we use it for the year 2021 without any shifts
rm(list = ls())
set.seed(42)


census <- read.csv2("Gemeinde_5Jahr_Geschlecht_1000A-2026_de.csv")
hdr <- as.character(unlist(colnames(census), use.names = FALSE))

# locate block starts
ix_total <- which(hdr == "Insgesamt")[1]
ix_male  <- which(hdr == "Männlich")[1]
ix_fem   <- which(hdr == "Weiblich")[1]
stopifnot(!is.na(ix_total), !is.na(ix_male), !is.na(ix_fem))

# helper to normalise age labels like "Unter 5 Jahre", "5 bis 9 Jahre", "90 Jahre und älter"
norm_age <- function(x) {
  x <- str_trim(x)
  x <- str_replace_all(x, fixed(" Jahre"), "")
  x <- str_replace_all(x, fixed("Unter "), "0-")
  x <- str_replace_all(x, " bis ", "-")
  x <- str_replace_all(x, fixed(" und älter"), "+")
  x <- str_replace_all(x, " ", "_")
  x <- str_replace_all(x, "[^[:alnum:]_+\\-]", "")
  tolower(x)
}

# id columns
new_names <- c("code", "region_name")

# total block
end_total  <- ix_male - 1
tot_labels <- hdr[ix_total:end_total]
tot_names  <- c("total_all", paste0("total_", norm_age(tot_labels[-1])))

# male block
end_male   <- ix_fem - 1
male_labels <- hdr[ix_male:end_male]
male_names  <- c("male_all", paste0("male_", norm_age(male_labels[-1])))

# female block
end_fem    <- ncol(census)
fem_labels <- hdr[ix_fem:end_fem]
fem_names  <- c("female_all", paste0("female_", norm_age(fem_labels[-1])))

final_names <- c(new_names, tot_names, male_names, fem_names)
stopifnot(length(final_names) == ncol(census))

# apply and drop header row
colnames(census) <- final_names
census <- census[-1, ]

census[] <- lapply(census, function(x) { x[which(x == '-')] <- 1; x })


new_names <- c(
  "code",
  "region_name",
  "total_all",
  "total_0_4",
  "total_5_9",
  "total_10_14",
  "total_15_19",
  "total_20_24",
  "total_25_29",
  "total_30_34",
  "total_35_39",
  "total_40_44",
  "total_45_49",
  "total_50_54",
  "total_55_59",
  "total_60_64",
  "total_65_69",
  "total_70_74",
  "total_75_79",
  "total_80_84",
  "total_85_89",
  "total_90plus",
  "male_all",
  "male_0_4",
  "male_5_9",
  "male_10_14",
  "male_15_19",
  "male_20_24",
  "male_25_29",
  "male_30_34",
  "male_35_39",
  "male_40_44",
  "male_45_49",
  "male_50_54",
  "male_55_59",
  "male_60_64",
  "male_65_69",
  "male_70_74",
  "male_75_79",
  "male_80_84",
  "male_85_89",
  "male_90plus",
  "female_all",
  "female_0_4",
  "female_5_9",
  "female_10_14",
  "female_15_19",
  "female_20_24",
  "female_25_29",
  "female_30_34",
  "female_35_39",
  "female_40_44",
  "female_45_49",
  "female_50_54",
  "female_55_59",
  "female_60_64",
  "female_65_69",
  "female_70_74",
  "female_75_79",
  "female_80_84",
  "female_85_89",
  "female_90plus"
)

colnames(census) <- new_names



## ---------- 2) Add aggregated age brackets ----------
# Compute aggregated age brackets using underscore-style bands
# (works for total_, male_, female_)
add_age_brackets <- function(df, prefixes = c("total","male","female")) {
  get <- function(pref, band) paste0(pref, "_", band)
  
  # make sure required source bands exist (create NA if missing)
  ensure <- function(df, pref, band) {
    nm <- get(pref, band)
    if (!nm %in% names(df)) df[[nm]] <- NA_real_
    df
  }
  
  needed <- c("15_19","20_24","25_29","30_34","35_39","40_44","45_49",
              "60_64","65_69","70_74","75_79","80_84","85_89","90plus")
  
  for (pref in prefixes) {
    for (b in needed) df <- ensure(df, pref, b)
    
    # 18–24 = 20–24 + 2/5 of 15–19
    df[[get(pref, "18_24")]] <- round(df[[get(pref, "20_24")]] + (2/5) * df[[get(pref, "15_19")]],0)
    
    # 25–34 = 25–29 + 30–34
    df[[get(pref, "25_34")]] <- rowSums(df[, c(get(pref, "25_29"), get(pref, "30_34"))], na.rm = TRUE)
    
    # 35–44 = 35–39 + 40–44
    df[[get(pref, "35_44")]] <- rowSums(df[, c(get(pref, "35_39"), get(pref, "40_44"))], na.rm = TRUE)
    
    # 45–49 = 45–49
    df[[get(pref, "45_59")]] <- rowSums(df[, c(get(pref, "45_49"), get(pref, "50_54"), get(pref, "55_59") )], na.rm = TRUE)
    
    # 60–69 = 60–64 + 65–69
    df[[get(pref, "60_69")]] <- rowSums(df[, c(get(pref, "60_64"), get(pref, "65_69"))], na.rm = TRUE)
    
    # 70+ = 70–74 + 75–79 + 80–84 + 85–89 + 90+
    df[[get(pref, "70plus")]] <- rowSums(
      df[, c(get(pref, "70_74"), get(pref, "75_79"), get(pref, "80_84"),
             get(pref, "85_89"), get(pref, "90plus"))],
      na.rm = TRUE
    )
  }
  
  df
}

# ensure numeric (except id columns) before aggregating
id_cols <- c("code","region_name")
num_cols <- setdiff(names(census), id_cols)
census[num_cols] <- lapply(census[num_cols], function(v) suppressWarnings(as.numeric(v)))

# add the aggregated columns
census <- add_age_brackets(census)


library(dplyr)
library(tidyr)

# aggregated brackets you created in add_age_brackets()
brks <- c("18_24","25_34","35_44","45_59","60_69","70plus")

male_cols   <- paste0("male_", brks)
female_cols <- paste0("female_", brks)
bracket_cols <- c(male_cols, female_cols)

# If any are not numeric (just in case), coerce:
census[bracket_cols] <- lapply(census[bracket_cols], function(x) suppressWarnings(as.numeric(x)))

# 1) Denominator: row-sum across the 12 age–gender brackets
census <- census %>%
  mutate(pop_total_bracketed = rowSums(across(all_of(bracket_cols)), na.rm = TRUE))

# 2) Shares (wide): one share column per bracket, sums to 1 across the 12
census <- census %>%
  mutate(across(
    all_of(bracket_cols),
    ~ .x / pop_total_bracketed,
    .names = "{.col}_share"
  ))

census <- census %>%
  mutate(
    code_str = str_pad(as.character(code), width = 8, pad = "0"),
    kreis_id = substr(code_str, 1, 5)   # Land+Kreis
  )

census <- census %>%
  mutate(code_str = as.character(code_str))

# 2) pad to 12 digits (ARS format)
census <- census %>%
  mutate(ars_12 = str_pad(code_str, width = 12, side = "left", pad = "0"))

# 3) derive 5-digit Kreis code: Land (2) + RegBez (1) + Kreis (2)
census <- census %>%
  mutate(
    kreis_code = substr(ars_12, 1, 5)   # e.g. "01001" = Flensburg
  )

######################
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# 1. Read -------------------------------------------------------------

df_raw <- read_csv2("12411-0018_de.csv", col_types = cols(.default = "c"))  # keep as char first

# I assume the first column is the one with date / kreis-code / kreis-name
names(df_raw)[1] <- "kreis_raw"
names(df_raw)[2] <- "kreis_name"

header2 <- df_raw[1, ]

new_names <- names(df_raw)
for (j in 3:ncol(df_raw)) {
  new_names[j] <- str_squish(
    paste0(new_names[j], " ", header2[[j]])
  )
}

names(df_raw) <- new_names

# 2. Create a year column and clean Kreis id / name -------------------

df <- df_raw %>%
  mutate(
    # rows that are just "31.12.2013" etc
    year_tmp = if_else(
      str_detect(kreis_raw, "^\\d{2}\\.\\d{2}\\.\\d{4}$"),
      str_sub(kreis_raw, 7, 10),  # take last 4 chars = year
      NA_character_
    )
  ) %>%
  fill(year_tmp) %>%                        # carry year down
  filter(!str_detect(kreis_raw, "^\\d{2}\\.\\d{2}\\.\\d{4}$")) %>%  # drop pure year rows
  mutate(
    year      = as.integer(year_tmp),
    kreis_id  = as.integer(str_extract(kreis_raw, "^\\d+")),
    kreis_name = kreis_name
  ) %>%
  select(year, kreis_id, kreis_name, everything(), -kreis_raw, -year_tmp)

# 3. Long format: one row per (kreis, year, gender, narrow age bin) ----


library(dplyr)
library(stringr)
library(tidyr)

df_long <- df %>%
  pivot_longer(
    cols = -c(year, kreis_id, kreis_name),
    names_to  = "var",
    values_to = "count"
  ) %>%
  mutate(
    count = as.numeric(count),
    
    # gender
    gender = case_when(
      str_detect(var, "männlich") ~ "male",
      str_detect(var, "weiblich") ~ "female",
      TRUE                        ~ NA_character_
    ),
    
    # extract lower / upper age from the German text
    age_min = as.numeric(str_extract(var, "(?<= )\\d+(?= bis| Jahre)")),
    age_max = as.numeric(str_extract(var, "(?<=bis unter )\\d+(?= Jahre)"))
  ) %>%
  # handle special labels precisely
  mutate(
    age_min = case_when(
      str_detect(var, "unter\\s+3\\s+Jahre") ~ 0,                 # only "unter 3 Jahre"
      TRUE                                   ~ age_min
    ),
    age_max = case_when(
      str_detect(var, "unter\\s+3\\s+Jahre") ~ 3,                 # only "unter 3 Jahre"
      str_detect(var, "65\\s+Jahre und mehr") ~ Inf,              # open-ended 65+
      TRUE                                    ~ age_max
    )
  ) %>%
  filter(!is.na(gender), !is.na(count))


# 4. Build your broad age groups (drop <18) ---------------------------

df_long <- df_long %>%
  mutate(
    age_group = case_when(
      age_min >= 18 & age_min < 25 ~ "18_25",
      age_min >= 25 & age_min < 35 ~ "25_35",
      age_min >= 35 & age_min < 45 ~ "35_45",
      age_min >= 45 & age_min < 60 ~ "45_60",
      age_min >= 60 & age_min < 65 ~ "60_65",
      age_min >= 65               ~ "65_plus",
      TRUE                         ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group))

# 5. Sum within (kreis, year, gender, broad age group) ----------------

agg <- df_long %>%
  group_by(year, kreis_id, kreis_name, gender, age_group) %>%
  summarise(n = sum(count, na.rm = TRUE), .groups = "drop")

# 6. (Optional) total population per Kreis & year ---------------------

kreis_totals <- df_long %>%
  group_by(year, kreis_id, kreis_name) %>%
  summarise(total_pop = sum(count, na.rm = TRUE), .groups = "drop")

# 7. Wide format: each (Kreis, gender, age_group) has columns for 2013/17/22 ----

agg_wide_year <- agg %>%
  unite(group_gender, gender, age_group, sep = "_") %>%   # e.g. "male_18_25"
  pivot_wider(
    names_from  = year,
    values_from = n,
    names_prefix = "y"
  )
# This gives columns y2013, y2017, y2022 per kreis_id / group_gender

# 8. Ratios: 2017/2022 and 2013/2022 for each group & Kreis ----------

agg_wide_year <- agg_wide_year %>%
  mutate(
    ratio_2021_2022 = y2021 / y2022,
    ratio_2017_2022 = y2017 / y2022,
    ratio_2013_2022 = y2013 / y2022
  )

###########



## ----------------------------------------------------------
## 1. Make a Kreis code for each Gemeinde in 2022 census
##    AGS: 2 digits Land + 3 digits Kreis + 3 digits Gemeinde
## ----------------------------------------------------------

brks   <- c("18_24","25_34","35_44","45_59","60_69","70plus")
genders <- c("male","female")

census <- census %>%
  mutate(
    code_str = str_pad(as.character(code), width = 8, pad = "0"),
    kreis_id = substr(code_str, 1, 5)   # Land+Kreis
  )

census$kreis_id = census$kreis_code

## ----------------------------------------------------------
## 2. Prepare Kreis-level ratios (2013/2022, 2017/2022)
## ----------------------------------------------------------

kreis_ratios <- agg_wide_year %>%
  # group_gender is like "male_18_24" or "female_35_44"
  tidyr::extract(
    group_gender,
    into  = c("gender", "age_group"),
    regex = "^(male|female)_(.*)$",  # gender, then the full rest as age_group
    remove = TRUE
  ) %>%
  mutate(
    kreis_id = str_pad(as.character(kreis_id), width = 5, pad = "0")
  ) %>%
  select(kreis_id, gender, age_group,
         ratio_2013_2022, ratio_2017_2022, ratio_2021_2022)



## ----------------------------------------------------------
## 3. Put Gemeinde 2022 counts in long form
##    (only the aggregate age groups you care about)
## ----------------------------------------------------------

census_pop_long <- census %>%
  select(
    code, region_name, kreis_id,
    all_of(paste0("male_",   brks)),
    all_of(paste0("female_", brks))
  ) %>%
  pivot_longer(
    cols = matches("^(male|female)_"),
    names_to      = c("gender", "age_group"),
    names_pattern = "^(male|female)_(.*)$",  # capture gender, then the full age group
    values_to     = "pop_2022"
  ) %>%
  mutate(
    gender   = factor(gender, levels = genders),
    age_group = factor(age_group, levels = brks),
    pop_2022 = as.numeric(pop_2022)
  )

## ----------------------------------------------------------
## 4. Join Kreis ratios and project 2013 & 2017 counts
## ----------------------------------------------------------

# First, harmonize age_group in kreis_ratios to match census_pop_long
kreis_ratios_std <- kreis_ratios %>%
  mutate(
    age_group = recode(
      as.character(age_group),
      "18_25"  = "18_24",
      "25_35"  = "25_34",
      "35_45"  = "35_44",
      "45_60"  = "45_59",
      "60_65"  = "60_69",
      "65_plus" = "70plus"
    )
  )

# Now you can join safely
census_pop_long_m <- census_pop_long %>%
  left_join(kreis_ratios_std,
            by = c("kreis_id", "gender", "age_group")) %>%
  mutate(
    pop_2013 = pop_2022 * ratio_2013_2022,
    pop_2017 = pop_2022 * ratio_2017_2022,
    pop_2021 = pop_2022 * ratio_2021_2022
  )


## ----------------------------------------------------------
## 5. Helper: from long (Gemeinde × gender × age_group)
##    to wide table for a single year with:
##       total_all_year
##       [gender_age_group] (counts)
##       [gender_age_group]_share (shares)
## ----------------------------------------------------------

make_year_table <- function(df_long, year_label) {
  pop_col <- paste0("pop_", year_label)
  
  df_year <- df_long %>%
    group_by(code, region_name, kreis_id) %>%
    mutate(
      total_all = sum(.data[[pop_col]], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      share = .data[[pop_col]] / total_all
    )
  
  # counts wide
  counts_wide <- df_year %>%
    select(code, region_name, kreis_id, age_group, gender,
           pop = !!sym(pop_col), total_all) %>%
    pivot_wider(
      id_cols = c(code, region_name, kreis_id, total_all),
      names_from  = c(gender, age_group),
      values_from = pop,
      names_sep   = "_"
    )
  
  # shares wide – columns like male_18_24_share
  shares_wide <- df_year %>%
    select(code, age_group, gender, share) %>%
    pivot_wider(
      id_cols = code,
      names_from  = c(gender, age_group),
      values_from = share,
      names_sep   = "_"
    ) %>%
    rename_with(\(x) paste0(x, "_share"), -code)
  
  counts_wide %>%
    left_join(shares_wide, by = "code")
}

## ----------------------------------------------------------
## 6. Build 3 separate data sets: 2013, 2017, 2022
## ----------------------------------------------------------
census_2013 <- make_year_table(census_pop_long_m , "2013")
census_2017 <- make_year_table(census_pop_long_m, "2017")
census_2021 <- make_year_table(census_pop_long_m, "2021")

# Example: keep only shares & totals for 2022, like your census_shares_only
share_cols_2022 <- c(
  paste0("male_",   brks, "_share"),
  paste0("female_", brks, "_share")
)

census_shares_2013 <- census_2013 %>%
  select(code, region_name, total_all,
         all_of(share_cols_2022))

census_shares_2017 <- census_2017 %>%
  select(code, region_name, total_all,
         all_of(share_cols_2022))

census_shares_2021 <- census_2021 %>%
  select(code, region_name, total_all,
         all_of(share_cols_2022))

# save if you like
save(census_shares_2021, file = "../output_data/census_2021.Rdata")
save(census_shares_2013, file = "../output_data/census_2013.Rdata")
save(census_shares_2017, file = "../output_data/census_2017.Rdata")

