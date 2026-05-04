#########################################################################
### Processing census data to create municipality - group shares data ###
#########################################################################

# census was conducted in 2022, but was backdated to 2021 to comply with the EU regulations
# hence, we use it for the year 2021 without any shifts
rm(list = ls())
set.seed(42)


census <- read.csv2("Gemeinde_5Jahr_Geschlecht_1000A-2026_de.csv")
colnames(census)

# Make clean, informative column names for your 'census' data frame

library(dplyr)
library(stringr)

# row 1 contains the German labels for age bands
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
# (Optional) Tidy/long version if you want one row per (municipality, sex, age_bracket)
census_shares_long <- census %>%
  select(code, region_name,total_all, pop_total_bracketed, all_of(bracket_cols)) %>%
  pivot_longer(
    cols = all_of(bracket_cols),
    names_to = c("sex","age_bracket"),
    names_pattern = "(male|female)_(.*)",
    values_to = "n"
  ) %>%
  mutate(share = n / pop_total_bracketed)


brks <- c("18_24","25_34","35_44","45_59","60_69","70plus")
share_cols <- c(paste0("male_", brks, "_share"),
                paste0("female_", brks, "_share"))

census_shares_only <- census |>
  dplyr::select(code, region_name, total_all, pop_total_bracketed, dplyr::all_of(share_cols))

save(census_shares_only, file = 'census_2021.Rdata')


######################################################################################
######################################################################################
# only sanity checks after
# sanity check: shares across the 12 brackets should sum to ~1
check <- census %>%
  transmute(code, region_name, share_sum = rowSums(across(ends_with("_share")), na.rm = TRUE))
summary(check$share_sum)

# checking the variation in the demogr composition of municipalities 
ggplot() + geom_histogram(aes(census$female_70plus_share))
ggplot() + geom_histogram(aes(census$male_35_44_share))
ggplot() + geom_histogram(aes(census_shares_only$male_18_24_share))

# checking eligble shares are reasonable:
census$share_eligb <-  census$pop_total_bracketed/census$total_all
ggplot() + geom_histogram(aes(census$pop_total_bracketed/census$total_all))
summary(census$share_eligb)



# later on: restrict analysis to gemeinde with eligble voters pop > 100 and pop < 5000
# result: 7410 municipalities out of 10790
census_pop_reduced <- census_shares_only[-which(census_shares_only$pop_total_bracketed < 200),]
census_pop_reduced <- census_pop_reduced[-which(census_pop_reduced$pop_total_bracketed > 5000),]

census_pop_reduced$share_eligb <-  census_pop_reduced$pop_total_bracketed/census_pop_reduced$total_all
summary(census_pop_reduced$share_eligb)

median(census_pop_reduced$pop_total_bracketed)



