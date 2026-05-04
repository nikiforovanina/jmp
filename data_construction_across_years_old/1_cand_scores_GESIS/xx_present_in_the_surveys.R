library(dplyr)
library(tibble)
library(readr)
library(stringr)
rm(list = ls())
# ----------------------------
# 0) INPUTS / PATHS
# ----------------------------
ww_cand <- read.csv("candidates_with_erststimmen.csv")
library(dplyr)
library(readr)

read_wk_names <- function(url) {
  read_delim(
    url,
    delim = ";",
    comment = "#",                 # <-- key: skip comment lines
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  )
}

wk13 <- read_wk_names("https://www.bundeswahlleiterin.de/dam/jcr/abc2fb4f-94e2-4833-bcb4-151c42773456/btw13_wahlkreisnamen.csv")
wk17 <- read_wk_names("https://www.bundeswahlleiterin.de/dam/jcr/90ae9719-97bb-43f9-8e26-3e9ef0f18609/btw17_wahlkreisnamen.csv")
wk21 <- read_wk_names("https://www.bundeswahlleiterin.de/dam/jcr/b46e4c9d-290c-4b96-a64b-0edee1c780a5/btw21_wahlkreisnamen_utf8.csv")

# now these exist:
names(wk13)

wk_state_13 <- wk13 %>% transmute(year = 2013, wk_num = WKR_NR, state_abbr = LAND_ABK)
wk_state_17 <- wk17 %>% transmute(year = 2017, wk_num = WKR_NR, state_abbr = LAND_ABK)
wk_state_21 <- wk21 %>% transmute(year = 2021, wk_num = WKR_NR, state_abbr = LAND_ABK)

wk_state_all <- bind_rows(wk_state_13, wk_state_17, wk_state_21)
parties_keep <- c("GRÜNE", "SPD", "AfD", "FDP", "DIE LINKE", "CDU", "CSU")

# Party code -> label (match ww_cand$party_label)
party_lut <- tibble::tribble(
  ~orig, ~party_label_lut,
  2  ,  "CDU",
  3  ,  "CSU",
  4  ,  "SPD",
  5  ,  "FDP",
  6  ,  "GRÜNE",
  7  ,  "DIE LINKE",
  322 ,  "AfD"
)

# East/West by state abbreviation
east_states <- c("BB","MV","SN","ST","TH","BE")
west_states <- c("BW","BY","HB","HH","HE","NI","NW","RP","SH","SL")

# ----------------------------
# 1) Helpers
# ----------------------------
read_wk_names_simple <- function(url) {
  # Bundeswahlleiter files are simple CSVs; delimiter varies, but read_delim handles most
  suppressWarnings(
    readr::read_delim(url, delim = ";", show_col_types = FALSE, locale = locale(encoding="UTF-8"))
  )
}

get_state_abbr_map <- function(wk_df) {
  # try to find a state-abbrev column (LAND_ABK / LAND / etc.)
  # and the district number column (WKR_NR)
  nm <- names(wk_df)
  
  wk_col <- nm[grepl("^WKR_NR$", nm)]
  if (length(wk_col) == 0) stop("Cannot find WKR_NR in wk file.")
  
  # best: a column containing 'ABK' (abbreviation)
  abk_col <- nm[grepl("ABK", nm, ignore.case = TRUE)]
  if (length(abk_col) > 0) {
    out <- wk_df %>%
      transmute(wk_num = .data[[wk_col[1]]],
                state_abbr = .data[[abk_col[1]]])
    return(out)
  }
  
  # fallback: a column that looks like LAND_NAME (full name)
  land_col <- nm[grepl("LAND", nm, ignore.case = TRUE) & !grepl("ABK", nm, ignore.case = TRUE)]
  if (length(land_col) == 0) stop("Cannot find a Bundesland column in wk file.")
  
  # map full names -> abbreviations
  name_to_abbr <- c(
    "Baden-Württemberg"="BW","Bayern"="BY","Berlin"="BE","Brandenburg"="BB",
    "Bremen"="HB","Hamburg"="HH","Hessen"="HE","Mecklenburg-Vorpommern"="MV",
    "Niedersachsen"="NI","Nordrhein-Westfalen"="NW","Rheinland-Pfalz"="RP",
    "Saarland"="SL","Sachsen"="SN","Sachsen-Anhalt"="ST","Schleswig-Holstein"="SH",
    "Thüringen"="TH"
  )
  
  out <- wk_df %>%
    transmute(wk_num = .data[[wk_col[1]]],
              state_name = as.character(.data[[land_col[1]]])) %>%
    mutate(state_abbr = unname(name_to_abbr[state_name]))
  
  return(out %>% select(wk_num, state_abbr))
}

make_present_year <- function(year, ww, rdata_path, obj_name, party_lut) {
  # load candidate survey file into temp env
  e <- new.env(parent = emptyenv())
  load(rdata_path, envir = e)
  cand <- get(obj_name, envir = e)
  
  cand_full <- cand %>% select(wknr, partei) %>%
    left_join(party_lut, by = c("partei" = "orig")) %>%
    mutate(wk_party = paste0(wknr, "_", party_label_lut))
  
  ww_y <- ww %>%
    filter(year == !!year, party_label %in% parties_keep) %>%
    mutate(wk_party = paste0(wk_num, "_", party_label)) %>%
    left_join(cand_full, by = "wk_party") %>%
    mutate(present = ifelse(is.na(partei), 0L, 1L))
  
  ww_y
}

# ----------------------------
# 2) Build present for 2013/2017/2021
# ----------------------------

ww_cand$party_label[which(ww_cand$party_label == 'CSU')] <- 'CDU'

rdata_13 <- "/Users/ninanikiforova/Desktop/жмп/data_construction_across_years/2_cand_survey_full_data_wen_waehlen/data/cand13_scored_full.Rdata"
rdata_17 <- "/Users/ninanikiforova/Desktop/жмп/data_construction_across_years/2_cand_survey_full_data_wen_waehlen/data/cand17_scored_full.Rdata"
rdata_21 <- "/Users/ninanikiforova/Desktop/жмп/data_construction_across_years/2_cand_survey_full_data_wen_waehlen/data/cand21_scored_full.Rdata"

ww_13_m <- make_present_year(2013, ww_cand, rdata_13, "cand13_scored_full", party_lut)
ww_17_m <- make_present_year(2017, ww_cand, rdata_17, "cand17_scored_full", party_lut)
ww_21_m <- make_present_year(2021, ww_cand, rdata_21, "cand21_scored_full", party_lut)

# ----------------------------
# 3) Add state + East/West (year-specific wk files)
# ----------------------------
#wk13 <- read_wk_names_simple(wk_url_13)
#wk17 <- read_wk_names_simple(wk_url_17)
#wk21 <- read_wk_names_simple(wk_url_21)

wk_state_all <- bind_rows(wk_state_13, wk_state_17, wk_state_21)

ww_all <- bind_rows(
  ww_13_m %>% mutate(year = 2013),
  ww_17_m %>% mutate(year = 2017),
  ww_21_m %>% mutate(year = 2021)
) %>%
  left_join(wk_state_all, by = c("year", "wk_num")) %>%
  mutate(
    eastwest = case_when(
      state_abbr %in% east_states ~ "East",
      state_abbr %in% west_states ~ "West",
      TRUE ~ NA_character_
    )
  )

# ----------------------------
# 4) SUMMARY SHARES + SAVE
# ----------------------------
unique(ww_all$party_label)
#ww_all <- ww_all %>% filter(!ww_all$party_label == 'CSU')

# overall share present by year
present_overall <- ww_all %>%
  group_by(year) %>%
  summarise(n = n(),
            share_present = mean(present, na.rm = TRUE),
            .groups = "drop")

# by party (year × party)
present_by_party <- ww_all %>% 
  group_by(party_label) %>%
  summarise(n = n(),
            share_present = mean(present, na.rm = TRUE),
            .groups = "drop")


# by party (year × party)
present_by_party <- ww_all %>% 
  group_by(year, party_label) %>%
  summarise(n = n(),
            share_present = mean(present, na.rm = TRUE),
            .groups = "drop")

# by party × East/West (year × party × region)
present_by_party_ew <- ww_all %>%
  filter(!is.na(eastwest)) %>%
  group_by(year, party_label, eastwest) %>%
  summarise(n = n(),
            share_present = mean(present, na.rm = TRUE),
            .groups = "drop")


# print + save
print(present_overall)
print(present_by_party)
print(present_by_party_ew)

write_csv(present_overall,      "present_share_overall.csv")
write_csv(present_by_party,     "present_share_by_party.csv")
write_csv(present_by_party_ew,  "present_share_by_party_eastwest.csv")

######


sig_tests <- ww_all %>%
  filter(!is.na(eastwest), !is.na(erststimme_pct), !is.na(present)) %>%
  group_by(year, party_label) %>%
  summarise(
    n_present = sum(present == 1),
    n_absent  = sum(present == 0),
    mean_present = mean(erststimme_pct[present == 1], na.rm = TRUE),
    mean_absent  = mean(erststimme_pct[present == 0], na.rm = TRUE),
    diff         = mean_present - mean_absent,
    p_value = tryCatch(
      t.test(erststimme_pct[present == 1],
             erststimme_pct[present == 0])$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(stars = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    p_value < 0.10  ~ ".",
    TRUE            ~ ""
  )) %>%
  arrange(year, party_label)

sig_tests


sig_tests %>%
  mutate(
    diff    = sprintf("%.2f", diff),
    p_value = sprintf("%.4f", p_value),
    mean_present = sprintf("%.2f", mean_present),
    mean_absent  = sprintf("%.2f", mean_absent)
  ) %>%
  select(year, party_label, n_present, n_absent,
         mean_present, mean_absent, diff, p_value, stars) %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      col.names = c("Year", "Party", "$n_{present}$", "$n_{absent}$",
                    "Mean (present)", "Mean (absent)", "Diff", "$p$-value", ""),
      escape    = FALSE,
      caption   = "Difference in Erststimme share by candidate presence",
      label     = "erststimme_present") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  save_kable("table_erststimme_present.tex")



sig_tests <- ww_all %>%
  filter(!is.na(eastwest), !is.na(erststimme_winner), !is.na(present)) %>%
  group_by(year, party_label) %>%
  summarise(
    n_present = sum(present == 1),
    n_absent  = sum(present == 0),
    mean_present = mean(erststimme_winner[present == 1], na.rm = TRUE),
    mean_absent  = mean(erststimme_winner[present == 0], na.rm = TRUE),
    diff         = mean_present - mean_absent,
    p_value = tryCatch(
      t.test(erststimme_winner[present == 1],
             erststimme_winner[present == 0])$p.value,
      error = function(e) NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(stars = case_when(
    p_value < 0.001 ~ "***",
    p_value < 0.01  ~ "**",
    p_value < 0.05  ~ "*",
    p_value < 0.10  ~ ".",
    TRUE            ~ ""
  )) %>%
  arrange(year, party_label)


sig_tests %>%
  mutate(
    diff    = sprintf("%.2f", diff),
    p_value = sprintf("%.4f", p_value),
    mean_present = sprintf("%.2f", mean_present),
    mean_absent  = sprintf("%.2f", mean_absent)
  ) %>%
  select(year, party_label, n_present, n_absent,
         mean_present, mean_absent, diff, p_value, stars) %>%
  kbl(format    = "latex",
      booktabs  = TRUE,
      col.names = c("Year", "Party", "$n_{present}$", "$n_{absent}$",
                    "Mean (present)", "Mean (absent)", "Diff", "$p$-value", ""),
      escape    = FALSE,
      caption   = "Difference in Erststimme share by candidate presence",
      label     = "erststimme_present") %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  save_kable("table_win_present.tex")

