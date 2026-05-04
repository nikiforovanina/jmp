rm(list = ls())

library(stargazer)
library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(tibble)


dat <- haven::read_dta("~/Desktop/DRAFT/pre_post_el_survey_2021/post_pre_el_2021.dta")
load("~/Desktop/nonparam_ae_lr/data_model/data/full_cand_position_21.RData")
load("~/Desktop/nonparam_ae_lr/data_model/data/cand21_scored_LR.Rdata")
load('wen_merged_with_imputations_2021.Rdata')


# Helpers
to_num <- function(x){
  v <- suppressWarnings(as.numeric(x))
  v[v %in% c(-99,-98,-97,-96,-95,-94,-93,-92,-91,-86,-83,-72,-71,-9,-8,-7,-6,-4,-3,-2,-1)] <- NA_real_
  v
}

party_lut_codes <- tibble::tribble(
  ~orig, ~party_id,
  2   ,   1,  # CDU
  3   ,   1,  # CSU -> CDU bucket
  4   ,   2,  # SPD
  6   ,   3,  # GRUENE
  5   ,   4,  # FDP
  7   ,   5,  # LINKE
  322 ,   6   # AfD
)

# 0) Start: keep lfdn and add q37, q75a
dat_short <- dat %>% 
  select(lfdn, bula, wknr, q35b, q35c, q35d, q35e, q35f, q35g, q35h, q37, q75a) %>%
  mutate(
    wknr    = as.numeric(wknr),
    q37_num = to_num(q37),
    q37_lab = as.character(haven::as_factor(q37, levels = "labels"))
  )

if (exists("dat_nat")) {
  dat_short <- dat_short %>% left_join(dat_nat, by = "wknr")
}

# 1) Derive respondent’s chosen party from q75a (codes + labels)
q75_code <- to_num(dat_short$q75a)
q75_lab  <- tolower(as.character(haven::as_factor(dat_short$q75a, levels = "labels")))

dat_short <- dat_short %>%
  mutate(
    q75_code = q75_code,
    q75_lab  = q75_lab,
    chosen_party = case_when(
      !is.na(q75_code) & q75_code == 322 ~ "AfD",
      !is.na(q75_code) & q75_code == 4   ~ "SPD",
      !is.na(q75_code) & q75_code == 5   ~ "FDP",
      !is.na(q75_code) & q75_code == 6   ~ "Greens",
      !is.na(q75_code) & q75_code == 7   ~ "Left",
      !is.na(q75_code) & q75_code %in% c(1,2,3) ~ {
        ifelse(str_detect(q75_lab, "\\bcsu\\b"), "CSU",
               ifelse(str_detect(q75_lab, "\\bcdu\\b"), "CDU", "CDU/CSU"))
      },
      str_detect(q75_lab, "\\bafd\\b")               ~ "AfD",
      str_detect(q75_lab, "\\bspd\\b")               ~ "SPD",
      str_detect(q75_lab, "\\bfdp\\b")               ~ "FDP",
      str_detect(q75_lab, "gr[üu]ne|gruene|greens")  ~ "Greens",
      str_detect(q75_lab, "die\\s*linke|\\bleft\\b") ~ "Left",
      str_detect(q75_lab, "\\bcsu\\b")               ~ "CSU",
      str_detect(q75_lab, "\\bcdu\\b")               ~ "CDU",
      TRUE ~ NA_character_
    )
  )

# 2) Long format: one row per (lfdn, bula, wknr, party), q37 is carried through unchanged per person
long_q35 <- dat_short %>%
  pivot_longer(
    cols = c(q35b, q35c, q35d, q35e, q35f, q35g, q35h),
    names_to   = "q35var",
    values_to  = "party_pos"
  ) %>%
  mutate(
    party = recode(q35var,
                   q35b = "CDU",
                   q35c = "CSU",
                   q35d = "SPD",
                   q35e = "FDP",
                   q35f = "Greens",
                   q35g = "Left",
                   q35h = "AfD"),
    party_pos = to_num(party_pos),
    chosen = case_when(
      party %in% c("CDU","CSU") & chosen_party %in% c("CDU","CSU","CDU/CSU") ~ 1L,
      party == chosen_party ~ 1L,
      TRUE ~ 0L
    ),
    # Consistent party_id: CDU/CSU=1, SPD=2, GRUENE=3, FDP=4, LINKE=5, AfD=6
    party_id = dplyr::case_when(
      tolower(party) %in% c("cdu","csu","cdu/csu") ~ 1L,
      tolower(party) == "spd"                      ~ 2L,
      tolower(party) %in% c("greens","grüne","gruene","green") ~ 3L,
      tolower(party) == "fdp"                      ~ 4L,
      tolower(party) %in% c("left","die linke")    ~ 5L,
      tolower(party) == "afd"                      ~ 6L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(
    lfdn, bula, wknr,
    q37_num, q37_lab,          # <- q37 carried over per individual
    party, party_id, party_pos, chosen
  )

########
wen_merged_with_imputations <- wen_merged_with_imputations %>% filter(is.na(econ_lr_score_mean))
wen_to_merge <- wen_merged_with_imputations[c( "wknr",  "party_id", "econ_lr_score"  , "anti_elite_score")]

cand21_scored_pid <- cand21_scored %>%
  mutate(
    wknr   = as.numeric(haven::zap_labels(wknr)),
    partei = as.numeric(haven::zap_labels(partei))
  ) %>%
  rename(partei_num = partei) %>%
  left_join(party_lut_codes, by = c("partei_num" = "orig")) %>%
  filter(!is.na(party_id)) %>%
  select(wknr, party_id, econ_lr_score, anti_elite_score) %>%
  mutate(party_id = as.integer(party_id))


cand_21_full <- rbind(cand21_scored_pid, wen_to_merge)

cand_21_full$m <- paste0(cand_21_full$wknr, '-', cand_21_full$party_id)
long_q35$m <- paste0(long_q35$wknr,'-', long_q35$party_id)
long_q35$party_id <- NULL
merged <- merge(long_q35, cand_21_full, by = c("m" = "m"), all.x = TRUE)

merged <- merged %>% filter(party_pos > 0)
merged$q37_num <- ifelse(merged$q37_num<0, NA, merged$q37_num)
merged <- merged %>% filter(!is.na(econ_lr_score))


library(lfe)
# predictive of my perception of policy position of parties
m1 <- felm(party_pos ~ econ_lr_score | bula + party_id | 0| lfdn, data = merged %>% filter(!bula %in% c(2,4,10, 11))  )
m2 <- felm(party_pos ~ econ_lr_score | bula + party_id | 0| lfdn, data = merged %>% filter(chosen == 1)  %>% filter(!bula %in% c(2,4,10, 11)) )
m3 <- felm(party_pos ~ econ_lr_score | bula + party_id | 0| lfdn, data = merged %>% filter(chosen == 1) %>% filter(party_id == 6) %>% filter(!bula %in% c(2,4,10, 11)) )

# not predictive of my own policy position
m4 <- felm(q37_num ~ econ_lr_score | bula + party_id | 0| lfdn, data = merged %>% filter(chosen == 1)  %>% filter(!bula %in% c(2,4,10, 11))  )


stargazer::stargazer(
  m1, m2, m3, m4,
  type = "latex",
  title = "Candidate–Party Left–Right Models",
  label = "tab:lr_models",
  column.labels = c("All Parties", "Chosen Parties", "Chosen AfD", "Chosen Parties"),
  dep.var.labels.include = FALSE,         # we'll show DV descriptions via add.lines
  covariate.labels = c("Candidate Position, Left-Right"),
  keep = c("econ_lr_score"),              # show only the main regressor
  add.lines = list(
    c("Dependent variable:",
      "Perceived Party Position, Left-Right",
      "Perceived Party Position, Left-Right",
      "Perceived Party Position, Left-Right",
      "Own Position, Left-Right"),
    c("State F.E.", "Yes", "Yes", "Yes", "Yes"),
    c("Party F.E.", "Yes", "Yes", "Yes", "Yes")
  ),
  omit.stat = c("f", "ser"),              # optional: hide F-stat & SER
  digits = 3,
  no.space = TRUE,
  float = TRUE, table.placement = "!htbp",
  notes = "All models include state (bula) and party fixed effects; standard errors clustered by respondent (lfdn).",
  notes.align = "l",
  out = 'regr_positions_perceived_real.tex'
)





