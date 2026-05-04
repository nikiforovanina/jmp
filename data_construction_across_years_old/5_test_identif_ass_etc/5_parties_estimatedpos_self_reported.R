rm(list = ls())
set.seed(42)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
library(dplyr); library(tidyr); library(purrr); library(ggplot2)
library(haven); library(forcats); library(stringr)
library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(labelled)
library(lfe)

library(ggplot2)
library(stargazer)

dat_raw <- haven::read_dta("post_pre_el_2021.dta")
dat <- dat_raw
ggplot(data = dat) + geom_histogram(aes(q35h)) + facet_wrap(.~bula) + xlim(0,12)

load("../4_full_data/data/full_cand_position_21.RData")
# load("~/Desktop/nonparam_ae_lr/data_model/data/cand21_scored_LR.Rdata") same


dat_short <- dat[c('bula', 'wknr', 'q35b', 'q35c', 'q35d', 'q35e', 'q35f', 'q35g', 'q35h', 'q75a', 'q37')]
#dat_short <- dat[c('bula', 'wknr', 'q39b', 'q39c', 'q39d', 'q39e', 'q39f', 'q39g', 'q39h', 'q75a')]


dat_short <- merge(dat_short, dat_nat, by = 'wknr')

# helper: labelled/numeric -> numeric, drop common GESIS missings
to_num <- function(x){
  v <- suppressWarnings(as.numeric(x))
  v[v %in% c(-99,-98,-97,-96,-95,-94,-93,-92,-91,-86,-83, -73, -72,-71,-9,-8,-7,-6,-4,-3,-2,-1)] <- NA_real_
  v
}

# 0) Start: keep lfdn too
dat_short <- dat %>% 
  select(lfdn, bula, wknr, q35b, q35c, q35d, q35e, q35f, q35g, q35h, q75a, q37)

# 1) Derive respondent’s chosen party from q75a (codes or labels)
q75_code <- to_num(dat_short$q75a)
q75_lab  <- tolower(as.character(haven::as_factor(dat_short$q75a, levels = "labels")))

dat_short <- dat_short %>%
  mutate(
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

# 2) Long format: one row per (lfdn, bula, wknr, party)
long_q35 <- dat_short %>%
  pivot_longer(
    cols = starts_with("q35"),
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
    )
  ) %>%
  select(lfdn, bula, wknr, party, party_pos, chosen, q37)

# peek
long_q35 %>% arrange(lfdn, bula, wknr, party) %>% print(n = 12)

long_q35 <- long_q35 %>% mutate(party_code = dplyr::case_when(tolower(party) 
                                                              %in% c("cdu","csu","cdu/csu") ~ 1L, tolower(party)=="spd" ~ 2L,
                                                              tolower(party) %in% c("green","greens","grüne","gruene") ~ 3L, 
                                                              tolower(party)=="fdp" ~ 4L, tolower(party) %in% c("left","die linke") ~ 5L, 
                                                              tolower(party)=="afd" ~ 6L, TRUE ~ NA_integer_))


dat_nat <- dat_nat %>%
  mutate(party6 = case_when(
    as.numeric(partei) %in% c(2, 3) ~ 1L,  # CDU/CSU
    as.numeric(partei) == 4        ~ 2L,  # SPD
    as.numeric(partei) == 6        ~ 3L,  # Greens
    as.numeric(partei) == 5        ~ 4L,  # FDP
    as.numeric(partei) == 7        ~ 5L,  # Left
    as.numeric(partei) == 322      ~ 6L,  # AfD
    TRUE                           ~ NA_integer_  # all other (negative/missing) codes
  ))

long_q35_clean <- long_q35 %>%
  mutate(
    wknr = as.character(as.numeric(wknr)),
    party_code = as.character(as_factor(party_code))
  )

dat_nat_clean <- dat_nat %>%
  mutate(
    wknr   = as.integer(unlabelled(wknr)),
    party6 = as.integer(party6)
  )

dat_nat_clean$m <- paste0(dat_nat_clean$wknr, dat_nat_clean$party6)
long_q35_clean$m <- paste0(long_q35_clean$wknr, long_q35_clean$party_code)


merged <- merge(long_q35_clean, dat_nat_clean, by = c("m" = "m"), all.x = TRUE)
merged <- merged %>% filter(!bula %in% c(2,4,10,11)) # subset only to the studied regions
merged <- merged %>% unique()

merged$LR_ego <- to_num(merged$q37)

m1 <- felm(econ_lr_score ~ LR_ego| bula + party6|0|0, data = merged )
m2 <- felm(econ_lr_score ~ LR_ego| bula + party6|0|0, data = merged %>% filter(chosen == 1) )
m3 <- felm(econ_lr_score ~ LR_ego| bula + party6|0|0, data = merged %>% filter(chosen == 1) %>% filter(party6 == 6) )

summary(m2)


# stargazer core (no header; we’ll add our own)
tab_body <- stargazer(
  m1, m2, m3,
  type = "latex",
  title = "",
  out = "self_pos_estimated_pos.tex",
  dep.var.caption = "",
  dep.var.labels = "",
  column.labels = c("All Parties", "Chosen Parties", "Chosen AfD"),
  model.numbers = TRUE,
  omit.stat = c("f", "ser"),
  covariate.labels = c("Self, L--R"),
  keep = c("LR_ego"),
  add.lines = list(
    c("State F.E.", "Yes", "Yes", "Yes"),
    c("Party F.E.", "Yes", "Yes", "Yes")
  ),
  star.cutoffs = c(0.1, 0.05, 0.01),
  digits = 3,
  no.space = TRUE,
  header = FALSE
)


