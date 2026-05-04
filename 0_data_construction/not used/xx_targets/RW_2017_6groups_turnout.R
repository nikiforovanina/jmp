state_names <- c(
  "1"  = "Schleswig-Holstein",
  "2"  = "Hamburg",
  "3"  = "Niedersachsen",
  "4"  = "Bremen",
  "5"  = "Nordrhein-Westfalen",
  "6"  = "Hessen",
  "7"  = "Rheinland-Pfalz",
  "8"  = "Baden-Württemberg",
  "9"  = "Bayern",
  "10" = "Saarland",
  "11" = "Berlin",
  "12" = "Brandenburg",
  "13" = "Mecklenburg-Vorpommern",
  "14" = "Sachsen",
  "15" = "Sachsen-Anhalt",
  "16" = "Thüringen"
)

get_age_gender_turnout_2017 <- function(STATE) {
  # common age groups for all states
  age_groups <- c(
    "18_21","21_25","25_30","30_35",
    "35_40","40_45","45_50","50_60",
    "60_70","70plus"
  )
  
  male_pct   <- NULL
  female_pct <- NULL
  
  ## ---------- 1 Schleswig-Holstein ----------
  if (STATE == 1) {
    male_pct   <- c(68.9, 63.8, 63.0, 68.8, 71.3, 74.3, 76.5, 78.1, 82.3, 82.6)
    female_pct <- c(69.1, 64.8, 65.5, 71.0, 75.1, 76.1, 79.6, 80.6, 83.1, 75.3)
    
    ## ---------- 2 Hamburg ----------
  } else if (STATE == 2) {
    male_pct   <- c(63.2, 61.4, 67.4, 73.7, 75.6, 75.3, 78.0, 77.5, 78.4, 80.8)
    female_pct <- c(65.6, 67.3, 74.2, 75.7, 77.1, 80.0, 79.6, 78.6, 79.6, 75.8)
    
    ## ---------- 3 Niedersachsen ----------
  } else if (STATE == 3) {
    male_pct   <- c(67.4, 65.2, 65.2, 68.8, 73.0, 75.4, 77.8, 78.3, 82.3, 82.5)
    female_pct <- c(69.2, 68.0, 67.8, 72.9, 75.2, 77.9, 78.6, 79.2, 82.1, 75.2)
    
    ## ---------- 4 Bremen ----------
  } else if (STATE == 4) {
    male_pct   <- c(57.2, 59.6, 60.6, 69.7, 65.8, 64.4, 70.4, 71.7, 75.5, 78.6)
    female_pct <- c(60.1, 59.5, 67.2, 68.3, 71.0, 70.5, 73.6, 75.0, 77.0, 73.1)
    
    ## ---------- 5 Nordrhein-Westfalen ----------
  } else if (STATE == 5) {
    male_pct   <- c(66.0, 64.1, 65.5, 68.9, 72.0, 73.5, 76.8, 78.7, 81.1, 81.5)
    female_pct <- c(68.5, 66.1, 68.3, 71.3, 73.7, 75.3, 77.9, 79.4, 80.4, 74.3)
    
    ## ---------- 6 Hessen ----------
  } else if (STATE == 6) {
    male_pct   <- c(70.7, 65.8, 67.0, 69.8, 73.3, 75.3, 79.3, 80.8, 82.6, 81.1)
    female_pct <- c(71.6, 68.9, 70.0, 71.1, 74.6, 77.7, 80.6, 81.5, 82.0, 74.2)
    
    ## ---------- 7 Rheinland-Pfalz ----------
  } else if (STATE == 7) {
    male_pct   <- c(70.8, 68.3, 65.9, 70.1, 74.5, 75.8, 78.6, 80.7, 83.5, 83.0)
    female_pct <- c(75.1, 69.9, 68.6, 73.9, 76.7, 77.5, 80.4, 80.9, 82.4, 75.5)
    
    ## ---------- 8 Baden-Württemberg ----------
  } else if (STATE == 8) {
    male_pct   <- c(72.2, 68.9, 71.5, 75.2, 76.1, 76.8, 79.9, 81.5, 83.4, 82.2)
    female_pct <- c(72.7, 70.6, 73.7, 76.8, 77.2, 78.6, 81.7, 81.7, 82.6, 74.0)
    
    ## ---------- 9 Bayern ----------
  } else if (STATE == 9) {
    male_pct   <- c(73.7, 70.3, 70.8, 74.6, 76.4, 78.5, 80.6, 81.2, 82.8, 81.4)
    female_pct <- c(76.6, 72.1, 74.1, 76.2, 77.3, 79.5, 80.8, 81.7, 82.1, 72.3)
    
    ## ---------- 10 Saarland ----------
  } else if (STATE == 10) {
    male_pct   <- c(71.2, 61.1, 64.8, 66.7, 71.7, 72.7, 77.4, 79.5, 81.2, 83.2)
    female_pct <- c(64.8, 68.0, 67.2, 69.8, 73.7, 79.4, 77.8, 81.9, 82.5, 75.9)
    
    ## ---------- 11 Berlin ----------
  } else if (STATE == 11) {
    male_pct   <- c(67.8, 65.0, 68.1, 73.6, 74.1, 76.3, 78.1, 77.8, 78.0, 77.2)
    female_pct <- c(70.1, 67.8, 72.4, 76.9, 79.5, 78.4, 82.3, 78.1, 78.5, 71.5)
    
    ## ---------- 12 Brandenburg ----------
  } else if (STATE == 12) {
    male_pct   <- c(66.1, 58.5, 61.0, 65.8, 71.0, 73.5, 76.5, 76.6, 78.6, 75.0)
    female_pct <- c(67.7, 63.3, 65.8, 70.2, 75.6, 76.5, 80.7, 78.5, 79.3, 66.9)
    
    ## ---------- 13 Mecklenburg-Vorpommern ----------
  } else if (STATE == 13) {
    male_pct   <- c(61.2, 55.1, 55.5, 61.0, 64.3, 67.3, 72.3, 73.7, 77.3, 76.6)
    female_pct <- c(59.5, 61.8, 58.4, 65.7, 67.5, 73.9, 76.8, 76.4, 79.0, 65.6)
    
    ## ---------- 14 Sachsen ----------
  } else if (STATE == 14) {
    male_pct   <- c(71.1, 65.0, 68.2, 72.1, 74.4, 77.6, 79.5, 77.6, 78.8, 75.5)
    female_pct <- c(70.8, 68.2, 72.3, 76.0, 78.4, 78.3, 82.5, 79.9, 79.2, 67.2)
    
    ## ---------- 15 Sachsen-Anhalt ----------
  } else if (STATE == 15) {
    male_pct   <- c(61.3, 52.6, 56.9, 61.9, 63.2, 67.5, 69.7, 70.0, 73.9, 70.0)
    female_pct <- c(65.3, 57.1, 62.9, 64.2, 67.9, 71.2, 75.3, 73.3, 74.2, 60.7)
    
    ## ---------- 16 Thüringen ----------
  } else if (STATE == 16) {
    male_pct   <- c(66.6, 64.6, 67.8, 69.7, 72.0, 75.0, 75.2, 76.7, 79.2, 75.9)
    female_pct <- c(65.4, 67.9, 69.4, 72.1, 76.0, 77.5, 78.3, 77.3, 79.3, 68.2)
    
  } else {
    stop("STATE not implemented yet")
  }
  
  # sanity check
  if (length(male_pct)   != length(age_groups) ||
      length(female_pct) != length(age_groups)) {
    stop("Length mismatch between age groups and turnout vectors.")
  }
  
  # convert percent -> proportion and name by age group
  male_turnout   <- setNames(male_pct   / 100, age_groups)
  female_turnout <- setNames(female_pct / 100, age_groups)
  
  list(
    male_turnout   = male_turnout,
    female_turnout = female_turnout,
    age_groups     = age_groups
  )
}