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

get_age_gender_turnout <- function(STATE) {
  # common age groups for all states
  age_groups <- c(
    "18_21","21_25","25_30","30_35",
    "35_40","40_45","45_50","50_60",
    "60_70","70plus"
  )
  
  male_pct   <- NULL
  female_pct <- NULL
  
  ## ---------- Schleswig-Holstein ----------
  if (STATE == 1) {
    male_pct   <- c(62.1, 58.1, 58.5, 64.2, 65.6, 70.7, 75.7, 75.7, 82.0, 83.1)
    female_pct <- c(62.6, 57.1, 61.7, 67.0, 70.1, 74.5, 75.2, 78.1, 82.6, 76.0)
    
    ## ---------- Hamburg ----------
  } else if (STATE == 2) {
    male_pct   <- c(56.7, 62.4, 65.7, 66.1, 69.8, 73.8, 73.7, 74.4, 77.3, 78.7)
    female_pct <- c(58.8, 64.2, 66.8, 70.6, 72.7, 75.3, 76.2, 75.6, 79.6, 70.2)
    
    ## ---------- Niedersachsen ----------
  } else if (STATE == 3) {
    male_pct   <- c(64.1, 58.7, 60.4, 63.9, 68.4, 72.1, 76.5, 77.6, 83.1, 83.3)
    female_pct <- c(62.2, 59.8, 62.3, 66.5, 70.1, 75.3, 76.8, 76.4, 82.5, 74.7)
    
    ## ---------- Bremen (not yet coded) ----------
  } else if (STATE == 4) {
    stop("STATE 4 (Bremen) not implemented yet")
    
    ## ---------- Nordrhein-Westfalen ----------
  } else if (STATE == 5) {
    male_pct   <- c(65.6, 60.7, 62.6, 65.7, 68.3, 72.1, 74.8, 76.3, 80.7, 81.4)
    female_pct <- c(64.9, 62.4, 64.2, 67.0, 70.5, 74.2, 76.4, 76.6, 80.2, 73.2)
    
    ## ---------- Hessen ----------
  } else if (STATE == 6) {
    male_pct   <- c(66.7, 61.7, 60.5, 66.0, 69.8, 71.9, 75.9, 77.6, 81.4, 81.7)
    female_pct <- c(68.2, 62.2, 64.8, 66.1, 69.6, 75.3, 77.2, 77.7, 81.6, 73.4)
    
    ## ---------- Rheinland-Pfalz ----------
  } else if (STATE == 7) {
    male_pct   <- c(65.2, 60.1, 60.8, 64.8, 67.9, 71.8, 74.2, 76.5, 81.6, 81.8)
    female_pct <- c(67.2, 60.3, 63.2, 65.0, 69.3, 73.5, 76.1, 77.2, 81.0, 74.2)
    
    ## ---------- Baden-Württemberg ----------
  } else if (STATE == 8) {
    male_pct   <- c(68.4, 64.6, 66.5, 68.6, 71.7, 74.2, 76.5, 78.9, 82.8, 82.2)
    female_pct <- c(68.3, 64.5, 66.8, 70.3, 71.5, 75.3, 77.5, 78.5, 81.7, 72.6)
    
    ## ---------- Bayern ----------
  } else if (STATE == 9) {
    male_pct   <- c(63.5, 60.9, 61.0, 64.5, 67.2, 70.9, 73.8, 75.3, 79.5, 79.1)
    female_pct <- c(63.1, 59.6, 62.6, 64.8, 68.0, 71.4, 73.9, 74.9, 78.6, 69.4)
    
    ## ---------- Saarland ----------
  } else if (STATE == 10) {
    male_pct   <- c(67.5, 62.7, 62.5, 66.5, 68.4, 69.8, 75.5, 75.8, 81.1, 79.7)
    female_pct <- c(66.0, 60.9, 64.8, 67.9, 69.5, 70.7, 75.5, 77.7, 79.5, 71.3)
    
    ## ---------- Berlin ----------
  } else if (STATE == 11) {
    male_pct   <- c(58.4, 58.8, 67.3, 70.0, 72.0, 73.9, 74.8, 73.9, 79.4, 80.1)
    female_pct <- c(62.2, 62.8, 68.8, 72.6, 74.2, 75.8, 77.5, 75.6, 79.3, 73.0)
    
    ## ---------- Brandenburg ----------
  } else if (STATE == 12) {
    male_pct   <- c(60.8, 54.4, 55.0, 58.1, 64.9, 67.2, 70.7, 70.8, 76.6, 74.4)
    female_pct <- c(61.8, 56.1, 56.6, 63.1, 67.6, 73.5, 74.1, 73.6, 77.7, 65.0)
    
    ## ---------- Mecklenburg-Vorpommern ----------
  } else if (STATE == 13) {
    male_pct   <- c(52.3, 44.9, 49.8, 53.5, 59.1, 64.9, 65.0, 67.6, 73.9, 74.2)
    female_pct <- c(48.7, 46.4, 53.4, 60.0, 62.2, 69.4, 69.6, 70.4, 75.3, 66.4)
    
    ## ---------- Sachsen ----------
  } else if (STATE == 14) {
    male_pct   <- c(63.6, 59.8, 62.1, 63.9, 66.9, 69.0, 70.2, 70.7, 75.5, 74.8)
    female_pct <- c(63.4, 60.9, 63.0, 67.3, 71.6, 73.8, 73.7, 74.1, 76.8, 65.6)
    
    ## ---------- Sachsen-Anhalt ----------
  } else if (STATE == 15) {
    male_pct   <- c(51.5, 45.8, 46.9, 50.0, 54.5, 59.4, 62.0, 63.9, 70.8, 70.1)
    female_pct <- c(47.7, 47.8, 48.9, 54.2, 59.9, 64.9, 64.7, 68.0, 71.8, 61.7)
    
    ## ---------- Thüringen ----------
  } else if (STATE == 16) {
    male_pct   <- c(60.3, 55.1, 58.8, 58.2, 63.3, 67.8, 67.3, 70.9, 76.1, 74.7)
    female_pct <- c(59.8, 57.1, 60.3, 63.5, 65.7, 72.9, 72.8, 72.9, 75.8, 66.0)
    
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