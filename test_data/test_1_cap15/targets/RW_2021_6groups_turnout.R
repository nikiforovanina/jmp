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
  
  ## ---------- Schleswig-Holstein (1) ----------
  if (STATE == 1) {
    male_pct   <- c(74.5, 70.5, 69.3, 71.4, 73.1, 75.6, 78.1, 80.5, 81.7, 82.4)
    female_pct <- c(75.4, 75.0, 74.9, 72.9, 77.0, 78.2, 79.8, 81.8, 81.5, 76.3)
    
    ## ---------- Hamburg (2) ----------
  } else if (STATE == 2) {
    male_pct   <- c(67.3, 74.0, 74.2, 74.5, 76.2, 77.3, 77.8, 79.5, 77.6, 78.6)
    female_pct <- c(74.3, 75.7, 79.8, 80.2, 79.1, 80.0, 80.7, 81.0, 79.6, 75.2)
    
    ## ---------- Niedersachsen (3) ----------
  } else if (STATE == 3) {
    male_pct   <- c(64.8, 65.6, 67.6, 67.3, 70.6, 72.0, 74.1, 77.5, 79.0, 79.5)
    female_pct <- c(72.6, 68.2, 70.7, 70.3, 73.3, 74.0, 77.8, 78.5, 79.4, 72.6)
    
    ## ---------- Bremen (4) ----------
  } else if (STATE == 4) {
    male_pct   <- c(59.4, 62.3, 65.0, 65.9, 68.2, 66.4, 68.5, 73.9, 74.8, 76.4)
    female_pct <- c(64.4, 68.5, 71.6, 70.4, 71.0, 72.3, 73.0, 76.4, 75.4, 72.6)
    
    ## ---------- Nordrhein-Westfalen (5) ----------
  } else if (STATE == 5) {
    male_pct   <- c(67.3, 68.6, 68.9, 70.2, 71.4, 73.8, 75.4, 79.5, 80.6, 80.3)
    female_pct <- c(69.7, 71.2, 73.4, 72.4, 74.9, 75.2, 77.8, 81.0, 80.4, 75.1)
    
    ## ---------- Hessen (6) ----------
  } else if (STATE == 6) {
    male_pct   <- c(66.2, 69.6, 69.5, 72.8, 72.3, 74.0, 75.5, 80.4, 82.2, 79.6)
    female_pct <- c(71.0, 72.0, 71.9, 72.7, 73.5, 75.0, 79.2, 80.4, 80.6, 72.2)
    
    ## ---------- Rheinland-Pfalz (7) ----------
  } else if (STATE == 7) {
    male_pct   <- c(70.6, 69.9, 69.5, 70.4, 74.0, 75.8, 77.8, 81.5, 80.6, 80.1)
    female_pct <- c(72.0, 70.7, 72.6, 73.1, 74.4, 76.3, 79.3, 82.1, 80.8, 75.0)
    
    ## ---------- Baden-Württemberg (8) ----------
  } else if (STATE == 8) {
    male_pct   <- c(70.7, 72.4, 73.7, 75.7, 76.7, 76.2, 78.9, 81.8, 81.8, 79.3)
    female_pct <- c(72.4, 73.7, 76.0, 76.8, 78.0, 76.7, 79.7, 82.3, 80.8, 71.4)
    
    ## ---------- Bayern (9) ----------
  } else if (STATE == 9) {
    male_pct   <- c(74.0, 75.7, 75.9, 78.1, 77.8, 78.9, 81.1, 82.7, 82.9, 81.8)
    female_pct <- c(77.1, 77.4, 78.0, 79.6, 79.7, 80.6, 81.6, 83.4, 82.2, 74.8)
    
    ## ---------- Saarland (10) ----------
  } else if (STATE == 10) {
    male_pct   <- c(67.4, 71.6, 70.5, 73.2, 73.9, 75.9, 75.1, 80.2, 82.0, 81.3)
    female_pct <- c(69.6, 71.1, 69.8, 74.2, 75.0, 76.3, 77.6, 81.9, 81.8, 74.0)
    
    ## ---------- Berlin (11) ----------
  } else if (STATE == 11) {
    male_pct   <- c(67.3, 67.5, 72.0, 74.5, 76.0, 74.4, 78.7, 76.4, 74.4, 74.1)
    female_pct <- c(72.2, 74.2, 78.8, 77.7, 77.6, 78.9, 81.5, 79.1, 75.1, 70.3)
    
    ## ---------- Brandenburg (12) ----------
  } else if (STATE == 12) {
    male_pct   <- c(68.0, 68.7, 66.1, 70.0, 74.5, 73.3, 77.8, 77.4, 78.6, 76.9)
    female_pct <- c(70.7, 72.3, 68.7, 74.3, 76.1, 78.4, 79.7, 79.7, 79.1, 71.2)
    
    ## ---------- Mecklenburg-Vorpommern (13) ----------
  } else if (STATE == 13) {
    male_pct   <- c(63.7, 62.7, 63.9, 66.5, 65.4, 66.2, 70.3, 73.4, 75.0, 73.6)
    female_pct <- c(64.2, 66.7, 64.7, 68.4, 68.3, 71.7, 74.7, 75.8, 76.3, 67.0)
    
    ## ---------- Sachsen (14) ----------
  } else if (STATE == 14) {
    male_pct   <- c(72.1, 71.4, 71.9, 74.1, 75.7, 76.4, 78.3, 79.0, 79.1, 76.5)
    female_pct <- c(75.0, 73.0, 73.7, 76.1, 78.6, 79.8, 80.5, 80.8, 79.7, 69.8)
    
    ## ---------- Sachsen-Anhalt (15) ----------
  } else if (STATE == 15) {
    male_pct   <- c(60.3, 55.6, 56.7, 62.1, 64.9, 66.7, 72.6, 70.5, 72.9, 69.1)
    female_pct <- c(62.4, 62.4, 62.5, 66.7, 66.4, 69.9, 73.5, 73.5, 73.7, 60.3)
    
    ## ---------- Thüringen (16) ----------
  } else if (STATE == 16) {
    male_pct   <- c(68.9, 68.7, 66.6, 70.4, 73.2, 72.9, 77.0, 78.3, 77.8, 75.9)
    female_pct <- c(71.3, 71.4, 68.6, 74.0, 75.0, 76.3, 80.8, 79.9, 78.5, 68.4)
    
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