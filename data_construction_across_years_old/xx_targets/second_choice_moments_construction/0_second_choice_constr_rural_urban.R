rm(list=ls())

library(dplyr)
library(haven)

#setwd("~/Desktop/nonparam_ae_lr/second_choice_moments_construction")
setwd("~/Desktop/data_construction/xx_targets/second_choice_moments_construction")
# 13
file_path <- "gesis_13.dta"
gesis_13_raw <- haven::read_dta(file_path)
gesis_13 <- haven::read_dta(file_path)

gesis_13$first_choice <- NA
gesis_13$second_choice <- NA
gesis_13$third_choice <- NA

gesis_13$first_choice <-  ifelse(gesis_13$V68 == 1, 'CDU', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 2, 'CDU', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 3, 'SPD', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 4, 'FDP', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 5, 'Green', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 6, 'Left', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 7, 'NPD', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 8, 'Piraten', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(gesis_13$V68 == 9, 'AfD', gesis_13$first_choice)
gesis_13$first_choice <-  ifelse(is.na(gesis_13$V68), 'Abstain', gesis_13$first_choice)

gesis_13$second_choice <-  ifelse(gesis_13$V69 == 1, 'CDU', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 2, 'CDU', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 3, 'SPD', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 4, 'FDP', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 5, 'Green', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 6, 'Left', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 7, 'NPD', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 8, 'Piraten', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 9, 'AfD', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(is.na(gesis_13$V69), 'Abstain', gesis_13$second_choice)
gesis_13$second_choice <-  ifelse(gesis_13$V69 == 98, 'keine weitere', gesis_13$second_choice)


gesis_13_AfD <- gesis_13 %>% filter(first_choice == 'AfD')
gesis_13_AfD <- gesis_13_AfD %>% filter(second_choice %in% c("SPD", "CDU", "CSU", "Left", "Green", "FDP")  )

gesis_13_AfD$bundesland <- gesis_13_AfD$V6
gesis_13_AfD$bundesland <- ifelse(gesis_13_AfD$bundesland == 12, 11, gesis_13_AfD$bundesland) #west-east berlin





# 17
file_path <- "gesis_17.dta"

# Read the Stata dataset file using the read_dta() function
gesis_17_raw <- haven::read_dta(file_path)
gesis_17 <- haven::read_dta(file_path)

gesis_17$first_choice <- NA
gesis_17$second_choice <- NA
gesis_17$third_choice <- NA

gesis_17$first_choice <-  ifelse(gesis_17$V61 == 1, 'CDU', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 2, 'CDU', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 3, 'SPD', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 4, 'Left', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 5, 'Green', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 6, 'FDP', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 7, 'AfD', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 8, 'NPD', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(gesis_17$V61 == 99, 'Abstain', gesis_17$first_choice)
gesis_17$first_choice <-  ifelse(is.na(gesis_17$V61), 'Abstain', gesis_17$first_choice)

gesis_17$second_choice <-  ifelse(gesis_17$V62 == 1, 'CDU', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 2, 'CDU', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 3, 'SPD', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 4, 'Left', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 5, 'Green', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 6, 'FDP', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 7, 'AfD', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 8, 'NPD', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(is.na(gesis_17$V62), 'Abstain', gesis_17$second_choice)
gesis_17$second_choice <-  ifelse(gesis_17$V62 == 98, 'keine weitere', gesis_17$second_choice)


gesis_17_AfD <- gesis_17 %>% filter(first_choice == 'AfD')
gesis_17_AfD <- gesis_17_AfD %>% filter(second_choice %in% c("SPD", "CDU", "CSU", "Left", "Green", "FDP")  )

gesis_17_AfD$bundesland <- gesis_17_AfD$V7
gesis_17_AfD$bundesland <- ifelse(gesis_17_AfD$bundesland == 12, 11, gesis_17_AfD$bundesland) #west-east berlin


# 21
file_path <- "gesis_21.dta"

# Read the Stata dataset file using the read_dta() function
gesis_21_raw <- haven::read_dta(file_path)
gesis_21 <- haven::read_dta(file_path)

gesis_21$first_choice <- NA
gesis_21$second_choice <- NA
gesis_21$third_choice <- NA

gesis_21$first_choice <- ifelse(gesis_21$V60 == 1, 'CDU', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 2, 'CDU', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 3, 'SPD', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 4, 'AfD', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 5, 'FDP', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 6, 'Left', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 7, 'Green', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 8, 'FW', gesis_21$first_choice)
gesis_21$first_choice <- ifelse(gesis_21$V60 == 99, 'Abstain', gesis_21$first_choice)

gesis_21$second_choice <- ifelse(gesis_21$V61 == 1, 'CDU', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 2, 'CDU', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 3, 'SPD', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 4, 'AfD', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 5, 'FDP', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 6, 'Left', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 7, 'Green', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 8, 'FW', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 99, 'Abstain', gesis_21$second_choice)
gesis_21$second_choice <- ifelse(gesis_21$V61 == 98, 'keine weitere', gesis_21$second_choice)

gesis_21_AfD <- gesis_21 %>% filter(first_choice == 'AfD')
gesis_21_AfD <- gesis_21_AfD %>% filter(second_choice %in% c("SPD", "CDU", "CSU", "Left", "Green", "FDP")  )

gesis_21_AfD$bundesland <- gesis_21_AfD$V6
gesis_21_AfD$bundesland <- ifelse(gesis_21_AfD$bundesland == 12, 11, gesis_21_AfD$bundesland) #west-east berlin


#calculating second choices disaggregating rural/urban

gesis_13_AfD <- gesis_13 %>%
  filter(first_choice == "AfD",
         second_choice %in% c("SPD","CDU","CSU","Left","Green","FDP")) %>%
  mutate(
    bundesland = ifelse(V6 == 12, 11, V6),     # west-east Berlin
    rural = ifelse(V8 < 4, 1, 0)               # your definition
  ) %>%
  filter(!is.na(second_choice), !is.na(rural)) %>%
  group_by(bundesland, rural) %>%
  mutate(N = n()) %>%
  group_by(bundesland, rural, second_choice) %>%
  summarise(
    share_party = round(n() / first(N), 2),
    N = first(N),
    .groups = "drop"
  )

gesis_17_AfD <- gesis_17 %>%
  filter(first_choice == "AfD",
         second_choice %in% c("SPD","CDU","CSU","Left","Green","FDP")) %>%
  mutate(
    bundesland = ifelse(V7 == 12, 11, V7),
    rural = ifelse(V9 < 4, 1, 0)
  ) %>%
  filter(!is.na(second_choice), !is.na(rural)) %>%
  group_by(bundesland, rural) %>%
  mutate(N = n()) %>%
  group_by(bundesland, rural, second_choice) %>%
  summarise(
    share_party = round(n() / first(N), 2),
    N = first(N),
    .groups = "drop"
  )

gesis_21_AfD <- gesis_21 %>%
  filter(first_choice == "AfD",
         second_choice %in% c("SPD","CDU","CSU","Left","Green","FDP")) %>%
  mutate(
    bundesland = ifelse(V6 == 12, 11, V6),
    rural = ifelse(V8 < 4, 1, 0)
  ) %>%
  filter(!is.na(second_choice), !is.na(rural)) %>%
  group_by(bundesland, rural) %>%
  mutate(N = n()) %>%
  group_by(bundesland, rural, second_choice) %>%
  summarise(
    share_party = round(n() / first(N), 2),
    N = first(N),
    .groups = "drop"
  )



write.csv(gesis_13_AfD, file = 'second_choice_2013_rural_urban.csv', row.names = FALSE)
write.csv(gesis_17_AfD, file = 'second_choice_2017_rural_urban.csv', row.names = FALSE)
write.csv(gesis_21_AfD, file = 'second_choice_2021_rural_urban.csv', row.names = FALSE)



