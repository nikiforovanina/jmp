########################################
setwd("~/Desktop/nonparam_to_clean")
library(haven)

cand13 <- read_dta('candidates_2013.dta')
#elite
cand13$d5a[1] #not reverse, 1-5
cand13$d5b[1] #not reverse, 1-5
cand13$d5c[1] #not reverse, 1-5
cand13$d5g[1] #not reverse, 1-5
cand13$d5h[1] # reverse, 1-5
cand13$d5f[1] # reverse, 1-5
#cand13$d5d[1] # reverse, 1-5
cand13$c20[1] # not reverse, 1-4

# LR: econ + culture
cand13$c2b[1] #reverse, 1-5
cand13$c2h[1] #not reverse, 1-5
cand13$c2g[1] #not reverse, 1-5
cand13$c3[1] #not reverse, 1-11
cand13$c19e[1] #not reverse, 1-5
cand13$c19f[1] #not reverse, 1-5

cand13$c2d #reverse, 1-5
cand13$c2i #not reverse, 1-5
cand13$c2j #not reverse, 1-5
cand13$c2a #reverse, 1-5
cand13$c9 #not reverse, 1-11


table(cand13$c9, cand13$a1)


cand17 <- read_dta('candidates_2017.dta')

#elite
cand17$d6a[1] #not reverse, 1-5
cand17$d6b[1] #not reverse, 1-5
cand17$d6c[1] #not reverse, 1-5
cand17$d6g[1] #not reverse, 1-5
cand17$d6h[1] #reverse, 1-5
cand17$d6f[1] #reverse, 1-5
#cand17$d6d[1] #reverse, 1-5
cand17$c20[1] # not reverse, 1-4

# LR: econ + culture
cand17$c2b[1] #reverse, 1-5
cand17$c2h[1] #not reverse, 1-5
cand17$c2g[1] #not reverse, 1-5
cand17$c3[1] #not reverse, 1-11
cand17$c18c[1] #reverse, 1-5 # i changed 18g from the support for the economy and industry to bundeswehr spending, as it is more telling along the LR divide
cand17$c18h[1] #not reverse, 1-5

cand17$c2d #reverse, 1-5
cand17$c2i #not reverse, 1-5
cand17$c2j #not reverse, 1-5
cand17$c2a #reverse, 1-5
cand17$c6 #not reverse, 1-11

table(cand17$c6, cand17$a1)

cand21 <- read_dta('candidates_2021.dta')

#elite
cand21$d7a[1] # reverse, 1-5
cand21$d7b[1] # reverse, 1-5
cand21$d7c[1] # reverse, 1-5
cand21$d7g[1] # reverse, 1-5
cand21$d7h[1] # not reverse, 1-5
cand21$d7f[1] # not reverse, 1-5
#cand21$d7d[1] # not reverse, 1-5 #lobby instead of interestgrouppen, same
cand21$c23[1] # not reverse, 1-4

# LR: econ + culture
cand21$c2b[1] # not reverse, 1-5
cand21$c2h[1] # reverse, 1-5
cand21$c2g[1] # reverse, 1-5
cand21$c5[1] # not reverse, 1-11
cand21$c11[1] # reverse, 1-11 , Steuern vs. Sozialleistungen (absense of better choice)

cand21$c2d #reverse, 1-5
cand21$c2i #reverse, 1-5
cand21$c2j #reverse, 1-5
cand21$c2a #not reverse, 1-5
cand21$c8 #not reverse, 1-11

table(cand21$c2a, cand21$partei)



# PUT TOGETHER IN A TABLE:
## ---- Setup ----
## ---- Setup ----
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
})

# safe label getter
var_label_safely <- function(d, v) {
  if (!v %in% names(d)) return(NA_character_)
  lbl <- attr(d[[v]], "label", exact = TRUE)
  if (is.null(lbl)) NA_character_ else as.character(lbl)
}

# helper to build a spec tibble for a given year/dataset + item list
build_year_table <- function(dat, year, domain, items) {
  tibble(
    year    = year,
    domain  = domain,
    var     = items$var,
    reverse = items$reverse,
    range   = items$range,
    label   = vapply(items$var, function(v) var_label_safely(dat, v), character(1))
  )
}

## ---- Read data ----
cand13 <- read_dta("candidates_2013.dta")
cand17 <- read_dta("candidates_2017.dta")
cand21 <- read_dta("candidates_2021.dta")

## ---- Define variables (from your manual notes) ----
# 2013
elite_13 <- tribble(
  ~var,  ~reverse, ~range,
  "d5a", FALSE,    "1–5",
  "d5b", FALSE,    "1–5",
  "d5c", FALSE,    "1–5",
  "d5g", FALSE,    "1–5",
  "d5h", TRUE,     "1–5",
  "d5f", TRUE,     "1–5",
  "c20", FALSE,    "1–4"
)
lr_econ_13 <- tribble(
  ~var,   ~reverse, ~range,
  "c2b",  TRUE,     "1–5",
  "c2h",  FALSE,    "1–5",
  "c2g",  FALSE,    "1–5",
  "c3",   FALSE,    "1–11",
  "c19e", FALSE,    "1–5",
  "c19f", FALSE,    "1–5"
)
lr_cult_13 <- tribble(
  ~var,  ~reverse, ~range,
  "c2d", TRUE,     "1–5",
  "c2i", FALSE,    "1–5",
  "c2j", FALSE,    "1–5",
  "c2a", TRUE,     "1–5",
  "c9",  FALSE,    "1–11"
)

# 2017
elite_17 <- tribble(
  ~var,  ~reverse, ~range,
  "d6a", FALSE,    "1–5",
  "d6b", FALSE,    "1–5",
  "d6c", FALSE,    "1–5",
  "d6g", FALSE,    "1–5",
  "d6h", TRUE,     "1–5",
  "d6f", TRUE,     "1–5",
  "c20", FALSE,    "1–4"
)
lr_econ_17 <- tribble(
  ~var,   ~reverse, ~range,
  "c2b",  TRUE,     "1–5",
  "c2h",  FALSE,    "1–5",
  "c2g",  FALSE,    "1–5",
  "c3",   FALSE,    "1–11",
  "c18c", TRUE,     "1–5",
  "c18h", FALSE,    "1–5"
)
lr_cult_17 <- tribble(
  ~var,  ~reverse, ~range,
  "c2d", TRUE,     "1–5",
  "c2i", FALSE,    "1–5",
  "c2j", FALSE,    "1–5",
  "c2a", TRUE,     "1–5",
  "c6",  FALSE,    "1–11"
)

# 2021
elite_21 <- tribble(
  ~var,  ~reverse, ~range,
  "d7a", TRUE,     "1–5",
  "d7b", TRUE,     "1–5",
  "d7c", TRUE,     "1–5",
  "d7g", TRUE,     "1–5",
  "d7h", FALSE,    "1–5",
  "d7f", FALSE,    "1–5",
  "c23", FALSE,    "1–4"
)
lr_econ_21 <- tribble(
  ~var,  ~reverse, ~range,
  "c2b", FALSE,    "1–5",
  "c2h", TRUE,     "1–5",
  "c2g", TRUE,     "1–5",
  "c5",  FALSE,    "1–11",
  "c11", TRUE,     "1–11"
)
lr_cult_21 <- tribble(
  ~var,  ~reverse, ~range,
  "c2d", TRUE,     "1–5",
  "c2i", TRUE,     "1–5",
  "c2j", TRUE,     "1–5",
  "c2a", FALSE,    "1–5",
  "c8",  FALSE,    "1–11"
)

## ---- Build per-year tables ----
tbl_13_elite <- build_year_table(cand13, 2013, "elite",     elite_13)
tbl_13_econ  <- build_year_table(cand13, 2013, "lr_econ",   lr_econ_13)
tbl_13_cult  <- build_year_table(cand13, 2013, "lr_culture",lr_cult_13)

tbl_17_elite <- build_year_table(cand17, 2017, "elite",     elite_17)
tbl_17_econ  <- build_year_table(cand17, 2017, "lr_econ",   lr_econ_17)
tbl_17_cult  <- build_year_table(cand17, 2017, "lr_culture",lr_cult_17)

tbl_21_elite <- build_year_table(cand21, 2021, "elite",     elite_21)
tbl_21_econ  <- build_year_table(cand21, 2021, "lr_econ",   lr_econ_21)
tbl_21_cult  <- build_year_table(cand21, 2021, "lr_culture",lr_cult_21)

vars_table <- bind_rows(
  tbl_13_elite, tbl_13_econ, tbl_13_cult,
  tbl_17_elite, tbl_17_econ, tbl_17_cult,
  tbl_21_elite, tbl_21_econ, tbl_21_cult
) %>%
  arrange(year, domain, var)

# inspect / export
dir.create("tables", showWarnings = FALSE, recursive = TRUE)
print(vars_table, n = Inf, width = Inf)
write_csv(vars_table, "tables/variable_labels_analysis_table.csv")

# LaTeX export
library(knitr)
latex_table <- kable(vars_table,
                     format = "latex",
                     booktabs = TRUE,
                     longtable = TRUE,
                     caption = "Survey variables used for scoring (2013--2021)",
                     label = "tab:variables")
write_file(latex_table, "tables/variable_labels_analysis_table.tex")