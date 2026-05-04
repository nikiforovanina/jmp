library(haven)
library(dplyr)
library(ggplot2)
library(modelsummary)

.in_source <- any(vapply(sys.calls(), function(x) {
  fn <- tryCatch(as.character(x[[1]])[1], error = function(e) "")
  identical(fn, "source") || identical(fn, "sys.source")
}, logical(1)))
if (interactive() && !.in_source && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
rm(.in_source)
setwd("..")  # operate from folder 1 root (reads candidates_*.dta, writes plots + .tex)

cand21 <- read_dta("candidates_2021.dta")
cand17 <- read_dta("candidates_2017.dta")
cand13 <- read_dta("candidates_2013.dta")

# ---- 2021 ----
cand21$a9b <- ifelse(cand21$a9b < 1, NA, cand21$a9b)
cand21$live_in_wahlkreis <- 2021 - cand21$a9b
cand21$age <- 2021 - cand21$geburtsjahr
cand21$born_inwk <- ifelse(cand21$age - cand21$live_in_wahlkreis == 0, 1, 0)

# ---- 2013 ----
cand13$a10b_org <- as.numeric(cand13$a10b_org)
cand13$a10b_org <- ifelse(cand13$a10b_org < 0, NA, cand13$a10b_org)
cand13$a10b_org_year  <- ifelse(cand13$a10b_org > 1900, 2013 - cand13$a10b_org, NA)
cand13$a10b_org_dauer <- substr(cand13$a10b_org, 1, 2)
cand13$live_in_wahlkreis <- ifelse(cand13$a10b_org > 1900, cand13$a10b_org_year, cand13$a10b_org_dauer)
cand13$live_in_wahlkreis <- as.numeric(cand13$live_in_wahlkreis)
cand13$age <- 2013 - cand13$e3
cand13$born_inwk <- ifelse(cand13$age - cand13$live_in_wahlkreis == 0, 1, 0)

# ---- 2017 ----
cand17$e3_org <- as.numeric(cand17$e3_org)
cand17$e3_org <- ifelse(cand17$e3_org < 0, NA, cand17$e3_org)
cand17$e3_year  <- ifelse(cand17$e3_org > 1900, 2017 - cand17$e3_org, NA)
cand17$e3_dauer <- substr(cand17$e3_org, 1, 2)
cand17$live_in_wahlkreis <- ifelse(cand17$e3_org > 1900, cand17$e3_year, cand17$e3_dauer)
cand17$live_in_wahlkreis <- as.numeric(cand17$live_in_wahlkreis)
cand17$age <- 2017 - cand17$geburtsjahr
cand17$born_inwk <- ifelse(cand17$age - cand17$live_in_wahlkreis == 0, 1, 0)

# ---- summary tables ----
sumtab <- tibble(
  year = c(2013, 2017, 2021),
  mean_live = c(mean(cand13$live_in_wahlkreis, na.rm=TRUE),
                mean(cand17$live_in_wahlkreis, na.rm=TRUE),
                mean(cand21$live_in_wahlkreis, na.rm=TRUE)),
  sd_live   = c(sd(cand13$live_in_wahlkreis, na.rm=TRUE),
                sd(cand17$live_in_wahlkreis, na.rm=TRUE),
                sd(cand21$live_in_wahlkreis, na.rm=TRUE)),
  share_born = c(mean(cand13$born_inwk, na.rm=TRUE),
                 mean(cand17$born_inwk, na.rm=TRUE),
                 mean(cand21$born_inwk, na.rm=TRUE))
)

print(sumtab)

modelsummary(sumtab, output = "live_in_wk_summary.tex", fmt = 3)

# ---- simple graphs ----
ggsave("hist_live_2013.png",
       ggplot(cand13) + geom_histogram(aes(live_in_wahlkreis), bins=30) + theme_bw(),
       width=6, height=4, dpi=300)

ggsave("hist_live_2017.png",
       ggplot(cand17) + geom_histogram(aes(live_in_wahlkreis), bins=30) + theme_bw(),
       width=6, height=4, dpi=300)

ggsave("hist_live_2021.png",
       ggplot(cand21) + geom_histogram(aes(live_in_wahlkreis), bins=30) + theme_bw(),
       width=6, height=4, dpi=300)



library(dplyr)
library(tibble)
library(knitr)
library(kableExtra)

sumtab <- tibble(
  year = c(2013, 2017, 2021),
  mean_live = c(mean(cand13$live_in_wahlkreis, na.rm=TRUE),
                mean(cand17$live_in_wahlkreis, na.rm=TRUE),
                mean(cand21$live_in_wahlkreis, na.rm=TRUE)),
  sd_live   = c(sd(cand13$live_in_wahlkreis, na.rm=TRUE),
                sd(cand17$live_in_wahlkreis, na.rm=TRUE),
                sd(cand21$live_in_wahlkreis, na.rm=TRUE)),
  share_born = c(mean(cand13$born_inwk, na.rm=TRUE),
                 mean(cand17$born_inwk, na.rm=TRUE),
                 mean(cand21$born_inwk, na.rm=TRUE)),
  share_lt5  = c(mean(cand13$live_in_wahlkreis < 5,  na.rm=TRUE),
                 mean(cand17$live_in_wahlkreis < 5,  na.rm=TRUE),
                 mean(cand21$live_in_wahlkreis < 5,  na.rm=TRUE)),
  share_lt10 = c(mean(cand13$live_in_wahlkreis < 10, na.rm=TRUE),
                 mean(cand17$live_in_wahlkreis < 10, na.rm=TRUE),
                 mean(cand21$live_in_wahlkreis < 10, na.rm=TRUE))
)

print(sumtab)

kable(sumtab, format="latex", booktabs=TRUE, digits=3,
      caption="Summary statistics: years living in district and shares") %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("live_in_wk_summary.tex")
