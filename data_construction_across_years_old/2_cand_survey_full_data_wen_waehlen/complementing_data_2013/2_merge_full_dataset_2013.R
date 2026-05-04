rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
rm(list = ls())
set.seed(42)


library(stargazer)
library(dplyr)
library(tidyr)
library(stringr)
library(haven)
library(tibble)


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

load("../../1_cand_scores_GESIS/data/cand13_scored_LR.Rdata")
load('wen_merged_with_imputations_2013.Rdata')

wen_merged_with_imputations <- wen_merged_with_imputations %>% filter(is.na(econ_lr_score_mean))
wen_to_merge <- wen_merged_with_imputations[c( "wknr",  "party_id", "econ_lr_score"  , "anti_elite_score")]


cand13_scored_pid <- cand13_scored %>%
  mutate(
    wknr   = as.numeric(haven::zap_labels(wknr)),
    partei = as.numeric(haven::zap_labels(partei))
  ) %>%
  rename(partei_num = partei) %>%
  #left_join(party_lut_codes, by = c("partei_num" = "orig")) %>%
  filter(!is.na(party_id)) %>%
  select(wknr, party_id, econ_lr_score, anti_elite_score) %>%
  mutate(party_id = as.integer(party_id))

cand13_scored_full <- rbind(cand13_scored_pid, wen_to_merge)

#check no duplicates:
#cand13_scored_full$wknr_party <- paste0(cand13_scored_full$wknr, cand13_scored_full$party_id)
#View(as.data.frame(table(cand13_scored_full$wknr_party)))



party_labels <- tibble::tribble(
  ~party_id, ~partei,
  1, 2,
  2, 4, 
  3, 6, 
  4, 5, 
  5, 7, 
  6, 322
)

cand13_scored_full <- cand13_scored_full |>
  dplyr::left_join(party_labels, by = "party_id") |>
  dplyr::select(-party_id)  # keep only the new 'partei' column

save(cand13_scored_full, file = "../data/cand13_scored_full.Rdata")





# ---------- shrink-to-party function ----------
# Caps within-party deviations from the party median and shrinks small deviations.
shrink_to_party <- function(df_scored,
                            lut,
                            max_dev     = 0.15,  # cap on absolute deviation from party median
                            lambda_with = 0.75,  # shrinkage factor when |dev| <= max_dev
                            clamp_min   = -2,  # clamp range after shrink
                            clamp_max   =  2) {
  stopifnot(all(c("wknr","partei","econ_lr_score","anti_elite_score") %in% names(df_scored)))
  stopifnot(all(c("orig","party_id") %in% names(lut)))
  
  # Map each row to a consolidated party_id (CDU/CSU merged)
  out <- df_scored %>%
    mutate(partei = as.numeric(partei)) %>%
    inner_join(lut, by = c("partei" = "orig")) %>%   # adds party_id
    filter(!is.na(party_id))
  
  # Party medians
  meds <- out %>%
    group_by(party_id) %>%
    summarise(
      econ_med  = median(econ_lr_score,    na.rm = TRUE),
      elite_med = median(anti_elite_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  out <- out %>% left_join(meds, by = "party_id")
  
  # shrink one axis
  shrink_axis <- function(val, med) {
    ifelse(
      is.na(val) | is.na(med),
      NA_real_,
      {
        dev <- val - med
        adj <- ifelse(abs(dev) <= max_dev,
                      med + lambda_with * dev,         # shrink small deviations
                      med + sign(dev) * max_dev)       # cap big deviations
        pmin(pmax(adj, clamp_min), clamp_max)          # clamp final range
      }
    )
  }
  
  out <- out %>%
    mutate(
      econ_lr_score    = shrink_axis(econ_lr_score,    econ_med),
      anti_elite_score = shrink_axis(anti_elite_score, elite_med)
    ) %>%
    select(wknr, partei, econ_lr_score, anti_elite_score)  # drop helper cols
  
  out
}

# ---------- apply shrink & save ----------
cand13_scored_full_shrunk <- shrink_to_party(
  df_scored   = cand13_scored_full,
  lut         = party_lut_codes,
  max_dev     = 0.5,
  lambda_with = 0.75,
  clamp_min   = -1.5,
  clamp_max   =  1.5
)

save(cand13_scored_full_shrunk,   file = "../data/cand13_scored_full_shrunk.Rdata")


################
library(dplyr)
library(ggplot2)

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

# attach collapsed party_id
cand13_scored_full_shrunk <- cand13_scored_full_shrunk %>%
  left_join(party_lut_codes, by = c("partei" = "orig"))

# nice labels for the legend
party_labels <- c(
  "1" = "CDU/CSU",
  "2" = "SPD",
  "3" = "Greens",
  "4" = "FDP",
  "5" = "Left",
  "6" = "AfD"
)

# example colors, but you can plug in your own
party_colors <- c(
  "1" = "#1b9e77",
  "2" = "#d95f02",
  "3" = "#7570b3",
  "4" = "#e7298a",
  "5" = "#66a61e",
  "6" = "#e6ab02"
)

cand13_scored_plot <- ggplot(
  cand13_scored_full_shrunk,
  aes(econ_lr_score, anti_elite_score, color = factor(party_id))
) +
  geom_point(size = 2, alpha = 0.8, na.rm = TRUE) +
  coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
  labs(
    x = "Economic Left–Right",
    y = "Anti-Elite (↑)",
    color = "Party"          # legend title
  ) +
  scale_color_manual(
    values = party_colors,
    breaks = names(party_labels),
    labels = party_labels,
    name   = "Party"
  ) +
  theme_minimal(base_size = 14)

cand13_scored_plot
ggsave("../graphs/plot_can_2013_LR.png", cand13_scored_plot, width = 7, height = 5, dpi = 300)



