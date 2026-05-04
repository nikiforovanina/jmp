# ===============================================================
# 1_plot_weights.R — w2 grid search results, plotted from the
# diagnostics CSVs written by 0_weights_by_region_state_pi.R.
#
# Produces three PNGs in ../graphs/, suffixed with SPEC_TAG so
# different specifications don't overwrite each other:
#
#   mae_east_<SPEC_TAG>.png   — MAE curve over w2, faceted by year (East)
#   mae_west_<SPEC_TAG>.png   — same, West
#   best_w2_<SPEC_TAG>.png    — best w2 per year + tolerance band,
#                                East vs West faceted
#
# Colours: East = #3CB371  /  West = #4169E1
# ===============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

source("0_configuration.R")
stopifnot(exists("SPEC_TAG"), nzchar(SPEC_TAG))

dir.create("../graphs", showWarnings = FALSE, recursive = TRUE)

col_east <- "#3CB371"
col_west <- "#4169E1"
region_colours <- c("East" = col_east, "West" = col_west)

# ---- load all diagnostics CSVs from this spec --------------------
diag_files <- list.files(
  "diagnostics",
  pattern = sprintf(
    "^w2_grid_region_state_pi_year\\d+_(east|west)_seed\\d+_%s\\.csv$",
    SPEC_TAG
  ),
  full.names = TRUE
)
if (!length(diag_files)) {
  stop("No diagnostics CSVs found in diagnostics/. ",
       "Run the weight jobs first.")
}
df_raw <- bind_rows(lapply(diag_files, read_csv, show_col_types = FALSE))
df_raw$mae <- as.numeric(df_raw$mae)
df_raw$w2  <- as.numeric(df_raw$w2)

df_clean <- df_raw %>%
  filter(status %in% c("optimal", "optimal_inaccurate"), is.finite(mae)) %>%
  group_by(year, geo_region, w2) %>% slice_tail(n = 1) %>% ungroup() %>%
  mutate(
    mae        = mae * 100,
    year       = factor(year, levels = sort(unique(year))),
    geo_region = factor(geo_region,
                        levels = c("west", "east"),
                        labels = c("West", "East"))
  )

# ---- w2 values to keep on the x-axis -----------------------------
w2_keep <- c(0.2, 0.25, 0.4, 0.5, 0.75, 1, 1.25, 2, 2.5, 4, 5)
x_min     <- 0
x_max     <- 5
x_by      <- 0.5
best_ymax <- 2

df_plot <- df_clean %>% filter(w2 %in% w2_keep)

# ---- MAE plot factory -------------------------------------------
make_mae_plot <- function(d_all, region_label, x_min, x_max, x_by) {

  col <- region_colours[region_label]
  d   <- d_all %>% filter(geo_region == region_label)
  if (nrow(d) == 0) return(NULL)

  d_best   <- d %>% group_by(year) %>%
    slice_min(mae, n = 1, with_ties = FALSE) %>% ungroup()
  vline_df <- data.frame(xint = seq(x_min, x_max, by = x_by))

  ggplot(d, aes(x = w2, y = mae, group = 1)) +
    geom_vline(data = vline_df, aes(xintercept = xint),
               colour = "grey70", linewidth = 0.2, alpha = 0.8) +
    geom_line(colour = col, linewidth = 0.85, alpha = 0.9) +
    geom_point(colour = "black", fill = col, size = 2.8,
               shape = 23, stroke = 0.6) +
    geom_point(data = d_best, aes(x = w2, y = mae),
               colour = "black", fill = col, shape = 23,
               size = 5, stroke = 1.4) +
    geom_vline(data = d_best, aes(xintercept = 1),
               colour = "gray40", linetype = "dashed",
               linewidth = 0.4, alpha = 0.4) +
    facet_wrap(vars(year), scales = "free_y") +
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = seq(x_min, x_max, by = x_by),
      labels = function(x) format(x, drop0trailing = TRUE, trim = TRUE),
      expand = expansion(mult = 0.04)
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.15)),
      labels = function(x) format(round(x, 1), nsmall = 1)
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 12, base_family = "sans") +
    theme(
      strip.text         = element_text(face = "bold", size = 11),
      strip.background   = element_rect(fill = "grey94", colour = NA),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.35, colour = "grey88"),
      axis.text.x        = element_text(size = 9, angle = 45, hjust = 1),
      legend.position    = "none",
      panel.spacing      = unit(2.0, "lines"),
      plot.margin        = margin(8, 12, 20, 12)
    )
}

# ---- best-w2 range plot -----------------------------------------
make_best_range_plot <- function(d_all, best_ymax, x_by) {

  band <- d_all %>%
    group_by(year, geo_region) %>%
    mutate(mae_min = min(mae)) %>%
    filter(mae <= mae_min * 1.005) %>%
    summarise(
      w2_best = w2[which.min(mae)],
      w2_lo   = min(w2),
      w2_hi   = max(w2),
      .groups = "drop"
    ) %>%
    mutate(year_n = as.integer(as.character(year)))

  band_tight <- d_all %>%
    group_by(year, geo_region) %>%
    mutate(mae_min = min(mae)) %>%
    filter(mae <= mae_min * 1.0025) %>%
    summarise(
      w2_lo = min(w2),
      w2_hi = max(w2),
      .groups = "drop"
    ) %>%
    mutate(year_n = as.integer(as.character(year)))

  ggplot(band, aes(x = year_n, colour = geo_region, fill = geo_region)) +
    geom_hline(yintercept = 1, linetype = "dashed",
               colour = "grey50", linewidth = 0.5) +
    geom_linerange(aes(ymin = w2_lo, ymax = w2_hi),
                   linewidth = 3.5, alpha = 0.35) +
    geom_linerange(data = band_tight,
                   aes(ymin = w2_lo, ymax = w2_hi),
                   linewidth = 6, alpha = 0.35) +
    geom_point(aes(y = w2_best), shape = 23, size = 4,
               stroke = 1, colour = "black") +
    facet_wrap(vars(geo_region)) +
    scale_colour_manual(values = region_colours) +
    scale_fill_manual(values   = region_colours) +
    scale_x_continuous(breaks = sort(unique(band$year_n)),
                       labels = sort(unique(band$year_n))) +
    scale_y_continuous(limits = c(0, best_ymax),
                       breaks = seq(0, best_ymax, by = x_by)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 16, base_family = "sans") +
    theme(
      strip.text         = element_text(face = "bold", size = 15),
      strip.background   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.35, colour = "grey88"),
      axis.text          = element_text(size = 14),
      legend.position    = "none",
      panel.spacing      = unit(2.0, "lines"),
      plot.margin        = margin(8, 12, 8, 12)
    )
}

# ---- save -------------------------------------------------------
n_years <- nlevels(df_plot$year)
w_mae   <- max(3.2 * n_years, 6)

for (rg in c("East", "West")) {
  p <- make_mae_plot(df_plot, rg, x_min, x_max, x_by)
  if (is.null(p)) { message("  Skipping ", rg, " (no rows)"); next }
  fname <- sprintf("../graphs/mae_%s_%s.png", tolower(rg), SPEC_TAG)
  ggsave(fname, p, width = w_mae, height = 4.2, dpi = 180, bg = "white")
  message("Saved -> ", fname)
}

p_best <- make_best_range_plot(df_plot, best_ymax, x_by)
fname  <- sprintf("../graphs/best_w2_%s.png", SPEC_TAG)
ggsave(fname, p_best,
       width = max(2.5 * n_years, 5), height = 3.5, dpi = 180, bg = "white")
message("Saved -> ", fname)

cat("\nDone.\n")
