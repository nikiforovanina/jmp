####### this code imputes positions for the candidates which are not in the main survey
####### based on the data from wen waehlen

.in_source <- any(vapply(sys.calls(), function(x) {
  fn <- tryCatch(as.character(x[[1]])[1], error = function(e) "")
  identical(fn, "source") || identical(fn, "sys.source")
}, logical(1)))
if (interactive() && !.in_source && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
rm(.in_source)

# Packages
library(tidyverse)
library(tidymodels)
library(tidyr)      # for unnest_longer
library(glmnet)

set.seed(42)

wen_answers <- read.csv('wen_waehlen_btw21_answers_wide.csv')

# wen_merged is produced by 0b_data_complement_2021_cand_survey_wen.R
load("wen_merged_2021.Rdata")

#=============================
# 0) Make one row per candidate_id in wen_merged
#=============================
# If wen_candidate_ids is a list-column of integers, expand it:
wen_by_cand <- wen_merged %>%
  tidyr::unnest_longer(wen_candidate_ids, values_to = "candidate_id", keep_empty = TRUE) %>%
  # If some rows have no candidate_id, keep them (we'll just be unable to join answers).
  mutate(candidate_id = as.integer(candidate_id)) %>%
  # In case there are duplicates after unnesting, keep one row per candidate_id
  group_by(candidate_id) %>%
  slice(1L) %>%
  ungroup()

#=============================
# 1) Join answers, ensure factors, and build modeling frames
#=============================
# Identify question columns (Q1..Q69)
answer_cols <- grep("^Q\\d+$", names(wen_answers), value = TRUE)

# Join answers to candidate rows and coerce all Q's to factors (with explicit Missing level)
df_all <- wen_by_cand %>%
  left_join(wen_answers, by = "candidate_id") %>%
  mutate(across(all_of(answer_cols),
                ~ forcats::fct_explicit_na(as.factor(.x), na_level = "(Missing)")))

# Two modeling tables (one per target)
# (1) Include `partei_label` as a predictor so the imputation can use party
# identity, not just survey answers. Without it, the elastic net pulled AfD
# (small, extreme) toward the population mean.
# In "legacy" mode we drop the partei_label predictor to reproduce the old
# pre-fix imputation (matches data_construction_across_years_old).
.cand_mode_imp <- Sys.getenv("CAND_SCORE_MODE", unset = "pooled")
.use_partei    <- !identical(.cand_mode_imp, "legacy")

if (.use_partei) {
  df_econ <- df_all %>%
    select(candidate_id, partei_label, econ_lr_score_mean, all_of(answer_cols)) %>%
    mutate(partei_label = factor(partei_label)) %>%
    distinct(candidate_id, .keep_all = TRUE)

  df_anti <- df_all %>%
    select(candidate_id, partei_label, anti_elite_score_mean, all_of(answer_cols)) %>%
    mutate(partei_label = factor(partei_label)) %>%
    distinct(candidate_id, .keep_all = TRUE)
} else {
  df_econ <- df_all %>%
    select(candidate_id, econ_lr_score_mean, all_of(answer_cols)) %>%
    distinct(candidate_id, .keep_all = TRUE)

  df_anti <- df_all %>%
    select(candidate_id, anti_elite_score_mean, all_of(answer_cols)) %>%
    distinct(candidate_id, .keep_all = TRUE)
}

# Split labeled vs. to-impute
econ_train <- df_econ %>% filter(!is.na(econ_lr_score_mean))
econ_impute <- df_econ %>% filter(is.na(econ_lr_score_mean))

anti_train <- df_anti %>% filter(!is.na(anti_elite_score_mean))
anti_impute <- df_anti %>% filter(is.na(anti_elite_score_mean))

#=============================
# 2) Shared preprocessing recipe: dummy-encode with safeguards
#=============================
econ_rec <- recipe(econ_lr_score_mean ~ ., data = econ_train) %>%
  update_role(candidate_id, new_role = "ID") %>%
  step_zv(all_predictors()) %>%                                   # drop zero-variance
  step_novel(all_nominal_predictors()) %>%                        # handle unseen levels at prediction time
  step_other(all_nominal_predictors(), threshold = 0.02) %>%      # lump very rare levels
  step_dummy(all_nominal_predictors(), one_hot = TRUE)            # one-hot for glmnet

anti_rec <- recipe(anti_elite_score_mean ~ ., data = anti_train) %>%
  update_role(candidate_id, new_role = "ID") %>%
  step_zv(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.02) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

#=============================
# 3) Model spec (elastic net) and tuning grid
#=============================
enet_spec <- linear_reg(
  penalty = tune(),    # lambda
  mixture = tune()     # 0 = ridge, 1 = lasso, in-between = elastic net
) %>% set_engine("glmnet")

# Cross-validation folds
econ_folds <- vfold_cv(econ_train, v = 10)
anti_folds <- vfold_cv(anti_train, v = 10)

# A reasonable search over lambda/mixture
tune_grid_vals <- tidyr::crossing(
  penalty = 10^seq(-4, 2, length.out = 50),
  mixture = seq(0, 1, by = 0.1)
)

# Workflows
econ_wf <- workflow() %>% add_recipe(econ_rec) %>% add_model(enet_spec)
anti_wf <- workflow() %>% add_recipe(anti_rec) %>% add_model(enet_spec)

#=============================
# 4) Tune with CV and pick best by RMSE
#=============================
econ_tuned <- tune_grid(
  econ_wf, resamples = econ_folds, grid = tune_grid_vals,
  metrics = metric_set(rmse, rsq)
)
econ_best <- select_best(econ_tuned, metric = "rmse")
econ_final_wf <- finalize_workflow(econ_wf, econ_best)
econ_fit <- fit(econ_final_wf, econ_train)

anti_tuned <- tune_grid(
  anti_wf, resamples = anti_folds, grid = tune_grid_vals,
  metrics = metric_set(rmse, rsq)
)
anti_best <- select_best(anti_tuned, metric = "rmse")
anti_final_wf <- finalize_workflow(anti_wf, anti_best)
anti_fit <- fit(anti_final_wf, anti_train)

# Peek at CV performance
econ_cv_metrics <- econ_tuned %>% collect_metrics() %>% filter(penalty == econ_best$penalty, mixture == econ_best$mixture)
anti_cv_metrics <- anti_tuned %>% collect_metrics() %>% filter(penalty == anti_best$penalty, mixture == anti_best$mixture)

print(econ_cv_metrics)
print(anti_cv_metrics)

# (4) Log chosen hyperparameters so old/new pipeline runs can be diffed.
# These files let you see whether a change in upstream GESIS scores caused
# tune_grid to pick a different (penalty, mixture) — a known amplifier of
# imputation drift.
dir.create("../data", showWarnings = FALSE)
log_hp <- function(best, cv_metrics, target) {
  tibble::tibble(
    year         = 2021L,
    target       = target,
    penalty      = best$penalty,
    mixture      = best$mixture,
    cv_rmse      = cv_metrics %>% filter(.metric == "rmse") %>% pull(mean) %>% .[1],
    cv_rsq       = cv_metrics %>% filter(.metric == "rsq")  %>% pull(mean) %>% .[1],
    n_train_rows = NA_integer_,  # filled in below
    selected_at  = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}
econ_hp <- log_hp(econ_best, econ_cv_metrics, "econ_lr_score") %>%
  mutate(n_train_rows = nrow(econ_train))
anti_hp <- log_hp(anti_best, anti_cv_metrics, "anti_elite_score") %>%
  mutate(n_train_rows = nrow(anti_train))
write.csv(econ_hp, "../data/hyperparams_2021_econ.csv", row.names = FALSE)
write.csv(anti_hp, "../data/hyperparams_2021_anti.csv", row.names = FALSE)
message(sprintf("[2021 econ] best: penalty=%g  mixture=%g  cv_rmse=%.3f  cv_rsq=%.3f",
                econ_hp$penalty, econ_hp$mixture, econ_hp$cv_rmse, econ_hp$cv_rsq))
message(sprintf("[2021 anti] best: penalty=%g  mixture=%g  cv_rmse=%.3f  cv_rsq=%.3f",
                anti_hp$penalty, anti_hp$mixture, anti_hp$cv_rmse, anti_hp$cv_rsq))

#=============================
# 4b) Variable importance: extract elastic-net coefficients at the
# selected (penalty, mixture) and persist them so we can see which
# wen-waehlen survey items end up driving the imputation.
#=============================
extract_varimp <- function(fit_obj, year, target,
                           best_penalty, best_mixture, cv_metrics) {
  parsnip_fit <- extract_fit_parsnip(fit_obj)
  coefs <- broom::tidy(parsnip_fit, penalty = best_penalty) %>%
    rename(coefficient = estimate) %>%
    mutate(
      year             = year,
      target           = target,
      best_penalty     = best_penalty,
      best_mixture     = best_mixture,
      cv_rmse          = cv_metrics %>% filter(.metric == "rmse") %>% pull(mean) %>% .[1],
      cv_rsq           = cv_metrics %>% filter(.metric == "rsq")  %>% pull(mean) %>% .[1],
      abs_coef         = abs(coefficient),
      question         = stringr::str_extract(term, "^Q\\d+"),
      answer_level     = stringr::str_remove(term, "^Q\\d+_?")
    ) %>%
    arrange(desc(abs_coef))
  coefs
}

econ_varimp_2021 <- extract_varimp(econ_fit, 2021, "econ_lr_score",
                                   econ_best$penalty, econ_best$mixture, econ_cv_metrics)
anti_varimp_2021 <- extract_varimp(anti_fit, 2021, "anti_elite_score",
                                   anti_best$penalty, anti_best$mixture, anti_cv_metrics)

dir.create("../data", showWarnings = FALSE)
write.csv(econ_varimp_2021, "../data/varimp_2021_econ.csv", row.names = FALSE)
write.csv(anti_varimp_2021, "../data/varimp_2021_anti.csv", row.names = FALSE)

econ_q_summary_2021 <- econ_varimp_2021 %>%
  filter(!is.na(question)) %>%
  group_by(question) %>%
  summarise(total_abs_coef = sum(abs_coef, na.rm = TRUE),
            n_levels_kept  = sum(coefficient != 0, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(total_abs_coef))

anti_q_summary_2021 <- anti_varimp_2021 %>%
  filter(!is.na(question)) %>%
  group_by(question) %>%
  summarise(total_abs_coef = sum(abs_coef, na.rm = TRUE),
            n_levels_kept  = sum(coefficient != 0, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(total_abs_coef))

write.csv(econ_q_summary_2021, "../data/varimp_2021_econ_by_question.csv", row.names = FALSE)
write.csv(anti_q_summary_2021, "../data/varimp_2021_anti_by_question.csv", row.names = FALSE)

message("[2021] saved variable-importance tables to ../data/varimp_2021_*.csv")

#=============================
# 5) Predict for missing rows and merge back in
#=============================
# Econ
econ_preds <- tibble(candidate_id = econ_impute$candidate_id) %>%
  bind_cols(predict(econ_fit, new_data = econ_impute)) %>%
  rename(econ_lr_score_mean_imputed = .pred)

df_econ_filled <- df_econ %>%
  left_join(econ_preds, by = "candidate_id") %>%
  mutate(econ_lr_score_mean = dplyr::coalesce(econ_lr_score_mean, econ_lr_score_mean_imputed)) %>%
  select(-econ_lr_score_mean_imputed)

# Anti-elite
anti_preds <- tibble(candidate_id = anti_impute$candidate_id) %>%
  bind_cols(predict(anti_fit, new_data = anti_impute)) %>%
  rename(anti_elite_score_mean_imputed = .pred)

df_anti_filled <- df_anti %>%
  left_join(anti_preds, by = "candidate_id") %>%
  mutate(anti_elite_score_mean = dplyr::coalesce(anti_elite_score_mean, anti_elite_score_mean_imputed)) %>%
  select(-anti_elite_score_mean_imputed)

#=============================
# 5b) Sanity check: per-party comparison of imputed vs training distribution.
# Flag a party if the imputed mean is more than 1 training_sd away from the
# training mean, or if the imputed median sign-flips a strongly-signed
# (|median|>0.3) training median. This is the canary for the AfD-style drift
# we observed (direct=1.39 vs imputed=1.08 with one imputed AfD at -0.22).
#=============================
sanity_check <- function(train_df, impute_df, preds_df, target, score_col) {
  preds_party <- impute_df %>%
    select(candidate_id, partei_label) %>%
    left_join(preds_df, by = "candidate_id")
  pred_col <- setdiff(colnames(preds_df), "candidate_id")[1]
  train_summary <- train_df %>%
    group_by(partei_label) %>%
    summarise(n_train = dplyr::n(),
              train_mean = mean(.data[[score_col]], na.rm = TRUE),
              train_sd   = sd(.data[[score_col]],   na.rm = TRUE),
              train_med  = median(.data[[score_col]], na.rm = TRUE),
              .groups = "drop")
  impute_summary <- preds_party %>%
    group_by(partei_label) %>%
    summarise(n_imp = dplyr::n(),
              imp_mean = mean(.data[[pred_col]], na.rm = TRUE),
              imp_med  = median(.data[[pred_col]], na.rm = TRUE),
              .groups = "drop")
  full_join(train_summary, impute_summary, by = "partei_label") %>%
    mutate(target = target,
           gap_mean = imp_mean - train_mean,
           flag_drift = !is.na(train_sd) & !is.na(gap_mean) &
                        abs(gap_mean) > train_sd,
           flag_signflip = !is.na(train_med) & !is.na(imp_med) &
                           abs(train_med) > 0.3 & sign(train_med) != sign(imp_med))
}
if (.use_partei) {
  econ_sanity <- sanity_check(econ_train, econ_impute, econ_preds,
                              "econ_lr_score", "econ_lr_score_mean")
  anti_sanity <- sanity_check(anti_train, anti_impute, anti_preds,
                              "anti_elite_score", "anti_elite_score_mean")
  sanity_2021 <- bind_rows(econ_sanity, anti_sanity)
  write.csv(sanity_2021, "../data/imputation_sanity_2021.csv", row.names = FALSE)
  flagged <- sanity_2021 %>% filter(flag_drift | flag_signflip)
  if (nrow(flagged) > 0) {
    warning(sprintf("[2021 imputation] %d party x target cell(s) flagged; see ../data/imputation_sanity_2021.csv",
                    nrow(flagged)))
    print(flagged)
  } else {
    message("[2021 imputation] sanity check: no per-party drift or sign-flip flagged.")
  }
} else {
  message("[2021 imputation] legacy mode: skipping per-party sanity check (partei_label not used as predictor).")
}

#=============================
# 6) Recombine the two targets and join back to the original table if you need
#=============================
# Combine candidate-level results
by_candidate_scored <- df_all %>%
  select(candidate_id) %>%
  distinct() %>%
  left_join(df_econ_filled %>% select(candidate_id, econ_lr_score_mean), by = "candidate_id") %>%
  left_join(df_anti_filled %>% select(candidate_id, anti_elite_score_mean), by = "candidate_id")

colnames(by_candidate_scored) <- c("candidate_id", 'econ_lr_score', 'anti_elite_score')

# Join back to the original rows (pre-unnest) if desired
wen_merged_with_imputations <- wen_by_cand %>%
  left_join(by_candidate_scored, by = "candidate_id")

# sanity checks
summary(wen_merged_with_imputations$econ_lr_score)
summary(wen_merged_with_imputations$anti_elite_score)

save(wen_merged_with_imputations, file = "wen_merged_with_imputations_2021.Rdata")






################### out-of-fold diagnostics for the imputations
# Re-evaluate finalized models with CV to get OOF predictions
econ_cv_fixed <- fit_resamples(
  econ_final_wf, resamples = econ_folds,
  control = control_resamples(save_pred = TRUE),
  metrics = metric_set(rmse, rsq_trad)
)
anti_cv_fixed <- fit_resamples(
  anti_final_wf, resamples = anti_folds,
  control = control_resamples(save_pred = TRUE),
  metrics = metric_set(rmse, rsq_trad)
)

econ_oof <- collect_predictions(econ_cv_fixed) %>%
  left_join(econ_train %>% mutate(.row = dplyr::row_number()) %>%
              select(.row, candidate_id), by = ".row")

anti_oof <- collect_predictions(anti_cv_fixed) %>%
  left_join(anti_train %>% mutate(.row = dplyr::row_number()) %>%
              select(.row, candidate_id), by = ".row")

# Calibration (should be close to 1:1 line)
yardstick::rsq_trad(econ_oof, truth = econ_lr_score_mean, estimate = .pred)
yardstick::rmse(econ_oof, truth = econ_lr_score_mean, estimate = .pred)

yardstick::rsq_trad(anti_oof, truth = anti_elite_score_mean, estimate = .pred)
yardstick::rmse(anti_oof, truth = anti_elite_score_mean, estimate = .pred)

# If you have party info available by candidate_id, check error by party:
party_map <- df_all %>% distinct(candidate_id, party_id, partei_label)

econ_by_party <- econ_oof %>%
  left_join(party_map, by = "candidate_id") %>%
  group_by(partei_label) %>%
  yardstick::rmse(truth = econ_lr_score_mean, estimate = .pred) %>%
  arrange(desc(.estimate))

anti_by_party <- anti_oof %>%
  left_join(party_map, by = "candidate_id") %>%
  group_by(partei_label) %>%
  yardstick::rmse(truth = anti_elite_score_mean, estimate = .pred) %>%
  arrange(desc(.estimate))


########
sd_econ <- sd(econ_oof$econ_lr_score_mean, na.rm=TRUE)
sd_anti <- sd(anti_oof$anti_elite_score_mean, na.rm=TRUE)

econ_rmse_sd <- as.numeric(yardstick::rmse(econ_oof, truth = econ_lr_score_mean, estimate = .pred)) / sd_econ
anti_rmse_sd <- as.numeric(yardstick::rmse(anti_oof, truth = anti_elite_score_mean, estimate = .pred)) / sd_anti

econ_rmse_sd
anti_rmse_sd
