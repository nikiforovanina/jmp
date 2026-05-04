####### this code imputes positions for the candidates which are not in the main survey
####### based on the data from wen waehlen

wen_answers <- read.csv('wen_waehlen_btw13_answers_wide.csv')

# Packages
library(tidyverse)
library(tidymodels)
library(tidyr)      # for unnest_longer
library(glmnet)

set.seed(2025)

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
df_econ <- df_all %>%
  select(candidate_id, econ_lr_score_mean, all_of(answer_cols)) %>%
  distinct(candidate_id, .keep_all = TRUE)

df_anti <- df_all %>%
  select(candidate_id, anti_elite_score_mean, all_of(answer_cols)) %>%
  distinct(candidate_id, .keep_all = TRUE)

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

save(wen_merged_with_imputations, file = "wen_merged_with_imputations_2013.Rdata")



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

# Calibration 
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


##
sd_econ <- sd(econ_oof$econ_lr_score_mean, na.rm=TRUE)
sd_anti <- sd(anti_oof$anti_elite_score_mean, na.rm=TRUE)

econ_rmse_sd <- 0.347 / sd_econ
anti_rmse_sd <- 0.445 / sd_anti

econ_rmse_sd
anti_rmse_sd

