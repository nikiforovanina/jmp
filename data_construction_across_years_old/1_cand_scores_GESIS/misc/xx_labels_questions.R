library(dplyr)
library(tibble)
library(purrr)

# Use the same variable lists you defined in compute_scores_2013()
elite_vars <- c("d5a","d5b","d5c","d5g","d5h","d5f","c20")
econ_vars  <- c("c2b","c2h","c2g","c3","c19e","c19f","c2d","c2i","c2j","c2a","c9")

# helper: safe variable label extractor
get_var_label <- function(df, v) {
  if (!v %in% names(df)) return(NA_character_)
  lab <- attr(df[[v]], "label")
  if (is.null(lab) || is.na(lab) || lab == "") NA_character_ else as.character(lab)
}

# helper: value labels (optional, but often useful)
get_value_labels <- function(df, v) {
  if (!v %in% names(df)) return(NA_character_)
  labs <- attr(df[[v]], "labels")
  if (is.null(labs) || length(labs) == 0) return(NA_character_)
  paste0(names(labs), "=", unname(labs), collapse = "; ")
}

labels_df <- tibble(
  var = c(econ_vars, elite_vars),
  dim = c(rep("Left–Right (econ)", length(econ_vars)),
          rep("Anti-Elite",       length(elite_vars)))
) %>%
  mutate(
    var_label   = map_chr(var, ~ get_var_label(cand13, .x)),
    value_labels = map_chr(var, ~ get_value_labels(cand13, .x)),
    present     = var %in% names(cand13)
  ) %>%
  arrange(dim, var)

print(labels_df, n = Inf)

# Save if you want
write.csv(labels_df, "question_labels_2013.csv", row.names = FALSE)



###################
table(cand13_scored_full_shrunk$partei)

cand13_scored_full_shrunk_s <- cand13_scored_full_shrunk %>% group_by(wknr) %>% summarize(n_wknr = n()) 

table(cand13_scored_full_shrunk_s$n_wknr)



cand13_scored_full_shrunk <- cand13_scored_full_shrunk %>% group_by(wknr) %>% mutate(n_wknr = n()) 

table(cand13_scored_full_shrunk$n_wknr[which(cand13_scored_full_shrunk$partei == 322)])



