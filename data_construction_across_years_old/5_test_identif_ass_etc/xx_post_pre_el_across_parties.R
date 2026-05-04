## ---------- Setup ----------
library(dplyr); library(tidyr); library(purrr); library(ggplot2)
library(haven); library(forcats); library(stringr); library(labelled)

dat <- dat_raw

## ---------- Party suffix from q75a (respondent vote) ----------
suffix_from_q75a <- function(x){
  xn  <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  if (!all(is.na(xn))) {
    out[xn %in% c(1,2,3)] <- "a"  # CDU/CSU
    out[xn == 4]          <- "d"  # SPD
    out[xn == 5]          <- "e"  # FDP
    out[xn == 6]          <- "f"  # Greens
    out[xn == 7]          <- "g"  # Left
    out[xn == 322]        <- "h"  # AfD
    return(out)
  }
  xl <- tolower(as.character(x))
  out[grepl("\\b(cdu|csu|cdu/csu|union)\\b", xl)] <- "a"
  out[grepl("\\bspd\\b", xl)]                     <- "d"
  out[grepl("\\bfdp\\b", xl)]                     <- "e"
  out[grepl("\\b(gr[üu]ne|gruene|greens|b[üu]ndnis\\s*90)\\b", xl)] <- "f"
  out[grepl("\\b(die\\s*linke|\\bleft\\b)\\b", xl)] <- "g"
  out[grepl("\\bafd\\b", xl)] <- "h"
  out
}

## ---------- Name helpers ----------
parse_afd_var <- function(var_afdh){
  m <- regexec("^([A-Za-z]\\d+)[a-z]([0-9]*)$", var_afdh)
  p <- regmatches(var_afdh, m)[[1]]
  if (length(p) != 3) stop("Bad variable name: ", var_afdh)
  list(base = p[2], idx = p[3])
}
cols_with_base_idx <- function(base, idx){
  pat <- paste0("^", base, "[a-z]", idx, "$")
  grep(pat, names(dat), value = TRUE)
}

## ---------- Titles ----------
english_title_base <- function(var_base){
  dplyr::case_when(
    var_base == "q60"  ~ "Candidate: knowledge",
    var_base == "q61"  ~ "Candidate: campaign effort",
    var_base == "q62"  ~ "Candidate: evaluation",
    var_base == "q130" ~ "Contact by candidate: Email/SMS",
    var_base == "q135" ~ "Contact by candidate: Posters",
    TRUE               ~ var_base
  )
}

## ---------- Pool by respondent's chosen party ----------
# For each row, pick the value from the party column that matches q75a
pool_by_vote <- function(var_afdh){
  pr <- parse_afd_var(var_afdh)
  base <- pr$base; idx <- pr$idx
  
  # columns available for this base+idx (across parties)
  all_cols <- cols_with_base_idx(base, idx)
  # build a quick look-up: suffix -> column name (if present)
  party_suffixes <- c("a","d","e","f","g","h")  # CDU/CSU, SPD, FDP, Greens, Left, AfD
  suf2col <- setNames(paste0(base, party_suffixes, idx), party_suffixes)
  suf2col[!suf2col %in% all_cols] <- NA_character_
  
  # respondent's suffix from q75a
  sfx <- suffix_from_q75a(dat$q75a)
  # compute, for each row, which column to read
  pick_col <- unname(suf2col[sfx])
  
  # Build a matrix of numeric codes for all party columns (missing columns -> NA)
  mat <- do.call(cbind, lapply(party_suffixes, function(s){
    cn <- suf2col[[s]]
    if (is.na(cn)) rep(NA_real_, nrow(dat)) else suppressWarnings(as.numeric(dat[[cn]]))
  }))
  colnames(mat) <- party_suffixes
  
  # Row-wise pick
  idxc <- match(sfx, party_suffixes)
  pooled_val <- rep(NA_real_, nrow(dat))
  ok <- !is.na(idxc)
  pooled_val[ok] <- mat[cbind(which(ok), idxc[ok])]
  
  # Try to recover value labels from any available party column
  first_col <- all_cols[1]
  vlab <- if (length(first_col)) tryCatch(labelled::val_labels(dat[[first_col]]), error = function(e) NULL) else NULL
  lab_map <- NULL
  if (!is.null(vlab) && length(vlab)) {
    lab_map <- setNames(as.character(names(vlab)), as.character(unname(vlab)))
  }
  pooled_lab <- if (!is.null(lab_map)) lab_map[as.character(pooled_val)] else NA_character_
  
  tibble(val = pooled_val, lab = pooled_lab, base = base, idx = idx)
}

## ---------- Clean & shape into distributions ----------
make_dist <- function(tb){  # tb is output from pool_by_vote()
  base <- tb$base[1]
  idx  <- tb$idx[1]
  
  # drop non-substantive; also drop "Mehrfachnennungen" for q61/q62 via label text
  bad_codes <- c(-99,-98,-97,-96,-95,-94,-93,-92,-91,-86,-83,-72,-71,-9,-8,-7,-6,-4,-3,-2,-1)
  keep <- !(tb$val %in% bad_codes)
  if (base %in% c("q61","q62")) {
    keep <- keep & !str_detect(tb$lab %||% "", "(?i)mehrfach")
  }
  tb <- tb[keep, , drop = FALSE]
  
  if (base == "q60") {
    map60 <- c(
      "1"="Right name, right party",
      "2"="Right name, wrong party",
      "3"="Right name, no party",
      "4"="Wrong name, party named",
      "5"="No name, no party",
      "6"="No name, party named"
    )
    out <- tb %>%
      transmute(code = as.integer(val)) %>%
      filter(code %in% 1:6) %>%
      count(code, name = "n") %>%
      complete(code = 1:6, fill = list(n = 0)) %>%
      mutate(ans = factor(map60[as.character(code)], levels = unname(map60)),
             ord = code)
    
  } else if (base == "q61") {
    out <- tb %>%
      transmute(code = as.integer(val)) %>%
      filter(code %in% 1:11) %>%
      count(code, name = "n") %>%
      complete(code = 1:11, fill = list(n = 0)) %>%
      mutate(ans = factor(as.character(code), levels = as.character(1:11)),
             ord = code)
    
  } else if (base == "q62") {
    # Convert to real score in [-5..5] no matter if we have codes (1..11)
    # or only the text labels (“-3”, “+4”, “0”, …).
    code  <- suppressWarnings(as.integer(tb$val))
    score <- ifelse(!is.na(code) & code >= 1 & code <= 11, code - 6L,
                    suppressWarnings(as.integer(readr::parse_number(as.character(tb$lab)))))
    
    levs <- c(-5:-1, 0, 1:5)  # exact order we want on the axis
    
    out <- tibble(score = score) %>%
      filter(score %in% levs) %>%
      count(score, name = "n") %>%
      tidyr::complete(score = levs, fill = list(n = 0)) %>%
      mutate(
        # factor levels lock the plotting order (don’t reorder later)
        ans = factor(as.character(score), levels = as.character(levs)),
        ord = score
      )
    
  } else if (base %in% c("q130","q135")) {
    out <- tb %>%
      transmute(code = as.integer(val)) %>%
      filter(code %in% 0:1) %>%
      count(code, name = "n") %>%
      complete(code = 0:1, fill = list(n = 0)) %>%
      mutate(ans = factor(ifelse(code==1,"Yes","No"),
                          levels = c("No","Yes")),
             ord = code)
  } else {
    out <- tb %>% count(lab, name = "n") %>%
      mutate(ans = factor(lab), ord = as.numeric(ans))
  }
  
  out %>%
    mutate(pct = n / sum(n),
           question_title = english_title_base(base),
           .keep = "unused")
}

## ---------- Build pooled (by vote) for all five outcomes ----------
build_pooled_by_vote_all <- function(){
  vars0 <- c("q60h1","q61h","q62h","q130h","q135h")
  bind_rows(lapply(vars0, function(v){
    pool_by_vote(v) |> make_dist()
  }))
}

## ---------- Plot ----------
plot_pooled_by_vote <- function(){
  pieces <- build_pooled_by_vote_all()

  # force per-question levels so they cannot be disturbed later
  q60_levels  <- c("Right name, right party",
                   "Right name, wrong party",
                   "Right name, no party",
                   "Wrong name, party named",
                   "No name, no party",
                   "No name, party named")

  pieces <- pieces %>%
    mutate(ans = as.character(ans)) %>%
    mutate(
      ans = case_when(
        question_title == "Candidate: knowledge"       ~ factor(ans, levels = q60_levels),
        question_title == "Candidate: campaign effort" ~ factor(ans, levels = as.character(1:11)),
        question_title == "Candidate: evaluation"      ~ factor(ans, levels = as.character(-5:5)),  # <-- 0 in the middle
        question_title == "Contact by candidate: Email/SMS"         ~ factor(ans, levels = c("No","Yes")),
        question_title == "Contact by candidate: Posters"           ~ factor(ans, levels = c("No","Yes")),
        TRUE ~ factor(ans)
      )
    )
  # NOTE: do NOT reorder again (this is what pushed 0 to the top)
  # pieces <- pieces %>% group_by(question_title) %>% mutate(ans = fct_reorder(ans, ord)) %>% ungroup()

  ggplot(pieces, aes(x = ans, y = pct)) +
    geom_col(width = 0.86, fill = "#6E8A23", color = "#6E8A23", alpha = 0.55) +
    coord_flip() +
    facet_wrap(~ question_title, nrow = 1, scales = "free") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Share (using only the voted party per respondent)") +
    theme_bw(base_size = 14) +
    theme(strip.text.x = element_text(face = "bold"),
          panel.spacing.x = unit(10, "pt"))
}

## ---------- Run & save ----------
p_vote <- plot_pooled_by_vote()
print(p_vote)

dir.create("figs", showWarnings = FALSE)
ggsave("figs/pooled_all_parties_5outcomes.png", p_vote, width = 18, height = 4.5, dpi = 300)

dat$q62a[1]

