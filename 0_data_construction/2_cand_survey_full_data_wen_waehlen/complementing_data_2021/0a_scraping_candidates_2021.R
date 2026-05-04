# --- packages ----
library(rvest)
library(xml2)
library(httr)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(readr)
library(progress)

# --- config ----
BASE_INDEX_WAHLKREISE <- "https://www.wen-waehlen.de/btw21/wahlkreise.html"

# Use a browser-like UA; servers sometimes downrank "rvest".
DEFAULT_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/120.0.0.0 Safari/537.36"
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# --- robust HTTP loader: avoid Brotli, decode as text, then read_html ----
http_get_html <- function(url,
                          ua = DEFAULT_UA,
                          pause_between = 0.3,
                          retries = 5,
                          timeout_sec = 30) {
  resp <- httr::RETRY(
    "GET", url,
    httr::user_agent(ua),
    # Opt out of Brotli to avoid systems without brotli support
    httr::add_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "de-DE,de;q=0.9,en-US;q=0.7,en;q=0.6",
      "Accept-Encoding" = "gzip, deflate" # <-- important
    ),
    httr::timeout(timeout_sec),
    times = retries,
    pause_min = 1, pause_base = 1.5, pause_cap = 8
  )
  httr::stop_for_status(resp)
  Sys.sleep(pause_between)
  # Decode to text first (prevents issues when libcurl can't decode 'br').
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  xml2::read_html(txt, encoding = "UTF-8")
}

# --- district list (tolerate relative/absolute) ----
get_district_urls <- function(index_url = BASE_INDEX_WAHLKREISE,
                              ua = DEFAULT_UA, pause_between = 0.3) {
  doc <- http_get_html(index_url, ua = ua, pause_between = pause_between)
  hrefs <- rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  hrefs <- hrefs[stringr::str_detect(hrefs, "(^|/)wahlkreis-\\d+\\.html")]
  urls <- xml2::url_absolute(unique(hrefs), index_url)
  tibble(
    district_url = urls,
    district_no  = as.integer(stringr::str_match(urls, "wahlkreis-(\\d+)\\.html$")[, 2])
  ) |> dplyr::arrange(district_no)
}

# --- candidate links on a district page (loose pattern) ----
get_candidate_urls_from_district <- function(district_url,
                                             ua = DEFAULT_UA,
                                             pause_between = 0.3) {
  doc <- http_get_html(district_url, ua = ua, pause_between = pause_between)
  hrefs <- rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  
  # Be permissive: any link containing "kandidaten/<digits>-"
  cand <- hrefs[stringr::str_detect(hrefs, "kandidaten/\\d+-")]
  cand <- xml2::url_absolute(unique(cand), district_url)
  
  tibble(
    candidate_url = cand,
    district_no   = as.integer(stringr::str_match(district_url, "wahlkreis-(\\d+)\\.html$")[, 2]),
    district_url  = district_url
  )
}

# --- parser: meta + answers (unchanged logic) ----
extract_meta_from_text <- function(txt) {
  name_party <- stringr::str_match(txt, "\\n\\s*([^\\n]+?)\\s*\\(([^\\)]+)\\)\\s*\\n")
  name  <- name_party[, 2] %||% NA_character_
  party <- name_party[, 3] %||% NA_character_
  district <- stringr::str_match(txt, "Wahlkreis:\\s*([^\\n]+)")[, 2] %||% NA_character_
  list_state <- stringr::str_match(txt, "Landesliste\\s+([^\\n,]+)")[, 2] %||% NA_character_
  tibble(name = name, party = party, district = district, list_state = list_state)
}

extract_answers_from_text <- function(txt) {
  loc <- stringr::str_locate(
    txt,
    regex("Beantwortung der politischen Forderung(en)? und Thesen", ignore_case = TRUE)
  )[1, ]
  if (is.na(loc[1])) return(tibble())
  sub <- stringr::str_sub(txt, start = loc[2] + 1L)
  sub <- stringr::str_replace_all(sub, "\\r", "")
  sub <- stringr::str_replace_all(sub, "Image", "")
  lines <- stringr::str_split(sub, "\\n")[[1]] |> trimws()
  lines <- lines[nzchar(lines)]
  
  answer_labels <- c("Ja!", "Nein!", "Eher ja", "Eher nicht", "Unentschieden", "Keine Angabe")
  out <- list(); current <- NULL; comment_acc <- character(); i <- 1L
  
  while (i <= length(lines)) {
    line <- lines[i]
    m <- stringr::str_match(line, "^(\\d{1,3})\\s+(.+)$")
    if (!is.na(m[1])) {
      if (!is.null(current)) {
        current$comment <- if (length(comment_acc)) paste(comment_acc, collapse = " ") else NA_character_
        out <- append(out, list(as_tibble(current))); comment_acc <- character()
      }
      current <- list(
        question_no    = as.integer(m[2]),
        question_title = m[3],
        answer         = NA_character_,
        important      = FALSE,
        comment        = NA_character_
      )
      i <- i + 1L
      while (i <= length(lines)) {
        l2 <- lines[i]; found_label <- FALSE
        for (lab in answer_labels) {
          if (stringr::str_starts(l2, fixed(lab))) {
            current$answer <- lab
            rest <- stringr::str_trim(substring(l2, nchar(lab) + 1L))
            if (stringr::str_detect(rest, regex("wichtig", ignore_case = TRUE))) current$important <- TRUE
            rest <- stringr::str_replace_all(rest, regex("wichtig!?\\s*", ignore_case = TRUE), "")
            if (nchar(rest) > 0) comment_acc <- c(comment_acc, rest)
            found_label <- TRUE; i <- i + 1L; break
          }
        }
        if (found_label) break
        i <- i + 1L
      }
      while (i <= length(lines)) {
        la <- lines[i]
        if (stringr::str_detect(la, "^\\d{1,3}\\s+")) break
        if (!(la %in% answer_labels) && nchar(la) > 0) comment_acc <- c(comment_acc, la)
        i <- i + 1L
      }
      next
    } else {
      i <- i + 1L
    }
  }
  if (!is.null(current)) {
    current$comment <- if (length(comment_acc)) paste(comment_acc, collapse = " ") else NA_character_
    out <- append(out, list(as_tibble(current)))
  }
  if (length(out) == 0) tibble() else bind_rows(out)
}

parse_candidate_page <- function(candidate_url, ua = DEFAULT_UA, pause_between = 0.3) {
  doc <- http_get_html(candidate_url, ua = ua, pause_between = pause_between)
  txt <- rvest::html_text2(doc)
  
  meta <- extract_meta_from_text(txt)
  answers <- extract_answers_from_text(txt)
  answered <- nrow(answers) > 0L
  candidate_id <- stringr::str_match(candidate_url, "/kandidaten/(\\d+)-")[, 2] %||% NA_character_
  
  meta <- meta |>
    mutate(
      candidate_id = candidate_id,
      candidate_url = candidate_url,
      answered = answered,
      n_answers = nrow(answers)
    )
  
  answers <- answers |>
    mutate(candidate_id = candidate_id, candidate_url = candidate_url)
  
  landes_links <- rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href")
  landes_links <- landes_links[!is.na(landes_links)]
  landes_links <- landes_links[stringr::str_detect(landes_links, "(^|/)kandidaten/landesliste-")]
  landes_links <- xml2::url_absolute(unique(landes_links), candidate_url)
  
  list(meta = meta, answers = answers, landesliste_links = landes_links)
}

scrape_wen_waehlen_btw21 <- function(user_agent = DEFAULT_UA,
                                     pause_between = 0.3,
                                     include_landeslisten = TRUE) {
  message("1) Fetching all districts …")
  districts <- get_district_urls(ua = user_agent, pause_between = pause_between)
  
  message("2) Enumerating candidate URLs from districts …")
  cand_district <- purrr::map_dfr(
    districts$district_url,
    \(u) tryCatch(get_candidate_urls_from_district(u, ua = user_agent, pause_between = pause_between),
                  error = function(e) tibble())
  )
  cand_urls <- unique(cand_district$candidate_url)
  message(sprintf("   Found %d unique candidate URLs from districts.", length(cand_urls)))
  
  if (length(cand_urls) == 0L) {
    message("   WARNING: no candidate URLs found on district pages. Check network/headers.")
  }
  
  message("3) Scraping candidate pages (district candidates) …")
  pb <- progress_bar$new(total = length(cand_urls),
                         format = "  candidates [:bar] :current/:total eta: :eta")
  res_list <- vector("list", length(cand_urls))
  for (i in seq_along(cand_urls)) {
    pb$tick()
    res_list[[i]] <- tryCatch(
      parse_candidate_page(cand_urls[i], ua = user_agent, pause_between = pause_between),
      error = function(e) NULL
    )
  }
  
  metas   <- bind_rows(lapply(res_list, `[[`, "meta"))
  answers <- bind_rows(lapply(res_list, `[[`, "answers"))
  landes  <- unique(unlist(lapply(res_list, `[[`, "landesliste_links"), use.names = FALSE))
  
  if (isTRUE(include_landeslisten) && length(landes)) {
    message("4) Discovering additional candidates from Landeslisten …")
    land_cand_urls <- c()
    for (ll in landes) {
      doc <- tryCatch(http_get_html(ll, ua = user_agent, pause_between = pause_between),
                      error = function(e) NULL)
      if (is.null(doc)) next
      ch <- rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href")
      ch <- ch[!is.na(ch)]
      ch <- ch[stringr::str_detect(ch, "kandidaten/\\d+-")]
      land_cand_urls <- c(land_cand_urls, xml2::url_absolute(ch, ll))
    }
    land_cand_urls <- setdiff(unique(land_cand_urls), cand_urls)
    if (length(land_cand_urls)) {
      message(sprintf("   Found %d additional list-only candidate URLs.", length(land_cand_urls)))
      pb2 <- progress_bar$new(total = length(land_cand_urls),
                              format = "  list-only  [:bar] :current/:total eta: :eta")
      res_list2 <- vector("list", length(land_cand_urls))
      for (i in seq_along(land_cand_urls)) {
        pb2$tick()
        res_list2[[i]] <- tryCatch(
          parse_candidate_page(land_cand_urls[i], ua = user_agent, pause_between = pause_between),
          error = function(e) NULL
        )
      }
      metas2   <- bind_rows(lapply(res_list2, `[[`, "meta"))
      answers2 <- bind_rows(lapply(res_list2, `[[`, "answers"))
      metas    <- bind_rows(metas, metas2)   |> distinct(candidate_id, .keep_all = TRUE)
      answers  <- bind_rows(answers, answers2) |> distinct(candidate_id, question_no, .keep_all = TRUE)
    }
  }
  
  # Always have the expected columns for the join, even if empty:
  if (nrow(cand_district) == 0L || !all(c("candidate_url","district_no") %in% names(cand_district))) {
    cand_district <- tibble(candidate_url = character(),
                            district_no   = integer(),
                            district_url  = character())
  }
  
  meta_with_dist <- metas |>
    left_join(cand_district |> distinct(candidate_url, district_no), by = "candidate_url")
  
  list(
    candidates   = meta_with_dist,
    answers_long = answers
  )
}


get_candidate_urls_from_district("https://www.wen-waehlen.de/btw21/wahlkreis-1.html") %>% head()






library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(progress)
library(tibble)

# 1) Discover all candidate URLs across all districts
ua <- DEFAULT_UA
pause <- 0.3

districts <- get_district_urls(ua = ua, pause_between = pause)

cand_district <- purrr::map_dfr(
  districts$district_url,
  \(u) tryCatch(get_candidate_urls_from_district(u, ua = ua, pause_between = pause),
                error = function(e) tibble())
)

cand_urls <- unique(cand_district$candidate_url)
length(cand_urls)
# (Optional) peek
head(cand_urls, 5)

# 2) Scrape each candidate page (robust to occasional failures)
safe_parse <- purrr::safely(
  \(url) parse_candidate_page(url, ua = ua, pause_between = pause),
  otherwise = NULL, quiet = TRUE
)

pb <- progress::progress_bar$new(total = length(cand_urls),
                                 format = "  scraping [:bar] :current/:total eta: :eta")
res_list <- vector("list", length(cand_urls))
for (i in seq_along(cand_urls)) {
  pb$tick()
  res_list[[i]] <- safe_parse(cand_urls[i])$result
}

# Drop any NULLs (if a page failed transiently)
res_list <- res_list[!vapply(res_list, is.null, logical(1))]

# 3) Combine into data frames
metas   <- dplyr::bind_rows(lapply(res_list, `[[`, "meta"))
answers <- dplyr::bind_rows(lapply(res_list, `[[`, "answers"))

# Attach district numbers (from discovery step)
candidates <- metas %>%
  left_join(distinct(cand_district, candidate_url, district_no), by = "candidate_url")

# (Optional) keep only candidates who actually answered
answered_candidates <- candidates %>% filter(answered)
answers_long <- answers %>%
  semi_join(answered_candidates %>% select(candidate_id), by = "candidate_id")

# 4) Save
readr::write_csv(candidates,   "wen_waehlen_btw21_candidates.csv")
readr::write_csv(answers_long, "wen_waehlen_btw21_answers_long.csv")

# (Optional) wide view (one column per thesis)
answers_wide <- answers_long %>%
  select(candidate_id, question_no, answer) %>%
  mutate(question_no = paste0("Q", question_no)) %>%
  pivot_wider(names_from = question_no, values_from = answer)
readr::write_csv(answers_wide, "wen_waehlen_btw21_answers_wide.csv")

# Sanity checks
dplyr::count(candidates, answered)
dplyr::glimpse(candidates)
dplyr::glimpse(answers_long)


