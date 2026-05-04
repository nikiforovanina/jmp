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

# --- config (2013) ----
BASE_INDEX_WAHLKREISE <- "https://www.wen-waehlen.de/btw13/wahlkreise.html"

# Browser-like UA & gzip/deflate only (avoid brotli issues on some R builds)
DEFAULT_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/120.0.0.0 Safari/537.36"
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

# --- robust HTTP loader: decode to text, opt out of br ----
http_get_html <- function(url,
                          ua = DEFAULT_UA,
                          pause_between = 0.3,
                          retries = 5,
                          timeout_sec = 30) {
  resp <- httr::RETRY(
    "GET", url,
    httr::user_agent(ua),
    httr::add_headers(
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      "Accept-Language" = "de-DE,de;q=0.9,en-US;q=0.7,en;q=0.6",
      "Accept-Encoding" = "gzip, deflate"
    ),
    httr::timeout(timeout_sec),
    times = retries,
    pause_min = 1, pause_base = 1.5, pause_cap = 8
  )
  httr::stop_for_status(resp)
  Sys.sleep(pause_between)
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

# --- candidate links on a district page (permissive) ----
get_candidate_urls_from_district <- function(district_url,
                                             ua = DEFAULT_UA,
                                             pause_between = 0.3) {
  doc <- http_get_html(district_url, ua = ua, pause_between = pause_between)
  hrefs <- rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href")
  hrefs <- hrefs[!is.na(hrefs)]
  cand <- hrefs[stringr::str_detect(hrefs, "kandidaten/\\d+-")]
  cand <- xml2::url_absolute(unique(cand), district_url)
  tibble(
    candidate_url = cand,
    district_no   = as.integer(stringr::str_match(district_url, "wahlkreis-(\\d+)\\.html$")[, 2]),
    district_url  = district_url
  )
}

# --- parse helpers (meta + answers) ----
extract_meta_from_text <- function(txt) {
  # "Name (PARTEI)" near the top
  name_party <- stringr::str_match(txt, "\\n\\s*([^\\n]+?)\\s*\\(([^\\)]+)\\)\\s*\\n")
  name  <- name_party[, 2] %||% NA_character_
  party <- name_party[, 3] %||% NA_character_
  
  # District & Landesliste lines (both present on 2013 pages)
  district   <- stringr::str_match(txt, "Wahlkreis:\\s*([^\\n]+)")[, 2] %||% NA_character_
  list_state <- stringr::str_match(txt, "Landesliste\\s+([^\\n,]+)")[, 2] %||% NA_character_
  
  tibble(name = name, party = party, district = district, list_state = list_state)
}

# Find the start of the answer section (handles 2013 & later wordings)
.locate_answers_start <- function(txt) {
  pats <- c(
    "Beantwortung der Thesen",                                  # 2013
    "Beantwortung der politischen Forderung(en)? und Thesen",   # 2021+
    "Forderungen/?\\s*&?\\s*Thesen"                             # fallback
  )
  for (p in pats) {
    loc <- stringr::str_locate(txt, regex(p, ignore_case = TRUE))[1, ]
    if (!is.na(loc[1])) return(loc)
  }
  matrix(c(NA_real_, NA_real_), nrow = 1, dimnames = list(NULL, c("start", "end")))
}

extract_answers_from_text <- function(txt) {
  loc <- .locate_answers_start(txt)
  if (is.na(loc[1])) return(tibble())  # candidate did not answer
  
  sub <- stringr::str_sub(txt, start = loc[2] + 1L)
  sub <- stringr::str_replace_all(sub, "\\r", "")
  lines <- stringr::str_split(sub, "\\n")[[1]] |> trimws()
  lines <- lines[nzchar(lines)]
  
  answer_labels <- c("Ja!", "Nein!", "Eher ja", "Eher nicht", "Unentschieden", "Keine Angabe")
  
  out <- list(); current <- NULL; comment_acc <- character(); i <- 1L
  
  while (i <= length(lines)) {
    line <- lines[i]
    
    # Thesis start, e.g. "1 [link]Mindestlohn einführen!"
    m <- stringr::str_match(line, "^(\\d{1,3})\\s+(.+)$")
    if (!is.na(m[1])) {
      # flush previous
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
      
      # find the first line that contains any answer label (label may be preceded by "Image")
      while (i <= length(lines)) {
        l2 <- lines[i]; found <- FALSE
        for (lab in answer_labels) {
          pos <- stringr::str_locate(l2, fixed(lab))[1, ]
          if (!is.na(pos[1])) {
            current$answer <- lab
            # text AFTER the label
            rest <- stringr::str_trim(substr(l2, pos[2] + 1L, nchar(l2)))
            
            # "important" star appears AFTER the label (keep 'Image' before label out)
            if (stringr::str_detect(rest, regex("Image|★|Stern", ignore_case = TRUE))) {
              current$important <- TRUE
              rest <- stringr::str_replace_all(rest, regex("Image|★|Stern", ignore_case = TRUE), "")
              rest <- stringr::str_squish(rest)
            }
            
            if (nchar(rest) > 0) comment_acc <- c(comment_acc, rest)
            found <- TRUE; i <- i + 1L; break
          }
        }
        if (found) break
        i <- i + 1L
      }
      
      # accumulate comment lines until next question block or end
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
  
  meta    <- extract_meta_from_text(txt)
  answers <- extract_answers_from_text(txt)
  answered <- nrow(answers) > 0L
  candidate_id <- stringr::str_match(candidate_url, "/kandidaten/(\\d+)-")[, 2] %||% NA_character_
  
  meta <- meta |>
    mutate(
      candidate_id  = candidate_id,
      candidate_url = candidate_url,
      answered      = answered,
      n_answers     = nrow(answers)
    )
  
  answers <- answers |>
    mutate(candidate_id = candidate_id, candidate_url = candidate_url)
  
  # discover Landeslisten from the profile (for optional list-only discovery)
  landes_links <- rvest::html_elements(doc, "a[href]") |> rvest::html_attr("href")
  landes_links <- landes_links[!is.na(landes_links)]
  landes_links <- landes_links[stringr::str_detect(landes_links, "(^|/)kandidaten/landesliste-")]
  landes_links <- xml2::url_absolute(unique(landes_links), candidate_url)
  
  list(meta = meta, answers = answers, landesliste_links = landes_links)
}

scrape_wen_waehlen_btw13 <- function(user_agent = DEFAULT_UA,
                                     pause_between = 0.3,
                                     include_landeslisten = TRUE) {
  message("1) Fetching all districts (2013) …")
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
  
  message("3) Scraping candidate pages …")
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
      metas    <- bind_rows(metas, metas2)     |> distinct(candidate_id, .keep_all = TRUE)
      answers  <- bind_rows(answers, answers2) |> distinct(candidate_id, question_no, .keep_all = TRUE)
    }
  }
  
  # ensure expected join columns exist even if empty
  if (nrow(cand_district) == 0L || !all(c("candidate_url","district_no") %in% names(cand_district))) {
    cand_district <- tibble(candidate_url = character(),
                            district_no   = integer(),
                            district_url  = character())
  }
  
  candidates <- metas |>
    left_join(cand_district |> distinct(candidate_url, district_no), by = "candidate_url")
  
  list(
    candidates   = candidates,
    answers_long = answers
  )
}

# --- run (all districts, incl. Landeslisten) ----
out13 <- scrape_wen_waehlen_btw13(include_landeslisten = TRUE)
candidates_2013   <- out13$candidates
answers_long_2013 <- out13$answers_long

# wide view (one column per thesis)
answers_wide_2013 <- answers_long_2013 %>%
  select(candidate_id, question_no, answer) %>%
  mutate(question_no = paste0("Q", question_no)) %>%
  pivot_wider(names_from = question_no, values_from = answer)

# --- save to disk ----
readr::write_csv(candidates_2013,   "wen_waehlen_btw13_candidates.csv")
readr::write_csv(answers_long_2013, "wen_waehlen_btw13_answers_long.csv")
readr::write_csv(answers_wide_2013, "wen_waehlen_btw13_answers_wide.csv")

# quick checks
dplyr::count(candidates_2013, answered)
glimpse(candidates_2013)
glimpse(answers_long_2013)