# remotes::install_github("systats/tidyTX")
library(dplyr)
library(tidytext)
library(quanteda)
# Helper-Funktionen laden
source(here::here("scr", "helpers.R"))
meta <- data.table::fread(here::here("data", "meta.csv"))
kommentare <- data.table::fread(here::here("data", "kommentare.csv")) %>%
  select(-c(
    kind, etag, response_kind, items_kind, 
    items_etag, response_etag)) %>% 
  rename(kommentar_id = id) %>% 
  # zusammenführen mit `meta`
  left_join(meta, by = c("snippet_video_id" = "content_details_video_id")) %>% 
  filter(content_details_video_published_at > lubridate::ymd("2020-01-01")) %>% 
  tibble::as_tibble()

kommentare_long <- kommentare %>%
  # ins lange Format, entfernt alle Satzzeichen
  unnest_tokens(
    # Name der neuen Spalte
    output = wort,
    # welche Spalte aufsplitten
    snippet_text_original,
    # welche Art der Tokenisierung
    token = "words"
  ) %>%
  mutate(text_display = clean_text_for_bert(wort)) %>%
  filter(
    # deutsche Stopwords entfernen
    !wort %in% stopwords::data_stopwords_stopwordsiso$de,
    !wort %in% c(
      "köppel", "www.youtube.com", "https", "youtu.be", "watch", "herr",
      "roger"
    )
  ) %>%
  mutate(
    # Zahlen entfernen
    wort = textclean::replace_number(wort, remove = TRUE)
  ) %>%
  # lemmata hinzufügen, falls vorhanden
  left_join(tidyTX::hash_lemma_de, by = c("wort" = "word")) %>%
  # wort mit lemma ersetzen
  mutate(wort = if_else(is.na(lemma), wort, lemma))

# Umwandeln in sparse Matrize
kommentare_sparse <- kommentare_long %>%
  # wie oft kommt ein Wort pro Video vor?
  count(wort, video_id) %>%
  arrange(video_id) %>%
  cast_dfm(document = video_id, term = wort, value = n) %>%
  dfm_trim(sparsity = 0.7)

readr::write_rds(kommentare_sparse, here::here("raw", "kommentare_sparse.rds"))

# Kovariaten für STM
kovariaten <- kommentare %>%
  mutate(
    date = as.Date(content_details_video_published_at),
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>% 
  distinct(video_id, year, month)

library(furrr)
library(stm)
plan(multiprocess)
stm_models <-
  tibble::tibble(
    K = c(
      seq.int(from = 10, to = 30, by = 5)
    )
  ) %>%
  mutate(
    topic_model = future_map(K, ~ stm(
      K = .x,
      documents = kommentare_sparse,
      init.type = "Spectral", verbose = FALSE,
      data = kovariaten,
      # zeitliche Dimension, Video-Clustering
      prevalence = ~ video_id + year + month,
      gamma.prior = "L1"
    ))
  )

readr::write_rds(stm_models, here::here("data", "stm_models_kommentare.rds"))
