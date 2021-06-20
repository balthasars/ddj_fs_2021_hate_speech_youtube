# remotes::install_github("systats/tidyTX")
library(dplyr)
library(tidytext)
library(quanteda)
meta <- data.table::fread(here::here("data", "meta.csv"))
untertitel <- data.table::fread(here::here("data", "untertitel.csv")) %>%
  left_join(meta, by = c("vid" = "content_details_video_id")) %>% 
  filter(content_details_video_published_at > lubridate::ymd("2020-01-01")) %>%
  tibble::as_tibble()

untertitel_long <- untertitel %>%
  # ins lange Format, entfernt alle Satzzeichen
  unnest_tokens(
    # Name der neuen Spalte
    output = wort, 
    # welche Spalte aufsplitten
    text, 
    # welche Art der Tokenisierung
    token = "words"
    ) %>%
  filter(
    # deutsche Stopwords entfernen
    !wort %in% stopwords::data_stopwords_stopwordsiso$de,
    !wort %in% c(
      # Youtube-Tag für Musik löschen
      "musik",
      # Wörter, die RK auch noch oft verwendet und in STMs auftreten
      "einfach", "sagen", "sehen", "geben", "nämlich",
      "eigentlich", "wichtig",
      "politik", "fragen", "sozusagen", "sicht", "weltwoche",
      "damen", "herren", "glauben", "finden", "interessant",
      "leute", "schweiz", ""
      )
  ) %>%
  mutate(
    # Zahlen entfernen
    wort = textclean::replace_number(wort)
  ) %>% 
  # lemmata hinzufügen, falls vorhanden
  left_join(tidyTX::hash_lemma_de, by = c("wort" = "word")) %>%
  # wort mit lemma ersetzen
  mutate(wort = if_else(is.na(lemma), wort, lemma))

# Umwandeln in sparse Matrize
untertitel_sparse <- untertitel_long %>%
  # wie oft kommt ein Wort pro Video vor?
  count(wort, vid) %>%
  arrange(vid) %>%
  cast_dfm(document = vid, term = wort, value = n) %>%
  dfm_trim(sparsity = 0.94)
  # dfm_trim(min_termfreq = 20, max_termfreq = 100)

readr::write_rds(untertitel_sparse, here::here("raw", "untertitel_sparse.rds"))

# Kovariaten für STM
kovariaten <- untertitel %>%
  mutate(
    date = as.Date(content_details_video_published_at),
    year = lubridate::year(date),
    month = lubridate::month(date),
    year_month = paste0(year, month)
  ) %>% 
  distinct(vid, year, month, year_month)

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
      documents = untertitel_sparse,
      init.type = "Spectral", 
      verbose = TRUE,
      data = kovariaten,
      seed = 8458159,
      max.em.its = 200,
      # zeitliche Dimension und Video-Clustering
      prevalence = ~ vid + year + s(month),
      gamma.prior = "L1"
      # from Shipan/Wuest/Gilardi 2020 Paper
      # control = list(rp.s = 0.05, rp.p = 2000,
      #                rp.d.group.size = 2000)
    ))
  )

readr::write_rds(stm_models, here::here("data", "stm_models.rds"))
