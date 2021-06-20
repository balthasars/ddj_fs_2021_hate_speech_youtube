library(dplyr)
paths_to_all_comments <- fs::dir_ls(
  here::here("raw", "comments")
)
# alle Kommentare im Kommentare-Ordner einlesen
comments_raw <- purrr::map_df(
  paths_to_all_comments, 
  data.table::fread, 
  drop = c("nexPageToken", "pageInfo_totalResults", "pageInfo_resultsPerPage")
  ) %>%
  tibble::as_tibble() %>%
  # allfällige Duplikate löschen
  distinct()

# Metadaten einlesen
meta_raw <- data.table::fread(file = here::here("raw", "meta_data_to_vidoes.csv")) %>% 
  tibble::as_tibble()

# Metadaten zu Episoden
meta <- meta_raw %>%
  # Videos zu Weltwoche Daily rausfiltern
  filter(grepl(x = localized_title, pattern = "Weltwoche Daily")) %>% 
  janitor::clean_names()

# save final version
data.table::fwrite(
  x = meta,
  here::here("data", "meta.csv")
)

# Kommentare zu Weltwoche Daily rausfiltern
kommentare <- comments_raw %>%
  filter(videoId %in% meta$id) %>% 
  janitor::clean_names()

data.table::fwrite(
  x = kommentare,
  here::here("data", "kommentare.csv")
)

# alle Untertitel einlesen 
paths_to_all_subtitles <- fs::dir_ls(
  here::here("raw", "subtitles")
)

untertitel <- purrr::map_df(
  paths_to_all_subtitles, 
  data.table::fread 
) %>%
  tibble::as_tibble() %>%
  # subset auf WW Daily
  filter(vid %in% meta$id) %>% 
  janitor::clean_names()

data.table::fwrite(
  x = untertitel,
  here::here("data", "untertitel.csv")
)
