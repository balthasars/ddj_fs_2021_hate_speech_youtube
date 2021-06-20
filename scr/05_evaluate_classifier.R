library(dplyr)
library(wrappingtransformers)
reticulate::use_python(python = "/usr/local/bin/python3.9", required = TRUE)
reticulate::use_virtualenv(virtualenv = "wrappingtransformers", required = TRUE)
wt_load_model("bert-base-german-cased-hatespeech-GermEval18Coarse")

germ_eval_2018_raw <- data.table::fread(
  here::here("raw", "germ_eval_2018", "germeval2018.test.txt"),
  col.names = c("tweet", "coarse", "fine"),
  quote = ""
) %>% 
  mutate(tweet_no = row_number())


# Helper, um Emojis zu entfernen ohne Umlaute zu verunstalten
remove_emojis <- function(string = "") {
  purrr::map_chr(string, ~ {
    string_as_int <- utf8ToInt(.x)
    # replace with empty space
    # https://stackoverflow.com/questions/59536855/is-there-any-way-to-remove-emoji-and-keep-other-special-characters-in-r
    # https://en.wikipedia.org/wiki/Whitespace_character
    string_as_int[which(string_as_int > 100000)] <- 160
    intToUtf8(string_as_int)
  })
}

# "Jap Haui Anonym🤔merci für diä ergänzig👍🏽" %>%
# remove_emojis()

# helper für Pre-Processing der Kommentare, so dass Tokenizer
# von transformers nicht mehr viel machen muss (kann Gewisses hier auch gar nicht).
clean_text_for_bert <- function(text) {
  text %>%
    # remove all emojis
    # gsub("\\p{So}|\\p{Cn}", "", ., perl = TRUE) %>%
    # Wörter mit Kerning zusammenkleben
    textclean::replace_kern() %>%
    # remove @-handles http://granades.com/2009/04/06/using-regular-expressions-to-match-twitter-users-and-hashtags/
    stringr::str_remove_all("@([A-Za-z0-9_]+)") %>%
    # remove links
    textclean::replace_url(replacement = "") %>%
    # remove html markup, Replace HTML tags, symbols
    textclean::replace_html(symbol = FALSE) %>%
    # remove e-mail addresses
    textclean::replace_email(replacement = "") %>%
    # hashtags
    stringr::str_remove_all("#") %>% 
    # replace one or more white space character with a single space
    textclean::replace_white() %>% 
    # replace """"
    stringr::str_remove_all('""""') %>% 
    # more than three dots (...)
    stringr::str_remove_all('[.]{3,}') %>% 
    # remove undetected emojis
    # stringr::str_remove_all('🏻') %>% 
    # stringr::str_remove_all('🏼') %>% 
    # stringr::str_remove_all('🏽') %>% 
    # stringr::str_remove_all('🏻') %>% 
    # stringr::str_remove_all('⁉️') %>% 
    # miscellaneous
    stringr::str_remove_all('"') %>% 
    # emoticons
    qdapRegex::rm_emoticon() %>%  
    # emojis
    remove_emojis() %>% 
    # twitter handles
    stringr::str_remove_all("@\\S+")
}

# preprocessing
tweets_preprocessed <- germ_eval_2018_raw %>%
  mutate(
    tweet_preprocessed = clean_text_for_bert(tweet)
  )

# helper for grouping into bins of size N
group_into_n_bins <- function(vector_input, group_size) {
  # how many groups of the indicated group size
  # minus the last one?
  if (length(vector_input) < group_size) {
    rep(1, length(vector_input))
  } else if (length(vector_input >= group_size)) {
    n_groups_of_group_size <- length(vector_input) %/% group_size
    # remainder
    size_last_group <- length(vector_input) %% group_size
    # create sequence for first n groups
    first_n_groups <- 1:n_groups_of_group_size %>%
      purrr::map(rep, group_size) %>%
      purrr::flatten_int()
    #
    # # create sequence for last group
    last_group <- rep(n_groups_of_group_size + 1, size_last_group)
    group_sequence <- c(first_n_groups, last_group)
    group_sequence
  }
}

# divide up strings by whitespace and then make into portions of
# 300ish "tokens".
# Das muss gemacht werden, da das BERT-Modell keine Inputs akzeptiert,
# die länger als 512 Tokens lang sind.

portions <- tweets_preprocessed %>%
  select(tweet_preprocessed, tweet_no) %>%
  # split up 
  mutate(comment_split = strsplit(tweet_preprocessed, split = " ")) %>%
  # unnest and make data long
  tidyr::unnest_longer(col = comment_split) %>% 
  # group by comment ID
  group_by(tweet_no) %>%
  # make portions of 300 "tokens"
  mutate(group_no = group_into_n_bins(comment_split, group_size = 300)) %>%
  ungroup() %>% 
  # make new comment portions 
  group_by(tweet_no, group_no) %>%
  summarize(comments_reportion = paste0(comment_split, collapse = " ")) %>% 
  ungroup()

# helper wrapping prediction function in purrr::safely to capture results and errors
safely_wt_predict_bert_base_german_cased_hatespeech <- purrr::safely(wt_predict_bert_base_german_cased_hatespeech)
# helper that will save the result inside `raw/classifier/results/xxx_xxx_xxx.csv` or in
# `raw/classifier/errors/xxx_xxx_xxx.csv` depending on the outcome
classify_and_save <- function(comment, comment_id_arg, portion_arg) {
  inference_output <- safely_wt_predict_bert_base_german_cased_hatespeech(comment)
  if (!rlang::is_null(inference_output$result)) {
    inference_output_df <- inference_output$result %>%
      mutate(
        comments_reportion = comment,
        id = comment_id_arg,
        # video_id = video_id_arg,
        group_no = portion_arg
      )
    # output abspeichern
    data.table::fwrite(
      x = inference_output_df,
      file = here::here(
        "raw", "classifier", "evaluate_with_germ_eval",
        paste0(comment_id_arg, "_", portion_arg, ".csv")
      )
    )
  } else if(rlang::is_null(inference_output$result)){
    inference_output$error %>%
      as.character() %>% 
      writeLines(
        text = .,
        con = here::here(
          "raw", "classifier", "errors",
          paste0(comment_id_arg, "_", portion_arg, "_", "germ_eval", ".txt")
        )
      )
  }
}

# # expect succcess
# esempio_long_valid <- "Sehr geehrter Herr Köppel, bitte googlen Sie einmal - zum Verhüllungsverbot - Islamische Revolution im Iran: Kopftuch statt Freiheit - Es fing 1979 an! Es hat NICHTS mit Religion zu tun! Viel Spaß und Grüße aus Deutschland"
# classify_and_save(
#   comment = esempio_long_valid, 
#   video_id_arg = "12345", 
#   comment_id_arg = "hihihi", portion_arg = "1"
#   )
# # failure
# classify_and_save(
#   # comment that is too long
#   comment = paste0(rep(esempio_long, 10), collapse = " "), 
#   video_id_arg = "12345", 
#   comment_id_arg = "hihihi", portion_arg = "1"
#   )

# Classifier aufrufen und Resultate abspeichern
portions %>%
  mutate(
    classification = purrr::pwalk(
      list(comments_reportion, tweet_no, group_no),
      classify_and_save
    )
  )

# Resultate zusammen abspeichern in eigenem File
classifier_resultate_pfade <- fs::dir_ls(
  here::here("raw", "classifier", "evaluate_with_germ_eval")
)
classifier_resultate <- purrr::map_df(
  classifier_resultate_pfade,
  data.table::fread
)

# Ab hier: Durschnittliche Scores pro Kommentar für Offense generieren, für 
# Kommentare, die in mehrere Portionen aufgeteilt wurden
ids_wo_mehrere_portionen <- classifier_resultate %>%
  filter(group_no > 1) %>% 
  pull(id)

ids_von_kommentaren_mit_mehrere_klassifizierungen <- classifier_resultate %>% 
  filter(id %in% ids_wo_mehrere_portionen) %>%
  # einzigartige Kombinationen von ID und Art des Labels
  distinct(label, id) %>%
  count(id) %>% 
  filter(n > 1) %>% 
  pull(id)

scores_fuer_kommentare_mit_mehreren_unterschiedliche_klassifizierte_labels <- classifier_resultate %>%
  filter(id %in% ids_von_kommentaren_mit_mehrere_klassifizierungen) %>% 
  group_by(group_no, id) %>%
  mutate(new_score = case_when(
    id %in% ids_von_kommentaren_mit_mehrere_klassifizierungen & label == "OFFENSE" ~ 0.5 - (score - 0.5),
    TRUE ~ score
  )) %>%
  summarize(
    avg_score = mean(new_score),
    label = if_else(avg_score > 0.5, "OFFENSE", "OTHER")
  ) %>%
  ungroup() %>% 
  rename(score = avg_score)

classifier_resultate_neu <- classifier_resultate %>%
  select(-c(comments_reportion)) %>% 
  # Entfernen von Kommentaren mit mehreren Klassifizierungen
  filter(!id %in% ids_von_kommentaren_mit_mehrere_klassifizierungen) %>%
  # durchschnittlichen Score von aufportionierten Kommentaren berechnen bei all jenen,
  # die nur in einer Portion waren oder mehrmals dasselbe Label zugeteilt wurde
  group_by(id, label, group_no) %>% 
  summarize(score = mean(score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  # Hinzufügen von den Scores für Kommentare, die in mehrere Portionen aufgeteilt waren
  # und dabei unterschiedliche Labels klassifiziert bekamen
  bind_rows(scores_fuer_kommentare_mit_mehreren_unterschiedliche_klassifizierte_labels)

check_ob_pro_kommentar_nur_ein_label <- classifier_resultate_neu %>% 
  count(id, label) %>% 
  filter(n > 1) %>% 
  nrow()

if(check_ob_pro_kommentar_nur_ein_label > 0){stop("Mehr als ein Label pro Kommentar, bitte prüfen!")}

data.table::fwrite(
  x = classifier_resultate_neu,
  here::here("data", "germ_eval_klassifiziert.csv")
)
