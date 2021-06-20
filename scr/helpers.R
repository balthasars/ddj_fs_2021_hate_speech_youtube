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
    remove_emojis()
}

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

# Funktion, um Themen zu finden, die stark mit dem Anteil der "Offense"-Themen korrelieren :
find_topic_correlations <- function(topic_model_input, K_input, dependent_variable = "prop_offense", document_sparse_matrix = untertitel_sparse) {
  
  # Gamma-Werte
  td_gamma <- topic_model_input %>%
    tidy(matrix = "gamma", document_names = rownames(document_sparse_matrix))
  
  # Wahrscheinlichkeit, dass jedes Wort von einem bestimmten Topic generiert wurde
  td_beta <- tidy(topic_model_input)
  # Top-10 Wörter mit höchster Wahrscheinlichkeit, einem Thema zuzugehören
  top_terms <- td_beta %>%
    arrange(beta) %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    arrange(-beta) %>%
    select(topic, term) %>%
    summarise(terms = list(term)) %>%
    mutate(terms = purrr::map(terms, paste, collapse = ", ")) %>%
    tidyr::unnest(cols = c(terms))
  
  # Daten zu Offense hinzufügen
  gamma_plus_offense <- td_gamma %>%
    tidyr::pivot_wider(values_from = gamma, names_from = topic, names_prefix = "t_") %>%
    left_join(
      filter(anteil_offense_an_kommentaren_pro_video, label == "OFFENSE"),
      by = c("document" = "content_details_video_id")
    )
  
  # Regressionsformel dynamisch mit den jeweiligen Topicnamen generieren
  topic_colnames_vec <- colnames(gamma_plus_offense) %>%
    as_tibble() %>%
    filter(startsWith(value, "t_")) %>%
    pull(value) %>%
    paste0(collapse = " + ") %>%
    paste0(dependent_variable, " ~ ", .) %>%
    as.formula()
  
  model <- 
    list(lm(formula = topic_colnames_vec, data = gamma_plus_offense)) %>%
    tibble::enframe(value = "lm_model") %>% 
    mutate(
      tidied = purrr::map(lm_model, broom::tidy)
    ) %>% 
    tidyr::unnest_wider(tidied) %>% 
    tidyr::unnest(col = term:p.value) %>% 
    tidyr::separate(
      col = "term",
      into = c("junk", "topic"),
      sep = "_",
      remove = FALSE
    ) %>% 
    select(-junk) %>% 
    mutate(topic = as.integer(topic)) %>% 
    left_join(top_terms, by = c("topic")) %>% 
    select(-name)
}

# Funktion, um Topic Models zu plotten
plot_topic_model_2 <- function(topic_model, n) {
  
  n_topics <- if_else(n > 20, 20, n)
  # Wahrscheinlichkeit, dass jedes Wort von einem bestimmten Topic generiert wurde
  td_beta <- tidy(topic_model)
  # Wahrscheinlichkeit, dass ein Dokument von ein bestimmtes Topic beinhaltet
  td_beta %>% 
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(
      topic = paste0("Topic ", topic),
      term = reorder_within(term, beta, topic)
    ) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~topic, scales = "free", ncol = 2) +
    coord_flip() +
    scale_x_reordered() +
    labs(
      x = NULL, y = expression(beta),
      title = "Höchste Wortwahrscheinlichkeiten für jedes Thema"
    )
}