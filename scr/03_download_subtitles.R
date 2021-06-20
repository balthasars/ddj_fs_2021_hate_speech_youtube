# example for youtube captions
library(youtubecaption)
reticulate::use_python(python = "/usr/local/bin/python3.9", required = TRUE)
reticulate::use_virtualenv(virtualenv = "wrappingtransformers", required = TRUE)

example_url <- "https://www.youtube.com/watch?v=NtYT-HAI2xc"

# helper wrapping get_caption to save to disk
get_caption_and_save_to_disk <- function(video_id) {
  video_link <- paste0("https://www.youtube.com/watch?v=", video_id)
  caption <- get_caption(video_link, language = "de")
  data.table::fwrite(
    x = caption,
    file = here::here(
      "raw", "subtitles",
      paste0(
        stringr::str_extract(video_link, "[^v=]+(?=/$|$)"),
        ".csv"
      )
    )
  )
}

# wrap in purrr::safely() to continue regardless of error
safely_get_caption_and_save_to_disk <- purrr::safely(get_caption_and_save_to_disk)

# read in all video IDs 
list_of_videos <- data.table::fread(file = here::here("raw", "list_of_videos.csv"))

errors_etc <- purrr::map(list_of_videos$content_details_video_id, safely_get_caption_and_save_to_disk)
