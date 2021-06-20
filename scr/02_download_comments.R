library(tuber)
library(dplyr)
tuber::yt_oauth(
  app_id = Sys.getenv("YOUTUBE_API_APP_ID"), 
  app_secret = Sys.getenv("YOUTUBE_API_CLIENT_SECRET")
)

# save to disk as CSV
list_of_videos <- data.table::fread(file = here::here("raw", "list_of_videos.csv"))

# helper to save comments by episode to disk upon retrieval,
# and removes any list columns that might still be left.

get_most_comments_and_save_to_disk <- function(video_id_input){
  result <- tuber::get_most_comments(video_id = video_id_input) %>%
    # make sure to remove list columns
    dplyr::select_if(~!is.list(.))
  data.table::fwrite(result, file = here::here("raw", "comments", paste0(video_id_input, ".csv")))
}
# wrap content_details_video_id in `purrr::safely()` to retrieve content regardless 
# of whether there is an error
safely_get_most_comments_and_save_to_disk <- purrr::safely(get_most_comments_and_save_to_disk)

# get all video comments
video_comments <- list_of_videos %>%
  dplyr::mutate(
    meta = purrr::map(
      # what to loop over
      content_details_video_id,
      # which function to apply repeatedly
      safely_get_most_comments_and_save_to_disk
    )
  )

comments_unnested <- video_comments %>%
  dplyr::select(-id) %>% 
  dplyr::mutate(meta_2 = purrr::transpose(meta)[["result"]]) %>% 
  dplyr::select(-c(etag, kind)) %>%
  tidyr::unnest_wider(meta_2) %>% 
  dplyr::select(-c(meta)) %>%
  tidyr::unnest(cols = everything())

# save errors to disk
