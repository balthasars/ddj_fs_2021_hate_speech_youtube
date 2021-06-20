# Package {tuber} must be installed from here! 
# remotes::install_github(repo = "https://github.com/balthasars/tuber")
# The changes I've contributed to the package will not load otherwise, as some have not yet
# been integrated into the package. 
library(tuber)
library(dplyr)
library(tidyr)
yt_oauth(
  app_id = Sys.getenv("YOUTUBE_API_APP_ID"), 
  app_secret = Sys.getenv("YOUTUBE_API_CLIENT_SECRET")
  )

weltwoche_id <- "UCq-b0dwW97YRZWgSCikWQRA"

# get IDs for all videos in Weltwoche's channel
video_list <- tuber::list_channel_videos(
  channel_id = weltwoche_id, 
  # get as many results as possible
  max_results = 51
  ) %>%
  as_tibble() %>%
  janitor::clean_names()

# save to disk as CSV
data.table::fwrite(video_list, file = here::here("raw", "list_of_videos.csv"))

# get all video meta data 
video_meta <- video_list %>%
  # metadata not yet provided as dataframe by {tuber} - working on that :)
  dplyr::mutate(
    meta = purrr::map(
      # what to loop over
      content_details_video_id,
      # which function to apply repeatedly
      get_video_details,
      # function parameters
      as.data.frame = TRUE,
      # which parts of the meta data should be retrieved?
      part = c("snippet", "statistics", "id", "topicDetails", "status")
    )
  ) %>% 
  dplyr::select(-id) %>%
  tidyr::unnest_wider(meta)

# save to disk as CSV
video_meta %>% 
  # remove list columns, this will only remove the topic list which isn't too informative
  dplyr::select_if(~!is.list(.)) %>%
  data.table::fwrite(file = here::here("raw", "meta_data_to_vidoes.csv"))