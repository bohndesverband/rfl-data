library(piggyback)
library(tidyverse)

var_season <- 2026

old_data <- readr::read_csv("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/adp_data.csv", col_types = "icidii") %>%
  dplyr::filter(season < var_season)

adp_data_raw <- jsonlite::read_json(paste0("https://api.myfantasyleague.com/", var_season, "/export?TYPE=adp&PERIOD=ALL&FCOUNT=12&IS_PPR=1&IS_KEEPER=R&IS_MOCK=-1&CUTOFF=5&DETAILS=&JSON=1"))$adp$player %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::mutate(
    season = var_season,
    mfl_id = id,
    pick_avg = averagePick,
    rfl_min = (as.numeric(rank) - 1) * 3 + 1,
    rfl_max = rfl_min + 2
  ) %>%
  dplyr::select(season, mfl_id, rank, pick_avg, rfl_min, rfl_max)

cli::cli_alert_info("Write Data")
readr::write_csv(rbind(old_data, adp_data_raw) %>% dplyr::distinct(), "adp_data.csv")

cli::cli_alert_info("Upload Data")
piggyback::pb_upload("adp_data.csv", "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)
