library(nflreadr)
library(tidyverse)
library(piggyback)

cli::cli_alert_info("Create Data")

current_season <- nflreadr::get_current_season()

players <- players <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1"))$players$player %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    player_name = name,
    pos = position,
    player_id = id
  )

starter <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=weeklyResults&L=63018&APIKEY=&W=YTD&JSON=1"))$allWeeklyResults$weeklyResults %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::filter(as.numeric(week) <= 13) %>%
  dplyr::select(-franchise) %>%
  tidyr::unnest(matchup) %>%
  tidyr::unnest_wider(matchup) %>%
  tidyr::unnest(franchise) %>%
  tidyr::unnest_wider(franchise) %>%
  dplyr::select(week, id, player) %>%
  dplyr::rename(franchise_id = id) %>%
  tidyr::unnest(player) %>%
  tidyr::unnest_wider(player) %>%
  dplyr::mutate(season = current_season) %>%
  dplyr::rename(
    starter_status = status,
    player_id = id,
    should_start = shouldStart,
    player_score = score
  ) %>%
  dplyr::filter(!is.na(should_start)) %>%
  dplyr::left_join(
    players %>% dplyr::select(-status), by = "player_id", multiple = "all"
  ) %>%
  dplyr::select(season, week, franchise_id, starter_status, player_id, player_name, pos, team, player_score, should_start) %>%
  dplyr::distinct()

cli::cli_alert_info("Write Data")
readr::write_csv(starter, paste0("rfl_starter_", current_season, ".csv"))

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_starter_", current_season, ".csv"), "bohndesverband/rfl-data", "starter_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "starter_data", overwrite = TRUE)
