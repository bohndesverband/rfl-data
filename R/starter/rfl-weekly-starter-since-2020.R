library(tidyverse)

current_season <- 2023

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
  dplyr::select(season, week, franchise_id, starter_status, player_id, player_name, pos, team, player_score, should_start)

readr::write_csv(starter, "output.csv")
