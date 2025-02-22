library(tidyverse)

# 2016 ----
brackets_2016 <- jsonlite::read_json("https://www48.myfantasyleague.com/2016/export?TYPE=playoffBracket&L=63018&APIKEY=&BRACKET_ID=1&JSON=1") %>%
  purrr::pluck("playoffBracket", "playoffRound") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::unnest(playoffGame) %>%
  tidyr::unnest_wider(playoffGame)

bf_finale_2016 <- brackets_2016 %>%
  dplyr::filter(week < max(week)) %>%
  dplyr::select(week, away, home) %>%
  tidyr::unnest_wider(away) %>%
  dplyr::rename(away_id = franchise_id, away_points = points) %>%
  dplyr::select(-seed, -winner_of_game) %>%
  tidyr::unnest_wider(home) %>%
  dplyr::rename(home_id = franchise_id, home_points = points) %>%
  dplyr::select(-seed, -winner_of_game)

finale_2016 <- brackets_2016 %>%
  dplyr::filter(
    week == max(week),
    !is.na(franchise_id)
  ) %>%
  dplyr::mutate(
    away_id = franchise_id[1],
    away_points = points[1],
    home_id = franchise_id[2],
    home_points = points[2]
  ) %>%
  dplyr::select(week, away_id, away_points, home_id, home_points) %>%
  dplyr::distinct()

post_2016 <- rbind(bf_finale_2016, finale_2016) %>%
  dplyr::mutate(postseason = "SB")

readr::write_rds(post_2016, "data/schedule/rfl-results-postseason-2016.rds")

# bis finale seit 2017 ----
var.season <- 2022
var.bracketID <- 1

bracket <- jsonlite::read_json(paste0("https://www48.myfantasyleague.com/", var.season, "/export?TYPE=playoffBracket&L=63018&APIKEY=&BRACKET_ID=", var.bracketID, "&JSON=1")) %>%
  purrr::pluck("playoffBracket", "playoffRound") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  tidyr::unnest(playoffGame) %>%
  tidyr::unnest_wider(playoffGame)

bracket_first_weeks <- bracket %>%
  dplyr::filter(week < max(week)) %>%
  dplyr::select(week, away, home) %>%
  tidyr::unnest_wider(away) %>%
  dplyr::rename(away_id = franchise_id, away_points = points) %>%
  dplyr::select(week, away_id, away_points, home) %>%
  tidyr::unnest_wider(home) %>%
  dplyr::rename(home_id = franchise_id, home_points = points) %>%
  dplyr::select(week, away_id, away_points, home_id, home_points)

# export single weeks
data2 <- bracket_first_weeks %>%
  dplyr::filter(week == 14)

write.csv(rbind(data, data2), paste0("data/schedule/rfl-results-postseason", var.season, ".csv"), row.names = F)

bracket_last_week <- bracket %>%
  dplyr::filter(
    week == max(week),
    !is.na(franchise_id)
  ) %>%
  dplyr::mutate(
    away_id = franchise_id[1],
    away_points = points[1],
    home_id = franchise_id[2],
    home_points = points[2]
  ) %>%
  dplyr::select(week, away_id, away_points, home_id, home_points) %>%
  dplyr::distinct()

bracket_1 <- rbind(bracket_first_weeks, bracket_last_week) %>% dplyr::mutate(postseason = "SB")
bracket_2 <- rbind(bracket_first_weeks, bracket_last_week) %>% dplyr::mutate(postseason = "SB")
bracket_5 <- rbind(bracket_first_weeks, bracket_last_week) %>% dplyr::mutate(postseason = "PB")
bracket_6 <- rbind(bracket_first_weeks, bracket_last_week) %>% dplyr::mutate(postseason = "TB")

# finale seit 2017 ----
var.bracketID <- 4

bracket <- jsonlite::read_json(paste0("https://www48.myfantasyleague.com/", var.season, "/export?TYPE=playoffBracket&L=63018&APIKEY=&BRACKET_ID=", var.bracketID, "&JSON=1")) %>%
  purrr::pluck("playoffBracket", "playoffRound") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::mutate(week = ifelse(var.season < 2021, 16, 17)) %>%
  tidyr::unnest_wider(away) %>%
  dplyr::rename(away_id = franchise_id, away_points = points) %>%
  dplyr::select(week, away_id, away_points, home) %>%
  tidyr::unnest_wider(home) %>%
  dplyr::rename(home_id = franchise_id, home_points = points) %>%
  dplyr::select(week, away_id, away_points, home_id, home_points) %>%
  dplyr::filter(!is.na(away_id))

bracket_3 <- bracket %>% dplyr::mutate(postseason = "SBC")
bracket_4 <- bracket %>% dplyr::mutate(postseason = "SB")

check <- rbind(bracket_1, bracket_2, bracket_3, bracket_4, bracket_5, bracket_6)
readr::write_rds(rbind(bracket_1, bracket_2, bracket_3, bracket_4, bracket_5, bracket_6), paste0("data/schedule/rfl-results-postseason-", var.season, ".rds"))
