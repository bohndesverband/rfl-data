library(tidyverse)
library(piggyback)
library(nflreadr)

cli::cli_alert_info("Create Data")
draft_season <- lubridate::year(lubridate::now())
mfl <- ffscrapr::mfl_connect(draft_season, 63018)

draft_data <- ffscrapr::ff_draft(mfl) %>%
  dplyr::rename(mfl_id = player_id) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = "mfl_id"
  ) %>%
  dplyr::left_join(
    jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", draft_season, "/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1"))$players$player %>%
      dplyr::tibble() %>%
      tidyr::hoist(1, "id", "status") %>%
      dplyr::select(id, status),
    by = c("mfl_id" = "id")
  ) %>%
  dplyr::mutate(
    season = draft_season,
    player_name = nflreadr::clean_player_names(player_name),
    is_rookie = dplyr::case_when(
      status == "R" ~ 1,
      TRUE ~ 0
    ),
    team = nflreadr::clean_team_abbrs(team)
  ) %>%
  dplyr::select(season, timestamp, overall, round, pick, franchise_id, franchise_name, mfl_id, gsis_id, player_name, pos, team, is_rookie)

cli::cli_alert_info("Write Data")
readr::write_csv(draft_data, paste0("rfl_draft_", draft_season, ".csv"))

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_draft_", draft_season, ".csv"), "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)
