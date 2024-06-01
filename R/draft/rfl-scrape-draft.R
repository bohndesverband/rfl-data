library(tidyverse)
library(piggyback)
library(nflreadr)

cli::cli_alert_info("Create Data")

var_draft_season <- nflreadr::get_current_season(TRUE)

draft_data <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", var_draft_season, "/export?TYPE=draftResults&L=63018&APIKEY=&JSON=1"))$draftResults$draftUnit$draftPick |>
  dplyr::tibble() |>
  tidyr::unnest_wider(1) |>
  dplyr::mutate(season = var_draft_season) |>
  dplyr::rename(mfl_id = player) |>
  dplyr::arrange(round, pick) |>
  dplyr::mutate(overall = dplyr::row_number()) |>
  dplyr::left_join(
    nflreadr::load_ff_playerids() |>
      dplyr::select(mfl_id, gsis_id),
    by = "mfl_id"
  ) |>
  dplyr::left_join(
    nflreadr::load_players() |>
      dplyr::select(gsis_id, display_name),
    by = "gsis_id",
    na_matches = "never"
  ) |>
  dplyr::left_join(
    jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", var_draft_season, "/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1"))$players$player |>
      dplyr::tibble() |>
      tidyr::unnest_wider(1),
    by = c("mfl_id" = "id")
  ) |>
  dplyr::mutate(
    player_name = ifelse(is.na(display_name), name, display_name),
    is_rookie = dplyr::case_when(
      status == "R" ~ 1,
      TRUE ~ 0
    )
  ) |>
  dplyr::select(season, overall, round, pick, franchise, mfl_id, gsis_id, player_name, position, team, is_rookie)

cli::cli_alert_info("Write Data")
readr::write_csv(draft_data, paste0("rfl_draft_", var_draft_season, ".csv"))

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_draft_", var_draft_season, ".csv"), "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)

write(jsonlite::toJSON(c("last_updated", format(Sys.time(), "%Y-%m-%d %X GMT"))), "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)
