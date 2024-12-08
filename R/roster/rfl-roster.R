library(nflreadr)
library(tidyverse)
library(piggyback)
library(ffscrapr)

cli::cli_alert_info("Create Data")

current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week(TRUE)

mfl <- ffscrapr::mfl_connect(current_season, 63018)

roster <- ffscrapr::ff_rosters(mfl, week = current_week) %>%
  dplyr::mutate(
    season = current_season,
    week = current_week
  ) %>%
  dplyr::select(season, week, franchise_id, player_id, roster_status)

if (current_week == 1) {
  cli::cli_alert_info("Write Data")
  readr::write_csv(roster, paste0("rfl_roster_", current_season, ".csv"))
} else {
  old_data <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/roster_data/rfl_roster_", current_season, ".csv"), col_types = "dicic") %>%
    dplyr::filter(week < current_week)

  cli::cli_alert_info("Write Data")
  readr::write_csv(rbind(old_data, roster), paste0("rfl_roster_", current_season, ".csv"))
}

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_roster_", current_season, ".csv"), "bohndesverband/rfl-data", "roster_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "roster_data", overwrite = TRUE)
