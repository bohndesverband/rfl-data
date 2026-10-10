library(nflreadr)
library(tidyverse)
library(piggyback)
library(ffscrapr)

cli::cli_alert_info("Create Data")


current_week <- nflreadr::get_current_week(TRUE)

if (current_week <= 17) {

  current_season <- nflreadr::get_current_season()
  #current_season <- 2026

  mfl <- ffscrapr::mfl_connect(current_season, 63018)

  #for (wk in 1:18) {
  #  current_week <- wk

    roster <- ffscrapr::ff_rosters(mfl, week = current_week) %>%
      dplyr::mutate(
        season = current_season,
        week = current_week
      ) %>%
      dplyr::group_by(season, week, franchise_id, player_id) %>%
      dplyr::arrange(roster_status) %>%
      # entferne ROSTER aus gruppen mit mehr als einem eintrag
      dplyr::filter(
        dplyr::n() == 1 | roster_status != "ROSTER"
      ) %>%
      # behalte nur IR wenn IR und TS vorhanden sind
      dplyr::filter(
        dplyr::row_number() == 1
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, franchise_id, player_id, player_name, pos, team, roster_status, franchise_name_old = franchise_name)

    if (current_week == 1) {
      cli::cli_alert_info("Write Data")
      readr::write_csv(roster, paste0("rfl_roster_", current_season, ".csv"))
    } else {
      old_data <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/roster_data/rfl_roster_", current_season, ".csv"), col_types = "dicic") %>%
        dplyr::filter(week < current_week)

      # lokal
      #old_data <- readr::read_csv(paste0("rfl_roster_", current_season, ".csv"), col_types = "dicic") %>%
      #  dplyr::filter(week < current_week)

      cli::cli_alert_info("Write Data")
      readr::write_csv(rbind(old_data, roster), paste0("rfl_roster_", current_season, ".csv"))
    }
  #}
}

#check <- old_data %>%
#  dplyr::group_by(season, week, franchise_id, player_id) %>%
#  dplyr::summarise(count = n(), .groups = "drop") %>%
#  dplyr::filter(count > 1)

#for (season in 2016:2026) {
cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_roster_", current_season, ".csv"), "bohndesverband/rfl-data", "roster_data", overwrite = TRUE)
#}

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "roster_data", overwrite = TRUE)
