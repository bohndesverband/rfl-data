library(tidyverse)
library(nflreadr)

current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week() - 1

if (current_week <= 13) {
  scores <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=playerScores&L=63018&W=", current_week, "&JSON=1")) %>%
    purrr::pluck("playerScores", "playerScore") %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1) %>%
    dplyr::left_join(
      jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=players&L=63018&JSON=1")) %>%
        purrr::pluck("players", "player") %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(1),
      by = "id"
    ) %>%
    dplyr::mutate(season = current_season) %>%
    dplyr::select(season, week, id, name, position, team, score) %>%
    dplyr::rename(
      player_id = id,
      player_name = name,
      pos = position,
      points = score
    )

  if (current_week == 1) {
    cli::cli_alert_info("Write Data")
    readr::write_csv(scores, paste0("rfl_playerscores_", current_season, ".csv"))
  } else {
    loadData <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/playerscores_data/rfl_playerscores_", current_season, ".csv"), col_types = "ddccccd") %>%
      dplyr::filter(as.numeric(week) < current_week)

    cli::cli_alert_info("Write Data")
    readr::write_csv(rbind(loadData, scores), paste0("rfl_playerscores_", current_season, ".csv"))
  }

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_playerscores_", current_season, ".csv"), "bohndesverband/rfl-data", "playerscores_data", overwrite = TRUE)

  timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  write(timestamp, "timestamp.json")
  piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "playerscores_data", overwrite = TRUE)
} else {
  cli::cli_alert_info("Regular Season is over.")
}
