library(tidyverse)
library(nflreadr)

current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week() - 1

if (current_week <= 17) {
  scores_list <- list()

  for (wk in 1:current_week) {
    scores_tmp <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=playerScores&L=63018&W=", wk, "&JSON=1")) %>%
      purrr::pluck("playerScores", "playerScore") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1)
    scores_list[[wk]] <- scores_tmp
  }

  scores <- dplyr::bind_rows(scores_list) %>%
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

  cli::cli_alert_info("Write Data")
  readr::write_csv(scores, paste0("rfl_playerscores_", current_season, ".csv"))

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_playerscores_", current_season, ".csv"), "bohndesverband/rfl-data", "playerscores_data", overwrite = TRUE)

  timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  write(timestamp, "timestamp.json")
  piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "playerscores_data", overwrite = TRUE)
}
