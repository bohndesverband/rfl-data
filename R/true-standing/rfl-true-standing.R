library(tidyverse)
library(nflreadr)

cli::cli_alert_info("Create Data")

current_season = nflreadr::get_current_season()
current_week = nflreadr::get_current_week() - 1

if (current_week <= 13) {
  schedule <- readr::read_csv("https://raw.githubusercontent.com/bohndesverband/rfl-data/main/data/rfl-schedules.csv", col_types = "iccc") %>%
    dplyr::filter(season == current_season) %>%
    dplyr::select(-season) %>%
    dplyr::mutate(
      game_id = paste0(week, franchise_id, opponent_id)
    )

  points <- purrr::map_df(seq(1, current_week), function(x) {
    jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=weeklyResults&L=63018&W=", x, "&JSON=1"))$weeklyResults$matchup %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::mutate(
        week = as.character(stringr::str_pad(x, 2, pad = "0")),
      ) %>%
      tidyr::unnest(franchise) %>%
      tidyr::unnest_wider(franchise) %>%
      dplyr::select(week, id, opt_pts, score) %>%
      dplyr::distinct() %>%
      dplyr::rename(
        franchise_id = id,
        pf = score,
        pp = opt_pts
      )
  }) %>%
    dplyr::mutate(
      pf = as.numeric(ifelse(is.na(pf), 0, pf)),
      pp = as.numeric(pp),
      coach = round(-1 * (pp - pf), 2)
    )

  results <- points %>%
    dplyr::left_join(
      schedule %>%
        tidyr::gather(key, franchise_id, c(franchise_id, opponent_id)) %>%
        dplyr::select(week, franchise_id, game_id),
      by = c("franchise_id", "week"),
      multiple = "all"
    ) %>%
    dplyr::left_join(
      schedule %>%
        dplyr::rename(away_id = franchise_id, home_id= opponent_id) %>%
        dplyr::select(-week),
      by = "game_id",
      multiple = "all"
    ) %>%
    dplyr::mutate(opponent_id = ifelse(franchise_id == away_id, home_id, away_id)) %>%
    dplyr::left_join(
      points %>%
        dplyr::select(week, franchise_id, pf_opponent = pf),
      by = c("week", "opponent_id" = "franchise_id"),
      multiple = "all"
    ) %>%
    dplyr::mutate(
      pf_opponent = ifelse(is.na(pf_opponent), 0, pf_opponent),
      win = ifelse(pf - pf_opponent > 0, 1, 0)
    ) %>%
    dplyr::left_join(
      points %>%
        dplyr::select(week, franchise_id, pf) %>%
        dplyr::distinct() %>%
        dplyr::group_by (week) %>%
        dplyr::arrange(pf) %>%
        dplyr::mutate(all_play_wins = row_number() - 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-pf),
      by = c("week", "franchise_id"),
      multiple = "all"
    ) %>%
    dplyr::select(-home_id, -away_id, -game_id, -opponent_id, -pf_opponent)

  true_standing <- results %>%
    dplyr::group_by(week, franchise_id) %>%
    dplyr::summarise(
      win = sum(win, na.rm = TRUE),
      dplyr::across(c(pf, pp, coach, all_play_wins), mean),
      .groups = "drop"
    ) %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(
      week = as.numeric(week),
      pf_rank = dplyr::dense_rank(dplyr::desc(pf)),
      pp_rank = dplyr::dense_rank(dplyr::desc(pp)),
      record_rank = dplyr::dense_rank(dplyr::desc(win)),
      all_play_rank = dplyr::dense_rank(dplyr::desc(all_play_wins)),
      coach_rank = dplyr::dense_rank(dplyr::desc(coach)),
      true_standing = pf_rank + pp_rank + record_rank + all_play_rank + coach_rank
    ) %>%
    dplyr::arrange(true_standing, win, dplyr::desc(pf)) %>%
    dplyr::mutate(true_rank = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(week <= 13) %>%
    dplyr::arrange(week)

  cli::cli_alert_info("Write Data")
  readr::write_csv(true_standing, paste0("rfl_true-standing_", current_season, ".csv"))
  #readr::write_csv(true_standing, "data/true-standing/rfl-true-standing-2023.csv")

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_true-standing_", current_season, ".csv"), "bohndesverband/rfl-data", "standing_data", overwrite = TRUE)

  timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  write(timestamp, "timestamp.json")
  piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "standing_data", overwrite = TRUE)
} else {
  cli::cli_alert_info("Regular Season is over")
}
