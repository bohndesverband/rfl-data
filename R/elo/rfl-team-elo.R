# https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/

library(tidyverse)
library(nflreadr)

cli::cli_alert_info("Create Data")

current_season <- nflreadr::most_recent_season()
current_week <- nflreadr::get_current_week() - 1

current_season <- 2023
current_week <- 2

#for (current_week in 1:13) {
  if(current_week == 1) {
    # in WK 1 die letzte ELO der vorsaison lesen
    read_season <- current_season - 1
  } else {
    read_season <- current_season
  }

  # wenn WK 2 2016
  #elo_past <- readr::read_csv("data/elo/rfl-elo-init.csv", col_types = "ciiccnnniiii") %>%
  #  dplyr::select(-game_id)

  #elo_past <- readr::read_csv(paste0("rfl_team-elo_", read_season, ".csv"), col_types = "iiccddddiiii")

  elo_past <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_team-elo_", read_season, ".csv"), col_types = "iiccddddiii")

  if (current_week == 1) {
    elo_past <- elo_past %>%
      dplyr::filter(week == max(week))
  } else {
    elo_past <- elo_past %>%
      dplyr::filter(week < current_week)
  }

  elo_last_week <- elo_past %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::arrange(week) %>%
    dplyr::summarise(franchise_elo_postgame = dplyr::last(franchise_elo_postgame), .groups = "drop") # der weg ist so nötig um für bye weeks in den PO daten zu kriegen

  scores <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=weeklyResults&L=63018&APIKEY=&W=", current_week, "&JSON=1")) %>%
    #purrr::pluck("weeklyResults", "franchise") %>% # pre 2020
    purrr::pluck("weeklyResults", "matchup") %>% # since 2020
    tibble::tibble() %>%
    tidyr::unnest_wider(1) %>%
    #dplyr::rename(franchiseid = id, franchisescore = score) %>%  # pre 2020
    tidyr::unnest(franchise) %>%  # since 2020
    tidyr::unnest_wider(franchise, names_sep = "") %>% # since 2020
    dplyr::select(franchiseid, franchisescore) %>%
    dplyr::mutate(franchisescore = round(as.numeric(franchisescore), 2)) %>%
    dplyr::distinct()

  schedule <- readr::read_csv("https://raw.githubusercontent.com/bohndesverband/rfl-data/main/data/rfl-schedules.csv", col_types = "iicc") %>%
    dplyr::filter(season == current_season & week == current_week) %>%

    # punkte
    dplyr::left_join(scores %>% dplyr::rename(franchise_score = franchisescore), by = c("franchise_id" = "franchiseid")) %>%
    dplyr::left_join(scores %>% dplyr::rename(opponent_score = franchisescore), by = c("opponent_id" = "franchiseid"))

  elo <- schedule %>%
    dplyr::filter(season == current_season, week == current_week) %>%

    # previous elo
    dplyr::left_join(
      elo_last_week %>%
        dplyr::rename(franchise_elo_pregame = franchise_elo_postgame) %>%
        dplyr::mutate(franchise_id = as.character(franchise_id)),
      by = "franchise_id",
    ) %>%
    dplyr::left_join(
      elo_last_week %>%
        dplyr::rename(opponent_elo_pregame = franchise_elo_postgame) %>%
        dplyr::mutate(franchise_id = as.character(franchise_id)),
      by = c("opponent_id" = "franchise_id")
    ) %>%

    dplyr::mutate(
      # if week 1 of season, recalc to default value
      franchise_elo_pregame = ifelse(week == 1, round((franchise_elo_pregame * (2/3)) + (1500 * (1/3))), franchise_elo_pregame),
      opponent_elo_pregame = ifelse(week == 1, round((opponent_elo_pregame * (2/3)) + (1500 * (1/3))), opponent_elo_pregame),

      score_diff = round(franchise_score - opponent_score, 2),
      result = case_when(
        score_diff > 0 ~ 1, # 1 für win
        score_diff < 0 ~ 0, # 0 für loss
        T ~ 0.5 # 0.5 für tie
      ),
      forecast = 1 / (10^(-(franchise_elo_pregame - opponent_elo_pregame) / 400 ) + 1),
      k = 36,
      forecast_delta = result - forecast,
      winning_elo = ifelse(result == 1, franchise_elo_pregame, opponent_elo_pregame),
      losing_elo = ifelse(result != 1, franchise_elo_pregame, opponent_elo_pregame),
      mov_multiplier = log(abs(score_diff) + 1) * (2.2 / (((winning_elo - losing_elo) * 0.001) + 2.2)),
      elo_shift = round(k * forecast_delta + mov_multiplier),
    ) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::mutate(
      franchise_elo_postgame = first(franchise_elo_pregame) + sum(elo_shift),
      franchise_id = as.character(franchise_id)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(1:7, score_diff, franchise_elo_pregame, opponent_elo_pregame, elo_shift, franchise_elo_postgame)

  if (current_week == 1) {
    cli::cli_alert_info("Write Data")
    readr::write_csv(elo, paste0("rfl_team-elo_", current_season, ".csv"))
    #readr::write_csv(elo, paste0("data/elo/rfl-elo-", current_season, ".csv"))
  } else {
    cli::cli_alert_info("Write Data")
    elo_old <- elo_past %>% filter(week < current_week)
    readr::write_csv(rbind(elo_old, elo), paste0("rfl_team-elo_", current_season, ".csv"))
    #readr::write_csv(rbind(elo_old, elo), paste0("data/elo/rfl-elo-", current_season, ".csv"))
  }

  #print(paste("Wrote week", current_week))
#}

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_team-elo_", current_season, ".csv"), "bohndesverband/rfl-data", "elo_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "elo_data", overwrite = TRUE)
