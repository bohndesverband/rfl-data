# https://www.thefantasyfootballers.com/articles/elo-you-how-this-rating-system-affects-fantasy-football/

library(tidyverse)
library(nflreadr)
library(piggyback)

cli::cli_alert_info("Create Data")

current_season <- nflreadr::most_recent_season()
current_week <- nflreadr::get_current_week() - 1

k <- 36 # varianz pro spiel

player_info <- nflreadr::load_players() %>%
  dplyr::select(gsis_id, display_name, position_group) %>%
  dplyr::rename(position = position_group) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(gsis_id, mfl_id),
    by = "gsis_id"
  ) %>%
  dplyr::filter(!is.na(mfl_id)) %>%
  dplyr::mutate(position = ifelse(position == "SPEC", "PK", position))

nfl_schedule <- nflreadr::load_schedules(current_season) %>%
  dplyr::select(game_id, season, week, home_team, away_team) %>%
  dplyr::mutate(
    away_opponent = home_team,
    home_opponent = away_team,
  ) %>%
  tidyr::gather(key, team, dplyr::ends_with("_opponent")) %>%
  dplyr::mutate(
    opponent = ifelse(home_team == team, away_team, home_team),
    team = nflreadr::clean_team_abbrs(team),
    opponent = nflreadr::clean_team_abbrs(opponent)
  ) %>%
  dplyr::select(-game_id, -key, -home_team, -away_team)

#for (current_week in 1:13) {
  if(current_week == 1) {
    # in WK 1 die letzte ELO der vorsaison lesen
    read_season <- current_season - 1
  } else {
    read_season <- current_season
  }

  # wenn WK 2 2016
  #elo_past <- readr::read_csv("data/elo/rfl-player-elo-init.csv", col_types = c("mfl_id" = "character"))

  elo_past <- purrr::map_df(2016:read_season, function(x) {
    readr::read_csv(
      #glue::glue("rfl_player-elo_{x}.csv"), # local
      glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/elo_data/rfl_player-elo_{x}.csv"), # remote
      col_types = "iicccccnniiiii"
    )
  }) %>%
    dplyr::filter(season < current_season | (season == current_season & week < current_week))

  last_player_elo <- elo_past %>%
      dplyr::group_by(mfl_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::summarise(player_elo_post = dplyr::last(player_elo_post), .groups = "drop")

  last_opponent_elo <- elo_past %>%
      dplyr::group_by(opponent_id) %>%
      dplyr::arrange(season, week) %>%
      dplyr::summarise(opponent_elo_post = dplyr::last(opponent_elo_post), .groups = "drop")

  scores <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=playerScores&L=63018&W=", current_week, "&YEAR=", current_season, "&JSON=1")) %>%
      purrr::pluck("playerScores", "playerScore") %>%
      tibble::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::select(-isAvailable) %>%
      dplyr::rename(mfl_id = id) %>%
      dplyr::mutate(score = as.numeric(score))

  start <- scores %>%
      dplyr::left_join(
          player_info %>%
              dplyr::select(mfl_id, gsis_id, position),
          by = "mfl_id"
      ) %>%
      dplyr::filter(!is.na(position)) %>%

      # create ranks
      dplyr::group_by(position) %>%
      dplyr::arrange(dplyr::desc(score)) %>%
      dplyr::mutate(
          pos_rank = row_number(),
          side = dplyr::case_when(
              position %in% c("RB", "WR", "TE") ~ "OFF",
              position %in% c("DL", "LB", "DB") ~ "DEF",
          ),
          start = dplyr::case_when(
              position %in% c("RB", "WR") & pos_rank <= 24 ~ 1,
              position %in% c("QB", "TE", "PK") & pos_rank <= 12 ~ 1,
              side == "DEF" & pos_rank <= 24 ~ 1,
              TRUE ~ 0
          )
      )

  flex <- start %>%
      dplyr::filter(side %in% c("OFF", "DEF") & start == 0) %>%
      dplyr::group_by(side) %>%
      dplyr::arrange(dplyr::desc(score)) %>%
      dplyr::mutate(
          flex_rank = row_number(),
          start = dplyr::case_when(
              side == "OFF" & flex_rank <= 24 ~ 1,
              side == "DEF" & flex_rank <= 36 ~ 1,
              TRUE ~ 0
          )
      )

  elo <- rbind(subset(start, position == "QB" | position == "PK" | start == 1), flex) %>%
      dplyr::select(-flex_rank, -side) %>%

      # calculate position average
      #dplyr::group_by(position) %>%
      #dplyr::mutate(
      #    opponent_score = mean(subset(score, start == 1)),
      #) %>%
      #dplyr::ungroup() %>%

    # get replacement points
      dplyr::group_by(week, position) %>%
      dplyr::mutate(
        opponent_score = subset(score, start == 0)[1]
      ) %>%
      dplyr::ungroup() %>%

      # add opponent data
      dplyr::left_join(
          nflreadr::load_rosters_weekly(current_season) %>%
              dplyr::filter(week == current_week) %>%
              dplyr::select(gsis_id, team) %>%
              dplyr::mutate(team = nflreadr::clean_team_abbrs(team)),
          by = "gsis_id",
          multiple = "first"
      ) %>%
      dplyr::filter(!is.na(team)) %>%
      dplyr::left_join(
          nfl_schedule %>%
              dplyr::filter(season == current_season, week == current_week) %>%
              dplyr::select(team, opponent),
          by = "team"
      ) %>%
      dplyr::mutate(
          opponent_id = paste(opponent, position, "OPP", sep = "_"),
      ) %>%

      # previous elo for player
      dplyr::left_join(
          last_player_elo %>%
              dplyr::rename(player_elo_pre = player_elo_post) %>%
              dplyr::mutate(mfl_id = as.character(mfl_id)),
          by = "mfl_id",
      ) %>%

      # previous elo for opponent
      dplyr::left_join(
          last_opponent_elo %>%
              dplyr::rename(opponent_elo_pre = opponent_elo_post),
          by = "opponent_id",
          relationship = "many-to-many"
      ) %>%

      dplyr::mutate(
          player_elo_pre = ifelse(is.na(player_elo_pre), 1500, player_elo_pre), # if no elo is found, set to base 1500
          opponent_elo_pre = ifelse(is.na(opponent_elo_pre), 1500, opponent_elo_pre), # if no elo is found, set to base 1500

          # player_elo_pre = ifelse(week == 1, round((player_elo_pre * (2/3)) + (1500 * (1/3))), player_elo_pre), # ursprünnglich wurde die elo in WK richtung 1500 korrigiert. bei spielen ist das aber eig nicht nötig
          opponent_elo_pre = ifelse(week == 1, 1500, opponent_elo_pre), # if week 1 of season, reset to default value

          # calculate elo
          score_diff = round(score - opponent_score, 2),
          result = dplyr::case_when(
            score_diff > 0 ~ 1, # 1 für win
            score_diff < 0 ~ 0, # 0 für loss
            TRUE ~ 0.5 # 0.5 für tie
          ),
          forecast = 1 / (10^(-(player_elo_pre - opponent_elo_pre) / 400 ) + 1),
          k = k,
          forecast_delta = result - forecast,
          winning_elo = ifelse(result == 1, player_elo_pre, opponent_elo_pre),
          losing_elo = ifelse(result != 1, player_elo_pre, opponent_elo_pre),
          mov_multiplier = log(abs(score_diff) + 1) * (2.2 / (((winning_elo - losing_elo) * 0.001) + 2.2)),
          elo_shift = round(k * forecast_delta + mov_multiplier),
          player_elo_post = player_elo_pre + elo_shift,
          opponent_elo_post = opponent_elo_pre - elo_shift
      ) %>%

      dplyr::group_by(opponent_id) %>%
      dplyr::mutate(
          season = current_season,
          opponent_elo_post = round(mean(opponent_elo_post))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(season, week, mfl_id, gsis_id, position, team, opponent_id, score, score_diff, player_elo_pre, opponent_elo_pre, elo_shift, player_elo_post, opponent_elo_post)

  if (current_week == 1) {
      cli::cli_alert_info("Write Data")
      readr::write_csv(elo, paste0("rfl_player-elo_", current_season, ".csv"))
      #readr::write_csv(elo, paste0("data/elo/rfl-player-elo-", current_season, ".csv"))
  } else {
      cli::cli_alert_info("Write Data")
      elo_old <- elo_past %>% filter(season == current_season & week < current_week)
      readr::write_csv(rbind(elo_old, elo), paste0("rfl_player-elo_", current_season, ".csv"))
      #readr::write_csv(rbind(elo_old, elo), paste0("data/elo/rfl-player-elo-", current_season, ".csv"))
  }

  #print(paste("Wrote week", current_week))

  #rm(elo, elo_past, last_player_elo, last_opponent_elo, scores, start, flex, read_season)
#}

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_player-elo_", current_season, ".csv"), "bohndesverband/rfl-data", "elo_data", overwrite = TRUE)

#timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
#  jsonlite::toJSON(auto_unbox = TRUE)

#write(timestamp, "timestamp.json")
#piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "elo_data", overwrite = TRUE)
