library(tidyverse)
library(nflreadr)

var_season <- 2016
var_week <- 1
k <- 36 # varianz pro spiel

player_info <- nflreadr::load_rosters(var_season) %>%
    dplyr::select(gsis_id, season, position) %>%
    dplyr::left_join(
        nflreadr::load_ff_playerids() %>%
            dplyr::select(mfl_id, gsis_id),
        by = "gsis_id",
        relationship = "many-to-many"
    ) %>%
    dplyr::filter(!is.na(mfl_id))

nfl_schedule <- nflreadr::load_schedules(var_season) %>%
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

scores <- jsonlite::read_json("https://www48.myfantasyleague.com/2016/export?TYPE=playerScores&L=63018&W=1&YEAR=2016&JSON=1") %>%
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
            position %in% c("QB", "TE", "K") & pos_rank <= 12 ~ 1,
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

elo <- rbind(
        start %>%
            dplyr::filter(start == 1 | position == "QB" | position == "K"),
        flex
    ) %>%
    dplyr::select(-flex_rank, -side) %>%

    # get replacement points
    dplyr::group_by(week, position) %>%
    dplyr::mutate(
      opponent_score = subset(score, start == 0)[1]
    ) %>%

    # calculate position average
    #dplyr::group_by(position) %>%
    #dplyr::mutate(
    #    opponent_score = mean(subset(score, start == 1)),
    #) %>%
    #dplyr::ungroup()

    # calculate elo
    dplyr::mutate(
        score_diff = round(score - opponent_score, 2),
        result = dplyr::case_when(
            score_diff > 0 ~ 1, # 1 für win
            score_diff < 0 ~ 0, # 0 für loss
            TRUE ~ 0.5 # 0.5 für tie
        ),
        player_elo_pre = 1500,
        opponent_elo_pre = 1500,
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

    # add opponent data
    dplyr::left_join(
        nflreadr::load_rosters_weekly(var_season) %>%
            dplyr::filter(week == var_week) %>%
            dplyr::select(gsis_id, team) %>%
            dplyr::mutate(team = nflreadr::clean_team_abbrs(team)),
        by = "gsis_id"
    ) %>%
    dplyr::filter(!is.na(team)) %>%
    dplyr::left_join(
        nfl_schedule %>%
            dplyr::filter(season == var_season, week == var_week) %>%
            dplyr::select(team, opponent),
        by = "team"
    ) %>%
    dplyr::mutate(
        opponent_id = paste(opponent, position, "OPP", sep = "_"),
    ) %>%
    dplyr::group_by(opponent_id) %>%
    dplyr::mutate(
        season = var_season,
        opponent_elo_post = round(mean(opponent_elo_post))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(season, week, mfl_id, gsis_id, position, team, opponent_id, score, score_diff, player_elo_pre, opponent_elo_pre, elo_shift, player_elo_post, opponent_elo_post)

write.csv(elo, "data/elo/rfl-player-elo-init.csv", row.names = F)
