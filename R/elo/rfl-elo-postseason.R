test5 <- read_rds("data/schedule/rfl-results-postseason-2020.rds") %>%
  dplyr::mutate(
    season = 2020,
    game_id = paste0(season, week, away_id, home_id),
    away_opponent = home_id,
    home_opponent = away_id,
    away_points = as.numeric(away_points),
    home_points = as.numeric(home_points)
  ) %>%
  tidyr::gather(key, franchise_id, c(away_id, home_id)) %>%
  dplyr::mutate(
    opponent_id = ifelse(key == "away_id", away_opponent, home_opponent),
    franchise_score = ifelse(key == "away_id", away_points, home_points),
    opponent_score = ifelse(key == "away_id", home_points, away_points),
    score_differential = franchise_score - opponent_score
  ) %>%
  dplyr::rename(bowl = postseason) %>%
  dplyr::select(game_id, season, week, franchise_id, opponent_id, franchise_score, opponent_score, score_differential, bowl)

  dplyr::filter(franchise_id == "0006") %>%
  dplyr::mutate(week = as.integer(week)) %>%
  dplyr::group_by(franchise_id) %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(

  )
