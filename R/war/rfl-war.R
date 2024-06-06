library(purrr)
library(readr)
library(dplyr)
library(piggback)
library(nflreadr)

cli::cli_alert_info("Create Data")

current_season <- nflreadr::most_recent_season()

# load data ----
starter <- purrr::map_df(current_season, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/starter_data/rfl_starter_{x}.csv"),
    col_types = "ddccccccdd"
  )
}) |>
  dplyr::filter(!is.na(player_score)) |> # remove all players that did not play
  dplyr::distinct() |> # remove doublicates from doubleheader matchups
  # combine defense positions, since there are no single positions (except LB)
  dplyr::mutate(
    pos = dplyr::case_when(
      pos %in% c("DT", "DE") ~ "DL",
      pos %in% c("CB", "S") ~ "DB",
      TRUE ~ pos
    )
  )

total_games <- starter |>
  dplyr::select(season, week) |>
  dplyr::distinct() |>
  dplyr::summarise(games = dplyr::n(), .groups = "drop") |>
  dplyr::pull(games)

# helper functions ----
# create weekly fantasy ranks
create_fantasy_ranks <- function(df, with_starting_pct = TRUE) {
  df <- df |>
    dplyr::group_by(season, week, pos)

  if (with_starting_pct == TRUE) {
    df <- df |>
      # we sort by start % first to value the more often started players more
      dplyr::arrange(dplyr::desc(start_pct), dplyr::desc(player_score))
  } else {
    df <- df |>
      dplyr::arrange(dplyr::desc(player_score))
  }

  df <- df |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::ungroup() |>

    dplyr::mutate(
      # create eligable status (is the player a starter in the theoretical weekly lineup?)
      eligable = dplyr::case_when(
        pos %in% c("QB", "TE", "PK") & rank <= 12 ~ 1,
        pos %in% c("RB", "WR", "DL", "LB", "DB") & rank <= 24 ~ 1,
        TRUE ~ 0
      )
    )
}

create_flex <- function(df, with_starting_pct = TRUE) {
  flex <- df |>
    dplyr::filter(eligable == 0) |>
    dplyr::mutate(
      flex = dplyr::case_when(
        pos %in% c("RB", "WR", "TE") ~ "FLEX", # combine all offense positions
        pos %in% c("DL", "LB", "DB") ~ "IDP", # combine all defense positions
        TRUE ~ "no"
      )
    ) |>
    dplyr::group_by(week, flex)

  if (with_starting_pct == TRUE) {
    flex <- flex |>
      # we sort by start % first to value the more often started players more
      dplyr::arrange(dplyr::desc(start_pct), dplyr::desc(player_score))
  } else {
    flex <- flex |>
      dplyr::arrange(dplyr::desc(player_score))
  }

  flex <- flex |>
    dplyr::mutate(rank_flex = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      eligable = dplyr::case_when(
        flex == "FLEX" & rank_flex <= 24 ~ 1, # first 24 flex spots are eligable
        flex == "IDP" & rank_flex <= 36 ~ 1, # first 24 IDP spots are eligable
        TRUE ~ eligable
      )
    )

  df <- df |>
    dplyr::filter(eligable == 1) |> # only keep starter
    dplyr::bind_rows(flex) # add all other players
}

# weekly starter data ----
starter_by_week <- starter |>
  dplyr::filter(starter_status == "starter") |> # filter for all starters
  # calculate starting % in the league
  dplyr::group_by(season, week, player_id) |>
  dplyr::mutate(start_pct = dplyr::n() / 3) |>
  dplyr::ungroup() |>

  dplyr::select(season, week, player_id, pos, start_pct, player_score) |>
  dplyr::distinct() |>
  create_fantasy_ranks()

eligable_players <- create_flex(starter_by_week)

# average team scores ----
avg_player_scores <- eligable_players |>
  dplyr::filter(eligable == 1) |>
  dplyr::mutate(pos = ifelse(is.na(flex), pos, flex)) |> # rewrite position
  dplyr::group_by(pos) |>
  dplyr::summarise(
    points_average_player = round(mean(player_score), 2), # create average position score
    sd = stats::sd(player_score), # create standard deviation
    .groups = "drop"
  ) |>
  dplyr::mutate(
    multiplier = dplyr::case_when(
      pos %in% c("QB", "TE", "PK") ~ 1,
      pos %in% c("RB", "WR", "FLEX", "DL", "LB", "DB") ~ 2,
      pos == "IDP" ~ 3
    ),
    sd = dplyr::case_when(
      pos %in% c("QB", "TE", "PK") ~ sd^2,
      pos %in% c("RB", "WR", "FLEX", "DL", "LB", "DB") ~ sd^2 * 2,
      pos == "IDP" ~ sd^2 * 3
    )
  )

avg_team_scores <- avg_player_scores |>
  dplyr::mutate(points_average_player = points_average_player * multiplier) |>
  dplyr::summarise(
    points = sum(points_average_player),
    sd = sqrt(sum(sd)),
    .groups = "drop"
  )

# replacement data ----
replacement_player_points <- starter |>
  dplyr::select(season, week, player_id, pos, player_score) |>
  dplyr::distinct() |>

  # get eligable status
  dplyr::left_join(
    eligable_players |>
      dplyr::select(season, week, player_id, eligable),
    by = c("season", "week", "player_id"),
    multiple = "first"
  ) |>
  dplyr::filter(eligable == 0 | is.na(eligable)) |> # remove all eligable players
  create_fantasy_ranks(FALSE) |>
  dplyr::filter(eligable == 1) |>
  dplyr::group_by(pos) |>
  dplyr::summarise(
    points_replacement_player = mean(player_score),
    .groups = "drop"
  ) |>
  dplyr::left_join(avg_player_scores |> dplyr::select(pos, points_average_player), by = "pos") |>
  dplyr::mutate(
    replacement_team_points = avg_team_scores$points - points_average_player + points_replacement_player,
    win_probability_replacement = pnorm(replacement_team_points, avg_team_scores$points, sd = avg_team_scores$sd),
    replacement_wins = total_games * win_probability_replacement
  ) |>
  dplyr::select(-points_average_player)

# war calculation ----
war <- starter |>
  dplyr::select(season, week, player_id, player_name, pos, player_score) |>
  dplyr::distinct() |>

  # calc season totals
  dplyr::group_by(player_id, player_name, season) |>
  dplyr::summarise(
    points = sum(player_score),
    games = dplyr::n(),
    .groups = "drop"
  ) |>

  # add info for missed games
  dplyr::group_by(player_id, player_name) |>
  dplyr::mutate(
    points = sum(points),
    games_played = sum(games),
    games_missed = total_games - games
  ) |>

  # add positions back
  dplyr::left_join(starter |> dplyr::select(player_id, pos) |> dplyr::distinct(), by = "player_id", multiple = "first") |>

  # add avg player data
  dplyr::left_join(avg_player_scores |> dplyr::select(pos, points_average_player), by = "pos") |>

  # war calculation
  dplyr::mutate(
    points_per_game = points / games_played,
    points_average_player = points_average_player,
    avg_team_points = avg_team_scores$points,
    war_team_points = avg_team_points - points_average_player + points_per_game,
    win_probability = pnorm(war_team_points, avg_team_points, sd = avg_team_scores$sd), # pnorm "abritary normal distribution"; calculates win probability from avg and sd
  ) |>
  dplyr::filter(!is.na(win_probability)) |>

  # add replacement data
  dplyr::left_join(replacement_player_points, by = "pos") |>
  dplyr::group_by(player_id, player_name) |>

  # further calculations
  dplyr::summarise(
    points= sum(points),
    across(c(games_missed, win_probability, avg_team_points, win_probability_replacement, replacement_wins), mean),
    win_probability = ifelse(
      games_missed == 0, win_probability, (win_probability + (win_probability_replacement * games_missed)) / (games_missed + 1)
    ),
    .groups = "drop"
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    expected_wins = total_games * win_probability,
    war = round(expected_wins - replacement_wins, 2) * 2,
    season = current_season
  ) |>
  dplyr::left_join(starter |> dplyr::select(player_id, pos) |> dplyr::distinct(), by = "player_id") |>
  dplyr::select(season, player_id, player_name, pos, points, war) |>
  dplyr::arrange(dplyr::desc(war))

cli::cli_alert_info("Write Data")
readr::write_csv(war, paste0("rfl_war_", var_draft_season, ".csv"))

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_war_", var_draft_season, ".csv"), "bohndesverband/rfl-data", "war_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) |>
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "war_data", overwrite = TRUE)
