library(nflreadr)
library(tidyverse)
library(piggyback)

cli::cli_alert_info("Create Data")

current_season <- nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

if(current_week == 18) {
  # load playoff brackets ----
  brackets <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=playoffBrackets&L=63018&APIKEY=&JSON=1"))$playoffBrackets$playoffBracket %>%
    dplyr::tibble() %>%
    tidyr::unnest_wider(1)

  set_column <- function(string) {
    return(ifelse(grepl(string, key), value, NA))
  }

  matchup_list <- list()
  for (bracket_index in 1:nrow(brackets)) {
    bracket_id <- brackets$id[bracket_index]

    #bracket_id <- 5

    bracket <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=playoffBracket&L=63018&APIKEY=&BRACKET_ID=", bracket_id, "&JSON=1"))$playoffBracket$playoffRound

    # wenn mehr als 1 spiel vorhanden
    if (length(bracket) > 2) {
      matchups <- bracket %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(1) %>%
        tidyr::spread(week, playoffGame) %>%
        tidyr::unnest_wider(everything(), names_sep = "") %>%
        tidyr::unnest_wider(everything(), names_sep = "") %>%
        tidyr::unnest_wider(everything(), names_sep = "") %>%
        tidyr::gather(key, value, everything()) %>%
        dplyr::mutate(
          week = stringr::str_sub(key, 1, 2), # erstelle woche aus ersten zeichen der keys
          round = as.integer(week) - min(as.integer(week)),
          key = stringr::str_sub(key, 3, stringr::str_length(key)),
          game_id = dplyr::case_when(
            # check if key starts with a number
            grepl("^[0-9]", key) ~ as.integer(stringr::str_sub(key, 1, 1)) + round * 2
          ),
          away_franchise_id = ifelse(grepl("awayfranchise_id", key),value, NA),
          away_points = ifelse(grepl("awaypoints", key),value, NA),
          home_franchise_id = ifelse(grepl("homefranchise_id", key),value, NA),
          home_points = ifelse(grepl("homepoints", key),value, NA),
        ) %>%
        dplyr::group_by(week, game_id) %>%
        dplyr::summarise_all(max, na.rm = TRUE) %>%
        dplyr::select(-game_id, -key, -value, -round) %>%
        dplyr::mutate(
          season = current_season,
          bracket = brackets$name[bracket_index]
        )

      matchup_list[[bracket_id]] <- matchups
    } else {
      matchups <- bracket %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(1, names_sep = "") %>%
        tidyr::gather(key, playoffGame, c(dplyr::ends_with("home"), dplyr::ends_with("away"))) %>%
        tidyr::unnest_wider(playoffGame, names_sep = "") %>%
        tidyr::gather(key2, value, dplyr::starts_with("playoff")) %>%
        dplyr::mutate(
          key = paste(stringr::str_sub(key, 2, stringr::str_length(key)), key2, sep = "_")
        ) %>%
        dplyr::select(-key2) %>%
        dplyr::group_by(key) %>%
        dplyr::summarise_all(max, na.rm = TRUE) %>%
        tidyr::spread(key, value) %>%
        dplyr::rename(
          week = ".1",
          away_franchise_id = away_playoffGamefranchise_id,
          away_points = away_playoffGamepoints,
          home_franchise_id = home_playoffGamefranchise_id,
          home_points = home_playoffGamepoints
        ) %>%
        dplyr::select(week, away_franchise_id, away_points, home_franchise_id, home_points) %>%
        dplyr::mutate(
          season = current_season,
          bracket = brackets$name[bracket_index]
        )

      matchup_list[[bracket_id]] <- matchups
    }
  }

  postseason <- do.call(rbind, matchup_list) %>%
    dplyr::mutate(
      week = as.integer(week),
      bowl = dplyr::case_when(
        grepl("Pro", bracket) ~ "PB",
        grepl("Toilet", bracket) ~ "TB",
        TRUE ~ "SB"
      )
    ) %>%
    dplyr::group_by(season, bowl) %>%
    dplyr::arrange(week) %>%
    dplyr::mutate(
      game_id = paste(bowl, season, stringr::str_pad(dplyr::row_number(), 2, pad = "0"), sep = "-")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      win = ifelse(home_points > away_points, "home", "away")
    ) %>%
    tidyr::gather("team", "franchise_id", away_franchise_id, home_franchise_id) %>%
    tidyr::gather("points_type", "franchise_points", away_points, home_points) %>%
    dplyr::filter(
      grepl("home", team) & grepl("home", points_type) |
        grepl("away", team) & grepl("away", points_type)
    ) %>%
    dplyr::mutate(
      match_result = dplyr::case_when(
        win == "home" & grepl("home", team) ~ "W",
        win == "away" & grepl("away", team) ~ "W",
        TRUE ~ "L"
      ),
    ) %>%
    dplyr::select(-win, -team, -points_type) %>%
    dplyr::mutate(
      po_finish = dplyr::case_when(
        bowl == "SB" & grepl("Super", bracket) & week == max(week) & match_result == "W" ~ 1,
        bowl == "SB" & grepl("Super", bracket) & week == max(week) & match_result == "L" ~ 2,
        bowl == "SB" & grepl("dre", bracket) & week == max(week) & match_result == "W" ~ 3,
        bowl == "SB" & grepl("dre", bracket) & week == max(week) & match_result == "L" ~ 4,
        bowl == "SB" & week == max(week) - 1 & match_result == "L" ~ 5,
        bowl == "SB" & week == max(week) - 2 & match_result == "L" ~ 6,
        bowl == "SB" & week == max(week) - 3 & match_result == "L" ~ 7,

        bowl != "SB" & week == max(week) & match_result == "W" ~ 1,
        bowl != "SB" & week == max(week) & match_result == "L" ~ 2,
        bowl != "SB" & week == max(week) - 1 & match_result == "L" ~ 3,
        bowl != "SB" & week == max(week) - 2 & match_result == "L" ~ 4,
        bowl != "SB" & week == max(week) - 3 & match_result == "L" ~ 5,
      ),
      title = ifelse(bowl == "SB" & po_finish == 1, 1, 0)
    ) %>%
    dplyr::select(season, bowl, bracket, week, game_id, franchise_id, franchise_points, match_result, po_finish, title) %>%
    dplyr::arrange(week, bowl)

  cli::cli_alert_info("Write Data")
  readr::write_csv(postseason, paste0("rfl_postseason_", current_season, ".csv"))

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_postseason_", current_season, ".csv"), "bohndesverband/rfl-data", "postseason_data", overwrite = TRUE)

  timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

  write(timestamp, "timestamp.json")
  piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "postseason_data", overwrite = TRUE)
} else if (current_week < 18) {
  cli::cli_alert_info("Season is not over yet")
} else {
  cli::cli_alert_info("Already processed postseason data")
}
