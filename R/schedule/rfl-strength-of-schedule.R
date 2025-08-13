library(tidyverse)
library(ffscrapr)

next_season <- 2025
conn <- ffscrapr::mfl_connect(next_season, 63018)
conn_last_season <- ffscrapr::mfl_connect(next_season - 1, 63018)

divisions <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", next_season, "/export?TYPE=league&L=63018&JSON=1"))$league$divisions$division %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(".") %>%
  #dplyr::mutate(
  #  name = stringr::str_remove(name, " Division")
  #) %>%
  dplyr::rename(div_id = id, div_name = name) %>%
  dplyr::left_join(
    jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", next_season, "/export?TYPE=league&L=63018&JSON=1"))$league$conferences$conference %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(".") %>%
      dplyr::rename(conf_name = name),
    by = c("conference" = "id")
  )

franchises <- ffscrapr::ff_franchises(conn) %>%
  dplyr::select(franchise_id, division) %>%
  dplyr::left_join(divisions, by = c("division" = "div_id"))

# schedule ----
schedule <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", next_season, "/export?TYPE=schedule&L=63018&APIKEY=aRNp3s%2BWvuWqx02mPlrBYDoeErox&W=&F=&JSON=1"))$schedule$weeklySchedule %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(".") %>%
  dplyr::mutate(week = as.integer(week)) %>%
  dplyr::filter(week <= 13) %>%
  tidyr::unnest("matchup") %>%
  tidyr::unnest_wider("matchup") %>%
  tidyr::unnest_wider("franchise", names_sep = "_") %>%
  tidyr::unnest_wider("franchise_1", names_sep = "_") %>%
  dplyr::rename(franchise_id = franchise_1_id) %>%
  dplyr::select(-dplyr::starts_with("franchise_1")) %>%
  tidyr::unnest_wider("franchise_2", names_sep = "_") %>%
  dplyr::rename(opponent_id = franchise_2_id) %>%
  dplyr::select(-dplyr::starts_with("franchise_2")) %>%
  dplyr::select(week, franchise_id, opponent_id)

# standings ----
# ab 2024 gibt es hier all play win %
standings <- ffscrapr::ff_standings(conn)

# claculate all_play_winpct
weeklyResultsRaw <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", next_season - 1, "/export?TYPE=weeklyResults&L=63018&W=YTD&JSON=1"))

allPlayPct <- weeklyResultsRaw$allWeeklyResults$weeklyResults %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(".") %>%
  dplyr::select(-franchise) %>%
  tidyr::unnest("matchup") %>%
  tidyr::unnest_wider("matchup") %>%
  tidyr::unnest_wider("franchise", names_sep = "_") %>%
  tidyr::unnest_wider("franchise_1", names_sep = "_") %>%
  tidyr::unnest_wider("franchise_2", names_sep = "_") %>%
  dplyr::select(week, franchise_1_id, franchise_1_score, franchise_2_id, franchise_2_score) %>%
  dplyr::slice(rep(1:dplyr::n(), each = 2)) %>%
  dplyr::mutate(
    team_id = ifelse(dplyr::row_number() %% 2, franchise_1_id, franchise_2_id),
    team_score = ifelse(dplyr::row_number() %% 2, franchise_1_score, franchise_2_score)
  ) %>%
  dplyr::select(week, team_id, team_score) %>%
  dplyr::group_by(week) %>%
  dplyr::distinct() %>%
  dplyr::arrange(team_score) %>%
  dplyr::mutate(
    weekly_wins = dplyr::row_number() - 1
  ) %>%
  dplyr::group_by(team_id) %>%
  dplyr::summarise(
    allplay_winpct = sum(weekly_wins) / (13 * 35)
  )

# strength of schedule ----
sos <- schedule %>%
  # schedule duplizieren um für jedes team jedes match zu erhalten
  dplyr::slice(rep(1:dplyr::n(), each = 2)) %>%
  dplyr::mutate(
    team_id = ifelse(dplyr::row_number() %% 2, franchise_id, opponent_id),
    opponent_id = ifelse(team_id == opponent_id, franchise_id, opponent_id)
  ) %>%
  dplyr::select(-franchise_id) %>%

  dplyr::left_join(
    franchises,
    by = c("team_id" = "franchise_id")
  ) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::rename(opponent_conf = conference, opponent_div = division) %>%
      dplyr::select(-conf_name, -div_name),
    by = c("opponent_id" = "franchise_id")
  ) %>%
  dplyr::mutate(
    matchup = dplyr::case_when(
      division == opponent_div ~ "Division",
      conference == opponent_conf ~ "Conference",
      TRUE ~ "Other"
    )
  ) %>%
  dplyr::select(-division:-opponent_conf) %>%

  dplyr::left_join(
    allPlayPct,
    by = c("opponent_id" = "team_id")
  ) %>%
  dplyr::group_by(team_id, matchup) %>%
  dplyr::mutate(
    sos_matchup = mean(allplay_winpct)
  ) %>%
  dplyr::select(-week, team_id, matchup, sos_matchup) %>%
  dplyr::distinct() %>%
  dplyr::group_by(team_id) %>%
  tidyr::spread(matchup, sos_matchup) %>%
  dplyr::summarise(
    Total = mean(allplay_winpct, na.rm = TRUE),
    Division = mean(Division, na.rm = TRUE),
    Conference = mean(Conference, na.rm = TRUE),
    Random = mean(Other, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(franchise_id = team_id) %>%
  dplyr::mutate(
    SOS = Total / max(Total)
  ) %>%
  dplyr::left_join(
    franchises %>%
      dplyr::select(-division, -conference),
    by = "franchise_id"
  )

write.csv(sos, paste0("rfl-data/data/schedule/rfl-sos-", next_season, ".csv"), row.names = FALSE)
