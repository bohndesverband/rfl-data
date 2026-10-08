library(tidyverse)
library(nflreadr)
library(curl)

cli::cli_alert_info("Create Data")

current_season = nflreadr::get_current_season()
current_week = nflreadr::get_current_week() - 1
# current_week <- 13

# league data ----
league <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=league&L=63018&APIKEY=&JSON=1")) %>%
  purrr::pluck("league")

## franchise data ----
franchises <- league %>%
  purrr::pluck("franchises", "franchise") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    franchise_name = name,
    franchise_id = id,
    div_id = division
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("divisions", "division") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        div_name = name,
        div_id = id,
        conf_id = conference
      ),
    by = "div_id"
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("conferences", "conference") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        conf_name = name
      ),
    by = c("conf_id" = "id")
  ) %>%
  dplyr::select(franchise_id, dplyr::starts_with("conf"), dplyr::starts_with("div"))

if (current_week <= 13) {
  schedule <- readr::read_csv("https://raw.githubusercontent.com/bohndesverband/rfl-data/main/data/rfl-schedules.csv", col_types = "iicc") %>%
    dplyr::filter(season == current_season & week <= current_week) %>%
    dplyr::select(-season)

  points <- purrr::map_df(seq(1, current_week), function(x) {
    jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=weeklyResults&L=63018&W=", x, "&JSON=1"))$weeklyResults$matchup %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::mutate(
        week = x
      ) %>%
      tidyr::unnest(franchise) %>% # post 2020
      tidyr::unnest_wider(franchise) %>% # post 2020
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
      pp = as.numeric(pp)
    )

  results <- schedule %>%
    dplyr::left_join(
      points,
      by = c("franchise_id", "week"),
      multiple = "all"
    ) %>%
    # add opponent id
    dplyr::left_join(
      points %>%
        dplyr::select(week, franchise_id, pf) %>%
        dplyr::rename(pf_opponent = pf),
      by = c("opponent_id" = "franchise_id", "week"),
      multiple = "all"
    ) %>%
    dplyr::mutate(
      pf_opponent = ifelse(is.na(pf_opponent), 0, pf_opponent),
      wins = ifelse(pf - pf_opponent > 0, 1, 0)
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
    dplyr::select(-opponent_id)

  true_standing <- results %>%
    # weekly stats
    dplyr::group_by(week, franchise_id) %>%
    dplyr::summarise(
      dplyr::across(c(wins, pf_opponent), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(c(pf, pp, all_play_wins), first),
      .groups = "drop"
    ) %>%
    dplyr::left_join(franchises, by = "franchise_id") %>%
    dplyr::rename(pa = pf_opponent) %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(
      week = as.numeric(week),
      eff = round(pf / pp, 2),
      pf_avg = mean(pf),
      games_total = ifelse(current_season > 2016 & current_season < 2020, 24, 26),
      wins_expected = round((wins * pf^2.37) / (pf^2.37 + pf_avg^2.37), 2),
      luck = round((2 * pf_avg) / pa, 2),
      quality = round(pf / pf_avg, 2),
      pf_rank = dplyr::dense_rank(dplyr::desc(pf)),
      pp_rank = dplyr::dense_rank(dplyr::desc(pp)),
      pa_rank = dplyr::dense_rank(dplyr::desc(pp)),
      eff_rank = dplyr::dense_rank(dplyr::desc(eff)),
      pa_rank = dplyr::dense_rank(pa),
      record_rank = dplyr::dense_rank(dplyr::desc(wins)),
      all_play_rank = dplyr::dense_rank(dplyr::desc(all_play_wins)),
      luck_rank = dplyr::dense_rank(dplyr::desc(luck)),
      quality_rank = dplyr::dense_rank(dplyr::desc(quality)),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(week <= 13) %>%
    dplyr::arrange(week) %>%

    # create running stats
    dplyr::group_by(franchise_id) %>%
    dplyr::arrange(week) %>%
    dplyr::mutate(
      wins_season = cumsum(wins),
      pf_season = round(cumsum(pf), 2),
      pp_season = round(cumsum(pp), 2),
      pa_season = round(cumsum(pa), 2),
      all_play_wins_season = cumsum(all_play_wins),
      eff_season = round(pf_season / pp_season, 2)
    ) %>%
    dplyr::group_by(week) %>%
    dplyr::mutate(
      pf_avg = mean(pf_season),
    ) %>%
    dplyr::group_by(franchise_id) %>%
    dplyr::arrange(week) %>%
    dplyr::mutate(
      games_left = games_total - (week * 2),
      wins_expected_season = round(wins_season + (games_left * pf_season^2.37) / (pf_season^2.37 + pf_avg^2.37), 2),
      luck_season = round((2 * pf_avg) / pa_season, 2),
      quality_season = round(pf_season / pf_avg, 2),
    ) %>%
    #filter(franchise_id == "0007")

    # create divison ranking
    dplyr::group_by(week, div_id) %>%
    dplyr::arrange(dplyr::desc(wins_season), dplyr::desc(pf_season)) %>%
    dplyr::mutate(div_rank = dplyr::row_number()) %>%

    # create conf ranking
    dplyr::group_by(week, conf_id) %>%
    dplyr::arrange(dplyr::desc(wins_season), dplyr::desc(pf_season)) %>%
    dplyr::mutate(conf_rank = dplyr::row_number()) %>%

    # add seeding
    dplyr::group_by(week, conf_id, div_rank) %>%
    dplyr::arrange(desc(wins_season), desc(pf_season)) %>%
    dplyr::mutate(
      seed = ifelse(div_rank == 1, dplyr::row_number(), NA),
    ) %>%
    dplyr::group_by(week, conf_id) %>%
    dplyr::arrange(seed, desc(wins_season), desc(pf_season)) %>%
    dplyr::mutate(
      seed = ifelse(is.na(seed), row_number(), seed),
      bowl = dplyr::case_when(
        seed <= 6 ~ 1, # 1 == SB
        seed <= 12 ~ 2, # 2 == PB
        TRUE ~ 3 # 3 == TB
      ),
    ) %>%
    dplyr::group_by(week, bowl) %>%
    dplyr::arrange(dplyr::desc(wins_season), dplyr::desc(pf_season)) %>%
    dplyr::mutate(
      seed = ifelse(bowl > 1, dplyr::row_number(), seed)
    ) %>%

    # create league ranking
    dplyr::group_by(week) %>%
    dplyr::mutate(
      po_helper = ifelse(bowl == 1, 1, 0)
    ) %>%
    dplyr::arrange(bowl, seed) %>%
    dplyr::mutate(
      league_rank = dplyr::row_number()
    ) %>%

    # add draftpick
    dplyr::arrange(po_helper, pp_season) %>%
    dplyr::mutate(pick = dplyr::row_number()) %>%

    # add category ranks
    dplyr::group_by(week) %>%
    dplyr::mutate(
      dplyr::across(
        c(pf_season, pp_season, pa_season, wins_season, all_play_wins_season, eff_season, wins_expected_season, luck_season, quality_season),
        ~ dplyr::min_rank(dplyr::desc(.x)),
        .names = "{.col}_rank"
      )
    ) %>%

    # cleanup
    dplyr::mutate(
      season = current_season,
      bowl = dplyr::case_when(
        bowl == 1 ~ "SB",
        bowl == 2 ~ "PB",
        bowl == 3 ~ "TB"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(week, league_rank) %>%
    dplyr::select(season, week, franchise_id, conf_id, div_id, pf, pp, eff, pa, wins, wins_expected, all_play_wins, luck, quality, dplyr::ends_with("_season"), dplyr::ends_with("_rank"), bowl, seed, pick)

  cli::cli_alert_info("Write Data")
  #readr::write_csv(true_standing, "data/true-standing/rfl-true-standing-2023.csv")
  readr::write_csv(true_standing, paste0("rfl_standing_", current_season, ".csv"))


  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_standing_", current_season, ".csv"), "bohndesverband/rfl-data", "standing_data", overwrite = TRUE)

  timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  write(timestamp, "timestamp.json")
  piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "standing_data", overwrite = TRUE)

} else {
  cli::cli_alert_info("Regular Season is over")
}
