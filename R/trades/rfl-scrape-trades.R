library(piggyback)
library(tidyverse)

current_year <- lubridate::year(Sys.Date())

# check if we are before april 1st
if (Sys.Date() < as.Date(paste0(current_year, "-04-01"))) {
  var_season <- lubridate::year(Sys.Date()) - 1
} else {
  var_season <- lubridate::year(Sys.Date())
}

find_week <- function(use_date) {
  week1_sep <- as.POSIXlt(paste0(lubridate::year(use_date), "-09-0", 1:7), tz = "GMT")
  monday1_sep <- week1_sep[week1_sep$wday == 1]
  first_game <- monday1_sep
  first_game$mday <- first_game$mday + 1 # ab Dienstag neue Woche
  current_week <- as.numeric(as.Date(use_date) - as.Date(first_game))%/%7 + 1

  if (current_week < 1 | current_week > 22)
    current_week <- 22
  return(current_week)
}

past_trades <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_trades_", var_season, ".csv"), col_types = "dddTdcccc")

last_entry <- past_trades %>%
  dplyr::select(trade_id) %>%
  dplyr::filter(trade_id == max(trade_id)) %>%
  dplyr::distinct() %>%
  dplyr::pull()

trade_data_raw <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", var_season, "/export?TYPE=transactions&L=63018&TRANS_TYPE=TRADE&JSON=1"))$transactions$transaction %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::select(timestamp, franchise, franchise2, franchise1_gave_up, franchise2_gave_up) %>%
  dplyr::arrange(timestamp) %>%
  dplyr::mutate(
    trade_id = as.numeric(paste0(var_season, sprintf("%003d", row_number())))
  ) %>%
  dplyr::filter(trade_id > last_entry)

franchise_infos <- jsonlite::read_json(paste0(paste0("https://www45.myfantasyleague.com/", var_season), "/export?TYPE=league&L=63018&APIKEY=&JSON=1")) %>%
  purrr::pluck("league", "franchises", "franchise") %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rename(
    franchise_name = name,
    franchise_id = id
  )

if (dim(trade_data_raw)[1] != 0) {
  franchise1 <- trade_data_raw %>%
    dplyr::select(trade_id, franchise, franchise1_gave_up) %>%
    tidyr::separate_rows(franchise1_gave_up, sep = ",") %>%
    dplyr::filter(franchise1_gave_up != "") %>%
    dplyr::rename(asset = franchise1_gave_up, franchise_id = franchise) %>%
    dplyr::mutate(franchise = "franchise_1")

  franchise2 <- trade_data_raw %>%
    dplyr::select(trade_id, franchise2, franchise2_gave_up) %>%
    tidyr::separate_rows(franchise2_gave_up, sep = ",") %>%
    dplyr::filter(franchise2_gave_up != "") %>%
    dplyr::rename(asset = franchise2_gave_up, franchise_id = franchise2) %>%
    dplyr::mutate(franchise = "franchise_2")

  trade_data <- rbind(franchise1, franchise2) %>%
    dplyr::mutate(trade_id = as.numeric(trade_id)) %>%
    dplyr::arrange(trade_id) %>%
    dplyr::left_join(
      trade_data_raw %>%
        dplyr::select(trade_id, timestamp),
      by = "trade_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_year, "/export?TYPE=contestPlayers&L=63018&APIKEY=aRNp3s%2BWvuWpx1OmPlrBYDoeErox&W=&F=&JSON=1"))$contest_players$player %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(1),
      by = c("asset" = "id")
    ) %>%

    dplyr::rowwise() %>%
    dplyr::mutate(
      date = lubridate::as_datetime(as.numeric(timestamp), tz = "GMT"),
      timestamp = as.double(timestamp),
      season = var_season,
      week = find_week(as.Date(date))
    ) %>%

    dplyr::rowwise() %>%
    dplyr::mutate(
      draft_pick = ifelse(grepl("DP_", asset), stringr::str_pad(as.numeric(stringr::str_split(asset, "_")[[1]][3]) + 1, 2, pad = "0"), NA),
      draft_round = dplyr::case_when(
        grepl("FP_", asset) ~ stringr::str_split(asset, "_")[[1]][4],
        grepl("DP_", asset) ~ stringr::str_split(asset, "_")[[1]][2]
      ),
      draft_year = dplyr::case_when(
        grepl("FP_", asset) ~ stringr::str_split(asset, "_")[[1]][3],
        grepl("DP_", asset) ~ as.character(lubridate::year(date))
      ),
      team = dplyr::case_when(
        grepl("FP_", asset) ~ stringr::str_split(asset, "_")[[1]][2],
        grepl("DP_", asset) ~ franchise_id,
        TRUE ~ team
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      draft_round = ifelse(grepl("DP_", asset), as.numeric(draft_round) + 1, draft_round),
      name = dplyr::case_when(
        draft_round == 1 ~ "1st Round Pick",
        draft_round == 2 ~ "2nd Round Pick",
        draft_round == 3 ~ "3rd Round Pick",
        draft_round %in% c(4, 5, 6, 7) ~ paste0(draft_round, "th Round Pick"),
        TRUE ~ paste0(name, " (", position, ", ", team, ")")
      ),
      asset_name = dplyr::case_when(
        grepl("FP_", asset) ~ paste(name, draft_year),
        grepl("DP_", asset) ~ paste0(draft_round, ".", draft_pick, " ", draft_year),
        TRUE ~ name
      ),
      #asset_id = ifelse(!is.na(draft_year), paste("DP", draft_round, sep = "_"), asset)
    ) %>%
    dplyr::select(season, trade_id, timestamp, date, week, franchise_id, franchise, asset, asset_name) %>%
    dplyr::rename(trade_side = franchise, asset_id = asset)

  cli::cli_alert_info("Write Data")
  readr::write_csv(rbind(past_trades, trade_data), paste0("rfl_trades_", var_season, ".csv"))

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_trades_", var_season, ".csv"), "bohndesverband/rfl-data", "trade_data", overwrite = TRUE)

  ## draftclass trades ----
  cli::cli_alert_info("Draftclass Trade Data")

  draft_class_trades <- purrr::map_df(2016:var_season, function(x) {
    vroom::vroom(
      glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/trade_data/rfl_trades_{x}.csv"),
      col_types = "dddTdcccc"
      )
    }) %>%
    dplyr::group_by(trade_id) %>%
    dplyr::arrange(trade_side) %>%
    dplyr::mutate(
      trade_partner = ifelse(trade_side == "franchise_2", first(franchise_id), last(franchise_id)),
      franchise_ids = paste(unique(franchise_id), collapse = ","),
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      prefix = dplyr::case_when(
        grepl("DP", asset_id) ~ "DP",
        grepl("FP", asset_id) ~ "FP",
        TRUE ~ NA
      ),
      pick_owner = ifelse(prefix == "FP", stringr::word(asset_id, 2, sep = "_"), trade_partner),
      pick_year = ifelse(prefix == "FP", stringr::word(asset_id, 3, sep = "_"), season),
      pick_round = dplyr::case_when(
        prefix == "FP" ~ as.numeric(stringr::word(asset_id, 4, sep = "_")),
        prefix == "DP" ~ as.numeric(stringr::word(asset_id, 2, sep = "_")) + 1,
        TRUE ~ NA
      )
    ) %>%

    # add draft order um exakten pick für getradete future picks zu erhalten
    dplyr::left_join(
      purrr::map_df(2017:var_season, function(x) {
        vroom::vroom(
          glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft-order_{x}.csv"),
          col_types = "ccic"
        )
      }),
      by = c("pick_year" = "season", "pick_owner" = "franchise_id")
    ) %>%

    # add franchise name to picks where no future draft order is available
    dplyr::left_join(
      franchise_infos %>%
        dplyr::select(franchise_id, trade_parter_name = franchise_name),
      by = c("pick_owner" = "franchise_id")
    ) %>%

    dplyr::mutate(
      asset_id_new = dplyr::case_when(
        # alle ehemaligen future picks, die jetzt in der gegenwart sind, erhalten eine ID für den aktuellen draft
        prefix == "FP" & pick_year <= var_season & !is.na(pick) ~ paste("DP", pick_round, pick, pick_year, sep = "_"),
        prefix == "DP" ~ paste("DP", as.numeric(stringr::word(asset_id, 3, sep = "_")) + 1, as.numeric(stringr::word(asset_id, 2, sep = "_")) + 1, season, sep = "_"),
        TRUE ~ asset_id
      ),
      pick = dplyr::case_when(
        prefix == "DP" ~ as.integer(stringr::word(asset_id, 3, sep = "_")) + 1,
        TRUE ~ as.integer(pick)
      )
    ) %>%

    # füge getätigte draftpicks an
    dplyr::left_join(
      purrr::map_df(2017:var_season - 1, function(x) {
        vroom::vroom(
          glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft_{x}.csv"),
          col_types = "iTiiiccccccci"
        )
      }) %>%
        dplyr::mutate(
          round = as.integer(round),
          pick_year = as.character(season),
          pos_grouped = dplyr::case_when(
            pos %in% c("DT", "DE") ~ "DL",
            pos %in% c("CB", "S") ~ "DB",
            TRUE ~ pos
          ),
          drafted_player = paste(player_name, paste0("(", pos_grouped, ", ", team, ")"))
        ) %>%
        dplyr::select(pick_year, round, pick, pick_team_id = franchise_id, drafted_player),
      by = c("pick_year", "pick_round" = "round", "pick")
    ) %>%

    #filter(trade_id == "2025021") %>%
    # neue asset names erzeugen
    dplyr::mutate(
      asset_name_new = dplyr::case_when(
        !is.na(drafted_player) ~ paste(pick_year, paste0(pick_round, ".", stringr::str_pad(pick, 2, "left", 0)), drafted_player),
        prefix == "FP" ~ paste(asset_name, trade_parter_name),
        TRUE ~ asset_name
      )
    ) %>%

    # trades zusammenfassen
    dplyr::group_by(trade_id) %>%
    dplyr::arrange(trade_side) %>%
    dplyr::mutate(
      asset_ids = paste(unique(asset_id_new), collapse = ","),
      asset_names = paste(unique(asset_name_new), collapse = ","),
    ) %>%
    dplyr::group_by(trade_id, trade_side) %>%
    dplyr::arrange(asset_name_new) %>%
    dplyr::summarise(
      dplyr::across(c(season, date, franchise_id, franchise_ids, asset_ids, asset_names, trade_partner, pick_team_id), first),
      trade_side_assets = paste(unique(asset_name_new), collapse = "\n"),
      .groups = "drop"
    ) %>%
    dplyr::select(-trade_side)

    cli::cli_alert_info("Write Data")
    readr::write_csv(draft_class_trades, "rfl_draftclass-trades.csv")

    cli::cli_alert_info("Upload Data")
    piggyback::pb_upload("rfl_draftclass-trades.csv", "bohndesverband/rfl-data", "trade_data", overwrite = TRUE)
} else {
  cli::cli_alert_info("No new Trades")
}

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "trade_data", overwrite = TRUE)
