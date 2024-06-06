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
    trade_id = paste0(var_season, sprintf("%003d", row_number()))
  ) %>%
  dplyr::filter(trade_id > last_entry)

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
    dplyr::arrange(trade_id) %>%
    dplyr::left_join(
      trade_data_raw %>%
        dplyr::select(trade_id, timestamp),
      by = "trade_id",
      relationship = "many-to-many"
    ) %>%
    dplyr::left_join(
      jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", var_season, "/export?TYPE=contestPlayers&L=63018&APIKEY=aRNp3s%2BWvuWrx0WmPlrBYDoeErox&W&JSON=1"))$contest_players$player %>%
        dplyr::tibble() %>%
        tidyr::unnest_wider(1),
      by = c("asset" = "id")
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      date = lubridate::as_datetime(as.numeric(timestamp), tz = "GMT"),
      season = lubridate::year(date),
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
      asset_id = ifelse(!is.na(draft_year), paste("DP", draft_round, sep = "_"), asset)
    ) %>%
    dplyr::select(season, trade_id, timestamp, date, week, franchise_id, franchise, asset_id, asset_name) %>%
    dplyr::rename(trade_side = franchise)

  cli::cli_alert_info("Write Data")
  readr::write_csv(rbind(past_trades, trade_data), paste0("rfl_trades_", var_season, ".csv"))

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_trades_", var_season, ".csv"), "bohndesverband/rfl-data", "trade_data", overwrite = TRUE)
} else {
  cli::cli_alert_info("No new Trades")
}

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "trade_data", overwrite = TRUE)
