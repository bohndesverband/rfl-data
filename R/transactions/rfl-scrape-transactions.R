library(nflreadr)
library(tidyverse)

cli::cli_alert_info("Create Data")
current_season <- nflreadr::get_current_season(TRUE)

transactions <- jsonlite::read_json(paste0("https://www45.myfantasyleague.com/", current_season, "/export?TYPE=transactions&L=63018&TRANS_TYPE=WAIVER,FREE_AGENT&JSON=1"))$transactions$transaction %>%
  dplyr::tibble() %>%
  tidyr::unnest_wider(1) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    # split transaction on | and get first element
    added = ifelse(type == "FREE_AGENT", stringr::str_split(transaction, "\\|")[[1]][1], added),
    dropped = ifelse(type == "FREE_AGENT", stringr::str_split(transaction, "\\|")[[1]][2], added),
    season = current_season,
    #timestamp = lubridate::as_datetime(as.numeric(timestamp), tz = "GMT"),
  ) %>%
  tidyr::gather(type_desc, player_ids, c(added, dropped)) %>%
  dplyr::select(season, timestamp, type, type_desc, franchise, player_ids) %>%
  # filter empty player_ids
  dplyr::filter(player_ids != "") %>%
  # split player_ids on , and unnest longer
  tidyr::separate_rows(player_ids, sep = ",") %>%
  dplyr::filter(player_ids != "") %>%
  dplyr::rename(franchise_id = franchise, player_id = player_ids)

cli::cli_alert_info("Write Data")
readr::write_csv(transactions, paste0("rfl_transactions_", current_season, ".csv"))

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_transactions_", current_season, ".csv"), "bohndesverband/rfl-data", "transactions_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "transactions_data", overwrite = TRUE)
