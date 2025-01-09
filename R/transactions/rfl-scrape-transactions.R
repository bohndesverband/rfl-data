library(nflreadr)
library(tidyverse)
library(ffscrapr)

cli::cli_alert_info("Create Data")
current_season <- nflreadr::get_current_season(TRUE)

mfl <- ffscrapr::mfl_connect(current_season, 63018)

transactions <- ffscrapr::ff_transactions(mfl, "WAIVER,TRADE,FREE_AGENT") %>%
  dplyr::select(-comments) %>%
  dplyr::mutate(
    season = current_season,
    player_name = nflreadr::clean_player_names(player_name)
  ) %>%
  dplyr::select(season, everything())

cli::cli_alert_info("Write Data")
readr::write_csv(transactions, paste0("rfl_transactions_", current_season, ".csv"))

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_transactions_", current_season, ".csv"), "bohndesverband/rfl-data", "transactions_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "transactions_data", overwrite = TRUE)
