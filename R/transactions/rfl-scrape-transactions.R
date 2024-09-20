library(nflreadr)
library(tidyverse)
library(ffscrapr)

cli::cli_alert_info("Create Data")
current_season <- lubridate::year(lubridate::now())

mfl <- ffscrapr::mfl_connect(current_season, 63018)

transactions <- ffscrapr::ff_transactions(mfl, "WAIVER,FREE_AGENT,TRADE") %>%
  dplyr::select(-comments)

cli::cli_alert_info("Write Data")
readr::write_csv(transactions, paste0("rfl_transactions_", current_season, ".csv"))

cli::cli_alert_info("Upload Data")
piggyback::pb_upload(paste0("rfl_transactions_", current_season, ".csv"), "bohndesverband/rfl-data", "transaction_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "rfl_transactions_", overwrite = TRUE)
