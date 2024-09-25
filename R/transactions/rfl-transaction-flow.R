library(tidyverse)

cli::cli_alert_info("Create Data")
f_all_transactions <- function(df) {
  df %>%
    # traded away
    dplyr::group_by(mfl_id, franchise_id) %>%
    dplyr::mutate(
      player_identifier = dplyr::case_when(
        is.na(player_identifier) & type_desc == "traded_away" ~ dplyr::first(player_identifier),
        TRUE ~ player_identifier
      )
    ) %>%

    #tradet for
    dplyr::group_by(timestamp, mfl_id) %>%
    dplyr::arrange(timestamp, type_desc) %>%
    dplyr::mutate(
      player_identifier = ifelse(
        type_desc == "traded_for", dplyr::first(player_identifier), player_identifier
      )
    ) %>%

    # drops
    dplyr::group_by(mfl_id, franchise_id) %>%
      dplyr::arrange(timestamp) %>%
      dplyr::mutate(player_identifier = dplyr::first(player_identifier)) %>%

    # adds von gedroppten spielern
    dplyr::group_by(mfl_id) %>%
    dplyr::arrange(dplyr::desc(is_fa), timestamp) %>%
    dplyr::mutate(
      player_identifier = if_else(
        is.na(player_identifier) & type_desc %in% c("added", "drafted") & dplyr::row_number() == dplyr::first(dplyr::row_number()[is.na(player_identifier) & type_desc %in% c("added", "drafted")]),
        dplyr::first(player_identifier),
        player_identifier
      ),
      is_fa = ifelse(dplyr::row_number() == 1 & is_fa == 1, 0, is_fa)
    )
}

current_year <- lubridate::year(lubridate::now())

draft <- readr::read_csv(
  glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft_{current_year}.csv"),
  col_types = "dTdddccccccci"
)

transactions <- readr::read_csv(
  glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/transactions_data/rfl_transactions_{current_year}.csv"),
  col_types = "iTccccccccc"
)

drafts_and_transactions <- rbind(
  draft %>%
    dplyr::mutate(
      type = "DRAFT",
      type_desc = "drafted"
    ) %>%
    dplyr::select(season, timestamp, type, type_desc, franchise_id, franchise_name, mfl_id, player_name, pos, team, is_rookie),
  transactions %>%
    dplyr::rename(mfl_id = player_id) %>%
    dplyr::mutate(is_rookie = "") %>%
    dplyr::select(season, timestamp, type, type_desc, franchise_id, franchise_name, mfl_id, player_name, pos, team, is_rookie)
) %>%

  dplyr::filter(!grepl("FP_", mfl_id) & !grepl("DP_", mfl_id) & !is.na(player_name)) %>% # remove draft picks
  dplyr::arrange(timestamp) %>%

  # füge player identifier hinzu (id des ersten teams vom spieler)
  dplyr::group_by(mfl_id) %>%
  dplyr::mutate(
    is_first_franchise = ifelse(cumsum(type_desc %in% c("drafted", "added")) < 3, 1, 0), # zähle, wie oft der spieler schon in einem roster war. wenn weniger als 3, ist es das erste team
    is_first_franchise = ifelse(cumsum(!duplicated(franchise_id) & is_first_franchise < 3) <= 3, 1, 0), # zähle die unique franchise_ids, die in den adds vorkommen. wenn <= 3 ist es das erste team
    player_identifier = ifelse(is_first_franchise == 1 & type_desc %in% c("drafted", "added"), paste(mfl_id, franchise_id, sep = "-"), NA)
  ) %>%
  dplyr::select(-is_first_franchise)

old_data <- readr::read_csv("https://github.com/bohndesverband/rfl-data/releases/download/transactions_data/rfl_transaction-flow.csv", col_types = "iTccdc")

drafts_and_transactions <- rbind(
  old_data,
  drafts_and_transactions %>%
    dplyr::select(season, timestamp, type, type_desc, franchise_id, mfl_id, player_identifier)
) %>%
  dplyr::mutate(is_fa = ifelse(type_desc == "dropped", 1, 0))

while (any(is.na(drafts_and_transactions$player_identifier))) {
  drafts_and_transactions <- drafts_and_transactions %>%
    f_all_transactions() %>%

    # reset order
    dplyr::ungroup() %>%
    dplyr::arrange(timestamp)

  has_missing_identifier <- drafts_and_transactions %>%
    filter(is.na(player_identifier))
}

final_data <- old_data %>%
  dplyr::mutate(
    season = ifelse(
      timestamp >= lubridate::ymd(paste0(lubridate::year(timestamp), "04-01")),
      lubridate::year(timestamp),
      lubridate::year(timestamp) - 1
    )
  )

cli::cli_alert_info("Write Data")
readr::write_csv(final_data, "rfl_transaction-flow.csv")

cli::cli_alert_info("Upload Data")
piggyback::pb_upload("rfl_transaction-flow.csv", "bohndesverband/rfl-data", "transactions_data", overwrite = TRUE)

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "transactions_data", overwrite = TRUE)
