library(nflreadr)
library(tidyverse)
library(curl)

current_season <- nflreadr::get_current_season()

rfl_drafts <- purrr::map_df(2017:current_season, function(x) {
  readr::read_csv(
    glue::glue("https://github.com/bohndesverband/rfl-data/releases/download/draft_data/rfl_draft_{x}.csv"),
    col_types = "dTdddcccccccd"
  )
})

merge <- rfl_drafts %>%
  dplyr::filter(is.na(gsis_id)) %>%
  dplyr::select(-gsis_id) %>%
  dplyr::left_join(
    nflreadr::load_ff_playerids() %>%
      dplyr::select(mfl_id, gsis_id),
    by = "mfl_id"
  ) %>%
  dplyr::select(season:mfl_id, gsis_id, player_name:is_rookie)

new_data <- rbind(rfl_drafts %>%
                    dplyr::filter(!is.na(gsis_id)),
                  merge) %>%
  dplyr::arrange(season, overall)

for (loop_season in 2017:current_season) {
  cli::cli_alert_info("Write Data")
  readr::write_csv(
    new_data %>%
      dplyr::filter(season == loop_season),
    glue::glue("rfl_draft_{loop_season}.csv")
  )

  cli::cli_alert_info("Upload Data")
  piggyback::pb_upload(paste0("rfl_draft_", loop_season, ".csv"), "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)
}

timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
  jsonlite::toJSON(auto_unbox = TRUE)

write(timestamp, "timestamp.json")
piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)
