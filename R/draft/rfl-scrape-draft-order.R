library(tidyverse)

draft_year <- 2026

standing_data <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/standing_data/rfl_standing_", draft_year - 1, ".csv"), col_types = "icccdddiiiddiiiiiiiiicii") %>%
  dplyr::filter(week == max(week)) %>%
  dplyr::select(week, franchise_id, pick)

postseason_teams <- standing_data %>%
  dplyr::filter(week == max(week) & pick > 24) %>%
  dplyr::select(-week)

postseason_results <- readr::read_csv(paste0("https://github.com/bohndesverband/rfl-data/releases/download/postseason_data/rfl_postseason_", draft_year - 1, ".csv"), col_types = "icciccdcii") %>%
  dplyr::filter(bowl == "SB" & (match_result == "L" | title == 1) & bracket != "Spiel um Platz drei") %>%
  dplyr::left_join(postseason_teams, by = "franchise_id") %>%
  dplyr::group_by(week) %>%
  dplyr::arrange(pick) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(week) %>%
  dplyr::mutate(
    pick = dplyr::case_when(
      week == 17 & title == 0 ~ 35,
      week == 17 & title == 1 ~ 36,
      TRUE ~ row_number() + 24
    ),
    week = 14
  ) %>%
  dplyr::arrange(pick) %>%
  dplyr::select(week, franchise_id, pick)

draft_order_postseason <- standing_data %>%
  dplyr::filter(week == 13) %>%
  dplyr::arrange(pick) %>%
  dplyr::filter(pick <= 24) %>%
  dplyr::mutate(week = 14) %>%
  dplyr::bind_rows(postseason_results) %>%
  dplyr::mutate(season = draft_year) %>%
  dplyr::select(season, franchise_id, pick)

# upload draft order to github ----
readr::write_csv(draft_order_postseason, paste0("rfl_draft-order_", draft_year, ".csv"))

piggyback::pb_upload(paste0("rfl_draft-order_", draft_year, ".csv"), "bohndesverband/rfl-data", "draft_data", overwrite = TRUE)
