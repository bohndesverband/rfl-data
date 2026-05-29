library(tidyverse)
library(feather)

current_season <- 2026

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
    franchise_abbrev = abbrev,
    franchise_logo = logo,
    franchise_icon = icon,
    division_id = division
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("divisions", "division") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        division_name = name,
        division_id = id,
        conference_id = conference
      ),
    by = "division_id"
  ) %>%
  dplyr::left_join(
    league %>%
      purrr::pluck("conferences", "conference") %>%
      dplyr::tibble() %>%
      tidyr::unnest_wider(1) %>%
      dplyr::rename(
        conference_name = name
      ),
    by = c("conference_id" = "id")
  ) %>%
  dplyr::mutate(season = current_season) %>%
  dplyr::select(season, franchise_id, franchise_name, franchise_abbrev, franchise_logo, franchise_icon, division_id, division_name, conference_id, conference_name)

readr::write_csv(franchises, "rfl-data/data/rfl-historic-franchises.csv", append = TRUE)

feather::write_feather(readr::read_csv("rfl-data/data/rfl-historic-franchises.csv"), "rfl-shiny-app/data/rfl_franchises_history.feather")
