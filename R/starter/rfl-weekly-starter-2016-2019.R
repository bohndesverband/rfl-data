library(jsonlite)
library(dplyr)
library(magrittr)

dataList <- list()
playerList <- list()
starterList <- list()
for (year in 2016:2019) {
  jsonPlayer <- jsonlite::fromJSON(paste("https://www45.myfantasyleague.com/", year, "/export?TYPE=players&L=63018&APIKEY=&DETAILS=&SINCE=&PLAYERS=&JSON=1", sep = ""))
  player <- jsonPlayer$players$player %>%
    mutate(season = year)

  playerList[[length(playerList) + 1]] <- player

  for (week in 1:12) {
    # scrape data
    jsonData <- jsonlite::fromJSON(paste("https://www45.myfantasyleague.com/", year, "/export?TYPE=weeklyResults&L=63018&APIKEY=&W=", week, "&JSON=1", sep = ""))
    results <- jsonData$weeklyResults$franchise %>% # ToDo: player Info hinzuf?gen
      mutate(
        season = year,
        week = week
      ) %>%
      select(season, week, id, score, opt_pts, starters, nonstarters, optimal, player, comments) # reihenfolge vereinheitlichen

    for (i in 1:nrow(results)) {
      row <- results[i,]
      franchiseID <- row$id

      starter <- do.call(rbind.data.frame, row$player) %>%
        mutate(
          franchise_id = franchiseID,
          season = year,
          week = week
        )

      starterList[[length(starterList) + 1]] <- starter
    }

    dataList[[length(dataList) + 1]] <- results
  }
}

data = do.call(rbind, dataList) %>%
  select(season, week, id) %>%
  rename(franchise_id = id)

data2 = do.call(rbind, playerList) %>%
  rename(player_id = id)

data3 = do.call(rbind, starterList) %>%
  rename(player_id = id)

dataClean <- data %>%
  left_join(data3, by = c("franchise_id", "season", "week")) %>%
  left_join(data2 %>% select(-status), by = c("player_id", "season")) %>%
  distinct() %>%
  rename(
    starter_status = status,
    should_start = shouldStart,
    player_score = score,
    player_name = name,
    pos = position
  ) %>%
  dplyr::select(season, week, franchise_id, starter_status, player_id, player_name, pos, team, player_score, should_start)

for (year in 2016:2019) {
  utils::write.csv(dataClean %>% filter(season == year), paste0("data/starter/rfl-starter-", year, ".csv"), row.names = F)
}

rm(dataList, starterList, playerList, jsonPlayer, jsonData, player, data, data2, data3, week, year, dataClean)
