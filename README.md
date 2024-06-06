# RFL data repository

This repository contains all code for automatic data creation for the RFL. All data is found in the releases.

## Workflow

1. 

### Variables

Für die automatische Erstellung von Daten im `data` Verzeichnis werden Piplines genutzt.

In der [gitLab Gruppe](https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group) werden dafür Umgebungsvariablen gesetzt.

| Name           | Value                                                                                      | Description                                                                                            |
| -------------- | ------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------ |
| `GITLAB_URL`   | `https://USERNAME:PASSWORD@gitlab.com/jakob-eschler/data-science/fantasy-football/rfl.git` | URL des Repositories, in das die Daten gepusht werden sollen. Inklusive Login Daten                    |
| `CURRENT_YEAR` | `2022`                                                                                     | Aktuelle Jahreszahl, die als Dateinamen genutzt werden können. **Muss mit Saisonstart erhöht werden.** |

### Pipelines

| Name                                                                                                    | Dependencies       | Schedule                                                  | Status                                                                                                                                                                            | Last Update                                                                                                                                                                                               |
| ------------------------------------------------------------------------------------------------------- | ------------------ | --------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [Starter](https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/starter)             |                    | Wöchentlich Di 8 Uhr & Do 16 Uhr                          | <img src="https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/starter/badges/main/pipeline.svg?ignore_skipped=true" type="image/svg+xml" height="20"/>       |                                                                                                                                                                                                           |
| [WAR](https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/war)                     | Starter            | Wöchentlich Do 17 Uhr                                     | <img src="https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/war/badges/main/pipeline.svg?ignore_skipped=true" type="image/svg+xml" height="20"/>           |                                                                                                                                                                                                           |
| [ELO](https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/elo)                     | Schedules          | Wöchentlich Di 9 Uhr & Do 17 Uhr                          | <img src="https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/elo/badges/main/pipeline.svg?ignore_skipped=true" type="image/svg+xml" height="20"/>           |                                                                                                                                                                                                           |
| [True Standing](https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/true-standing) | Starter, Schedules | Wöchentlich Di 9 Uhr & Do 17 Uhr                          | <img src="https://gitlab.com/jakob-eschler/data-science/fantasy-football/rfl-group/true-standing/badges/main/pipeline.svg?ignore_skipped=true" type="image/svg+xml" height="20"/> |                                                                                                                                                                                                           |
| [Trades](https://github.com/bohndesverband/rfl-data/releases/tag/trade_data)                            |                    | Mai (Draft): täglich 0 Uhr <br>ansonsten dienstags 18 Uhr | ![trades workflow](https://github.com/bohndesverband/rfl-data/actions/workflows/trades.yml/badge.svg)                                                                             | ![last trade update](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fgithub.com%2Fbohndesverband%2Frfl-data%2Freleases%2Fdownload%2Ftrade_data%2Ftimestamp.json&query=last_updated&label=%20) |
| [Drafts](https://github.com/bohndesverband/rfl-data/releases/tag/draft_data)                            |                    | Mai (Draft): täglich 0 Uhr                                | ![draft workflow](https://github.com/bohndesverband/rfl-data/actions/workflows/drafts.yml/badge.svg)                                                                              | ![last draft update](https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fgithub.com%2Fbohndesverband%2Frfl-data%2Freleases%2Fdownload%2Fdraft_data%2Ftimestamp.json&query=last_updated&label=%20) |
