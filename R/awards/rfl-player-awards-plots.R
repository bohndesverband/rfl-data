library(tidyverse)
library(nflreadr)
library(piggyback)
library(cropcircles)

cli::cli_alert_info("Create Data")

# variables ----
current_season = nflreadr::get_current_season()
current_week <- nflreadr::get_current_week()

color_grey_light <- "#d2dae2"
color_grey_mid <- "#808e9b"
color_grey_dark <- "#485460"
color_black <- "#1e272e"
color_red <- "#f53b57"
color_blue <- "#3c40c6"
color_cyan <- "#0fbcf9"
color_petrol <- "#00d8d6"
color_green <- "#05c46b"
color_orange <- "#ffa801"
color_yellow <- "#ffd32a"
color_bg <- "white"
color_text <- color_black

colors_position <- c(
  "QB" = "#feca57",
  "RB" = "#1dd1a1",
  "WR" = "#54a0ff",
  "TE" = "#ff6b6b",
  "PK" = "#c8d6e5",
  "DL" = "#48dbfb",
  "LB" = "#ff9ff3",
  "DB" = "#00d2d3"
)

font <- "Poppins"

position_order <- c("QB", "RB", "WR", "TE", "FLX", "PK", "DL", "LB", "DB", "IDP")
positions <- c("QB", "RB", "WR", "TE", "PK", "DL", "LB", "DB")

# helper ----
player_annotaions <- function() {
  player_data <- current_awards %>%
    dplyr::group_by(award) %>%
    dplyr::filter(dplyr::row_number() == 1)

  player_annotations <- list(
    # award title
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = award),
      x = 3.5, y = 1.32, hjust = 0.5, color = color_orange, size = 5
    ),
    # player name
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = display_name),
      x = 3.5, y = -2.4, hjust = 0.5, color = color_black, size = 4.5
    ),
    # award description
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = subline),
      x = 3.5, y = -2.65, vjust = 1, hjust = 0.5, color = color_grey_mid, size = 4, lineheight = 1
    )
  )

  return(player_annotations)
}

segments <- data.frame(
  x1 = c(rep(0.5, 3), 1, 4),
  x2 = c(rep(6.5, 3), 3, 6),
  y1 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  y2 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  color = c(rep(color_bg, 3), rep(color_orange, 2))
)

plot_default <- list(
  ggplot2::scale_color_manual(values = colors_position, guide = "none"),
  ggplot2::theme_void(),
  ggplot2::theme(
    text = ggplot2::element_text(color = color_text, family = font, lineheight = 1.2),
    #plot.margin = ggplot2::margin(t = 0, r = 0, b = 30, l = 0),
    plot.background = ggplot2::element_rect(fill = color_bg, color = color_bg),

    plot.title.position = "plot",

    legend.key.size = ggplot2::unit(12, "pt"),
    legend.text = ggplot2::element_text(size = 12),
  )
)

plot_default_bar <- list(
  geom_text(mapping = ggplot2::aes(label = war), vjust = 3, size = 8, color = color_bg),
  geom_text(aes(label = sapply(display_name, function(x) paste(strwrap(x, width = 5), collapse = "\n"))), vjust = -0.5, hjust = 0.5, size = 6, color = color_grey_mid, lineheight = 1),
  geom_hline(yintercept = 0, color = color_grey_mid, linewidth = 0.3),
  nflplotR::geom_nfl_headshots(aes(player_gsis = gsis_id), y = 0, height = 0.23, vjust = 0),
  ggplot2::scale_fill_manual(values = colors_position, guide = "none"),
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 24, face = "bold", lineheight = 0.8, margin = ggplot2::margin(l = 25)),
    plot.subtitle = ggplot2::element_text(size = 16, lineheight = 1, margin = ggplot2::margin(l = 25))
  )
)

if(current_week == 14) {
  # data ----
  ## base data ----
  rfl_awards <- readr::read_csv("https://raw.githubusercontent.com/bohndesverband/rfl-data/refs/heads/main/data/awards/rfl-player-awards.csv", col_types = "icccccddicc") %>%
    # create pctl in different categories
    # all award candidates
    dplyr::mutate(
      points_total_pctl = scales::rescale(points, c(0, 1)),
      war_total_pctl = scales::rescale(war, c(0, 1))
    ) %>%

    # all award candidates in same year
    dplyr::group_by(season) %>%
    dplyr::mutate(
      points_season_pctl = scales::rescale(points, c(0, 1)),
      war_season_pctl = scales::rescale(war, c(0, 1))
    ) %>%

    # all award candidates in same award
    dplyr::group_by(award) %>%
    dplyr::mutate(
      points_award_pctl = scales::rescale(points, c(0, 1)),
      war_award_pctl = scales::rescale(war, c(0, 1))
    ) %>%

    dplyr::ungroup() %>%
    tidyr::gather(category, pctl, dplyr::ends_with("pctl")) %>%
    dplyr::mutate(
      award_f = factor(award, c("MVP", "OPOY", "DPOY", "CPOY", "APOY", "GPOY", "OROY", "DROY")),
      category_f= factor(
        category,
        levels = c("points_total_pctl", "points_season_pctl", "points_award_pctl", "war_award_pctl", "war_season_pctl", "war_total_pctl")
      ),
      group = dplyr::case_when(
        grepl("total_pctl", category) ~ paste0("Alle Kandidaten 2016-", current_season),
        grepl("award_pctl", category) ~ paste0("Alle Kandidaten im selben Award 2016-", current_season),
        grepl("season_pctl", category) ~ paste("Alle Kandidaten", current_season),
      ),
      subline = dplyr::case_when(
        award == "MVP" ~ "Meiste Wins over Replacement (WAR)",
        award == "OPOY" ~ "Meiste WAR: Offense non-MVP",
        award == "DPOY" ~ "Meiste WAR: Defense non-MVP",
        award == "CPOY" ~ "Hat die letzte Saison auf IR beendet\nund seit dem die höchste WAR Differenz",
        award == "APOY" ~ "Meiste Fantasy Punkte durch die Luft (QB)",
        award == "GPOY" ~ "Meiste Fantasy Punkte am Boden",
        award == "OROY" ~ "Meiste WAR: Offensive Rookie",
        award == "DROY" ~ "Meiste WAR: Defensive Rookie"
      )
    ) %>%

    # add images
    dplyr::left_join(
      nflreadr::load_players() %>%
        dplyr::select(gsis_id, headshot),
      by = "gsis_id"
    )

  for (current_season in 2016:2024) {

  cli::cli_alert_info("Create Current Awards")

  ## current awards ----
  current_awards <- rfl_awards %>%
    dplyr::filter(season == current_season & rank == 1)

  # plots ----
  ## player awards ----
  player_awards_plot <- ggplot2::ggplot(current_awards, ggplot2::aes(x = category_f, y = pctl)) +
    ggplot2::facet_wrap(~ award_f, ncol = 4) +
    ggplot2::coord_radial(start = -0.55 * pi, end = 0.55 * pi, clip = "off", inner.radius = 0.4) +

    ggplot2::geom_col(mapping = ggplot2::aes(alpha = group), fill = color_grey_mid, width = 0.965, position = "dodge") +

    # custom grid lines and annotations
    ggplot2::geom_text(label = "FPts", x = 0.9, y = 1.55, color = color_grey_mid, size = 3, vjust = 1, hjust = 0, lineheight = 0.35) +
    ggplot2::geom_text(label = "WAR", x = 0.45, y = -3.7, color = color_grey_mid, size = 3, vjust = 1, hjust = 1, lineheight = 0.35) +
    ggplot2::geom_segment(data = segments, mapping = ggplot2::aes(x = x1, xend = x2, y = y1, yend = y2, color = color), linewidth = 0.5) +
    ggplot2::scale_color_identity()

  for (player in unique(rfl_awards$award)) {
    player_awards_plot <- player_awards_plot +
      player_annotaions()
  }

  player_awards_plot_new <- player_awards_plot +
      ggnewscale::new_scale_color() +
      # player background
      ggplot2::geom_point(data = subset(current_awards, category == "points_total_pctl"), mapping = ggplot2::aes(color = pos), y = -1.06, size = 52.2) +

      # player
      ggimage::geom_image(
        data = current_awards %>% dplyr::group_by(award) %>% dplyr::filter(dplyr::row_number() == 1),
        mapping = ggplot2::aes(image = cropcircles::circle_crop(headshot)),
        y = -1,
        x = -2.2,
        asp = 1, size = 0.56
      ) +

      ggplot2::scale_alpha_manual(values = c(0.3, 0.6, 1)) +

      ggplot2::labs(
        title = paste("RFL Spieler Auszeichnungen", current_season),
        subtitle = "Darstellung der besten Spieler in ihren Kategorien.\nDie Balken stehen für die Perzentile, in denen die Spieler innerhalb der Top-3 Spieler jeder Kategorie (Kandidaten) ranken.",
        alpha = "",
        color = NULL
      ) +

      plot_default +
      ggplot2::theme(
        legend.position = "top",
        plot.title = ggplot2::element_text(size = 24, face = "bold", lineheight = 0.8, margin = ggplot2::margin(t = -40, b = 0, l = 25)),
        plot.subtitle = ggplot2::element_text(size = 16, lineheight = 1, margin = ggplot2::margin(t = 10, b = 25, l = 25)),
        strip.text = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.spacing.x = ggplot2::unit(0, "mm"),
        panel.spacing.y = ggplot2::unit(25, "mm"),
      )

  cli::cli_alert_info("Write Current Awards")
  ggplot2::ggsave(paste0("rfl-player-awards-plot-", current_season, ".jpg"), player_awards_plot_new, width = 2700, height = 1400, dpi = 144, units = "px")

  cli::cli_alert_info("Upload Current Awards")
  piggyback::pb_upload(paste0("rfl-player-awards-plot-", current_season, ".jpg"), "bohndesverband/rfl-data", "awards_data", overwrite = TRUE)
}

  ## all MVPs ----
  league_mvps <- rfl_awards %>%
    dplyr::filter(award == "MVP" & rank == 1) %>%
    dplyr::select(season, gsis_id, display_name, pos, war) %>%
    dplyr::distinct()

  league_mvps_plot <- ggplot2::ggplot(league_mvps, aes(x = season, y = war, fill = pos)) +
    ggplot2::geom_col() +

    plot_default +
    plot_default_bar +

    ggplot2::scale_x_continuous(breaks = c(2016:current_season)) +
    ggplot2::scale_y_continuous(limits = c(0, 6)) +

    ggplot2::labs(
      title = paste0("RFL MVPs 2016-", current_season),
      subtitle = "Die Spieler mit den meisten Wins Above Replacement einer jeden Saison.",
      fill = ""
    ) +

    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 25, r = 0, b = 25, l = 0),
      legend.position = "top",
      legend.margin = ggplot2::margin(t = 3, unit = "mm"),
      axis.text.x = ggplot2::element_text(color = color_grey_light, size = 20)
    )

  cli::cli_alert_info("Write League MVPs")
  ggplot2::ggsave("rfl-mvps.jpg", league_mvps_plot, width = 2700, height = 1400, dpi = 144, units = "px")

  cli::cli_alert_info("Upload League MVPs")
  piggyback::pb_upload("rfl-mvps.jpg", "bohndesverband/rfl-data", "awards_data", overwrite = TRUE)

  timestamp <- list(last_updated = format(Sys.time(), "%Y-%m-%d %X", tz = "Europe/Berlin")) %>%
    jsonlite::toJSON(auto_unbox = TRUE)

  write(timestamp, "timestamp.json")
  piggyback::pb_upload("timestamp.json", "bohndesverband/rfl-data", "awards_data", overwrite = TRUE)
} else if (current_week < 14) {
  cli::cli_alert_info("Season is not over yet")
} else {
  cli::cli_alert_info("Already processed player awards data")
}
