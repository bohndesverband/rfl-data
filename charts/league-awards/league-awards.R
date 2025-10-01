library(tidyverse)
library(patchwork)
library(camcorder)
library(ggplot2)
library(sysfonts)
library(showtext)
library(geomtextpath)
library(ggimage)
library(cropcircles)
library(ggnewscale)

# data ----
league_awards <- readr::read_csv("league_awards.csv", col_types = "iiccccdddccccdcccc") %>%
  dplyr::mutate(
    category_f= factor(
      category,
      levels = c("points_total_pctl", "points_award_pctl", "points_season_pctl", "war_season_pctl", "war_award_pctl", "war_total_pctl")
    ),
    award_f = factor(award, c("MVP", "OPOY", "DPOY", "CPOY", "APOY", "GPOY", "OROY", "DROY"))
  ) %>%
  dplyr::rename(award_rank = rank)

awards_2022 <- league_awards %>%
  dplyr::filter(season == 2022 & award_rank == 1)
league_position_mvps <- readr::read_csv("league_position_mvps.csv") %>%
  dplyr::rename(award_rank = rank)

# plot content ----
title <- "RFL Player\nAwards 2022"
subtitle <- "Visualization of the most valuable
players in different categories"

copy <- dplyr::tibble(
  content = 'When it comes to player values in fantasy football, the
"wins above replacement" (WAR) metric is very popular.

WAR will tell fantasy managers how many more head-to-head
matchups they should expect to win in a season from starting
a particular player vs. that player’s expected replacement-level
fill-in. (fantasypoints.com)',
  x = 0,
  y = 0.59
)

caption <- 'Twitter: @JakobEschler - Website: jakob-eschler.de - Github: jak3sch'

# start recording ----
camcorder::gg_record(
  dir = file.path("recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 297, # width of saved image
  height = 210, # height of saved image
  units = "mm", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# plot ----
color_bg <- "#222f3e"
color_light <- "#c8d6e5"
color_accent <- "#feca57"
color_muted <- "#425B78"

sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

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

plot_reset <- theme(
  plot.background = element_rect(color = NA, fill = color_bg),
  plot.margin = ggplot2::margin(c(0, 0, 0, 0))
)

plot_default <- list(
  ggplot2::scale_color_manual(values = colors_position, guide = "none"),
  ggplot2::theme_void(),
  ggplot2::theme(
    plot.title = ggplot2::element_text(color = color_accent, family = "accent", hjust = 0.5, size = 42, margin = ggplot2::margin(c(0, 0, 0, 0))),
    plot.subtitle = ggplot2::element_text(color = color_light, family = "base", hjust = 0.5, size = 20, margin = ggplot2::margin(t = 2, b = 4, unit = "mm"), lineheight = 0.4),
    legend.text = ggplot2::element_text(color = color_light, size = 20),
    legend.key.size = unit(3, "mm")
  )
)

plot_default_bar <- list(
  geom_text(mapping = ggplot2::aes(label = war), vjust = 1.7, size = 10, color = color_bg, family = "accent"),
  #geom_text(aes(label = paste0("#", award_rank)), vjust = 5, color = color_bg, size = 6, family = "base"),
  geom_text(aes(label = sapply(display_name, function(x) paste(strwrap(x, width = 5), collapse = "\n"))), vjust = -0.5, hjust = 0.5, size = 7, family = "base", color = color_light, lineheight = 0.3),
  geom_hline(yintercept = 0, color = color_light, size = 0.3),
  nflplotR::geom_nfl_headshots(aes(player_gsis = gsis_id), y = 0, height = 0.25, vjust = 0),
  ggplot2::scale_fill_manual(values = colors_position, guide = "none")
)

# layout ----
## right ----
### league awards ----
#### helper ----
player_annotaions <- function() {
  player_data <- awards_2022 %>%
    dplyr::group_by(gsis_id) %>%
    dplyr::filter(dplyr::row_number() == 1)

  player_annotations <- list(
    # award title
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = award),
      x = 3.5, y = 1.28, hjust = 0.5, color = color_accent, family = "accent", size = 8
    ),
    # player name
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = display_name),
      x = 3.5, y = -3.6, hjust = 0.5, color = color_light, family = "accent", size = 10
    ),
    # award description
    ggplot2::geom_text(
      data = player_data,
      mapping = ggplot2::aes(label = subline),
      x = 3.5, y = -4.1, vjust = 1, hjust = 0.5, color = color_muted, family = "base", size = 8, lineheight = 0.35
    )
  )

  return(player_annotations)
}

segments <- data.frame(
  x1 = c(rep(0.5, 3), 1, 4),
  x2 = c(rep(6.5, 3), 3, 6),
  y1 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  y2 = c(0.25, 0.50, 0.75, 1.35, 1.35),
  color = c(rep(color_bg, 3), rep(color_accent, 2))
)

#### plot ----
league_awards_plot <- ggplot2::ggplot(awards_2022, aes(x = category_f, y = pctl)) +
  ggplot2::facet_wrap(~ award_f, ncol = 4) +
  ggplot2::coord_polar(start = -3.15, clip = "off") +

  ggplot2::geom_col(mapping = ggplot2::aes(alpha = group), fill = color_light, width = 0.97, position = "dodge") +

  # custom grid lines and annotations
  ggplot2::geom_text(label = "FPts", x = 5.85, y = -4.4, color = color_light, size = 5, family = "base", vjust = 1, hjust = 0.5, lineheight = 0.35) +
  ggplot2::geom_text(label = "WAR", x = 6.15, y = 1.4, color = color_light, size = 5, family = "base", vjust = 1, hjust = 0.5, lineheight = 0.35) +
  ggplot2::geom_segment(data = segments, mapping = aes(x = x1, xend = x2, y = y1, yend = y2, color = color), linewidth = 0.3) +
  ggplot2::scale_color_identity()

for (player in unique(league_awards$gsis_id)) {
  league_awards_plot <- league_awards_plot +
    player_annotaions()
}

league_awards_plot <- league_awards_plot +
  ggnewscale::new_scale_color() +

  # player background
  ggplot2::geom_point(mapping = aes(color = pos), y = -1.5, size = 17.85) +

  # player
  ggimage::geom_image(
    data = awards_2022 %>% dplyr::group_by(gsis_id) %>% dplyr::filter(dplyr::row_number() == 1),
    mapping = ggplot2::aes(image = cropcircles::circle_crop(headshot)),
    y = -1.5,
    asp = 1, size = 0.395
  ) +

  ggplot2::scale_alpha_manual(values = c(0.3, 1, 0.6)) +
  ggplot2::scale_y_continuous(limits = c(-1.5, 1.35)) +
  ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 2.5)) +

  ggplot2::labs(
    title = "Player Awards 2022",
    subtitle = "Visualization of the best players in their category. Bars stand for the perecentile,\nin which the player ranks across the top three players (award candidates) since the start of the league.",
    alpha = ""
  ) +

  plot_default +
  ggplot2::theme(
    legend.position = "top",
    strip.text = ggplot2::element_blank(),
    panel.spacing.x = ggplot2::unit(5, "mm"),
    panel.spacing.y = ggplot2::unit(0, "mm"),
  )

### best players ----
position_mvps_plot <- ggplot2::ggplot(league_position_mvps, aes(x = factor(award_rank, levels = c(2,1,3)), y = colHeight)) +
  ggplot2::facet_wrap(~factor(pos, c("QB", "RB", "WR", "TE", "PK", "DL", "LB", "DB")), ncol = 4, strip.position = "bottom") +
  ggplot2::geom_col(aes(fill = pos)) +

  ggplot2::labs(
    title = "The Most Valuable Players 2022",
    subtitle = "The players with the most wins above replacement in their position group."
  ) +

  ggplot2::scale_y_continuous(limits = c(0, 1.2)) +

  plot_default_bar +

  plot_default +
  ggplot2::theme(
    legend.position = "none",
    strip.text = ggplot2::element_text(vjust = 1, hjust = 0.5, color = color_light, family = "accent", size = 20, lineheight = 0.35),
    panel.spacing = ggplot2::unit(5, "mm")
  )

### combine ----
right <- patchwork::wrap_elements(
  league_awards_plot +
    patchwork::plot_spacer() +
    position_mvps_plot +
    patchwork::plot_layout(ncol = 1, heights = c(0.47, 0.06, 0.47)) &
    plot_reset
)


## left ----
### Recent MVPs ----
recent_mvps <- league_awards %>%
  dplyr::filter(award == "MVP" & award_rank == 1) %>%
  dplyr::select(season, gsis_id, display_name, pos, war) %>%
  dplyr::distinct()

recent_mvps_plot <- ggplot2::ggplot(recent_mvps, aes(x = season, y = war, fill = pos)) +
  ggplot2::geom_col() +

  plot_default_bar +

  ggplot2::scale_x_continuous(breaks = c(2016:2022)) +
  ggplot2::scale_y_continuous(limits = c(0, 6)) +

  ggplot2::labs(
    title = "League MVPs 2016-2022",
    subtitle = "The player with the most WAR for each season.",
    caption = caption,
    fill = ""
  ) +

  plot_default +
  ggplot2::theme(
    legend.position = "top",
    legend.margin = ggplot2::margin(t = 3, unit = "mm"),
    axis.text.x = ggplot2::element_text(color = color_light, family = "accent", size = 20),
    plot.caption = ggtext::element_markdown(
      size = 30,
      vjust = 0,
      hjust = 0,
      color = color_light,
      family = "base",
      margin = ggplot2::margin(t = 10, unit = "mm")
    ),
    plot.caption.position = "plot"
  )

### combine ----
left <- patchwork::wrap_elements(
  ggplot(copy, aes(x = x, y = y, label = content)) +
    ggplot2::geom_text(
      size = 11,
      hjust = 0,
      vjust = 1,
      color = color_light,
      family = "base",
      lineheight = 0.4
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 0.7), expand = c(0, 0)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_void() +

    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 120,
        hjust = 0,
        family = "accent",
        color = color_accent,
        lineheight = 0.3,
        margin = ggplot2::margin(b = 8, unit = "mm")
      ),
      plot.subtitle = ggplot2::element_text(
        size = 42,
        family = "base",
        face = "bold",
        hjust = 0,
        lineheight = 0.4,
        color = color_light
      )
    ) +
    recent_mvps_plot +
    patchwork::plot_layout(ncol = 1, heights = c(0.5, 0.5)) &
    plot_reset
)

# final ----
page_layout <- "
  11222
"

final <- left + right +
  patchwork::plot_layout(design = page_layout) +
  patchwork::plot_layout(widths = c(rep(297, 2)), heights = 210) &
  ggplot2::theme(
    plot.margin = ggplot2::margin(c(6, 4.5, 5.7, 5.7), unit = "mm"),
    plot.background = element_rect(color = NA, fill = color_bg)
  )

final

# export ----
camcorder::gg_playback(
  name = "plot.gif",
  device = "png",
  first_image_duration = 5,
  last_image_duration = 10,
  frame_duration = .5,
  background = color_bg,
  image_resize = 800,
  stoprecording = TRUE
)

ggplot2::ggsave("plot.png", final, width = 297, height = 210, units = "mm")
