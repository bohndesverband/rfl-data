library(tidyverse)
library(patchwork)
library(camcorder)
library(ggplot2)
library(sysfonts)
library(showtext)

# data ----
data <- readr::read_csv("data.csv")
super_bowl <- readr::read_csv("super_bowl.csv")

# plot content ----
title <- "RFL Postseason
Participation"
subtitle <- "Visualization of each team's participations
in the RFL postseason between 2016 and 2022"

copy <- dplyr::tibble(
  content = "The Rocketbeans Football League (RFL) consists of 36 teams playing in two
conferences with three divisions each.

The three division winners and the three next best teams play in the Superbowl
for the league title, while the next best 12 teams play in the Probowl and
the 12 worst teams play in the Toiletbowl for the honor.",
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
colors <- c("#f9ca24", "#c8d6e5", "#f0932b", "#425B78")

sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

plot_reset <- theme(
  plot.background = element_rect(color = NA, fill = color_bg),
  plot.margin = ggplot2::margin(c(0, 0, 0, 0))
)

## plot helper ----
plot_default <- list(
  ggplot2::theme_void(),
  ggplot2::labs(
    color = ""
  ),
  ggplot2::theme(
    legend.position = "top",
    legend.text = ggplot2::element_text(color = color_light, size = 16),
    panel.grid.major.x = ggplot2::element_line(color = "#425B78", linewidth = 0.1),
    axis.text.x = ggplot2::element_text(color = color_light, family = "base", size = 16, vjust = -2)
  )
)

# layout ----
## right ----
### helper ----
# render geom_curve with dynamic curvature

# function for a single geom_curve()
curve_geom <- function(franchise_name, xstart, xend, curvature, color) {
  ggplot2::geom_curve(
    # since I use a facet plot, I need the geom only visible in a specific panel
    data = data.frame(franchise_name = {{franchise_name}}),
    aes(x = {{xstart}}, xend = {{xend}}, color = {{color}}),
    y = 0.1, curvature = {{curvature}}, yend = 0.1, linewidth = 0.3)
}

# function to apply on the ggplot
add_curves <- function(franchise_name) {
  franchise_info <- super_bowl %>%
    dplyr::filter(franchise_name == {{franchise_name}})

  # use helper function to create a geom_curve() for each entry in the franchise_info df
  mapply(
    curve_geom,
    franchise_name = {{franchise_name}},
    xstart = franchise_info$season,
    xend = franchise_info$next_appearance,
    curvature = franchise_info$curvature,
    color = franchise_info$result_color
  )
}

### base grid ----
right <- ggplot2::ggplot(super_bowl, aes(x = season, color = result_color, y = 0.1)) +
    ggplot2::facet_wrap(~franchise_name, scales = "free_y", ncol = 2, strip.position = "left", labeller = ggplot2::label_wrap_gen(width = 15))

### add curves ----
for (franchise in unique(super_bowl$franchise_name)) {
  right <- right +
    add_curves(franchise)
}

right <- patchwork::wrap_elements(
  right +
    ggplot2::geom_point(aes(size = po_finish, color = result_color)) +

    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_size_continuous(range = c(1.5, 0.5), guide = "none") +
    ggplot2::scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0)) +

    ggplot2::labs(
      title = "Superbowl Finishes",
      subtitle = "All teams with at least two appearances in the RFL Super Bowl",
    ) +

    plot_default +
    plot_reset +
    ggplot2::theme(
      plot.margin = ggplot2::margin(l = 5, unit = "mm"),
      plot.title = ggplot2::element_text(color = color_accent, family = "accent", hjust = 0.5, size = 42, margin = ggplot2::margin(c(0, 0, 0, 0))),
      plot.subtitle = ggplot2::element_text(color = color_light, family = "base", hjust = 0.5, size = 20, margin = ggplot2::margin(t = 2, b = 4, unit = "mm")),
      strip.text.y.left = ggplot2::element_text(angle = 90, color = color_light, family = "accent", size = 14, lineheight = 0.35),
      panel.spacing.x = ggplot2::unit(8, "mm"),
      panel.spacing.y = ggplot2::unit(5, "mm"),
    )
)

## left ----
### other bowls ----
other_bowls <- ggplot(data = subset(data, !(franchise_name %in% unique(super_bowl$franchise_name))), aes(x = season, y = franchise_name, size = po_finish, color = factor(bowl, levels = c("SB", "PB", "TB")))) +
  ggplot2::geom_rect(xmin = 2015, xmax = 2016.8, ymin = 0, ymax = 36, fill = color_bg, color = NA) +

  ggplot2::geom_curve(data = subset(data, season == 2021 & bowl == "PB" & po_finish == min(po_finish)), aes(xend = season - 0.5, y = 16.5, yend = 12), curvature = 0.5, size = 0.3, color = color_light) +
  ggplot2::geom_point(position = ggplot2::position_nudge(y = 0.5)) +
  ggplot2::geom_text(
    data = subset(data, season == 2021 & bowl == "PB" & po_finish == min(po_finish)),
    aes(x = season - 0.5), y = 10,
    label = "Size of points\nstands for\nplacement in Bowl",
    size = 6, lineheight = 0.4, color = color_light, angle = 10, show.legend = FALSE) +

  ggplot2::scale_color_manual(labels = c("Super Bowl", "Pro Bowl", "Toilet Bowl"), values = c("#ff9f43", "#2e86de", "#ee5253")) +
  ggplot2::scale_size_continuous(range = c(2, 0.5), guide = "none") +
  ggplot2::scale_x_continuous(limits = c(2015.8, 2022), expand = c(0, 0.05), breaks = 2017:2022) +

  ggplot2::labs(
    caption = caption
  ) +

  plot_default +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(color = color_light, size = 14, hjust = 1, vjust = -1.3, margin = ggplot2::margin(r = -1.9, unit = "cm"), family = "accent"),
    panel.grid.major.y = ggplot2::element_line(color = "#425B78", linewidth = 0.1),
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
    other_bowls +
    patchwork::plot_layout(ncol = 1, heights = c(4, 7)) &
    plot_reset
)

# final ----
page_layout <- "
  1111222222
"

final <- left + right +
  patchwork::plot_layout(heights = c(1, 1)) +
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
