library(tidyverse)
library(patchwork)
library(camcorder)
library(ggtext)
library(ggplot2)
library(treemapify)
library(sysfonts)
library(showtext)
library(gt)
library(gtExtras)
library(webshot2)

# data ----
data <- readr::read_csv("data.csv") %>%
  dplyr::group_by(unit) %>%
  dplyr::group_modify(~ dplyr::add_row(.x, .before = 0)) %>%
  dplyr::mutate(season = ifelse(dplyr::row_number() == 1, 2015, season))
annotations <- readr::read_csv("annotations.csv")

title <- "Personnel Groupings
used by Jena Dragons"
subtitle <- "Visualization of the used fantasy football lineup
variations between 2016 and 2022"

copy <- dplyr::tibble(
  content = "Since the launch of the Rocketbeans Football League (RFL) in 2016, numerous
matchups have been played. The lineup rules call for 1 QB, 2 RB, 2 WR, 1 TE,
1 PK and 2 flex spots for the offense. For the defense, 2 DL (DT and DE), 2 LB,
and 2 DB (CB and S) positions are required, as well as 3 flex spots.

These requirements provide ample room for individual decisions, which I
wanted to visualize. The result will be part of the comprehensive league report
for last season.",
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
colors <- c("#2e86de", "#ee5253", "#10ac84",  "#ff9f43")
colors_shape <- c("#ff9ff3", "#5f27cd", "#feca57", "#54a0ff")

sysfonts::font_add_google("Vidaloka", family = "accent")
sysfonts::font_add_google("Open Sans", family = "base", bold.wt = 800)
showtext::showtext_auto()

plot_reset <- theme(
  plot.background = element_rect(color = NA, fill = color_bg),
  plot.margin = ggplot2::margin(c(0, 0, 0, 0))
)

## plot helper ----
plot <- function(data) {
  ggplot2::ggplot(data, aes(area = count, fill = as.factor(grouping_order), subgroup = grouping, subgroup2 = personnel)) +
    ggplot2::facet_wrap(~season, ncol = 4) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_subgroup2_border(color = color_bg, size = 3) +

    ggplot2::geom_text(data = subset(data, season != 2015), aes(label = season), x = 0.05, y = 0.05, vjust = 0, hjust = 0, family = "base", fontface = "bold", color = color_bg, size = 4.5) +
    ggplot2::geom_text(data = subset(data, xmax - xmin > 0.1), aes(label = ifelse(count_above_league_avg > 0, paste0("+", count_above_league_avg), count_above_league_avg), x = xmax - 0.05, y = ymax - 0.05), hjust = 1, vjust = 1, family = "base", fontface = "bold", color = color_bg, size = 4.5) +

    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_color_manual(values = colors_shape) +
    ggplot2::scale_shape_manual(values = c(17, 15, 19, 17)) +
    ggplot2::guides(fill = "none", color = "none", shape = guide_legend()) +
    ggplot2::coord_fixed() +

    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +

    ggplot2::theme_void() +
    plot_reset +
    ggplot2::theme (
      legend.position = "none",
      panel.spacing = ggplot2::unit(3, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        size = 22,
        color = color_accent,
        family = "accent"
      )
    )
}

## add points helper ----
points <- function(data, pos) {
  geom_point <- ggplot2::geom_point(
    data = subset({{data}}, position == {{pos}} & is_to_big < 1),
    aes(x = xoffset, y = yoffset, shape = position, color = as.factor(grouping_order)),
    size = 1.3,
    alpha = 0.9
  )
}

## plots ----
offense <- plot(data = subset(data, unit == "offense"))
defense <- plot(data = subset(data, unit == "defense"))

example_data <- data %>%
  dplyr::filter(
    unit == "defense" & season == 2016
  )

example <- plot(subset(example_data, unit == "defense" & season == 2016)) +
  ggplot2::theme(
    plot.margin = ggplot2::margin(c(0, 0, 0, 0)),
    plot.background = element_blank()
  )

for(pos in as.vector(unique(annotations$position))) {
  offense <- offense +
    points(
      data = subset(annotations, unit == "offense"),
      pos = pos
    )

  defense <- defense +
    points(
      data = subset(annotations, unit == "defense"),
      pos = pos
    )

  example <- example +
    points(
      data = subset(annotations, unit == "defense" & season == 2016),
      pos = pos
    )
}

# layout ----
## right ----
### tables ----
gt_default <- function(df) {
  df %>%
    gt::gt(rowname_col = "personnel") %>%
    gt::cols_hide(grouping) %>%
    gt::tab_options(
      table.background.color = color_bg,
      table.font.names = "Open Sans",
      table.border.top.style = "hidden",
      table.border.right.style = "hidden",
      table.border.bottom.style = "hidden",
      table.border.left.style = "hidden",
      column_labels.hidden = TRUE
    ) %>%
    gt::data_color(
      method = "numeric",
      palette = c(color_bg, color_accent)
    )
}

gt_to_png <- function(filter) {
  gt <- table_data %>%
    dplyr::filter(grouping %in% {{filter}}) %>%
    gt_default()

  tmp <- tempfile(fileext = '.png') #generate path to temp .png file
  gt::gtsave(gt, tmp, expand = 0, vwidth = 550) #save gt table as png
  table_png <- png::readPNG(tmp, native = TRUE) # read tmp png file

  return(table_png)
}

table_data <- data %>%
  dplyr::group_by(unit, grouping, personnel) %>%
  dplyr::summarise(count = sum(count), .groups = "drop") %>%
  dplyr::group_by(unit, grouping) %>%
  dplyr::arrange(dplyr::desc(count)) %>%
  dplyr::mutate(
    unit = stringr::str_to_title(unit),
    grouping_clean = dplyr::case_when(
      grouping %in% c("3", "4") ~ paste(grouping, "WR"),
      grouping == "252" ~ "2DL, 5LB, 2 DB",
      grouping == "243" ~ "2DL, 4LB, 3 DB",
      grouping == "234" ~ "2DL, 3LB, 4 DB",
      grouping == "225" ~ "2DL, 2LB, 5 DB",
    )
  ) %>%
  dplyr::group_by(unit, grouping_clean)


table_1_img <- gt_to_png(c("4", "3"))
table_2_img <- gt_to_png(c("252"))
table_3_img <- gt_to_png(c("243"))
table_4_img <- gt_to_png(c("234", "225"))

table_layout <- "
  1356
  1356
  1356
  1356
  1356
  1456
  2456
"

tables <- patchwork::wrap_elements(table_1_img) +
  patchwork::plot_spacer() +
  table_2_img +
  patchwork::plot_spacer() +
  table_3_img +
  table_4_img +
  patchwork::plot_layout(design = table_layout) &
  plot_reset

### combine ----
grid_subtitle <- function(text) {
  grid::grobTree(
    grid::textGrob(
      label = {{text}},
      vjust = 1,
      hjust = 0.55,
      gp = grid::gpar(col = color_accent, fontsize = 40, fontfamily = "accent", fill = color_bg, lwd = 0)
    )
  )
}

right <- ggplot2::ggplot() +
  ggplot2::theme_void() +
  offense +
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  defense +
  patchwork::plot_layout(ncol = 1, heights = c(0.17, 0.4095, 0.0005, 0.4095)) +
  patchwork::inset_element(patchwork::wrap_elements(grid_subtitle("Offense")), top = 2.35, left = 0, right = 0.25, bottom = 2) +
  patchwork::inset_element(patchwork::wrap_elements(grid_subtitle("Defense")), top = 1.06, left = 0, right = 0.25, bottom = 0.5)

## left ----
### guide ----
example_annotation <- function(xstart, xend, ystart, yend, curve = -0.5, shape = 19, size = 1.5, stroke = 0, color = color_light, fill = NA, label = "Text", ...) {
  if(curve > 0 & xstart > xend) {
    hjust <- 1
    labelx <- xend - 0.07
  } else {
    hjust = 0
    labelx <- xend + 0.07
  }

  list(
    ggplot2::geom_point(aes(x = xstart, y = ystart), shape = shape, size = size, color = color, stroke = stroke),
    ggplot2::geom_curve(aes(x = xstart, xend = xend, y = ystart, yend = yend), curvature = curve, color = color_light, linewidth = 0.3),
    ggplot2::geom_text(aes(x = labelx, y = yend), label = label, color = color_light, size = 6, lineheight = 0.45, family = "base", vjust = 0.5, hjust = hjust, ...)

  )
}

guides <- ggplot2::ggplot() +
  # color annotation
  example_annotation(
    xstart = 2,
    xend = 2.2,
    ystart = 0.65,
    yend = 0.85,
    label = "Colors stand for the grouping.\nFor offense that means number of started WR,\nfor the defense it is a 3 digit code consisting\nof the number of started DL, LB and DB (eg. 252)"
  ) +

  # tiles annotation
  example_annotation(
    xstart = 2.39,
    xend = 2.65,
    ystart = 0.5,
    yend = 0.4,
    size = 6,
    curve = -0.2,
    label = "Tiles stand for started personnel."
  ) +

  # symbols annotation
  example_annotation(
    xstart = 2.05,
    xend = 1.8,
    ystart = 0.48,
    yend = 0.6,
    curve = 0.3,
    label = "Symbols on the largest areas illustrate the personnel.\nFrom top to bottom:\nOffense: WR, TE, RB\nDefense: S, CB, LB, DE, DT\nThe number of symbols in one row stands for the\nnumber of started players of that position."
  ) +

  # season annotation
  example_annotation(
    xstart = 2.07,
    xend = 2.2,
    ystart = 0.39,
    yend = 0.2,
    curve = 0.4,
    label = "Number stands\nfor the played\nseason."
  ) +

  # abv. league avg annotation
  example_annotation(
    xstart = 2.4,
    xend = 2.6,
    ystart = 0.67,
    yend = 0.6,
    curve = 0.4,
    label = "Number stands for the started\npersonnel above league average."
  ) +

  ggplot2::scale_x_continuous(limits = c(0, 4), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +

  ggplot2::labs(
    caption = caption
  ) +

  ggplot2::theme_void() +
  ggplot2::theme(
    plot.caption = ggtext::element_markdown(
      size = 30,
      vjust = 0,
      hjust = 0,
      color = color_light,
      family = "base",
    )
  ) +
  patchwork::inset_element(example, top = 0.7, bottom = 0.37, left = 0.1, right = 1, align_to = "plot", on_top = FALSE)

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
    ggplot2::scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0)) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::theme_void() +

    ggplot2::theme(
      plot.margin = ggplot2::margin(0),
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

    guides +
    patchwork::plot_layout(ncol = 1, heights = c(1, 1)) &
    plot_reset
)

# final ----
page_layout <- "
  111222
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
