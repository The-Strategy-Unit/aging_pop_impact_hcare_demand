# README
# Custom ggplot2 theme for this project


# packages ----
library("ggplot2")

# theme_1045 ----
# @param base_size, default font size
# @param base_family, default font family
theme_1045 <- function(base_size = 10, base_family = "Fira Sans") {

  half_line <- base_size / 2

  theme(
    # elements in this first block aren't used directly,
    # but are inherited by others
    line = element_line(
      colour = "#2c2825",
      linewidth = .5,
      linetype = 1,
      lineend = "butt",
      arrow = NULL,
      inherit.blank = FALSE
    ),
    rect = element_rect(
      fill = "#ffffff",
      colour = "#2c2825",
      linewidth = .5,
      linetype = 1,
      inherit.blank = FALSE
    ),
    text = element_text(
      family = base_family,
      face = "plain",
      colour = "#686f73",
      size = base_size,
      hjust = .5,
      vjust = .5,
      angle = 0,
      lineheight = .9,
      margin = margin(),
      debug = FALSE,
      inherit.blank = FALSE
    ),

    # axis title
    axis.title = element_text(),
    axis.title.x = element_text(
      vjust = 1,
      margin = margin(t = half_line)
    ),
    axis.title.y = element_text(
      vjust = 1,
      angle = 90,
      margin = margin(r = half_line)
    ),
    axis.title.x.top = element_text(
      vjust = 0,
      margin = margin(b = half_line)
    ),
    axis.title.y.right = element_text(
      vjust = 0,
      angle = -90,
      margin = margin(l = half_line)
    ),
    # axis line
    axis.line = element_line(linewidth = .2, color = "#d3d3d3"),
    axis.line.x = element_line(linewidth = .2, color = "#d3d3d3"),
    axis.line.y = element_blank(),
    # axis ticks
    axis.ticks = element_line(linewidth = .4, colour = "#686f73"),
    axis.ticks.x = element_line(linewidth = .4, color = "#d3d3d3"),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(2, "mm"),
    # axis text
    axis.text = element_text(
      colour = "#686f73",
      size = base_size,
      hjust = 0
    ),
    axis.text.x = element_text(
      hjust = .5,
      vjust = .5,
      margin = margin(t = .8 * half_line / 2)
    ),
    axis.text.y = element_text(
      hjust = 1,
      margin = margin(r = .8 * half_line / 2)
    ),
    axis.text.x.top = element_text(
      hjust = .5,
      vjust = 0,
      margin = margin(b = .8 * half_line / 2)
    ),
    axis.text.y.right = element_text(
      hjust = 0,
      margin = margin(l = .8 * half_line / 2)
    ),

    # legend
    legend.background = element_rect(fill = NA, colour = NA),
    legend.margin = margin(2, 2, 2, 2, "mm"),
    legend.spacing = unit(4, "mm"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.key = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(1, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(size = base_size),
    legend.title = element_text(hjust = 0),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "centre",
    legend.box = NULL,
    legend.box.margin = margin(0, 0, 0, 0, "mm"),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(4, "mm"),

    # panel
    panel.border = element_blank(),
    panel.spacing = unit(2, "mm"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#d3d3d3", linewidth = .2),
    panel.grid.minor = element_blank(),
    panel.ontop = FALSE,
    panel.background = element_rect(fill = "#f8f8f7", colour = NA),

    # strip
    strip.background = element_rect(fill = "#e7e7e7", color = NA),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.text = element_text(
      colour = "#2c2825",
      size = base_size,
      hjust = 0,
      vjust = .5,
      margin = margin(
        t = half_line * .5,
        b = half_line * .5,
        l = half_line * .5
      )
    ),
    strip.switch.pad.grid = unit(2, "mm"),
    strip.switch.pad.wrap = unit(2, "mm"),

    # plot
    # plot.background = element_blank(),
    plot.background = element_rect(fill = "#f8f8f7", colour = NA),
    plot.title = element_text(
      family = "Fira Sans Medium",
      size = base_size * 1.3,
      hjust = 0,
      vjust = 1,
      lineheight = 1.2,
      margin = margin(b = half_line * 2)
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      colour = "#686f73",
      size = base_size,
      hjust = 0,
      vjust = 1,
      margin = margin(b = half_line * 2)
    ),
    plot.caption = element_text(
      colour = "#686f73",
      size = base_size,
      hjust = 0,
      vjust = 1,
      margin = margin(t = half_line * 2)
    ),
    plot.caption.position = "plot",
    plot.margin = margin(half_line, half_line, half_line, half_line),
    complete = TRUE
  )
}
