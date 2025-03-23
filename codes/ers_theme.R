#' ERS Theme
#'
#' @examples
#' ggplot2::ggplot() + ERSTheme::ers_theme()
#' @export

ers_theme = function() {
  ggplot2::theme(
    line                  = ggplot2::element_line(
      colour = "black",
      size = 0.5,
      linetype = 1,
      lineend = "butt",
      arrow = FALSE
    ),
    rect                  = ggplot2::element_rect(
      fill = "white",
      colour = NA,
      size = 0.5,
      linetype = 1
    ),
    text                  = ggplot2::element_text(
      family = "sans",
      face = "plain",
      colour = "black",
      size = 9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.title.x          = ggplot2::element_text(
      vjust = 1,
      margin = ggplot2::margin(
        t = 2.75,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.title.x.top      = ggplot2::element_text(
      vjust = 0,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 2.75,
        l = 0,
        unit = "pt"
      )
    ),
    axis.title.y          = ggplot2::element_blank(),
    axis.text.x           = ggplot2::element_text(
      vjust = 1,
      margin = ggplot2::margin(
        t = 2.2,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.text.x.top       = ggplot2::element_text(
      vjust = 0,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 2.2,
        l = 0,
        unit = "pt"
      )
    ),
    axis.text.y           = ggplot2::element_text(
      hjust = 1,
      margin = ggplot2::margin(
        t = 0,
        r = 2.2,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    axis.text.y.right     = ggplot2::element_text(
      hjust = 0,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 2.2,
        unit = "pt"
      )
    ),
    axis.ticks            = ggplot2::element_blank(),
    axis.line             = ggplot2::element_blank(),
    legend.margin         = ggplot2::margin(
      t = 5.5,
      r = 5.5,
      b = 5.5,
      l = 5.5,
      unit = "pt"
    ),
    legend.spacing        = ggplot2::unit(11, units = "pt"),
    legend.key            = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key.size       = ggplot2::unit(1.2, units = "lines"),
    legend.title          = ggplot2::element_text(hjust = 0, size = 9),
    legend.text           = ggplot2::element_text(size = 9),
    legend.position       = "bottom",
    legend.justification  = "center",
    legend.box.margin     = ggplot2::margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0,
      unit = "cm"
    ),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing    = ggplot2::unit(11, units = "pt"),
    panel.background      = NULL,
    panel.border          = ggplot2::element_blank(),
    panel.spacing         = ggplot2::unit(5.5, units = "pt"),
    panel.grid.major.x    = ggplot2::element_blank(),
    panel.grid.minor.x    = ggplot2::element_blank(),
    panel.grid.major.y    = ggplot2::element_line(colour = "grey92"),
    panel.grid.minor.y    = ggplot2::element_line(size = ggplot2::rel(0.5)),
    plot.title            = ggplot2::element_text(
      face = "bold",
      size = 10.5,
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(
        t = 5.5,
        r = 0,
        b = 5.5,
        l = 0,
        unit = "pt"
      )
    ),
    plot.title.position   = "plot",
    plot.subtitle         = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 5,
        l = 0,
        unit = "pt"
      )
    ),
    plot.caption          = ggplot2::element_text(
      size = 8,
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(
        t = 5.5,
        r = 0,
        b = 0,
        l = 0,
        unit = "pt"
      )
    ),
    plot.caption.position = "plot",
    plot.tag              = ggplot2::element_text(
      size = 8,
      hjust = 0,
      vjust = 0.5,
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 10,
        l = 0,
        unit = "pt"
      )
    ),
    plot.tag.position     = c(0, 1),
    plot.margin           = ggplot2::margin(
      t = 10,
      r = 5.5,
      b = 5.5,
      l = 5.5,
      unit = "pt"
    ),
    strip.background      = ggplot2::element_rect(fill = "grey85", colour = "grey20"),
    strip.placement       = "inside",
    strip.text            = ggplot2::element_text(
      colour = "grey10",
      size = ggplot2::rel(0.8),
      margin = ggplot2::margin(
        t = 4.4,
        r = 4.4,
        b = 4.4,
        l = 4.4,
        unit = "pt"
      )
    ),
    strip.text.y          = ggplot2::element_text(angle = -90),
    strip.switch.pad.grid = ggplot2::unit(2.75, units = "pt"),
    strip.switch.pad.wrap = ggplot2::unit(2.75, units = "pt"),
    strip.text.y.left     = ggplot2::element_text(angle = 90)
  )
}
