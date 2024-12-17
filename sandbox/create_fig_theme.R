
color_fg <- "#93a1a1"
color_bg <- "#002B37"
color_base <- "#073642"
color_base2 <- "#eee8d5"
color_red <- "#dc322f"
color_orange <- "#cb4b16"
color_yellow <- "#b58900"
color_green <- "#859900"
color_blue <- "#268bd2"

theme_sleek <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = color_fg),
      axis.ticks = ggplot2::element_line(color = color_fg),
      axis.text = ggplot2::element_text(color = color_fg),
      strip.text = ggplot2::element_text(color = color_fg),
      text = ggplot2::element_text(color = color_fg),
      legend.background = ggplot2::element_rect(fill = NA),
      panel.background = ggplot2::element_rect(fill = color_bg),
      plot.background = ggplot2::element_rect(fill = color_bg, color = color_bg),
      strip.background = ggplot2::element_rect(fill = color_bg, color = color_bg)
    )
}

