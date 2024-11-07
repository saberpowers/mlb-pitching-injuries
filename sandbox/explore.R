
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

biomechanics <- data.table::fread("data/biomechanics/2024.csv")
pitch <- data.table::fread("data/pitch/2024.csv")
event <- data.table::fread("data/event/2024.csv")

data <- pitch |>
  dplyr::left_join(event, by = c("game_id", "year", "event_index")) |>
  dplyr::left_join(
    y = biomechanics,
    by = c("play_id" = "org_movement_id"),
    suffix = c("_pitch", "_biomech")
  )

primary_pitch <- data |>
  dplyr::count(pitcher_id, pitch_type) |>
  dplyr::arrange(-n) |>
  dplyr::group_by(pitcher_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  # only include pitchers whose primary pitch is a fastball (cutters don't count)
  dplyr::filter(pitch_type %in% c("FF", "SI")) |>
  # probably only starters with reasonably large samples
  dplyr::filter(n > 250) |>
  dplyr::select(pitcher_id, pitch_type)

pitcher_inning <- data |>
  dplyr::inner_join(primary_pitch, by = c("pitcher_id", "pitch_type")) |>
  dplyr::filter(!is.na(movement_num)) |>
  dplyr::group_by(pitcher_id, inning) |>
  dplyr::summarize(
#    pitches = dplyr::n(),
    dplyr::across(dplyr::ends_with("proj_max"), c(mean = mean)),
    .groups = "drop"
  )

{
  pdf("~/Downloads/temp.pdf")
  figure <- pitcher_inning |>
    dplyr::filter(inning %in% c(1, 5)) |>
    dplyr::mutate(inning = paste0("inning", inning)) |>
    tidyr::pivot_longer(cols = dplyr::ends_with("_mean"), names_to = "metric") |>
    tidyr::pivot_wider(names_from = inning, values_from = value) |>
    ggplot2::ggplot(ggplot2::aes(x = inning1, y = inning5)) +
    ggplot2::facet_wrap(~ metric, ncol = 3, scales = "free") +
    ggplot2::geom_point(color = color_fg, alpha = 0.5) +
    ggplot2::geom_abline(color = color_fg) +
    ggplot2::labs(
      title = "Average momentum by inning",
      subtitle = "Primary FF/SI only, minimum 250 pitches",
      x = "1st Inning",
      y = "5th Inning"
    ) +
    theme_sleek()
  print(figure)
  dev.off()
}
