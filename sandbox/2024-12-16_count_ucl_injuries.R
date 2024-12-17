
biomechanics <- data.table::fread("data/biomechanics/2024.csv")
pitch <- data.table::fread("data/pitch/2024.csv")
event <- data.table::fread("data/event/2024.csv")
game <- data.table::fread("data/game/2024.csv")
tommy_john <- data.table::fread("data/injury/tommy_john.csv") |>
  dplyr::mutate(surgery = "tommy_john")
internal_brace <- data.table::fread("data/injury/internal_brace.csv") |>
  dplyr::mutate(surgery = "internal_brace")
player <- data.table::fread("data/player.csv")

injury <- tommy_john |>
  dplyr::filter(position == "P") |>
  dplyr::bind_rows(internal_brace) |>
  dplyr::mutate(
    date_surgery = as.Date(date_surgery, format = "%m/%d/%y"),
    year = lubridate::year(date_surgery)
  )

{
  pdf("fig/ucl_surgeries.pdf", height = 5, width = 8)
  plot <- injury |>
    dplyr::filter(year > 2009) |>
    dplyr::mutate(level = ifelse(level == "MLB", "MLB", "Minors/Amateur")) |>
    ggplot2::ggplot(ggplot2::aes(x = year, fill = level, color = level)) +
    ggplot2::geom_bar(color = color_bg, position = "stack") +
    ggplot2::scale_x_continuous(breaks = 2010:2024, name = "Year") +
    ggplot2::scale_y_continuous(limits = c(0, 200), name = "") +
    ggplot2::scale_fill_manual(name = "Level", values = c(color_fg, color_blue)) +
    ggplot2::scale_color_manual(name = "Level", values = c(color_fg, color_bg)) +
    ggplot2::stat_bin(
      ggplot2::aes(y = ggplot2::after_stat(count), label = ggplot2::after_stat(count)),
      bins = 15,
      geom = "text",
      vjust = -1
    ) +
    ggplot2::labs(
      title = "Total UCL Surgeries",
      subtitle = "Tommy John and Internal Brace, Major + Minor + some amateur",
      caption = "Data: Tommy John Surgery List (@MLBPlayerAnalys)"
    ) +
    theme_sleek() +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.15, 0.85))
  print(plot)
  dev.off()
}

data <- pitch |>
  sabRmetrics::get_quadratic_coef() |>
  sabRmetrics::get_trackman_metrics() |>
  dplyr::left_join(game, by = "game_id") |>
  dplyr::left_join(event, by = c("game_id", "event_index")) |>
  dplyr::left_join(
    y = biomechanics,
    by = c("play_id" = "org_movement_id"),
    suffix = c("_pitch", "_biomech")
  ) |>
  dplyr::mutate(release_speed_mph = 0.681818 * release_speed) |>  # convert feet per second to mph
  # Calculate the pitch number of each pitch within each pitcher-game
  dplyr::group_by(game_id, pitcher_id) |>
  dplyr::arrange(game_id, event_index, play_index) |>
  dplyr::mutate(pitch_number = 1:dplyr::n()) |>
  dplyr::ungroup()

primary_pitch <- data |>
  dplyr::count(pitcher_id, pitch_type) |>
  dplyr::arrange(-n) |>
  dplyr::group_by(pitcher_id) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  # only include pitchers whose primary pitch is a fastball (cutters don't count)
  dplyr::filter(pitch_type %in% c("FF", "SI")) |>
  dplyr::select(pitcher_id, pitch_type)

starter <- data |>
  dplyr::group_by(pitcher_id) |>
  dplyr::summarize(
    pitches = dplyr::n(),
    games = length(unique(game_id)),
    pitches_per_game = pitches / games,
    .groups = "drop"
  ) |>
  dplyr::filter(pitches_per_game >= 75)

model_data <- data |>
  dplyr::inner_join(primary_pitch, by = c("pitcher_id", "pitch_type")) |>
  dplyr::inner_join(starter, by = "pitcher_id")

model_fit <- lme4::lmer(
  formula = release_speed_mph ~ pitch_number + (1 + pitch_number | pitcher_id) + (1 + pitch_number | pitcher_id:game_id),
  data = model_data
)

fixef <- lme4::fixef(model_fit)

pitcher_ranef <- lme4::ranef(model_fit)$pitcher_id |>
  tibble::as_tibble(rownames = "player_id") |>
  dplyr::mutate(player_id = as.integer(player_id)) |>
  dplyr::left_join(player, by = "player_id") |>
  dplyr::select(player_id, name_full, intercept = `(Intercept)`, slope = pitch_number)

game_ranef <- lme4::ranef(model_fit)[["pitcher_id:game_id"]] |>
  tibble::as_tibble(rownames = "pitcher_game") |>
  dplyr::mutate(
    player_id = stringr::str_split(pitcher_game, pattern = ":") |>
      sapply(FUN = function(x) x[1]) |>
      as.integer(),
    game_id = stringr::str_split(pitcher_game, pattern = ":") |>
      sapply(FUN = function(x) x[2]) |>
      as.integer()
  ) |>
  dplyr::left_join(player, by = "player_id") |>
  dplyr::left_join(game, by = "game_id") |>
  dplyr::select(player_id, name_full, date, intercept = `(Intercept)`, slope = pitch_number) |>
  dplyr::left_join(
    pitcher_ranef,
    by = c("player_id", "name_full"), suffix = c("_game", "_pitcher")
  ) |>
  dplyr::mutate(
    intercept_total = fixef["(Intercept)"] + intercept_pitcher + intercept_game,
    slope_total = fixef["pitch_number"] + slope_pitcher + slope_game
  )

{
  pitcher <- 641482
  pitcher_name <- player |>
    dplyr::filter(player_id == pitcher) |>
    dplyr::pull(name_full)
  game_ranef_filtered <- game_ranef |>
    dplyr::filter(player_id == pitcher)
  pdf("~/Downloads/temp.pdf")
  plot <- model_data |>
    dplyr::filter(pitcher_id == pitcher) |>
    ggplot2::ggplot(ggplot2::aes(x = pitch_number, y = release_speed_mph)) +
    ggplot2::geom_point(color = color_fg, alpha = 0.5) +
    ggplot2::facet_wrap(ggplot2::vars(date)) +
    ggplot2::geom_abline(
      data = game_ranef_filtered,
      ggplot2::aes(slope = slope_total, intercept = intercept_total),
      color = color_fg
    ) +
    ggplot2::labs(title = pitcher_name, x = "Pitch Number", y = "Fastball Velocity") +
    theme_sleek()
  print(plot)
  dev.off()
}
