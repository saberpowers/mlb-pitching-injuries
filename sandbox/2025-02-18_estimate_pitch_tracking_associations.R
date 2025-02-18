
# Read data from file ----

data_player <- data.table::fread("data/player.csv")

data_season_summary <- data.table::fread("data/pitcher_season_summary.csv") |>
  dplyr::group_by(year, player_id) |>
  dplyr::arrange(-batters_faced) |>
  dplyr::summarize(
    level = level[1],
    games = sum(games),
    batters_faced = sum(batters_faced),
    pitches = sum(pitches),
    .groups = "drop"
  ) |>
  dplyr::mutate(role = ifelse(pitches / games >= 75, "starter", "reliever"))

data_pitch_tracking <- pitchinj::read_data_pitch_tracking()

data_ucl_injury <- pitchinj::read_data_ucl_injury()


# Wrangle data ----

previous_fb_velo <- data_pitch_tracking |>
  dplyr::transmute(player_id, year = year + 1, fb_velo)

data <- data_season_summary |>
  dplyr::left_join(data_player, by = "player_id") |>   # for position
  # Get most recent role if current role is NA
  dplyr::arrange(year) |>
  dplyr::group_by(player_id) |>
  tidyr::fill(role, .direction = "down") |>
  dplyr::ungroup() |>
  dplyr::filter(
    is.na(position) | position == "P",
    (batters_faced >= 27)    # NOTE: Minimum 27 BF to count in denominator
  ) |>
  dplyr::full_join(data_ucl_injury, by = c("year", "player_id"), suffix = c("_pss", "_injury")) |>
  dplyr::filter(
    is.na(level_injury) | level_injury %in% c("Major", "Minor"),
    year > 2008
  ) |>
  dplyr::mutate(batters_faced = dplyr::coalesce(batters_faced, 0)) |>
  dplyr::left_join(previous_fb_velo, by = c("player_id", "year")) |>
  dplyr::transmute(
    year, player_id, name_full, batters_faced, role, fb_velo, date_surgery,
    level = ifelse(level_pss == "MLB", "Majors", "Minors"), level_pss
  )

data |>
  dplyr::filter(year > 2022) |>
  dplyr::group_by(year, level_pss) |>
  dplyr::summarize(mean(is.na(fb_velo)))
