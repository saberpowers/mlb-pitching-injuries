
source("sandbox/create_fig_theme.R")

player <- data.table::fread("data/player.csv")

pitcher_season_summary <- data.table::fread("data/pitcher_season_summary.csv") |>
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

tommy_john_public <- data.table::fread("data/ucl_injury_public/tommy_john.csv") |>
  dplyr::mutate(surgery = "tommy_john")
internal_brace_public <- data.table::fread("data/ucl_injury_public/internal_brace.csv") |>
  dplyr::mutate(surgery = "internal_brace")
ucl_injury_public <- tommy_john_public |>
  dplyr::filter(position == "P") |>
  dplyr::bind_rows(internal_brace_public) |>
  dplyr::mutate(
    date_surgery = as.Date(date_surgery, format = "%m/%d/%y"),
    year = lubridate::year(date_surgery)
  ) |>
  dplyr::select(player_id, level, date_surgery, surgery, year)

ucl_injury_secret <- data.table::fread("data/ucl_injury.csv") |>
  dplyr::mutate(date_surgery = as.Date(date_surgery, format = "%m/%d/%y"))

ucl_injury <- ucl_injury_secret |>
  dplyr::full_join(ucl_injury_public,
    by = c("player_id", "year"),
    suffix = c("_secret", "_public")
  ) |>
  dplyr::transmute(
    player_id, year,
    date_surgery = dplyr::coalesce(date_surgery_secret, date_surgery_public),
    level = dplyr::case_when(
      !is.na(level_secret) ~ level_secret,
      level_public == "MLB" ~ "Major",
      level_public %in% c("AAA", "AA", "A+", "A", "A-", "Rk") ~ "Minor",
      level_public %in% c("Coll", "HS") ~ "Amateur"
    )
  )

previous_year_pitches <- pitcher_season_summary |>
  dplyr::transmute(year = year + 1, player_id, previous_pitches = pitches)

data <- pitcher_season_summary |>
  dplyr::left_join(player, by = "player_id") |>   # for position
  # Get most recent role if current role is NA
  dplyr::arrange(year) |>
  dplyr::group_by(player_id) |>
  tidyr::fill(role, .direction = "down") |>
  dplyr::ungroup() |>
  dplyr::filter(
    is.na(position) | position == "P",
    (batters_faced >= 27)    # NOTE: Minimum 27 BF to count in denominator
  ) |>
  dplyr::full_join(ucl_injury, by = c("year", "player_id"), suffix = c("_pss", "_injury")) |>
  dplyr::left_join(previous_injury, by = c("year", "player_id")) |>
  dplyr::filter(
    is.na(level_injury) | level_injury %in% c("Major", "Minor"),
    year > 2008
  ) |>
  dplyr::mutate(batters_faced = dplyr::coalesce(batters_faced, 0)) |>
  dplyr::transmute(
    year, player_id, name_full, batters_faced, role, date_surgery,
    level = ifelse(level_pss == "MLB", "Majors", "Minors"), previous_pitches
  )

data |>
  dplyr::count(year, level, injury = !is.na(date_surgery)) |>
  tidyr::pivot_wider(names_from = injury, values_from = n) |>
  dplyr::mutate(count = `TRUE` + `FALSE`, injury_rate = `TRUE` / count) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = injury_rate, fill = level)) +
  ggplot2::geom_col(position = "dodge")

{
  pdf("fig/survival_curve_by_role.pdf")
  plot <- ggsurvfit::survfit2(
    formula = survival::Surv(time = batters_faced, event = !is.na(date_surgery)) ~ role,
    data = data
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggplot2::scale_color_manual(values = c(color_blue, color_orange)) +
    ggplot2::scale_fill_manual(values = c(color_blue, color_orange)) +
    theme_sleek()
  print(plot)
  dev.off()
}

{
  pdf("fig/temp.pdf")
  plot <- ggsurvfit::survfit2(
    survival::coxph(
      formula = survival::Surv(time = batters_faced, event = !is.na(date_surgery)) ~ role + previous_pitches,
      data = data
    )
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval()
  print(plot)
  dev.off()
}

{
  pdf("fig/survival_curve_by_level.pdf")
  plot <- ggsurvfit::survfit2(
    formula = survival::Surv(time = batters_faced, event = !is.na(date_surgery)) ~ level,
    data = data
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggplot2::scale_color_manual(values = c(color_blue, color_orange)) +
    ggplot2::scale_fill_manual(values = c(color_blue, color_orange)) +
    theme_sleek()
  print(plot)
  dev.off()
}

{
  pdf("fig/survival_curve_by_year.pdf")
  plot <- ggsurvfit::survfit2(
    formula = survival::Surv(time = batters_faced, event = !is.na(date_surgery)) ~ level,
    data = data
  ) |>
    ggsurvfit::ggsurvfit() +
    ggsurvfit::add_confidence_interval() +
    ggplot2::scale_color_manual(values = c(color_blue, color_orange)) +
    ggplot2::scale_fill_manual(values = c(color_blue, color_orange)) +
    theme_sleek()
  print(plot)
  dev.off()
}
