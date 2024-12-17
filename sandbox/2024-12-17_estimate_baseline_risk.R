
tommy_john <- data.table::fread("data/injury/tommy_john.csv") |>
  dplyr::mutate(surgery = "tommy_john")
internal_brace <- data.table::fread("data/injury/internal_brace.csv") |>
  dplyr::mutate(surgery = "internal_brace")
player <- data.table::fread("data/player.csv")

pitcher_season_summary <- NULL
pss_files <- list.files("data/pitcher_season_summary")
for (file in pss_files) {
  pitcher_season_summary <- data.table::fread(file.path("data/pitcher_season_summary", file)) |>
    tibble::add_column(year = as.integer(substring(file, 1, 4)), .before = 1) |>
    dplyr::bind_rows(pitcher_season_summary)
}

injury <- tommy_john |>
  dplyr::filter(position == "P") |>
  dplyr::bind_rows(internal_brace) |>
  dplyr::mutate(
    date_surgery = as.Date(date_surgery, format = "%m/%d/%y"),
    year = lubridate::year(date_surgery)
  ) |>
  dplyr::select(player_id, level, date_surgery, surgery, year)

data <- pitcher_season_summary |>
  dplyr::full_join(injury, by = c("year", "pitcher_id" = "player_id")) |>
  dplyr::left_join(player, by = c("pitcher_id" = "player_id")) |>   # for position
  # Get most recent role if current role is NA
  dplyr::arrange(year) |>
  dplyr::group_by(pitcher_id) |>
  tidyr::fill(role, .direction = "down") |>
  dplyr::ungroup() |>
  dplyr::filter(
    position == "P",
    year >= min(pitcher_season_summary$year),
    (level == "MLB") | (batters_faced >= 27)    # NOTE: Minimum 27 BF to count in denominator
  ) |>
  dplyr::select(year, pitcher_id, name_full, batters_faced, role, level, date_surgery)

summary <- data |>
  dplyr::count(year, role, injury = !is.na(date_surgery)) |>
  tidyr::pivot_wider(names_from = injury, values_from = n) |>
  dplyr::mutate(count = `TRUE` + `FALSE`, injury_rate = `TRUE` / count) |>
  dplyr::select(year, role, count, injury_rate) |>
  dplyr::arrange(role)

knitr::kable(summary, digits = 3)
