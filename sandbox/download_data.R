
cluster <- parallel::makeCluster(parallel::detectCores() - 1)

for (year in 2024:2010) {

  data_baseballsavant <- sabRmetrics::download_baseballsavant(
    start_date = glue::glue("{year}-01-01"),
    end_date = glue::glue("{year}-12-31"),
    cl = cluster
  )

  data_batter <- data_baseballsavant |>
    dplyr::group_by(batter_id) |>
    dplyr::summarize(
      plate_appearances = length(unique(paste(game_id, event_index))),
      .groups = "drop"
    )

  data_pitcher <- data_baseballsavant |>
    dplyr::group_by(pitcher_id) |>
    dplyr::summarize(
      games = length(unique(game_id)),
      batters_faced = length(unique(paste(game_id, event_index))),
      pitches = dplyr::n(),
      .groups = "drop"
    ) |>
    # Remove position players pitching
    dplyr::left_join(data_batter, by = c("pitcher_id" = "batter_id")) |>
    dplyr::filter(is.na(plate_appearances) | (batters_faced > plate_appearances)) |>
    dplyr::transmute(
      pitcher_id, games, batters_faced, pitches,
      # We see a lot of pitchers who average a little over 90 pitches per game, and we see a handful
      # of pitchers who average under 60 pitches per game (likely sometimes starting, sometimes
      # relieving). The 75-pitch threshold is intended to discriminate between these two groups.
      role = ifelse(pitches / games > 75, "starter", "reliever")
    )

  write.csv(
    data_pitcher,
    file = glue::glue("data/pitcher_season_summary/{year}.csv"),
    row.names = FALSE
  )

}

parallel::stopCluster(cluster)
