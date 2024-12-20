
devtools::install_github("saberpowers/sabRmetrics", ref = "v1.1.0")

cluster <- parallel::makeCluster(parallel::detectCores())
pitcher_season_summary <- sabRmetrics::download_season_summary(
  year = 2024:2009,
  level = c("MLB", "AAA", "AA", "A+", "A", "SS", "CL", "DSL"),
  position = "pitching",
  cl = cluster
)
parallel::stopCluster(cluster)

pitcher_season_summary |>
  dplyr::select(
    year, level, league_id, player_id,
    games = gamesPitched, batters_faced = battersFaced, pitches = numberOfPitches
  ) |>
  data.table::fwrite(file = "data/pitcher_season_summary.csv", append = FALSE)

