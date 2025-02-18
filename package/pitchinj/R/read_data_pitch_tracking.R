#' Read pitch tracking data
#' 
#' This function reads pitch tracking data from three different sources and consolidates it into
#' one row per pitcher-year.
#' 
#' @return a table of primary fastball velo indexed by pitcher_id and year
#' 
#' @export
#' 
read_data_pitch_tracking <- function() {

  data_majors <- data.table::fread("data/pitch_tracking/majors_hawkeye.csv") |>
    dplyr::transmute(
      player_id = pitcher, year, level = "majors", pitches = TotalPitches, fb_velo = PrimaryFB_Velo
    )

  data_minors_hawkeye <- data.table::fread("data/pitch_tracking/minors_hawkeye.csv") |>
    dplyr::transmute(
      player_id = pitcher, year, level = "minors", vendor = "he",
      pitches = TotalPitches, fb_velo = PrimaryFB_Velo
    )

  data_minors_trackman <- data.table::fread("data/pitch_tracking/minors_trackman.csv") |>
    dplyr::transmute(
      player_id = pitcher_id, year, level = "minors", vendor = "tm",
      pitches = TotalPitches, fb_velo = PrimaryFB_Velo
    )

  data_minors_wide <- dplyr::bind_rows(data_minors_hawkeye, data_minors_trackman) |>
    dplyr::filter(!is.na(fb_velo)) |>
    tidyr::pivot_longer(cols = c("pitches", "fb_velo")) |>
    dplyr::mutate(name = paste(name, vendor, sep = "_")) |>
    dplyr::select(-vendor) |>
    tidyr::pivot_wider()

  data_minors <- data_minors_wide |>
    # Players with a discrepancy of more than 1 mph are suspicious and may be mapping errors
    # After some more data cleaning, maybe we can remove this filter
    dplyr::filter(is.na(fb_velo_he) | is.na(fb_velo_tm) | abs(fb_velo_he - fb_velo_tm) < 1) |>
    tidyr::replace_na(list(pitches_he = 0, pitches_tm = 0, fb_velo_he = 0, fb_velo_tm = 0)) |>
    dplyr::transmute(
      player_id, year, level,
      pitches = pitches_he + pitches_tm,
      fb_velo = (pitches_he * fb_velo_he + pitches_tm * fb_velo_tm) / (pitches_he + pitches_tm)
    )

  data_pitch_tracking_wide <- dplyr::bind_rows(data_majors, data_minors) |>
    dplyr::filter(!is.na(fb_velo)) |>
    tidyr::pivot_longer(cols = c("pitches", "fb_velo")) |>
    dplyr::mutate(name = paste(name, level, sep = "_")) |>
    dplyr::select(-level) |>
    tidyr::pivot_wider()

  data_pitch_tracking <- data_pitch_tracking_wide |>
    # We see bigger discrepancies between majors and minors FB velo, but anything more than 2 mph
    # may mean that a different pitch type has been classified as the primary fastball.
    dplyr::filter(
      is.na(fb_velo_majors) | is.na(fb_velo_minors) | abs(fb_velo_majors - fb_velo_minors) < 2
    ) |>
    tidyr::replace_na(
      list(pitches_majors = 0, pitches_minors = 0, fb_velo_majors = 0, fb_velo_minors = 0)
    ) |>
    dplyr::transmute(
      player_id, year,
      pitches = pitches_minors + pitches_majors,
      fb_velo = (pitches_majors * fb_velo_majors + pitches_minors * fb_velo_minors) /
        (pitches_majors + pitches_minors)
    )

  return(data_pitch_tracking)
}
