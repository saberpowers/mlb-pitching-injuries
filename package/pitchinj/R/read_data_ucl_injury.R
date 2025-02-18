#' Read UCL injury data
#' 
#' This function reads UCL injury data from multiple sources and combines them.
#' 
#' @return a table of UCL injuries indexed by player_id and year
#' 
#' @export
#' 
read_data_ucl_injury <- function() {

  tommy_john_jroegele <- data.table::fread("data/ucl_injury_jroegele/tommy_john.csv") |>
    dplyr::mutate(surgery = "tommy_john")
  internal_brace_jroegele <- data.table::fread("data/ucl_injury_jroegele/internal_brace.csv") |>
    dplyr::mutate(surgery = "internal_brace")
  ucl_injury_jroegele <- tommy_john_jroegele |>
    dplyr::filter(position == "P") |>
    dplyr::bind_rows(internal_brace_jroegele) |>
    dplyr::mutate(
      date_surgery = as.Date(date_surgery, format = "%m/%d/%y"),
      year = lubridate::year(date_surgery)
    ) |>
    dplyr::select(player_id, level, date_surgery, surgery, year)

  ucl_injury_reported <- data.table::fread("data/ucl_injury.csv") |>
    dplyr::mutate(date_surgery = as.Date(date_surgery, format = "%m/%d/%y"))

  ucl_injury <- ucl_injury_reported |>
    dplyr::full_join(ucl_injury_jroegele,
      by = c("player_id", "year"),
      suffix = c("_reported", "_jroegele")
    ) |>
    dplyr::transmute(
      player_id, year,
      date_surgery = dplyr::coalesce(date_surgery_reported, date_surgery_jroegele),
      level = dplyr::case_when(
        !is.na(level_reported) ~ level_reported,
        level_jroegele == "MLB" ~ "Major",
        level_jroegele %in% c("AAA", "AA", "A+", "A", "A-", "Rk") ~ "Minor",
        level_jroegele %in% c("Coll", "HS") ~ "Amateur"
      )
    )

  return(ucl_injury)
}