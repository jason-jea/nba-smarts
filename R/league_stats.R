get_active_players <- function(IsOnlyCurrentSeason=1, LeagueID="00", Season) {
  endpoint_str <- "commonallplayers"

  if (missing(Season)) {
    if (month(Sys.Date()) >= 1 & month(Sys.Date()) < 9) {
      year <- year(Sys.Date()) - 1
    } else {
      year <- year(Sys.Date())
    }
    Season <- paste0(year, "-", as.integer(substr(year, 3, 4)) + 1)
  }

  params <- c(
    IsOnlyCurrentSeason = IsOnlyCurrentSeason,
    LeagueID = LeagueID,
    Season = Season
  )

  all_players_df <- .unpack_stats_request(.stats_request(endpoint = endpoint_str, params = params))

  return(all_players_df)
}
