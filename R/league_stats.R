get_active_players <- function(IsOnlyCurrentSeason=1, LeagueID="00", Season) {
  endpoint_str <- "commonallplayers"

  if (missing(Season)) {
    Season <- paste0(year(Sys.Date()), "-", as.integer(substr(year(Sys.Date()), 3, 4)) + 1)
  }

  params <- c(
    IsOnlyCurrentSeason = IsOnlyCurrentSeason,
    LeagueID = LeagueID,
    Season = Season
  )
  all_players_df <- .unpack_stats_request(.stats_request(endpoint = endpoint_str, params = params))

  return(all_players_df)
}
