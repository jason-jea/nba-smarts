#' Generic function handling GET requests to stats.nba.com endpoints.
#' Note that headers are set to mimic https://www.nba.com on a browser making requests to load stats.
#' @param endpoint name of stats.nba.com endpoint
#' @param params named vector containing parameters
#' @return http GET response object contain the resource (aka endpint) name, list of parameters, and a list of results
#' @examples
#' \dontrun{
#' parameters <- c(
#'   "DateFrom"="",
#'   "DateTo"="",
#'   "GameSegment"="",
#'   "LastNGames"="0",
#'   "LeagueID"="",
#'   "Location"="",
#'   "MeasureType"="Base",
#'   "Month"="0",
#'   "OpponentTeamID"="0",
#'   "Outcome"="",
#'   "PORound"="",
#'   "PaceAdjust"="N",
#'   "PerMode"="Totals",
#'   "Period"="0",
#'   "PlayerID"="2544",
#'   "PlusMinus"="N",
#'   "Rank"="N",
#'   "Season"="2019-20",
#'   "SeasonSegment"="",
#'   "SeasonType"="Regular+Season",
#'   "ShotClockRange"="",
#'   "VsConference"="",
#'   "VsDivision"=""
#' )
#'
#' endpoint_str <- "playerdashboardbyyearoveryear"
#'
#' .stats_request(endpoint = endpoint_str, params = parameters)
#' }
.stats_request <- function(endpoint, params) {
  url <- "https://stats.nba.com/stats/"
  param_str = paste0(paste0(names(params), "=", params), collapse = "&")
  url <- paste0(url, endpoint, "?", param_str)

  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.50 Safari/537.36',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https://www.nba.com/',
    `Origin` = 'https://www.nba.com',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )

  response <- fromJSON(content(GET(url, add_headers(headers)), as = "text"))

  return(response)
}

.unpack_stats_request <- function(response) {
  stage_df <- response$resultSets

  # stats.nba.com request reponse is a dataframe of dataframes, where each row represents a dataset.
  # This is because a request could return multiple TYPES of datasets, such as stats by year, and then also a career total dataset.
  # The columns seem to be consistent:
  # 1) column of vectors named "name", representing the name of the dataset
  # 2) column of vector lists named "headers", where each element is a vector of characters that map to column headers for that particular dataset.
  # 3) column of
  final_df <- pmap_df(stage_df, function(name, headers, rowSet){
    if (length(rowSet) == 0) {
      dataset <- data.frame(t(headers), stringsAsFactors = FALSE)
      colnames(dataset) <- c(headers)
      dataset <- dataset[FALSE,]
    }
    else {
      dataset <- data.frame(rowSet, stringsAsFactors = FALSE)
      colnames(dataset) <- headers
    }

    return(dataset)
  })

  return(final_df)
}

calculate_fantasy_points <- function(
  df,
  pts_value = .5,
  reb_value = 1,
  asst_value = 1,
  trey_value = .5,
  tov_value = -1,
  stl_value = 2,
  blk_value = 2,
  dd2_value = 3,
  td3_value = 3
) {

  return(
    df %>%
      mutate(
        points = as.integer(PTS)*pts_value + as.integer(REB)*reb_value + as.integer(FG3M)*trey_value +
          as.integer(AST)*asst_value - as.integer(TOV)*tov_value + as.integer(STL)*stl_value +
          as.integer(BLK)*blk_value + as.integer(DD2)*dd2_value + as.integer(TD3)*td3_value
      )
  )
}

gen_season_year_str <- function(x) {
  return(paste0(x, "-", as.integer(substr(x, 3, 4)) + 1))
}
