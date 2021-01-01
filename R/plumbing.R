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

#' Unpacks the JSON response from a request to a stats.nba.com endpoint.  Attempts to return a clean dataframe with predictable column headers.
#' Always used in conjunction with .stats_request()
#' @param response Respronse content as a JSON string
#' @return Dataframe containing results from response.
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
#' final_df <- .unpack_stats_request(.stats_request(endpoint = endpoint_str, params = parameters))
#' }
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

    names(dataset) <- tolower(names(dataset))

    return(dataset)
  })

  return(final_df)
}

#' Calculate fantasy point total based on a standard set of categories.
#' @param df Dataframe containing totals of different statsitcal categories
#' @param ... Value associated with respective stastical category
#' @return Dataframe passed into df argument with one additional column named points, contianing the calculated fantasy points value of that row
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
        points = as.integer(pts)*pts_value + as.integer(reb)*reb_value + as.integer(fg3m)*trey_value +
          as.integer(ast)*asst_value - as.integer(tov)*tov_value + as.integer(stl)*stl_value +
          as.integer(blk)*blk_value + as.integer(dd2)*dd2_value + as.integer(td3)*td3_value
      )
  )
}

#' Constructs a season year string from a year integer
#' @param x integer representing year
#' @return Season year string
#' @examples
#' gen_season_year_str(2020)
gen_season_year_str <- function(x) {
  return(paste0(x, "-", as.integer(substr(x, 3, 4)) + 1))
}
