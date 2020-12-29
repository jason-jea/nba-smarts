library(httr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)
library(lubridate)

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
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv =72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https://www.nba.com/',
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

calculate_fantasy_points <- function(df) {
  return(
    df %>%
      mutate(
        points = as.integer(PTS)*.5 + as.integer(REB) + as.integer(AST) - as.integer(TOV) + as.integer(STL)*2 + as.integer(BLK)*2 + as.integer(DD2)*3 + as.integer(TD3)*3 
      )
  )
}
