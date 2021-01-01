install.packages(c("httr", "dplyr", "tidyr", "jsonlite", "purrr", "stringr", "lubridate", "devtools"))

library(devtools)

install_github("jzj59/nba-smarts", ref = "main")
install_github("cloudyr/aws.s3", ref = "main")

library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(purrr)
library(stringr)
library(lubridate)
library(aws.s3)
library(nbasmarts)
library(RPostgreSQL)

source("R/utils.R")

season <- "2020"
test <- get_player_gamelogs(
  PlayerID = 203500,
  Season = paste0(season, "-", as.integer(substr(season, 3, 4)) + 1)
)

active_players <- get_active_players()

widget_stats <- map_dfr(active_players[,"PERSON_ID"],function(x){
  print(match(x, active_players[,"PERSON_ID"]))
  stats <- get_nba_fantasy_widget_stats(PlayerID=x)
  Sys.sleep(1)
  return(stats)
})

widget_stats <- widget_stats %>%
  mutate_at(vars(-PLAYER_NAME, -TEAM_ABBREVIATION, -PLAYER_POSITION), as.numeric)
names(widget_stats) <- tolower(names(widget_stats))

active_players <-
  active_players %>%
  mutate(PERSON_ID = as.integer(PERSON_ID)) %>%
  inner_join(widget_stats %>% select(PLAYER_ID, PLAYER_POSITION),by=c("PERSON_ID" = "PLAYER_ID"))

player_stats <- map_dfr(active_players[,"PERSON_ID"],function(x){
  stats <- get_player_season_stats(PlayerID=x) %>%
    filter(GROUP_SET == "By Year")

  Sys.sleep(2)
  return(stats)
})

player_stats %>%
  write.csv("~/nba-smarts/exploration/player_stats.csv", row.names = FALSE)

active_players %>%
  write.csv("~/nba-smarts/exploration/active_players.csv", row.names = FALSE)

aws_creds <- get_app_aws_credentials("~/nba-smarts/app/nba-smarts/Data/credentials")
put_object(
  file = "~/nba-smarts/exploration/player_stats.csv",
  object = "stats/player_stats.csv",
  bucket = "nba-smarts",
  key = aws_creds['aws_access_key_id'],
  secret = aws_creds['aws_secret_access_key']
)
put_object(
  file = "~/nba-smarts/exploration/active_players.csv",
  object = "stats/active_players.csv",
  bucket = "nba-smarts",
  key = aws_creds['aws_access_key_id'],
  secret = aws_creds['aws_secret_access_key']
)

ids <- unique(active_players$PERSON_ID)

seasons <- c("2019")

player_logs_p1 <-
  map_dfr(seasons, function(season) {
    map_dfr(ids[1:30], function(x){
      print(match(x, ids))
      print(paste0("getting log for player ", x, " in season ", season))

      df <- get_player_gamelogs(
        PlayerID = x,
        Season = paste0(season, "-", as.integer(substr(season, 3, 4)) + 1)
      )[,c(1:3,5,8:13,15:16,18:19,23:27,31,34:35)]

      Sys.sleep(2.2)

      return(df)
    })
  })
player_logs_p2 <-
  map_dfr(seasons, function(season) {
    map_dfr(ids[31:150], function(x){
      print(match(x, ids))
      print(paste0("getting log for player ", x, " in season ", season))

      df <- get_player_gamelogs(
        PlayerID = x,
        Season = paste0(season, "-", as.integer(substr(season, 3, 4)) + 1)
      )[,c(1:3,5,8:13,15:16,18:19,23:27,31,34:35)]

      Sys.sleep(2.2)

      return(df)
    })
  })
player_logs_p3 <-
  map_dfr(seasons, function(season) {
    map_dfr(ids[151:length(ids)], function(x){
      print(match(x, ids))
      print(paste0("getting log for player ", x, " in season ", season))

      df <- get_player_gamelogs(
        PlayerID = x,
        Season = paste0(season, "-", as.integer(substr(season, 3, 4)) + 1)
      )[,c(1:3,5,8:13,15:16,18:19,23:27,31,34:35)]

      Sys.sleep(2.2)

      return(df)
    })
  })
player_logs <- rbind.data.frame(player_logs_p1, player_logs_p2, player_logs_p3)
player_logs <- player_logs[,c(2,1,3:22)]
names(player_logs) <- tolower(names(player_logs))

creds <- get_app_credentials("~/nba-smarts/app/nba-smarts/Data/env")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(
  drv,
  dbname = creds["proddb_dbname"],
  host = creds["proddb_host"],
  port = creds["proddb_port"],
  user = creds["proddb_user"],
  password = creds["proddb_password"]
)

dbWriteTable(con, c("stats", "player_game_logs"), player_logs, row.names=FALSE, append=TRUE)

player_logs %>%
  write.csv("~/nba-smarts/exploration/player_logs.csv", row.names = FALSE)

put_object(
  file = "~/nba-smarts/exploration/player_logs.csv",
  object = "stats/player_logs.csv",
  bucket = "nba-smarts",
  key = creds['aws_access_key_id'],
  secret = creds['aws_secret_access_key']
)

names(active_players) <- tolower(names(active_players))
dbWriteTable(con, c("stats", "active_players"), active_players, row.names=FALSE, append=TRUE)

player_stats <- map_dfr(active_players[,"PERSON_ID"],function(x){
  stats <- get_player_season_stats(PlayerID=x) %>%
    filter(GROUP_SET == "By Year")
  return(stats)
})
player_stats <- player_stats[,c(66,2:8,10:12,14:15,17:18,20:26,30,33:34)]
names(player_stats) <- tolower(names(player_stats))
names(player_stats)[1] <- "player_id"
names(player_stats)[2] <- "season"
dbWriteTable(con, c("stats", "player_season_totals"), player_stats, row.names=FALSE, append=TRUE)
