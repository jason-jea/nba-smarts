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
library(dbplyr)

source("app/nba-smarts/R/utils.R")

creds <- get_app_credentials("app/nba-smarts/Data/env")
con <- create_db_conn(creds)

active_players_db <- tbl(con, in_schema("stats", "active_players"))
active_players <-
  active_players_db %>% collect()

ids <- unique(active_players$person_id)

if (month(Sys.Date()) < 9) {
  season <- year(Sys.Date()) - 1
} else {
  season <- year(Sys.Date())
}

max_game_date <- dbGetQuery(con, "select max(game_date) as max_game_date from stats.player_game_logs")[,1]

player_logs <-
  map_dfr(ids, function(x){
    print(match(x, ids))
    print(paste0("getting log for player ", x, " in season ", season))

    df <- get_player_gamelogs(
      PlayerID = x,
      DateFrom = URLencode(format(max_game_date - 1, "%m/%d/%Y"), reserved = TRUE),
      Season = paste0(season, "-", as.integer(substr(season, 3, 4)) + 1)
    )[,c(1:3,5,8:13,15:16,18:19,23:27,31,34:35)]

    #Sys.sleep(2)

    return(df)
  })

player_logs <- player_logs[,c(2,1,3:22)]
player_logs$game_date <- as.Date(player_logs$game_date)

db_upsert(con = con, target_tbl = c("stats", "player_game_logs"), player_logs, keys = c("player_id", "matchup", "game_date"))

query <- "
truncate table stats.player_season_totals;
insert into stats.player_season_totals
select player_id, season_year,
 team_id,
team_abbreviation,
max(game_date) as max_game_date,
count(*) as gp,
count(case when wl = 'W' then 1 else null end) as w,
count(case when wl = 'L' then 1 else null end) as l,
sum(\"min\") as \"min\",
sum(fgm) as fgm,
sum(fga) as fga,
sum(fg3m) as fg3m,
sum(fg3a) as fg3a,
sum(ftm) as ftm,
sum(fta) as fta,
null::integer as oreb,
null::integer as dreb,
sum(reb) as reb,
sum(ast) as ast,
sum(tov) as tov,
sum(stl) as stl,
sum(blk) as blk,
sum(pts) as pts,
sum(dd2) as dd2,
sum(td3) as td3
from stats.player_game_logs
join (select distinct team_id, team_abbreviation from stats.active_players) a using(team_abbreviation)
group by 1,2,3,4
 ;
"
dbSendQuery(con = con, query)
