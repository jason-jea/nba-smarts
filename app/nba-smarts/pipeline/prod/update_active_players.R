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

active_players <- get_active_players()

creds <- get_app_credentials("app/nba-smarts/Data/env")
con <- create_db_conn(creds)

active_players_db <- tbl(con, in_schema("stats", "active_players"))
old_active_players <-
  active_players_db %>% collect()

new_players <- active_players %>%
  filter(!person_id %in% old_active_players$person_id)

widget_stats <- map_dfr(new_players[,"person_id"],function(x){
  print(match(x, new_players[,"person_id"]))
  stats <- get_nba_fantasy_widget_stats(PlayerID=x)
  Sys.sleep(1)
  return(stats)
})

new_players <-
  new_players %>%
  #mutate(person_id = as.integer(person_id)) %>%
  inner_join(widget_stats %>% select(player_id, player_position),by=c("person_id" = "player_id")) %>%
  mutate(person_id = as.integer(person_id))

db_upsert(con = con, target_tbl = c("stats", "active_players"), source_tbl = new_players, keys = c("person_id"))
