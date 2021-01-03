library(R6)

FantasyTeam <- R6Class("FantasyTeam", list(
  team_id = NULL,
  initialize = function(team_id) {
    stopifnot(is.integer(team_id) | is.numeric(team_id))

    self$team_id <- team_id
  },
  get_team_name = function() {
    con <- establish_db_connection()
    f_team_id <- as.integer(self$team_id)
    name <- tbl(con, in_schema("fantasy", "teams")) %>% filter(id == f_team_id) %>% select(name) %>% collect
    return(name[1,1,drop=TRUE])
  },
  get_players = function(){
    con <- establish_db_connection()
    f_team_id <- as.integer(self$team_id)
    return(tbl(con, in_schema("stats", "active_players")) %>% filter(fantasy_team_id == f_team_id) %>% collect())
  },
  add_player = function(player_id) {
    con <- establish_db_connection()
    query <- sqlInterpolate(con,
      "update stats.active_players
      set fantasy_team_id = ?team_id
      where person_id = ?player_id",
      team_id = self$team_id,
      player_id = player_id
    )
    dbSendQuery(con, query)
  }
  )
)
