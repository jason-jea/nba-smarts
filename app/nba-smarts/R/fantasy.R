FantasyTeam <- R6Class("FantasyTeam", list(
  team_id = NULL,
  initialize = function(team_id) {
    stopifnot(is.integer(team_id) | is.numeric(team_id))

    self$team_id <- team_id
  },
  get_team_name = function() {
    con <- establish_db_connection()
    team_id <- as.integer(self$team_id)
    name <- tbl(con, in_schema("fantasy", "teams")) %>% filter(id == team_id) %>% select(name) %>% collect
    return(name[1,1,drop=TRUE])
  },
  add_player = function(player_id) {
    con <- establish_db_connection()
    query <- sqlInterpolate(con,
      "update fantasy.players
      set team_id = ?team_id
      where player_id = ?player_id",
      team_id = self$team_id,
      player_id = player_id
    )
    dbSendQuery(con, query)
  }
  )
)
