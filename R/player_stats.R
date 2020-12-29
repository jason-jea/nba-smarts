get_player_season_stats <- 
  function(
    DateFrom="",
    DateTo="",
    GameSegment="",
    LastNGames="0",
    LeagueID="",
    Location="",
    MeasureType="Base",
    Month="0",
    OpponentTeamID="0",
    Outcome="",
    PORound="",
    PaceAdjust="N",
    PerMode="Totals",
    Period="0",
    PlayerID,
    PlusMinus="N",
    Rank="N",
    Season,
    SeasonSegment="",
    SeasonType="Regular+Season",
    ShotClockRange="",
    VsConference="",
    VsDivision=""
  ) {
    endpoint_str <- "playerdashboardbyyearoveryear"
    
    if (missing(Season)) {
      Season <- paste0(year(Sys.Date()), "-", as.integer(substr(year(Sys.Date()), 1, 2)) + 1)
    }
    
    if (missing(PlayerID)) {
      stop("Please provide PlayerID.")
    }
    
    params <- c(
      DateFrom=DateFrom,
      DateTo=DateTo,
      GameSegment=GameSegment,
      LastNGames=LastNGames,
      LeagueID="",
      Location=Location,
      MeasureType=MeasureType,
      Month=Month,
      OpponentTeamID=OpponentTeamID,
      Outcome=Outcome,
      PORound=PORound,
      PaceAdjust=PaceAdjust,
      PerMode=PerMode,
      Period=Period,
      PlayerID=PlayerID,
      PlusMinus=PlusMinus,
      Rank=Rank,
      Season=Season,
      SeasonSegment=SeasonSegment,
      SeasonType=SeasonType,
      ShotClockRange=ShotClockRange,
      VsConference=VsConference,
      VsDivision=VsDivision
    )
    
    player_df <- .unpack_stats_request(.stats_request(endpoint = endpoint_str, params = params))
    
    player_df <- player_df %>%
      mutate_at(vars(-GROUP_SET, -GROUP_VALUE, -TEAM_ABBREVIATION, -MAX_GAME_DATE, -CFPARAMS), as.numeric) %>%
      mutate(PlayerID = PlayerID)
    return(player_df)
  }

get_nba_fantasy_widget_stats <- 
  function(
    ActivePlayers="Y",
    LastNGames=0,
    LeagueID="00",
    Season,
    SeasonType="Regular+Season",
    TodaysOpponent=0,
    TodaysPlayers="N",
    PlayerID
  ) {
    
    endpoint_str <- "fantasywidget"
    
    if (missing(Season)) {
      Season <- paste0(year(Sys.Date()), "-", as.integer(substr(year(Sys.Date()), 1, 2)) + 1)
    }
    
    if (missing(PlayerID)) {
      stop("Please provide PlayerID.")
    }
    
    params <- c(
      ActivePlayers=ActivePlayers,
      LastNGames=LastNGames,
      LeagueID=LeagueID,
      Season=Season,
      SeasonType=SeasonType,
      TodaysOpponent=TodaysOpponent,
      TodaysPlayers=TodaysPlayers,
      PlayerID=PlayerID
    )
    
    widget_df <- .unpack_stats_request(.stats_request(endpoint = endpoint_str, params = params)) %>%
      mutate(PlayerID = PlayerID)
    
    return(widget_df)
  }

get_player_gamelogs <- 
  function(
    DateFrom = "",
    DateTo = "",
    LastNGames = 0,
    LeagueID = "00",
    MeasureType = "Base",
    Month = 0,
    OpponentTeamID = 0,
    PerMode = "Totals",
    PlayerID,
    Season,
    SeasonType = "Regular+Season"
  ) {
    endpoint_str <- "playergamelogs"
    
    if (missing(Season)) {
      Season <- paste0(year(Sys.Date()), "-", as.integer(substr(year(Sys.Date()), 1, 2)) + 1)
    }
    
    if (missing(PlayerID)) {
      stop("Please provide PlayerID.")
    }
    
    params <- c(
      DateFrom = DateFrom,
      DateTo = DateTo,
      LastNGames = LastNGames,
      LeagueID = LeagueID,
      MeasureType = MeasureType,
      Month = Month,
      OpponentTeamID = OpponentTeamID,
      PerMode = PerMode,
      PlayerID = PlayerID,
      Season = Season,
      SeasonType = SeasonType
    )
    
    game_logs <- .unpack_stats_request(.stats_request(endpoint = endpoint_str, params = params)) %>%
      mutate_at(vars(-SEASON_YEAR, -PLAYER_NAME, -TEAM_ABBREVIATION, -TEAM_NAME, -GAME_DATE, -MATCHUP, -WL), as.numeric)
    
    return(game_logs)
  }
  
  