create_yoy_df <- function(player_stats, active_players, prev_year, curr_year, point_values) {

  prev_year_str <- paste0(prev_year, "-", as.integer(substr(prev_year, 3, 4)) + 1)
  curr_year_str <- paste0(curr_year, "-", as.integer(substr(curr_year, 3, 4)) + 1)
  return(
    player_stats %>%
      filter(season %in% c(prev_year_str, curr_year_str)) %>%
      mutate(
        season = case_when(
          season == prev_year_str ~ "prev_year",
          TRUE ~ "curr_year"
        )
      ) %>%
      inner_join(
        active_players %>% select(player_id = person_id, player_position, display_first_last),
        by = c("player_id")
      ) %>%
      calculate_fantasy_points(
        df = .,
        pts_value = point_values$pts_value,
        reb_value = point_values$reb_value,
        asst_value = point_values$asst_value,
        trey_value = point_values$trey_value,
        stl_value = point_values$stl_value,
        blk_value = point_values$blk_value,
        tov_value = point_values$tov_value,
        td3_value = point_values$td3_value,
        dd2_value = point_values$dd2_value
      ) %>%
      group_by(
        name = display_first_last,
        player_position,
        year = season
      ) %>%
      summarise(
        gp = sum(gp),
        points = sum(points)
      ) %>%
      ungroup %>%
      mutate(
        points_p_game = points/gp
      ) %>%
      select(-points, -gp) %>%
      spread(
        key = year,
        value = points_p_game
      )
  )
}
