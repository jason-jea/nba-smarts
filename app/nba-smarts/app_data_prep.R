create_yoy_df <- function(player_stats, active_players, prev_year, curr_year, point_values) {
  prev_year_str <- paste0(prev_year, "-", as.integer(substr(prev_year, 3, 4)) + 1)
  curr_year_str <- paste0(curr_year, "-", as.integer(substr(curr_year, 3, 4)) + 1)
  return(
    player_stats %>%
      filter(GROUP_VALUE %in% c(prev_year_str, curr_year_str)) %>%
      mutate(
        GROUP_VALUE = case_when(
          GROUP_VALUE ==prev_year_str ~ "prev_year",
          TRUE ~ "curr_year"
        )
      ) %>%
      inner_join(
        active_players %>% select(PLAYER_ID = PERSON_ID, PLAYER_POSITION, DISPLAY_FIRST_LAST),
        by = c("PLAYER_ID")
      ) %>%
      calculate_fantasy_points(
        pts_value = point_values$point_value,
        reb_value = point_values$reb_value,
        asst_value = point_values$asst_value,
        trey_value = point_values$trey_value,
        steal_value = point_values$steal_value,
        blk_value = point_values$blk_value,
        to_value = point_values$to_value,
        td3_value = point_values$td3_value,
        dd2_value = point_values$dd2_value,
      ) %>%
      group_by(
        name = DISPLAY_FIRST_LAST,
        PLAYER_POSITION,
        year = GROUP_VALUE
      ) %>%
      summarise(
        GP = sum(GP),
        points = sum(points)
      ) %>%
      ungroup %>%
      mutate(
        points_p_game = points/GP
      ) %>%
      select(-points, -GP) %>%
      spread(
        key = year,
        value = points_p_game
      )
  )
}
