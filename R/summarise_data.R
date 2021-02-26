source("D:/OneDrive/University/Y4/Disertation/Data/R/packages.R", local = TRUE)


#' Summarise PBP file into key metrics per player per game style
#'
#' @param tib Raw pbp tibble or dataframe
#'
#' @return Summarised tibble
#'
#' @examples
summarise_pbp <- function(tib) {
  
  break_points <- c("0-40", "15-40", "30-40", "40-A")
  short_rally <- c("4", "3", "2")
  long_rally <- c("9+", "20+")
  
  return(
    tib %>% filter(is.na(`Player Game Style`) == FALSE) %>% group_by(
      Player, 
      `Player Game Style`,
    ) %>% summarise(
      matches = length(unique(`Match ID`)),
      points = length(unique(`Point ID`)),
      points_on_serve = as.double(length(unique(na.omit(if_else(Serving == 1, `Point ID`, NA_real_))))),
      points_on_return = as.double(length(unique(na.omit(if_else(Serving == 0, `Point ID`, NA_real_))))),
      serve_total = sum(if_else(Serving == 1, 1, 0)),
      serve_1_in = sum(if_else(Serving == 1 & Serve == "First Serve In", 1, 0)),
      serve_1_win = sum(if_else(Serving == 1 & Serve == "First Serve In" & PointWon == 1, 1, 0)),
      serve_2_in = sum(if_else(Serving == 1 & Serve == "Second Serve In", 1, 0)),
      serve_2_win = sum(if_else(Serving == 1 & Serve == "Second Serve In" & PointWon == 1, 1, 0)),
      df_total = sum(if_else(Serving == 1 & Serve == "Second Serve Out", 1, 0)),
      return_total = sum(if_else(Serving == 0, 1, 0)),
      return_1_total = sum(if_else(Serving == 0 & Serve == "First Serve In", 1, 0)),
      return_1_win = sum(if_else(Serving == 0 & Serve == "First Serve In" & PointWon == 1, 1, 0)),
      return_2_total = sum(if_else(Serving == 0 & Serve != "First Serve In", 1, 0)),
      return_2_win = sum(if_else(Serving == 0 & Serve != "First Serve In" & PointWon == 1, 1, 0)),
      break_point_total = sum(if_else(Serving == 0 & Scoreline %in% break_points, 1, 0)),
      break_point_win = sum(if_else(Serving == 0 & Scoreline %in% break_points & PointWon == 1, 1, 0)),
      shots_total = sum(na.omit(if_else(Serving == 1, ceiling(`Shot Count` / 2), floor(`Shot Count` / 2)))),
      shots_on_serve_total = sum(na.omit(if_else(Serving == 1, ceiling(`Shot Count` / 2), NA_real_))),
      shots_on_return_total = sum(na.omit(if_else(Serving == 0, floor(`Shot Count` / 2), NA_real_))),
      winners_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                    str_detect(finish, "Winner"), 1, 0), na.rm = TRUE),
      s1_aces_total = sum(if_else(Serve == "First Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                    str_detect(finish, "Ace"), 1, 0), na.rm = TRUE),
      s2_aces_total = sum(if_else(Serve == "Second Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                    str_detect(finish, "Ace"), 1, 0), na.rm = TRUE),
      s1_unreturned_total = sum(if_else(Serve == "First Serve In" & Serving == 1 & is.na(`Shot Count`) == FALSE & 
                                 str_detect(finish, "Return Error"), 1, 0), na.rm = TRUE),
      s2_unreturned_total = sum(if_else(Serve == "Second Serve In" & Serving == 1 & is.na(`Shot Count`) == FALSE & 
                                          str_detect(finish, "Return Error"), 1, 0), na.rm = TRUE),
      forced_errors_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                          finish == "Forced Error", 1, 0), na.rm = TRUE),
      unforced_errors_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                            finish == "Unforced Error", 1, 0), na.rm = TRUE),
      return_errors_total = sum(if_else(Serving == 0 & is.na(`Shot Count`) == FALSE & 
                                          str_detect(finish, "Return Error"), 1, 0), na.rm = TRUE),
      r1_winners_total = sum(if_else(Serve == "First Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                          finish == "Return Winner", 1, 0), na.rm = TRUE),
      r2_winners_total = sum(if_else(Serve == "Second Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                       finish == "Return Winner", 1, 0), na.rm = TRUE),
      short_rally_total = sum(if_else(`Rally Length` %in% short_rally | is.na(`Rally Length`), 1, 0)),
      medium_rally_total = sum(if_else(`Rally Length` == "5 to 8", 1, 0), na.rm = TRUE),
      long_rally_total = sum(if_else(`Rally Length` %in% long_rally, 1, 0)),
      short_rally_win = sum(if_else((`Rally Length` %in% short_rally | is.na(`Rally Length`)) & 
                                      PointWon == 1, 1, 0)),
      medium_rally_win = sum(if_else(`Rally Length` == "5 to 8" & PointWon == 1, 1, 0), na.rm = TRUE),
      long_rally_win = sum(if_else(`Rally Length` %in% long_rally & PointWon == 1, 1, 0)),
      approaches_total = sum(if_else(is.na(`Player Net Approach`) == FALSE, 1, 0)),
      intentional_approaches_total = sum(if_else(str_detect(`Player Net Approach`, "Intentional"), 1, 0), 
                                         na.rm = TRUE),
      reactive_approaches_total = sum(if_else(str_detect(`Player Net Approach`, "Reactive"), 1, 0), 
                                      na.rm = TRUE),
      approaches_win = sum(if_else(is.na(`Player Net Approach`) == FALSE & PointWon == 1, 1, 0)),
      drop_shots_total = sum(if_else(`Player Drop Shot` == "Drop Shot", 1, 0), na.rm = TRUE),
      s1_b3_attacking = sum(if_else(Serving == 1 & Serve == "First Serve In" & 
                                      `Ball 3 Situation` == "Attacking", 1, 0), na.rm = TRUE),
      s2_b3_attacking = sum(if_else(Serving == 1 & Serve == "Second Serve In" & 
                                      `Ball 3 Situation` == "Attacking", 1, 0), na.rm = TRUE),
      s2_b3_neutral = sum(if_else(Serving == 1 & Serve == "Second Serve In" & 
                                      `Ball 3 Situation` == "Neutral", 1, 0), na.rm = TRUE),
      r1_b3_defensive = sum(if_else(Serving == 0 & Serve == "First Serve In" & 
                                      `Ball 3 Situation` == "Defensive", 1, 0), na.rm = TRUE),
      r1_b3_neutral = sum(if_else(Serving == 0 & Serve == "First Serve In" & 
                                    `Ball 3 Situation` == "Neutral", 1, 0), na.rm = TRUE),
      r2_b3_defensive = sum(if_else(Serving == 0 & Serve == "Second Serve In" & 
                                      `Ball 3 Situation` == "Defensive", 1, 0), na.rm = TRUE),
      defensive_winners_total = sum(if_else(FinalShot == 1 & Situation == "Defensive" &
                                              str_detect(finish, "Winner"), 1, 0), na.rm = TRUE),
      attacking_errors_total = sum(if_else(FinalShot == 1 & Situation == "Attacking" &
                                             str_detect(finish, "Error"), 1, 0), na.rm = TRUE)
    ) %>% mutate(
      serve_1_in_pct = serve_1_in / serve_total,
      serve_1_win_pct = serve_1_win / serve_1_in,
      serve_2_win_pct = serve_2_win / (serve_total - serve_1_in),
      return_1_win_pct = return_1_win / return_1_total,
      return_2_win_pct = return_2_win / return_2_total,
      break_point_win_pct = break_point_win / break_point_total,
      winners_pct = winners_total/shots_total,
      forced_errors_pct = forced_errors_total/shots_total,
      unforced_errors_pct = unforced_errors_total/shots_total,
      return_errors_pct = return_errors_total/return_total,
      short_rally_pct = short_rally_win/short_rally_total,
      medium_rally_pct = medium_rally_win/medium_rally_total,
      long_rally_pct = long_rally_win/long_rally_total,
      intentional_approaches_pct = intentional_approaches_total/approaches_total,
      reactive_approaches_pct = reactive_approaches_total/approaches_total,
      approaches_win_pct = approaches_win/approaches_total,
      points_per_match = points/matches,
      
      approaches_per_rally = approaches_total / points,
      intentional_approaches_per_rally = intentional_approaches_total / points,
      drop_shots_per_rally = drop_shots_total / points,
      shots_per_serve = shots_on_serve_total / serve_total,
      shots_per_return = shots_on_return_total / return_total,
      aces_per_serve = (s1_aces_total + s2_aces_total) / serve_total,
      df_per_serve = df_total / serve_total,
      effective_s1 = (s1_b3_attacking + s1_aces_total + s1_unreturned_total) / serve_1_in,
      effective_s2 = (s2_b3_attacking + s2_b3_neutral + s2_aces_total + s2_unreturned_total) / (serve_2_in + df_total),
      effective_r1 = (r1_b3_defensive + r1_b3_neutral + r1_winners_total) / return_1_total,
      effective_r2 = (r2_b3_defensive + r2_winners_total) / return_2_total,
      defensive_winners_per_rally = defensive_winners_total / points,
      attacking_errors_per_rally = attacking_errors_total / points,
      ## TODO: Calculate rest of proportions
    )
  )
  
}

#' Summarise PBP file into key metrics per player per game style per match
#'
#' @param tib Raw pbp tibble or dataframe
#'
#' @return Summarised tibble
#'
#' @examples
summarise_pbp_match_specific <- function(tib) {
  
  break_points <- c("0-40", "15-40", "30-40", "40-A")
  short_rally <- c("4", "3", "2")
  long_rally <- c("9+", "20+")
  
  return(
    tib %>% filter(is.na(`Player Game Style`) == FALSE) %>% group_by(
      Player, 
      `Player Game Style`,
      `Match ID`,
      Date
    ) %>% summarise(
      matches = length(unique(`Match ID`)),
      points = length(unique(`Point ID`)),
      points_on_serve = as.double(length(unique(na.omit(if_else(Serving == 1, `Point ID`, NA_real_))))),
      points_on_return = as.double(length(unique(na.omit(if_else(Serving == 0, `Point ID`, NA_real_))))),
      serve_total = sum(if_else(Serving == 1, 1, 0)),
      serve_1_in = sum(if_else(Serving == 1 & Serve == "First Serve In", 1, 0)),
      serve_1_win = sum(if_else(Serving == 1 & Serve == "First Serve In" & PointWon == 1, 1, 0)),
      serve_2_in = sum(if_else(Serving == 1 & Serve == "Second Serve In", 1, 0)),
      serve_2_win = sum(if_else(Serving == 1 & Serve == "Second Serve In" & PointWon == 1, 1, 0)),
      df_total = sum(if_else(Serving == 1 & Serve == "Second Serve Out", 1, 0)),
      return_total = sum(if_else(Serving == 0, 1, 0)),
      return_1_total = sum(if_else(Serving == 0 & Serve == "First Serve In", 1, 0)),
      return_1_win = sum(if_else(Serving == 0 & Serve == "First Serve In" & PointWon == 1, 1, 0)),
      return_2_total = sum(if_else(Serving == 0 & Serve != "First Serve In", 1, 0)),
      return_2_win = sum(if_else(Serving == 0 & Serve != "First Serve In" & PointWon == 1, 1, 0)),
      break_point_total = sum(if_else(Serving == 0 & Scoreline %in% break_points, 1, 0)),
      break_point_win = sum(if_else(Serving == 0 & Scoreline %in% break_points & PointWon == 1, 1, 0)),
      shots_total = sum(na.omit(if_else(Serving == 1, ceiling(`Shot Count` / 2), floor(`Shot Count` / 2)))),
      shots_on_serve_total = sum(na.omit(if_else(Serving == 1, ceiling(`Shot Count` / 2), NA_real_))),
      shots_on_return_total = sum(na.omit(if_else(Serving == 0, floor(`Shot Count` / 2), NA_real_))),
      winners_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                    str_detect(finish, "Winner"), 1, 0), na.rm = TRUE),
      s1_aces_total = sum(if_else(Serve == "First Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                    str_detect(finish, "Ace"), 1, 0), na.rm = TRUE),
      s2_aces_total = sum(if_else(Serve == "Second Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                    str_detect(finish, "Ace"), 1, 0), na.rm = TRUE),
      s1_unreturned_total = sum(if_else(Serve == "First Serve In" & Serving == 1 & is.na(`Shot Count`) == FALSE & 
                                          str_detect(finish, "Return Error"), 1, 0), na.rm = TRUE),
      s2_unreturned_total = sum(if_else(Serve == "Second Serve In" & Serving == 1 & is.na(`Shot Count`) == FALSE & 
                                          str_detect(finish, "Return Error"), 1, 0), na.rm = TRUE),
      forced_errors_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                          finish == "Forced Error", 1, 0), na.rm = TRUE),
      unforced_errors_total = sum(if_else(FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                            finish == "Unforced Error", 1, 0), na.rm = TRUE),
      return_errors_total = sum(if_else(Serving == 0 & is.na(`Shot Count`) == FALSE & 
                                          str_detect(finish, "Return Error"), 1, 0), na.rm = TRUE),
      r1_winners_total = sum(if_else(Serve == "First Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                       finish == "Return Winner", 1, 0), na.rm = TRUE),
      r2_winners_total = sum(if_else(Serve == "Second Serve In" & FinalShot == 1 & is.na(`Shot Count`) == FALSE & 
                                       finish == "Return Winner", 1, 0), na.rm = TRUE),
      short_rally_total = sum(if_else(`Rally Length` %in% short_rally | is.na(`Rally Length`), 1, 0)),
      medium_rally_total = sum(if_else(`Rally Length` == "5 to 8", 1, 0), na.rm = TRUE),
      long_rally_total = sum(if_else(`Rally Length` %in% long_rally, 1, 0)),
      short_rally_win = sum(if_else((`Rally Length` %in% short_rally | is.na(`Rally Length`)) & 
                                      PointWon == 1, 1, 0)),
      medium_rally_win = sum(if_else(`Rally Length` == "5 to 8" & PointWon == 1, 1, 0), na.rm = TRUE),
      long_rally_win = sum(if_else(`Rally Length` %in% long_rally & PointWon == 1, 1, 0)),
      approaches_total = sum(if_else(is.na(`Player Net Approach`) == FALSE, 1, 0)),
      intentional_approaches_total = sum(if_else(str_detect(`Player Net Approach`, "Intentional"), 1, 0), 
                                         na.rm = TRUE),
      reactive_approaches_total = sum(if_else(str_detect(`Player Net Approach`, "Reactive"), 1, 0), 
                                      na.rm = TRUE),
      approaches_win = sum(if_else(is.na(`Player Net Approach`) == FALSE & PointWon == 1, 1, 0)),
      drop_shots_total = sum(if_else(`Player Drop Shot` == "Drop Shot", 1, 0), na.rm = TRUE),
      s1_b3_attacking = sum(if_else(Serving == 1 & Serve == "First Serve In" & 
                                      `Ball 3 Situation` == "Attacking", 1, 0), na.rm = TRUE),
      s2_b3_attacking = sum(if_else(Serving == 1 & Serve == "Second Serve In" & 
                                      `Ball 3 Situation` == "Attacking", 1, 0), na.rm = TRUE),
      s2_b3_neutral = sum(if_else(Serving == 1 & Serve == "Second Serve In" & 
                                    `Ball 3 Situation` == "Neutral", 1, 0), na.rm = TRUE),
      r1_b3_defensive = sum(if_else(Serving == 0 & Serve == "First Serve In" & 
                                      `Ball 3 Situation` == "Defensive", 1, 0), na.rm = TRUE),
      r1_b3_neutral = sum(if_else(Serving == 0 & Serve == "First Serve In" & 
                                    `Ball 3 Situation` == "Neutral", 1, 0), na.rm = TRUE),
      r2_b3_defensive = sum(if_else(Serving == 0 & Serve == "Second Serve In" & 
                                      `Ball 3 Situation` == "Defensive", 1, 0), na.rm = TRUE),
      defensive_winners_total = sum(if_else(FinalShot == 1 & Situation == "Defensive" &
                                              str_detect(finish, "Winner"), 1, 0), na.rm = TRUE),
      attacking_errors_total = sum(if_else(FinalShot == 1 & Situation == "Attacking" &
                                             str_detect(finish, "Error"), 1, 0), na.rm = TRUE)
    ) %>% mutate(
      serve_1_in_pct = serve_1_in / serve_total,
      serve_1_win_pct = serve_1_win / serve_1_in,
      serve_2_win_pct = serve_2_win / (serve_total - serve_1_in),
      return_1_win_pct = return_1_win / return_1_total,
      return_2_win_pct = return_2_win / return_2_total,
      break_point_win_pct = break_point_win / break_point_total,
      winners_pct = winners_total/shots_total,
      forced_errors_pct = forced_errors_total/shots_total,
      unforced_errors_pct = unforced_errors_total/shots_total,
      return_errors_pct = return_errors_total/return_total,
      short_rally_pct = short_rally_win/short_rally_total,
      medium_rally_pct = medium_rally_win/medium_rally_total,
      long_rally_pct = long_rally_win/long_rally_total,
      intentional_approaches_pct = intentional_approaches_total/approaches_total,
      reactive_approaches_pct = reactive_approaches_total/approaches_total,
      approaches_win_pct = approaches_win/approaches_total,
      points_per_match = points/matches,
      
      approaches_per_rally = approaches_total / points,
      intentional_approaches_per_rally = intentional_approaches_total / points,
      drop_shots_per_rally = drop_shots_total / points,
      shots_per_serve = shots_on_serve_total / serve_total,
      shots_per_return = shots_on_return_total / return_total,
      aces_per_serve = (s1_aces_total + s2_aces_total) / serve_total,
      df_per_serve = df_total / serve_total,
      effective_s1 = (s1_b3_attacking + s1_aces_total + s1_unreturned_total) / serve_1_in,
      effective_s2 = (s2_b3_attacking + s2_b3_neutral + s2_aces_total + s2_unreturned_total) / (serve_2_in + df_total),
      effective_r1 = (r1_b3_defensive + r1_b3_neutral + r1_winners_total) / return_1_total,
      effective_r2 = (r2_b3_defensive + r2_winners_total) / return_2_total,
      defensive_winners_per_rally = defensive_winners_total / points,
      attacking_errors_per_rally = attacking_errors_total / points,
      ## TODO: Calculate rest of proportions
    )
  )
  
}
#' Summarise SBS file into key metrics per player per game style
#'
#' @param tib Raw sbs tibble or dataframe
#'
#' @return Summarised tibble
#'
#' @examples
summarise_sbs <- function(tib) {
  
  ## TODO
  
}